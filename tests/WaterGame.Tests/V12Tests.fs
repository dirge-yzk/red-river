module WaterGame.Tests.V12Tests

open Xunit
open WaterGame.Core
open WaterGame.App
open WaterGame.App.Model

let near (a: float) (b: float) = abs (a - b) <= 1e-6 * max 1.0 (abs a)

let normal = { Scenarios.normalYear with Construction = None } // 工事が、発注した季節に完成する (資材・建設は V17Tests で確かめる)

let policyFor (sc: Scenario) (state: GameState) =
    let season = Season.ofTurn state.Turn

    { AgTargetWU = Turn.effectiveAgDemand sc state season
      UrbanTargetWU = Turn.effectiveUrbanDemand sc state state.Turn
      EnvTargetWU = sc.MinEnvFlowWU[season]
      Priority = state.Priority
      PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 0.0) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = Accurate
      Action = NoAction }

/// 指定の施設が完成済みの状態から始める
let withBuilt (built: Investment list) (s: GameState) =
    { s with Invested = Set.union s.Invested (Set.ofList built) }

let allBuilt (sc: Scenario) (s: GameState) =
    s
    |> withBuilt (
        [ Reservoir; RainwaterHarvest; EffluentReuse; WaterSaving; IrrigationEfficiency ]
        @ [ for z in sc.Zones -> ObservationWell z.Id ]
    )
    |> fun s -> { s with Wells = s.Wells |> Map.map (fun _ _ -> WellBuilt); Survey = s.Survey |> Map.map (fun _ _ -> Reconnoitered) }

// ---------- 貯水池 (A1) ----------

[<Fact>]
let ``貯水池の動きは 蒸発 貯留 放流の範囲で 容量と貯留量を超えない`` () =
    let r = Scenarios.normalYear.Reservoir.Value

    for op in [ -5000.0; -300.0; 0.0; 200.0; 1500.0; 99999.0 ] do
        for start in [ 0.0; 500.0; 2800.0; 3000.0 ] do
            let f = Hydrology.reservoirStep r.CapacityWU 0.08 start 1200.0 op
            Assert.True(f.EndWU >= -1e-9 && f.EndWU <= r.CapacityWU + 1e-9, $"op={op} start={start} end={f.EndWU}")
            Assert.True(near f.EvaporationWU (start * 0.08))
            Assert.True(f.StoredWU <= 1200.0 + 1e-9)
            Assert.True(f.StoredWU * f.ReleasedWU = 0.0)
            Assert.True(near f.EndWU (start - f.EvaporationWU + f.StoredWU - f.ReleasedWU))

[<Fact>]
let ``貯水池を建てた季節は使えず 次の季節から使える`` () =
    let sc = normal
    let w = Weather.generate sc 7
    let s0 = Turn.initialState sc
    // 1年分の資金を与える
    let s0 = { s0 with Balance = 5000.0 }
    let p = { policyFor sc s0 with Action = Invest Reservoir; ReservoirOpWU = 1000.0 }
    let s1, r1 = Turn.resolve sc s0 p w[0]
    Assert.True(r1.ReservoirFlow.IsNone)
    Assert.True(s1.Invested.Contains Reservoir)
    Assert.Equal(0.0, s1.ReservoirWU)
    let s2, r2 = Turn.resolve sc s1 { policyFor sc s1 with ReservoirOpWU = 800.0 } w[1]
    Assert.True(r2.ReservoirFlow.IsSome)
    Assert.True(r2.ReservoirFlow.Value.StoredWU > 0.0)
    Assert.True(s2.ReservoirWU > 0.0)

[<Fact>]
let ``貯水池に貯めるほど 取水点の利用可能量と下流流量が減り 放流すると増える`` () =
    let sc = normal
    let w = Weather.generate sc 3
    let s0 = Turn.initialState sc |> withBuilt [ Reservoir ]
    let s0 = { s0 with ReservoirWU = 2000.0 }
    let resolve op = Turn.resolve sc s0 { policyFor sc s0 with ReservoirOpWU = op } w[0] |> snd
    let hold, none, rel = resolve 800.0, resolve 0.0, resolve -800.0
    Assert.True(hold.Allocation.AvailableWU < none.Allocation.AvailableWU)
    Assert.True(rel.Allocation.AvailableWU > none.Allocation.AvailableWU)
    Assert.True(near (none.Allocation.AvailableWU - hold.Allocation.AvailableWU) hold.ReservoirFlow.Value.StoredWU)

[<Fact>]
let ``貯水池がない状態での操作は無視され 水収支は崩れない`` () =
    let sc = normal
    let w = Weather.generate sc 3
    let s0 = Turn.initialState sc
    let _, r = Turn.resolve sc s0 { policyFor sc s0 with ReservoirOpWU = 900.0 } w[0]
    Assert.True(r.ReservoirFlow.IsNone)
    Assert.Equal(0.0, r.Policy.ReservoirOpWU)

// ---------- 河川から地下水への浸透 (A6) ----------

[<Fact>]
let ``浸透の強さは 地下水位が低いほど大きく 公開範囲の1.5倍以上なら0`` () =
    let a = Groundwater.spec normal Farm
    let gref = (fst a.InitialGRangeWU + snd a.InitialGRangeWU) / 2.0
    Assert.Equal(0.0, Groundwater.seepageStrength a (1.5 * gref))
    Assert.Equal(0.0, Groundwater.seepageStrength a (3.0 * gref))
    Assert.Equal(1.0, Groundwater.seepageStrength a (0.5 * gref))
    Assert.Equal(1.0, Groundwater.seepageStrength a 0.0)
    Assert.True(Groundwater.seepageStrength a gref > Groundwater.seepageStrength a (1.2 * gref))

[<Fact>]
let ``地下水位が低いと農地で川の水が地下水に吸われ 下流の流量が減る`` () =
    let sc = normal
    let w = Weather.generate sc 4
    let s0 = Turn.initialState sc

    let at factor =
        let gw = s0.GwWU |> Map.map (fun z _ -> (let a = Groundwater.spec sc z in (fst a.InitialGRangeWU + snd a.InitialGRangeWU) / 2.0 * factor))
        Turn.resolve sc { s0 with GwWU = gw } (policyFor sc s0) w[0] |> snd

    let low, high = at 0.4, at 2.0
    let seep (r: TurnResult) z = (r.Zones |> List.find (fun x -> x.Zone = z)).SeepageWU
    Assert.True(seep low Farm > 0.0)
    Assert.True(seep low Urban > 0.0)
    Assert.Equal(0.0, seep high Farm)
    // 森林は流れ込む河川水がなく 湿地は配分地点より下流なので 浸透しない
    Assert.Equal(0.0, seep low Forest)
    Assert.Equal(0.0, seep low Wetland)
    Assert.True(low.Balance.IsWithinTolerance && high.Balance.IsWithinTolerance)

[<Fact>]
let ``浸透した分は 地下水の貯留量に加わり 区域の収支も一致する`` () =
    let sc = normal
    let w = Weather.generate sc 4
    let s0 = Turn.initialState sc
    let low = { s0 with GwWU = s0.GwWU |> Map.map (fun _ g -> g * 0.3) }
    let _, r = Turn.resolve sc low (policyFor sc low) w[0]

    for z in r.Zones do
        Assert.True(near z.GwEndWU (max 0.0 (z.GwStartWU + z.DeepDrainageWU + z.SeepageWU - z.PumpedWU - z.DischargeWU)))
        Assert.True(abs z.ResidualWU < 1e-6, $"{z.Zone} residual={z.ResidualWU}")

// ---------- 再生水・雨水利用・高効率ポンプ・井戸の増強 (A7) ----------

[<Fact>]
let ``再生水を入れると 供給した生活産業の河川水の一部が下流へ戻り 収支は一致する`` () =
    let sc = normal
    let w = Weather.generate sc 5
    let s0 = Turn.initialState sc
    let _, base0 = Turn.resolve sc s0 (policyFor sc s0) w[0]
    let withEff = s0 |> withBuilt [ EffluentReuse ]
    let _, r = Turn.resolve sc withEff (policyFor sc withEff) w[0]
    Assert.True(near r.Allocation.ReturnFlowWU (r.Allocation.UrbanSupplyWU * sc.Invest.EffluentReturnRate))
    Assert.True(near r.Allocation.DownstreamFlowWU (base0.Allocation.DownstreamFlowWU + r.Allocation.ReturnFlowWU))
    Assert.True(r.Balance.IsWithinTolerance)
    Assert.True(near r.Balance.ExternalSupplyWU (base0.Balance.ExternalSupplyWU - r.Allocation.ReturnFlowWU))

[<Fact>]
let ``雨水利用と節水は需要を減らし 観測井戸のある区域は揚水費が下がる`` () =
    let sc = normal
    let s0 = Turn.initialState sc
    let base0 = Turn.effectiveUrbanDemand sc s0 0
    let rain = s0 |> withBuilt [ RainwaterHarvest ]
    Assert.True(near (Turn.effectiveUrbanDemand sc rain 0) (base0 * (1.0 - sc.Invest.RainwaterDemandReduction)))
    // 節水キャンペーン: 農業・生活産業の両方
    let sv = s0 |> withBuilt [ WaterSaving ]
    let k = 1.0 - sc.Invest.WaterSavingDemandReduction
    Assert.True(near (Turn.effectiveUrbanDemand sc sv 0) (base0 * k))
    Assert.True(near (Turn.effectiveAgDemand sc sv Summer) (sc.AgDemandWU[Summer] * k))

    // 同じ揚水で、観測井戸のある区域の揚水費だけが下がる
    let w = Weather.generate sc 5
    let wells = { s0 with Wells = s0.Wells |> Map.map (fun _ _ -> WellBuilt) }
    let big = { policyFor sc wells with AgTargetWU = 0.0; UrbanTargetWU = 0.0; PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 5000.0) |> Map.ofList }
    let _, a = Turn.resolve sc wells big w[1]
    let obs = wells |> withBuilt [ ObservationWell Farm ]
    let _, b = Turn.resolve sc obs big w[1]
    let farmPumped = (a.Zones |> List.find (fun z -> z.Zone = Farm)).PumpedWU
    Assert.True(a.Costs.Pumping > 0.0 && farmPumped > 0.0)
    Assert.True(b.Costs.Pumping < a.Costs.Pumping)
    Assert.True(near a.GwSupplyWU b.GwSupplyWU)

[<Fact>]
let ``貯水池は貯水池のあるシナリオだけ選べる`` () =
    let sc = normal
    let s0 = { Turn.initialState sc with Balance = 9000.0 }
    let tut = Scenarios.tutorial
    let t0 = { Turn.initialState tut with Balance = 9000.0 }
    Assert.False(Turn.isValidAction tut t0 (Invest Reservoir))
    Assert.True(Turn.isValidAction sc s0 (Invest Reservoir))

// ---------- 観測井戸 (A2) ----------

[<Fact>]
let ``観測井戸のある区域は 季節ごとに推定の幅が狭まり 下限で止まる`` () =
    let sc = normal
    let w = Weather.generate sc 6
    let mutable s = Turn.initialState sc |> withBuilt [ ObservationWell Farm ]
    let mutable prev = s.Estimates[Farm].RelErr
    let other0 = s.Estimates[Forest].RelErr

    for t in 0..7 do
        let s', _ = Turn.resolve sc s (policyFor sc s) w[t]
        let e = s'.Estimates[Farm].RelErr
        Assert.True(e <= prev + 1e-12)
        Assert.True(e >= sc.Invest.ObservationFloor - 1e-12)
        prev <- e
        s <- s'

    Assert.True(near prev sc.Invest.ObservationFloor)
    // 観測井戸のない区域は 変わらない
    Assert.Equal(other0, s.Estimates[Forest].RelErr)
    // 推定は 実際の値の下限幅の内側に入る
    let actual = s.GwWU[Farm]
    Assert.True(abs (s.Estimates[Farm].GwWU - actual) <= actual * sc.Invest.ObservationFloor * 1.0001 + 1e-6)

[<Fact>]
let ``観測井戸を設置した季節は 推定が変わらず 次の季節から効く`` () =
    let sc = normal
    let w = Weather.generate sc 6
    let s0 = { Turn.initialState sc with Balance = 3000.0 }
    let s1, r1 = Turn.resolve sc s0 { policyFor sc s0 with Action = Invest(ObservationWell Farm) } w[0]
    Assert.True(r1.Observations.IsEmpty)
    let _, r2 = Turn.resolve sc s1 (policyFor sc s1) w[1]
    Assert.Equal(1, r2.Observations.Length)

[<Fact>]
let ``観測井戸のほうが精度が高ければ 調査の結果で推定は悪化しない`` () =
    let sc = normal
    let h = (Turn.initialState sc).Hidden[Farm]
    let better = { GwWU = 1000.0; YieldWU = 800.0; RelErr = 0.05 }
    let est, rep = Groundwater.applySurvey sc.Costs h 1000.0 Reconnaissance better
    Assert.Equal(better, est)
    Assert.Equal(0.05, rep.RelErr)

// ---------- できごと (B1) ----------

[<Fact>]
let ``できごとはシナリオの日程どおりで 乱数を使わず 気象とは独立`` () =
    let sc = Scenarios.longTrial
    Assert.Equal<Map<int, EventCard>>(Map.ofList sc.EventSchedule, Events.generate sc)

    let noEvents = { sc with EventSchedule = [] }
    Assert.Equal<ActualWeather list>(Weather.generate sc 5, Weather.generate noEvents 5)
    // 日程のターンは、ゲームの長さに収まり、重ならない
    Assert.True(sc.EventSchedule |> List.forall (fun (t, _) -> t >= 0 && t < sc.Turns))
    Assert.Equal(sc.EventSchedule.Length, sc.EventSchedule |> List.map fst |> List.distinct |> List.length)

[<Fact>]
let ``できごとが起こるのは 長期の試練と渇水だけで 平年や入門では起こらない`` () =
    for sc in Scenarios.all do
        let total = (Events.generate sc).Count

        if sc.Name = "長期の試練" || sc.Name = "渇水" then
            Assert.True(total > 0, sc.Name)
        else
            Assert.Equal(0, total)

[<Fact>]
let ``できごとの効果は 需要 交付金 揚水能力 揚水費に反映される`` () =
    let sc = normal
    let s0 = Turn.initialState sc

    let card kind _season =
        { Name = "t"
          Description = "t"
          Kind = kind }

    let at turn kind =
        { s0 with Turn = turn; Events = Map.ofList [ turn, card kind None ] }

    // 交付金 (春)
    let g = at 0 (GrantMultiplier 0.5)
    Assert.True(near (Turn.grantThisTurn sc g) (sc.Finance.AnnualGrant * 0.5))
    Assert.True(near (Turn.availableFunds sc g) (s0.Balance + sc.Finance.AnnualGrant * 0.5))
    // 臨時の補助金 (春以外でも加わる)
    let x = at 1 (ExtraFunds 600.0)
    Assert.True(near (Turn.grantThisTurn sc x) 600.0)
    // 需要
    let ag = at 1 (AgDemandMultiplier 1.25)
    Assert.True(near (Turn.effectiveAgDemand sc ag Summer) (sc.AgDemandWU[Summer] * 1.25))
    let ur = at 1 (UrbanDemandMultiplier 1.3)
    Assert.True(near (Turn.effectiveUrbanDemand sc ur 1) (Turn.urbanDemand sc 1 * 1.3))
    // 揚水能力
    let pc = at 1 (PumpCapacityMultiplier 0.5)
    Assert.True(near (Turn.wellCapacity sc pc Farm) ((Groundwater.spec sc Farm).WellCapacityWU * 0.5))

[<Fact>]
let ``できごとがあっても 全シナリオ 全施設で水収支の残差は許容誤差内`` () =
    for sc in Scenarios.all do
        for seed in [ 1; 2; 3 ] do
            let w = Weather.generate sc seed
            let mutable s = Turn.initialState sc |> allBuilt sc
            s <- { s with ReservoirWU = (match sc.Reservoir with Some r -> r.CapacityWU / 2.0 | None -> 0.0) }

            for t in 0 .. sc.Turns - 1 do
                let op = (if t % 3 = 0 then 700.0 elif t % 3 = 1 then -500.0 else 0.0)
                let pump = sc.Zones |> List.map (fun z -> z.Id, 1500.0) |> Map.ofList
                let p = { policyFor sc s with ReservoirOpWU = op; PumpLimitWU = pump }
                let s', r = Turn.resolve sc s p w[t]
                Assert.True(r.Balance.IsWithinTolerance, $"{sc.Name} seed={seed} turn={t} residual={r.Balance.ResidualWU}")
                Assert.True(s'.ReservoirWU >= -1e-9)
                s <- s'

// ---------- 評価のプレビュー (B3) ----------

[<Fact>]
let ``評価のプレビューは 結果が出るまで出ず 地下水は隠しの真の値に依存しない`` () =
    let sc = normal
    let w = Weather.generate sc 2
    let s0 = Turn.initialState sc
    Assert.True((Evaluation.preview sc s0).IsNone)
    let s1, _ = Turn.resolve sc s0 (policyFor sc s0) w[0]
    let a = Evaluation.preview sc s1
    Assert.True(a.IsSome)
    // 隠しの地下水貯留量だけを変えても プレビューは変わらない
    let doubled = { s1 with GwWU = s1.GwWU |> Map.map (fun _ g -> g * 0.2) }
    Assert.Equal(a, Evaluation.preview sc doubled)

// ---------- UI の状態 ----------

let private now = System.DateTime(2026, 1, 1, 0, 0, 0, System.DateTimeKind.Utc)

let private send (msgs: Msg list) (m: Model) = msgs |> List.fold (fun m msg -> fst (Model.update msg m)) m

[<Fact>]
let ``貯水池の操作は 貯水池があるときだけ 貯水量と容量の範囲で受け付ける`` () =
    let m0 = { Model.newGame normal 1 now with Overlay = NoOverlay }
    Assert.Equal(0.0, (m0 |> send [ SetReservoirOp 500.0 ]).ReservoirOp)
    let built = { m0.Game with Invested = m0.Game.Invested.Add Reservoir; ReservoirWU = 1000.0 }
    let m1 = { m0 with Game = built }
    let cap = normal.Reservoir.Value.CapacityWU
    Assert.Equal(cap, (m1 |> send [ SetReservoirOp 99999.0 ]).ReservoirOp)
    Assert.Equal(-1000.0, (m1 |> send [ SetReservoirOp -99999.0 ]).ReservoirOp)

[<Fact>]
let ``タブ(0〜4)を選べて 範囲外のタブは無視される`` () =
    let m0 = { Model.newGame normal 1 now with Overlay = NoOverlay }
    Assert.Equal(2, (m0 |> send [ SelectTab 2 ]).RightTab)
    Assert.Equal(4, (m0 |> send [ SelectTab 4 ]).RightTab)
    Assert.Equal(0, (m0 |> send [ SelectTab 5 ]).RightTab)
    Assert.Equal(0, (m0 |> send [ SelectTab -1 ]).RightTab)

[<Fact>]
let ``カードのページは 送れて 別のカードに替わると先頭へ戻る`` () =
    let m0 = { Model.newGame normal 1 now with Overlay = NoOverlay }
    let m1 = m0 |> send [ EndTurn now ]
    Assert.Equal(0, m1.OverlayPage)
    Assert.Equal(2, (m1 |> send [ SetPage 2 ]).OverlayPage)
    Assert.Equal(0, (m1 |> send [ SetPage -3 ]).OverlayPage)
    // 日報を閉じて次の季節の日報を開くと、ページは 0 に戻る
    let m2 = m1 |> send [ SetPage 2; CloseOverlay now; EndTurn now ]
    Assert.Equal(0, m2.OverlayPage)
    Assert.Equal(1, (m0 |> send [ SetManualPage 1 ]).ManualPage)

[<Fact>]
let ``評価のプレビューの財政は 今のペースが続いた場合の最終残高で見込む`` () =
    let sc = normal
    let w = Weather.generate sc 2
    let mutable s = Turn.initialState sc

    for t in 0..3 do
        s <- fst (Turn.resolve sc s (policyFor sc s) w[t])

    let indicators, _, _ = (Evaluation.preview sc s).Value
    let fin = indicators |> List.find (fun i -> i.Name = "財政の健全性")
    let perTurn = (s.Balance - sc.Finance.InitialBalance) / 4.0
    let projected = s.Balance + perTurn * float (sc.Turns - 4)
    let minBalance = s.History |> List.map _.BalanceEnd |> List.fold min sc.Finance.InitialBalance
    Assert.True(near fin.Score (Evaluation.finScore sc.Eval.TargetBalance projected minBalance))
