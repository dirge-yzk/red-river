module WaterGame.Tests.V03Tests

open Xunit
open WaterGame.Core

let near (a: float) (b: float) = abs (a - b) <= 1e-6 * max 1.0 (abs a)

/// シナリオごとの行動計画。ターンごとの主要行動、揚水上限、農業の河川目標を減らす量を決める
type Plan =
    { Name: string
      PumpLimit: float
      Action: int -> MajorAction
      RiverCut: float }

let plans =
    [ { Name = "なし"; PumpLimit = 0.0; Action = (fun _ -> NoAction); RiverCut = 0.0 }
      { Name = "投資と井戸"
        PumpLimit = 900.0
        RiverCut = 900.0
        Action =
          function
          | 0 -> Survey(Farm, Reconnaissance)
          | 1 -> BuildWell Farm
          | 2 -> Invest(ObservationWell Farm)
          | 3 -> Invest IrrigationEfficiency
          | 4 -> Invest(ObservationWell Forest)
          | 5 -> Invest(ObservationWell Wetland)
          | _ -> NoAction }
      { Name = "使い切る"
        PumpLimit = 5000.0
        RiverCut = 3000.0
        Action =
          function
          | 0 -> Survey(Farm, Reconnaissance)
          | 1 -> BuildWell Farm
          | 2 -> Survey(Forest, Reconnaissance)
          | 3 -> BuildWell Forest
          | 4 -> Survey(Wetland, Reconnaissance)
          | 5 -> BuildWell Wetland
          | 6 -> Invest IrrigationEfficiency
          | _ -> NoAction } ]

let policyOf (sc: Scenario) (plan: Plan) (state: GameState) =
    let season = Season.ofTurn state.Turn
    let demand = Turn.effectiveAgDemand sc state season

    { AgTargetWU = max 0.0 (demand - (if plan.RiverCut > 0.0 && state.Wells[Farm] = WellBuilt then plan.RiverCut else 0.0))
      UrbanTargetWU = sc.UrbanDemandWU[season]
      EnvTargetWU = sc.MinEnvFlowWU[season]
      Priority = Turn.defaultPriority
      PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, plan.PumpLimit) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = Accurate
      Action = plan.Action state.Turn }

let play (sc: Scenario) (seed: int) (plan: Plan) =
    Weather.generate sc seed
    |> List.fold (fun s w -> Turn.resolve sc s (policyOf sc plan s) w |> fst) (Turn.initialState sc)

let private gamesLazy =
    lazy
        [ for sc in Scenarios.all do
            for seed in [ 1 .. 30 ] do
                for plan in plans -> sc, play sc seed plan ]

let games () = gamesLazy.Value
let results () = games () |> List.collect (fun (_, g) -> g.History)

let normal = { Scenarios.normalYear with Construction = None } // 工事が、発注した季節に完成する (資材・建設は V17Tests で確かめる)
let drought = Scenarios.drought

// ---------- 財政 ----------

[<Fact>]
let ``期末残高 = 期首残高 + 交付金 + 収入 + 達成報酬 - 費用の合計`` () =
    for r in results () do
        Assert.True(near r.BalanceEnd (r.BalanceStart + r.Grant + r.Revenue + r.Plan.Reward - r.Plan.Fine - r.Costs.Total))

[<Fact>]
let ``残高は翌ターンへ繰り越され 最初の残高はシナリオの初期残高`` () =
    for sc, g in games () do
        Assert.Equal(sc.Finance.InitialBalance, g.History.Head.BalanceStart)

        for (a, b) in List.pairwise g.History do
            Assert.Equal(a.BalanceEnd, b.BalanceStart)

        Assert.Equal(g.Balance, (List.last g.History).BalanceEnd)

[<Fact>]
let ``管理交付金は毎年の春にだけ受け取る`` () =
    for sc, g in games () do
        for r in g.History do
            // できごと (交付金の削減・臨時の補助金) があれば、その分が変わる
            // 上司の信頼 (季節のはじめの値) で、年度の交付金の倍率が変わる (指令のあるシナリオだけ)
            let trustK = if sc.Directives.IsSome then 0.8 + 0.4 * r.Plan.TrustBefore / 100.0 else 1.0
            let baseGrant = if r.Season = Spring then sc.Finance.AnnualGrant * trustK else 0.0

            let expected =
                match r.Event |> Option.map _.Kind with
                | Some(GrantMultiplier k) -> baseGrant * k
                | Some(ExtraFunds x) -> baseGrant + x
                | _ -> baseGrant

            Assert.True(near expected r.Grant)
            if r.Season <> Spring then Assert.True(r.Event.IsSome || r.Grant = 0.0)

[<Fact>]
let ``収入は農業の充足率に比例し 灌漑効率改善で需要が減っても同じ生産なら同額`` () =
    for sc, g in games () do
        for r in g.History do
            let agK =
                match r.Event |> Option.map _.Kind with
                | Some(AgDemandMultiplier k) -> k
                | _ -> 1.0

            let ag = r.AgFillRate * sc.AgDemandWU[r.Season] * agK * sc.Finance.AgRevenuePerWU
            let urban = r.UrbanFillRate * r.UrbanDemandWU * sc.Finance.UrbanRevenuePerWU
            Assert.True(near r.Revenue (ag + urban))

[<Fact>]
let ``調査・建設・投資は実行時点の資金の範囲内でしか実行されない`` () =
    for _, g in games () do
        for r in g.History do
            if r.ActionTaken <> NoAction then
                let optional = r.Costs.Survey + r.Costs.Build + r.Costs.Invest
                Assert.True(optional <= r.BalanceStart + r.Grant + 1e-9, $"turn {r.Turn} {r.ActionTaken}")

[<Fact>]
let ``資金が足りない任意行動は選べず 資金が足りれば選べる`` () =
    let s0 = Turn.initialState normal
    // 夏ターン (交付金なし) まで進めた状態を作り、残高だけを変える
    let w = Weather.generate normal 1
    let s1, _ = Turn.resolve normal s0 (policyOf normal plans[0] s0) w[0]
    Assert.Equal(Summer, Season.ofTurn s1.Turn)
    let recon = Survey(Farm, Reconnaissance)
    Assert.False(Turn.isValidAction normal { s1 with Balance = 100.0 } recon)
    Assert.True(Turn.isValidAction normal { s1 with Balance = 250.0 } recon)

    let raw = { policyOf normal plans[0] s1 with Action = recon }
    Assert.Equal(NoAction, (Turn.normalizePolicy normal { s1 with Balance = 100.0 } raw).Action)

[<Fact>]
let ``春は交付金を加えた額で判定し マイナス残高でも交付金で賄える範囲なら選べる`` () =
    let s0 = { Turn.initialState normal with Balance = -500.0 }
    let recon = Survey(Farm, Reconnaissance)
    // 春: -500 + 交付金 = 正の額 >= 200
    Assert.Equal(Spring, Season.ofTurn s0.Turn)
    Assert.True(Turn.isValidAction normal s0 recon)
    // 交付金のない夏に残高がマイナスなら選べない
    let summer = { s0 with Turn = 1 }
    Assert.False(Turn.isValidAction normal summer recon)

[<Fact>]
let ``井戸の維持管理費は季節開始時に建設済みの井戸の数に比例する`` () =
    for sc, g in games () do
        let mutable wells = 0

        for r in g.History do
            Assert.True(near r.Costs.Maintenance (float wells * sc.Finance.WellMaintenance))

            // 井戸は、完成した季節の終わりに建設済みになる (資材・建設のあるシナリオでは、完成が遅れることがある)
            let built =
                if sc.Construction.IsSome then
                    r.Works.Completed |> List.filter (fun p -> (match p.Target with BuildWell _ -> true | _ -> false)) |> List.length
                else
                    (match r.ActionTaken with BuildWell _ -> 1 | _ -> 0)

            wells <- wells + built

// ---------- 設備投資 ----------

let private withInvested (sc: Scenario) (inv: Investment) =
    // 春に投資を実行した直後の状態 (効果は次の季節から)
    // 資金は十分に与える (ここで確かめたいのは投資の効果で、資金制約は別のテストで扱う)
    let s0 = { Turn.initialState sc with Balance = 10000.0 }
    let w = Weather.generate sc 5
    let action = Invest inv
    let policy = { policyOf sc plans[0] s0 with Action = action }
    let s1, r1 = Turn.resolve sc s0 policy w[0]
    s0, s1, r1, w

[<Fact>]
let ``灌漑効率改善は翌季から農業の需要を減らすが 収入の基準は変わらない`` () =
    let _, s1, r1, w = withInvested normal IrrigationEfficiency
    Assert.Equal(normal.AgDemandWU[Spring], r1.AgDemandWU)
    let reduced = Turn.effectiveAgDemand normal s1 Summer
    Assert.True(near reduced (normal.AgDemandWU[Summer] * (1.0 - normal.Invest.IrrigationDemandReduction)))
    let _, r2 = Turn.resolve normal s1 (policyOf normal plans[0] s1) w[1]
    Assert.True(near r2.AgDemandWU reduced)
    // 需要が減っても、同じ生産(充足率 100%)なら収入は元の需要を基準にする
    if r2.AgFillRate >= 0.999 then
        let ag = normal.AgDemandWU[Summer] * normal.Finance.AgRevenuePerWU
        let urban = r2.UrbanFillRate * normal.UrbanDemandWU[Summer] * normal.Finance.UrbanRevenuePerWU
        Assert.True(near r2.Revenue (ag + urban))

[<Fact>]
let ``同じ設備投資は1度しか実施できない`` () =
    let _, s1, _, _ = withInvested normal (ObservationWell Farm)
    let funded = { s1 with Balance = 100000.0 }
    Assert.False(Turn.isValidAction normal funded (Invest(ObservationWell Farm)))
    Assert.True(Turn.isValidAction normal funded (Invest(ObservationWell Forest)))
    Assert.True(Turn.isValidAction normal funded (Invest IrrigationEfficiency))

[<Fact>]
let ``投資を含む全ゲームで水収支残差が許容誤差内に収まる`` () =
    for r in results () do
        Assert.True(r.Balance.IsWithinTolerance, Turn.balanceLogLine r)

// ---------- シナリオ ----------

[<Fact>]
let ``シナリオは6つで 名前が異なる`` () =
    Assert.Equal(6, Scenarios.all.Length)
    Assert.Equal(6, Scenarios.all |> List.map _.Name |> List.distinct |> List.length)

[<Fact>]
let ``どのシナリオでも全ターンで気象カードを引ける`` () =
    for sc in Scenarios.all do
        for turn in 0 .. sc.Turns - 1 do
            let season = Season.ofTurn turn

            let candidates =
                sc.Cards |> List.filter (fun c -> c.Season = season && turn >= c.FromTurn && turn <= c.ToTurn)

            Assert.NotEmpty(candidates)

[<Fact>]
let ``同一シナリオ同一シードなら気象と隠しパラメータが再現され シナリオが違えば変わる`` () =
    for sc in Scenarios.all do
        for seed in [ 1; 9; 77 ] do
            Assert.Equal<ActualWeather list>(Weather.generate sc seed, Weather.generate sc seed)

    Assert.NotEqual<ActualWeather list>(Weather.generate normal 3, Weather.generate drought 3)

[<Fact>]
let ``渇水シナリオは前半の降水が平年より少なく 農業需要は平年以上`` () =
    // 前半は最初の6ターン、後半は残りの6ターン
    let firstYear sc seed =
        Weather.generate sc seed |> List.take 6 |> List.sumBy _.PrecipMm

    let secondYear sc seed =
        Weather.generate sc seed |> List.skip 6 |> List.sumBy _.PrecipMm

    let seeds = [ 1 .. 200 ]
    let mean f sc = seeds |> List.averageBy (f sc)
    Assert.True(mean firstYear drought < mean firstYear normal * 0.85)
    // 後半は平年と同じカード構成
    Assert.True(abs (mean secondYear drought - mean secondYear normal) < mean secondYear normal * 0.05)

    for season in [ Spring; Summer; Autumn ] do
        Assert.True(drought.AgDemandWU[season] >= normal.AgDemandWU[season])

[<Fact>]
let ``渇水は農業と環境流水を重視し 平年は全指標を同じ重みで見る`` () =
    Assert.Equal(1.0, normal.Eval.AgWeight)
    Assert.Equal(normal.Eval.AgWeight, normal.Eval.EnvWeight)
    Assert.Equal(normal.Eval.GwWeight, normal.Eval.FinWeight)
    Assert.True(drought.Eval.AgWeight > drought.Eval.GwWeight)
    Assert.True(drought.Eval.EnvWeight > drought.Eval.FinWeight)

[<Fact>]
let ``どのシナリオでも地下水と土壌水分は負にならず 農業充足率は1を超えない`` () =
    for r in results () do
        Assert.InRange(r.AgFillRate, 0.0, 1.0)

        for z in r.Zones do
            Assert.True(z.SoilEndWU >= 0.0 && z.GwEndWU >= -1e-9)

// ---------- 評価 ----------

let private ind score weight =
    { Name = "x"
      Score = score
      Weight = weight
      Detail = "" }

[<Fact>]
let ``総合点は重み付き平均と最低指標の混合 (0.6 と 0.4) で 偏りがあるほど下がる`` () =
    // 3つが満点でも1つが0なら、平均 0.75 と最低 0 の混合
    Assert.True(near (Evaluation.combine [ ind 1.0 1.0; ind 1.0 1.0; ind 1.0 1.0; ind 0.0 1.0 ]) 0.45)
    // 偏りがなければ平均そのまま
    Assert.True(near (Evaluation.combine [ ind 0.8 1.0; ind 0.8 1.0 ]) 0.8)
    // 同じ平均なら、偏りが大きいほど低い
    Assert.True(Evaluation.combine [ ind 0.7 1.0; ind 0.7 1.0 ] > Evaluation.combine [ ind 0.9 1.0; ind 0.5 1.0 ])
    // 重みが効く: 重い指標が高いほど平均が上がる
    Assert.True(Evaluation.combine [ ind 1.0 3.0; ind 0.8 1.0 ] > Evaluation.combine [ ind 1.0 1.0; ind 0.8 3.0 ])
    // 他の指標が良くなれば、最低指標が同じでも総合が上がる (上限で打ち切られない)
    Assert.True(Evaluation.combine [ ind 0.5 1.0; ind 1.0 1.0 ] > Evaluation.combine [ ind 0.5 1.0; ind 0.7 1.0 ])

[<Fact>]
let ``財政の評価は基準以上で満点 マイナスを経験すると減点される`` () =
    Assert.Equal(1.0, Evaluation.finScore 2000.0 2500.0 800.0)
    Assert.True(near (Evaluation.finScore 2000.0 1000.0 800.0) 0.5)
    Assert.True(near (Evaluation.finScore 2000.0 2500.0 -10.0) 0.8)
    Assert.Equal(0.0, Evaluation.finScore 2000.0 (-300.0) (-300.0))

[<Fact>]
let ``地下水の評価は基準比以上で満点 コスト上昇フラグごとに減点される`` () =
    Assert.Equal(1.0, Evaluation.gwScore 0.65 0.9 0)
    Assert.Equal(1.0, Evaluation.gwScore 0.65 0.65 0)
    Assert.True(near (Evaluation.gwScore 0.65 0.65 1) 0.85)
    Assert.True(Evaluation.gwScore 0.65 0.3 0 < 0.5)

[<Fact>]
let ``最終評価は5指標で シナリオの重みが反映される`` () =
    for sc, g in games () |> List.truncate 12 do
        let s = Evaluation.summarize sc g
        Assert.Equal(5, s.Indicators.Length)
        Assert.Equal(sc.Eval.AgWeight, s.Indicators[0].Weight)
        Assert.Equal(sc.Eval.UrbanWeight, s.Indicators[1].Weight)
        Assert.Equal(sc.Eval.EnvWeight, s.Indicators[2].Weight)
        Assert.Equal(sc.Eval.GwWeight, s.Indicators[3].Weight)
        Assert.Equal(sc.Eval.FinWeight, s.Indicators[4].Weight)
        Assert.InRange(s.Score, 0.0, 1.0)
        Assert.True(s.Indicators |> List.forall (fun i -> i.Score >= 0.0 && i.Score <= 1.0))

[<Fact>]
let ``ランクの境界`` () =
    Assert.Equal(S, Evaluation.rankOf 0.90)
    Assert.Equal(A, Evaluation.rankOf 0.89)
    Assert.Equal(B, Evaluation.rankOf 0.65)
    Assert.Equal(C, Evaluation.rankOf 0.64)

// ---------- デブリーフ ----------

[<Fact>]
let ``推定値と実際の推移は開始時を含めて全ターン分ある`` () =
    let g = play normal 4 plans[1]

    for z in normal.Zones do
        let trace = Evaluation.gwTrace normal g z.Id
        Assert.Equal(g.History.Length + 1, trace.Length)
        Assert.Equal(0, trace.Head.Turn)
        Assert.Equal(g.Hidden[z.Id].InitialG, trace.Head.Actual)
        Assert.Equal((Groundwater.priorEstimate (Groundwater.spec normal z.Id)).GwWU, trace.Head.Estimated)
        Assert.Equal(g.GwWU[z.Id], (List.last trace).Actual)

[<Fact>]
let ``未調査の区域は推定と実際が乖離しうるが 調査した区域は誤差範囲に収まる`` () =
    for seed in [ 1 .. 20 ] do
        let g = play normal seed plans[1]
        // 農地は概査済み (±25%)。その後の推定は涵養・揚水の公開情報で進むので、調査直後の誤差が大きく崩れない
        let afterSurvey = (g.History |> List.find (fun r -> not r.SurveyReports.IsEmpty))
        let est = afterSurvey.EstimatesAfter[Farm].GwWU
        let actual = afterSurvey.ActualGwAfter[Farm]
        Assert.True(abs (est - actual) <= actual * normal.Costs.ReconRelErr + 1e-6)

[<Fact>]
let ``デブリーフは区域ごとに1行で 遅い調査と未調査を区別する`` () =
    let none = play normal 2 plans[0]
    let lines = Evaluation.debrief normal none
    Assert.Equal(normal.Zones.Length, lines.Length)
    Assert.All(lines, fun l -> Assert.Contains("最後まで調査しなかった", l))

    // 第4ターン以降に初めて調査すると「もっと早く」の解説になる
    let late =
        { plans[0] with Action = (fun t -> if t = 3 then Survey(Farm, Reconnaissance) else NoAction) }

    let lateGame = play normal 2 late
    let farmLine = Evaluation.debrief normal lateGame |> List.find (fun l -> l.Contains(ZoneId.label Farm))
    Assert.Contains("もっと早く", farmLine)

    // 第1ターンに調査すれば「早めに調査」の解説になる
    let early = { plans[0] with Action = (fun t -> if t = 0 then Survey(Farm, Reconnaissance) else NoAction) }
    let earlyLine = Evaluation.debrief normal (play normal 2 early) |> List.find (fun l -> l.Contains(ZoneId.label Farm))
    Assert.Contains("早めに調査", earlyLine)
