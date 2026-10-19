module WaterGame.Tests.V10Tests

open Xunit
open WaterGame.Core

let near (a: float) (b: float) = abs (a - b) <= 1e-6 * max 1.0 (abs a)

let normal = Scenarios.normalYear
let drought = Scenarios.drought

let policyFor (sc: Scenario) (state: GameState) =
    let season = Season.ofTurn state.Turn

    { AgTargetWU = Turn.effectiveAgDemand sc state season
      UrbanTargetWU = sc.UrbanDemandWU[season]
      EnvTargetWU = sc.MinEnvFlowWU[season]
      Priority = state.Priority
      PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 0.0) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = Accurate
      Action = NoAction }

let permutations =
    [ [ Agriculture; Municipal; Environment ]
      [ Agriculture; Environment; Municipal ]
      [ Municipal; Agriculture; Environment ]
      [ Municipal; Environment; Agriculture ]
      [ Environment; Agriculture; Municipal ]
      [ Environment; Municipal; Agriculture ] ]

// ---------- 4区域・12ターン ----------

[<Fact>]
let ``全シナリオが4区域で ターン数は4の倍数 河川は 森林 農地 市街地 湿地 の順に流れる`` () =
    for sc in Scenarios.all do
        Assert.True(sc.Turns > 0 && sc.Turns % 4 = 0, sc.Name)

        Assert.Equal<ZoneId list>([ Forest; Farm; Urban; Wetland ], sc.Zones |> List.map _.Id)
        Assert.Equal(Urban, sc.AllocationAfter)
        // どの区域にも帯水層・投資の設定がある
        for z in sc.Zones do
            Assert.Contains(sc.Aquifers, fun a -> a.Zone = z.Id)

[<Fact>]
let ``全ターンで水収支が一致し 季節が巡る`` () =
    for sc in Scenarios.all do
        let seed = 7
        let w = Weather.generate sc seed
        Assert.Equal(sc.Turns, w.Length)
        let final = w |> List.fold (fun s x -> Turn.resolve sc s (policyFor sc s) x |> fst) (Turn.initialState sc)
        Assert.Equal(sc.Turns, final.History.Length)
        Assert.True(final.History |> List.forall (fun r -> r.Balance.IsWithinTolerance), sc.Name)
        Assert.Equal<Season list>([ for t in 0 .. sc.Turns - 1 -> Season.ofTurn t ], final.History |> List.map _.Season)

[<Fact>]
let ``市街地は地表流出が大きく 農地や森林より流出係数が高い`` () =
    let coef z = (normal.Zones |> List.find (fun x -> x.Id = z)).RunoffCoef
    Assert.True(coef Urban > coef Farm && coef Urban > coef Forest && coef Urban > coef Wetland)

// ---------- 3項目の配分優先順位 ----------

[<Fact>]
let ``優先順位は6通りの並びがすべて有効で 重複や欠落は無効`` () =
    Assert.Equal(6, permutations |> List.distinct |> List.length)
    Assert.All(permutations, fun p -> Assert.True(Turn.isValidPriority p))
    Assert.False(Turn.isValidPriority [ Agriculture; Agriculture; Environment ])
    Assert.False(Turn.isValidPriority [ Agriculture; Environment ])
    Assert.False(Turn.isValidPriority [])

[<Fact>]
let ``春は優先順位を変更でき 無効な並びは無視され 春以外は変更できない`` () =
    let s0 = Turn.initialState normal
    let w = Weather.generate normal 1

    for p in permutations do
        let n = Turn.normalizePolicy normal s0 { policyFor normal s0 with Priority = p }
        Assert.Equal<Allocatee list>(p, n.Priority)

    let bad = Turn.normalizePolicy normal s0 { policyFor normal s0 with Priority = [ Environment; Environment; Environment ] }
    Assert.Equal<Allocatee list>(Turn.defaultPriority, bad.Priority)

    let s1, _ = Turn.resolve normal s0 { policyFor normal s0 with Priority = [ Environment; Agriculture; Municipal ] } w[0]
    Assert.Equal<Allocatee list>([ Environment; Agriculture; Municipal ], s1.Priority)
    // 夏は変更を要求しても無視される
    let n = Turn.normalizePolicy normal s1 { policyFor normal s1 with Priority = Turn.defaultPriority }
    Assert.Equal<Allocatee list>([ Environment; Agriculture; Municipal ], n.Priority)

[<Fact>]
let ``配分は優先順位の高い用途から先に確保され 3項目の合計は利用可能量を超えない`` () =
    let cases = [ 0.0; 500.0; 2000.0; 4000.0; 9000.0; 50000.0 ]

    for p in permutations do
        for avail in cases do
            let a = Hydrology.allocate p avail 300.0 3000.0 2000.0 2500.0
            Assert.True(a.AgSupplyWU + a.UrbanSupplyWU + a.EnvReserveWU <= avail + 1e-9)
            Assert.True(a.AgSupplyWU <= 3000.0 + 1e-9 && a.UrbanSupplyWU <= 2000.0 + 1e-9)
            Assert.True(near a.DownstreamFlowWU (avail - a.AgSupplyWU - a.UrbanSupplyWU + 300.0))

[<Fact>]
let ``水が足りないとき 後回しにした用途から不足する`` () =
    // 利用可能量 3,000 に対し、農業の目標 2,500 と生活産業の目標 2,000 は同時に満たせない
    let first = Hydrology.allocate [ Municipal; Agriculture; Environment ] 3000.0 0.0 2500.0 2000.0 0.0
    Assert.Equal(2000.0, first.UrbanSupplyWU)
    Assert.Equal(1000.0, first.AgSupplyWU)

    let flipped = Hydrology.allocate [ Agriculture; Municipal; Environment ] 3000.0 0.0 2500.0 2000.0 0.0
    Assert.Equal(2500.0, flipped.AgSupplyWU)
    Assert.Equal(500.0, flipped.UrbanSupplyWU)

[<Fact>]
let ``環境流水を最優先にすると 先に下流へ残す分を確保してから用水を配分する`` () =
    let a = Hydrology.allocate [ Environment; Municipal; Agriculture ] 4000.0 0.0 2500.0 2000.0 3000.0
    Assert.Equal(3000.0, a.EnvReserveWU)
    Assert.Equal(1000.0, a.UrbanSupplyWU)
    Assert.Equal(0.0, a.AgSupplyWU)
    // 予約した分は下流にそのまま残る (二重に供給しない)
    Assert.True(a.DownstreamFlowWU >= 3000.0 - 1e-9)

[<Fact>]
let ``生活産業の取水目標は需要を超えられず 0 未満にもならない`` () =
    let s0 = Turn.initialState normal
    let big = Turn.normalizePolicy normal s0 { policyFor normal s0 with UrbanTargetWU = 1e9 }
    let neg = Turn.normalizePolicy normal s0 { policyFor normal s0 with UrbanTargetWU = -5.0 }
    Assert.Equal(normal.UrbanDemandWU[Spring], big.UrbanTargetWU)
    Assert.Equal(0.0, neg.UrbanTargetWU)

[<Fact>]
let ``揚水した地下水は 優先順位の高い用水から先に未充足需要へ供給される`` () =
    // 河川水からの取水目標を 0 にして、全区域に井戸と十分な揚水上限を与え、地下水だけで賄う状況を作る
    for sc in Scenarios.all do
        for first in [ Agriculture; Municipal ] do
            let s0 = Turn.initialState sc

            let wells = sc.Zones |> List.map (fun z -> z.Id, WellBuilt) |> Map.ofList
            let surveyed = sc.Zones |> List.map (fun z -> z.Id, TestDrilled) |> Map.ofList
            // 夏: 需要が大きい季節
            let s = { s0 with Turn = 1; Wells = wells; Survey = surveyed }
            let other = if first = Agriculture then Municipal else Agriculture
            let priority = [ first; other; Environment ]
            let w = Weather.generate sc 3

            let policy =
                { policyFor sc s with
                    AgTargetWU = 0.0
                    UrbanTargetWU = 0.0
                    // 優先順位の変更は春だけなので、状態側に反映しておく
                    Priority = priority
                    PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 100000.0) |> Map.ofList }

            let _, r = Turn.resolve sc { s with Priority = priority } policy w[1]
            let unmetAg = r.AgDemandWU
            let unmetUrban = r.UrbanDemandWU
            Assert.Equal(0.0, r.Allocation.AgSupplyWU)
            Assert.Equal(0.0, r.Allocation.UrbanSupplyWU)
            Assert.True(near r.GwSupplyWU (r.AgGwSupplyWU + r.UrbanGwSupplyWU))
            // 地下水が足りないときは、優先された用水が先に満たされる
            let totalUnmet = unmetAg + unmetUrban

            if r.GwSupplyWU < totalUnmet - 1e-6 then
                let firstUnmet, firstGw = if first = Agriculture then unmetAg, r.AgGwSupplyWU else unmetUrban, r.UrbanGwSupplyWU
                Assert.True(near firstGw (min firstUnmet r.GwSupplyWU))

[<Fact>]
let ``生活産業用水の充足率は河川水と地下水の合計で決まり 収入に反映される`` () =
    for sc in Scenarios.all do
        let seed = 11
        let w = Weather.generate sc seed
        let final = w |> List.fold (fun s x -> Turn.resolve sc s (policyFor sc s) x |> fst) (Turn.initialState sc)

        for r in final.History do
            let expected = (r.Allocation.UrbanSupplyWU + r.UrbanGwSupplyWU) / r.UrbanDemandWU
            Assert.True(near r.UrbanFillRate (min 1.0 expected))

[<Fact>]
let ``生活産業を最後にして水が足りないと 農業や環境より先に生活産業が不足する`` () =
    // 夏の渇水カードで、需要を大きくしたシナリオを作り、優先順位だけを入れ替えて比べる
    let tight = { drought with UrbanDemandWU = drought.UrbanDemandWU |> Map.map (fun _ v -> v * 4.0) }
    let seed = 5
    let s0 = Turn.initialState tight
    let w = Weather.generate tight seed

    let run priority =
        let p = { policyFor tight s0 with Priority = priority }
        let _, r = Turn.resolve tight s0 p w[0]
        r

    let urbanFirst = run [ Municipal; Agriculture; Environment ]
    let urbanLast = run [ Agriculture; Environment; Municipal ]
    Assert.True(urbanLast.UrbanFillRate < urbanFirst.UrbanFillRate || urbanFirst.UrbanFillRate >= 0.999)

// ---------- 評価: 5指標 ----------

[<Fact>]
let ``最終評価は5指標で 生活産業の充足度を含み どれも0から1`` () =
    for sc in Scenarios.all do
        let seed = 4
        let w = Weather.generate sc seed
        let final = w |> List.fold (fun s x -> Turn.resolve sc s (policyFor sc s) x |> fst) (Turn.initialState sc)
        let s = Evaluation.summarize sc final
        Assert.Equal(5, s.Indicators.Length)
        Assert.Contains(s.Indicators, fun i -> i.Name.Contains "生活・産業")
        Assert.All(s.Indicators, fun i -> Assert.InRange(i.Score, 0.0, 1.0))
        Assert.InRange(s.UrbanFillRate, 0.0, 1.0)

// ---------- 学習目標の振り返り ----------

let private finalOf (sc: Scenario) seed (plan: int -> MajorAction) (limit: float) =
    Weather.generate sc seed
    |> List.fold
        (fun s w ->
            let p =
                { policyFor sc s with
                    Action = plan s.Turn
                    PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, limit) |> Map.ofList
                    AgTargetWU = 0.0 }

            Turn.resolve sc s p w |> fst)
        (Turn.initialState sc)

[<Fact>]
let ``振り返りは学習目標1から5まで1つずつあり 実績の数値を含む`` () =
    let final = finalOf normal 3 (fun _ -> NoAction) 0.0
    let s = Evaluation.summarize normal final
    let rs = Evaluation.reflections normal final s
    Assert.Equal<int list>([ 1; 2; 3; 4; 5 ], rs |> List.map (fun (n, _, _) -> n))
    Assert.All(rs, fun (_, title, text) -> Assert.False(System.String.IsNullOrWhiteSpace(title + text)))
    let text n = rs |> List.pick (fun (k, _, t) -> if k = n then Some t else None)
    // 目標1: 降水の行き先の内訳と残差
    Assert.Contains("蒸発散", text 1)
    Assert.Contains("残差", text 1)
    // 目標3: 3つの用途と優先順位
    Assert.Contains("優先順位", text 3)
    Assert.Contains("12", text 3)

[<Fact>]
let ``振り返りの目標5は フラグが立った区域を名指しし 立たなければ回避できたと伝える`` () =
    // 揚水コスト上昇が起きない遊び方
    let calm = finalOf normal 3 (fun _ -> NoAction) 0.0
    let calmText =
        Evaluation.reflections normal calm (Evaluation.summarize normal calm)
        |> List.pick (fun (n, _, t) -> if n = 5 then Some t else None)

    Assert.Contains("上昇しませんでした", calmText)

    // 閾値を割らせる: 農地の貯留量を閾値の直上にして、井戸で汲む
    let spec = Groundwater.spec normal Farm
    let s0 = Turn.initialState normal

    let primed =
        { s0 with
            GwWU = s0.GwWU |> Map.add Farm (spec.ThresholdWU * 0.5)
            Survey = s0.Survey |> Map.add Farm Reconnoitered
            Wells = s0.Wells |> Map.add Farm WellBuilt }

    let w = Weather.generate normal 3

    let policy =
        { policyFor normal primed with
            AgTargetWU = 0.0
            PumpLimitWU = normal.Zones |> List.map (fun z -> z.Id, 1800.0) |> Map.ofList }

    let after, _ = Turn.resolve normal primed policy w[0]
    Assert.True(after.CostFlags[Farm])
    let text =
        Evaluation.reflections normal after (Evaluation.summarize normal after)
        |> List.pick (fun (n, _, t) -> if n = 5 then Some t else None)

    Assert.Contains(ZoneId.label Farm, text)
    Assert.Contains("恒久的", text)

[<Fact>]
let ``振り返りの目標4は 調査回数と調査した区域・しなかった区域の誤差を伝える`` () =
    let plan t = if t = 0 then Survey(Farm, Reconnaissance) else NoAction
    let final = finalOf normal 3 plan 0.0
    let text =
        Evaluation.reflections normal final (Evaluation.summarize normal final)
        |> List.pick (fun (n, _, t) -> if n = 4 then Some t else None)

    Assert.Contains("1 回", text)
    Assert.Contains("調査した区域", text)
    Assert.Contains("未調査の区域", text)

[<Fact>]
let ``振り返りの目標1は 内訳の割合を合計すると100%に近い`` () =
    // 降水 = 蒸発散 + 用水供給 + 下流端への流出 + 貯留の増減 なので、割合の合計は 100%
    for sc in Scenarios.all do
        let final = finalOf sc 9 (fun _ -> NoAction) 0.0
        let h = final.History
        let p = h |> List.sumBy (fun r -> r.Balance.PrecipInWU)
        let parts =
            h
            |> List.sumBy (fun r ->
                r.Balance.ActualEtWU + r.Balance.ExternalSupplyWU + r.Balance.OutflowWU + r.Balance.StorageChangeWU)

        Assert.True(near p parts)

// ---------- シナリオ集 (入門・標準・特殊・高難度) ----------

let private byName name = Scenarios.all |> List.find (fun s -> s.Name = name)

[<Fact>]
let ``シナリオは 入門 標準 特殊 高難度 の区分を持ち 短い入門と長い高難度がある`` () =
    let levels = Scenarios.all |> List.map _.Level |> List.distinct |> Set.ofList
    Assert.Equal<Set<string>>(Set.ofList [ "入門"; "標準"; "特殊"; "高難度" ], levels)
    Assert.Equal(4, (byName "チュートリアル").Turns)
    Assert.Equal(12, normal.Turns)
    Assert.Equal(20, (byName "長期の試練").Turns)
    // 特殊シナリオは2〜3つ
    let specials = Scenarios.all |> List.filter (fun s -> s.Level = "特殊")
    Assert.InRange(specials.Length, 2, 3)
    Assert.All(Scenarios.all, fun s -> Assert.False(System.String.IsNullOrWhiteSpace s.Summary))

[<Fact>]
let ``ヒントはチュートリアルにだけあり 全ターンに1つずつ範囲内で用意されている`` () =
    let tut = byName "チュートリアル"
    Assert.Equal<int list>([ 0 .. tut.Turns - 1 ], tut.Hints |> Map.keys |> List.ofSeq)

    for sc in Scenarios.all do
        if sc.Name <> tut.Name then
            Assert.True(sc.Hints.IsEmpty, sc.Name)

[<Fact>]
let ``市街地拡大では 都市の需要が年ごとに伸び 舗装で流出係数が上がる`` () =
    let sc = byName "市街地拡大"
    let g = sc.UrbanDemandGrowthPerYear
    Assert.True(g > 0.0)
    // 1年目 (春) と 3年目 (春) の需要
    let d0 = Turn.urbanDemand sc 0
    let d2 = Turn.urbanDemand sc 8
    Assert.True(near d2 (d0 * (1.0 + g) * (1.0 + g)))
    // 平年は伸びない
    Assert.Equal(Turn.urbanDemand normal 0, Turn.urbanDemand normal 8)

    let state0 = Turn.initialState sc
    let urbanZone = sc.Zones |> List.find (fun z -> z.Id = Urban)
    let coefAt turn = (Turn.effectiveZone sc { state0 with Turn = turn } urbanZone).RunoffCoef
    Assert.True(coefAt 8 > coefAt 4 && coefAt 4 > coefAt 0)
    Assert.True(coefAt 100 <= 0.9 + 1e-9)
    // 他の区域は変わらない
    let farm = sc.Zones |> List.find (fun z -> z.Id = Farm)
    Assert.Equal(farm.RunoffCoef, (Turn.effectiveZone sc { state0 with Turn = 8 } farm).RunoffCoef)

[<Fact>]
let ``都市の需要が伸びるシナリオでも 取水目標は その時点の需要を超えられない`` () =
    let sc = byName "市街地拡大"
    let s = { Turn.initialState sc with Turn = 8 }
    let p = Turn.normalizePolicy sc s { policyFor sc s with UrbanTargetWU = 1e9 }
    Assert.True(near p.UrbanTargetWU (Turn.urbanDemand sc 8))

[<Fact>]
let ``乾燥地は降水が少なく 地下水の範囲が広い`` () =
    let arid = byName "乾燥地"
    let mean sc =
        [ 1 .. 100 ] |> List.averageBy (fun seed -> Weather.generate sc seed |> List.sumBy _.PrecipMm)

    Assert.True(mean arid < mean normal * 0.7)

    let width (sc: Scenario) =
        (Groundwater.priorEstimate (Groundwater.spec sc Farm)).RelErr

    Assert.True(width arid > width normal)

[<Fact>]
let ``長期の試練は 平年より費用が高く 資金が少なく 長い`` () =
    let hard = byName "長期の試練"
    Assert.True(hard.Costs.WellCost > normal.Costs.WellCost)
    Assert.True(hard.Invest.IrrigationCost > normal.Invest.IrrigationCost)
    Assert.True(hard.Finance.AnnualGrant < normal.Finance.AnnualGrant)
    Assert.True(hard.Finance.InitialBalance < normal.Finance.InitialBalance)
    Assert.True(hard.Turns > normal.Turns)
    // 大渇水の期間は、乾燥期より降水が少ない
    let mean (turns: int list) =
        [ 1 .. 100 ]
        |> List.averageBy (fun seed ->
            let w = Weather.generate hard seed
            turns |> List.averageBy (fun t -> w[t].PrecipMm))

    Assert.True(mean [ 12; 13; 14; 15 ] < mean [ 0; 1; 2; 3 ] * 0.8)

[<Fact>]
let ``チュートリアルは 案内どおりの操作(春に概査 夏に井戸)を1年で実行でき 秋から井戸が使える`` () =
    let tut = byName "チュートリアル"
    let s0 = Turn.initialState tut
    let w = Weather.generate tut 1
    let policy s a = { policyFor tut s with Action = a }
    Assert.True(Turn.isValidAction tut s0 (Survey(Farm, Reconnaissance)))
    let s1, _ = Turn.resolve tut s0 (policy s0 (Survey(Farm, Reconnaissance))) w[0]
    Assert.True(Turn.isValidAction tut s1 (BuildWell Farm))
    let s2, _ = Turn.resolve tut s1 (policy s1 (BuildWell Farm)) w[1]
    Assert.Equal(WellBuilt, s2.Wells[Farm])
    let s3, _ = Turn.resolve tut s2 (policy s2 NoAction) w[2]
    let s4, _ = Turn.resolve tut s3 (policy s3 NoAction) w[3]
    Assert.True(Turn.isFinished tut s4)

[<Fact>]
let ``チュートリアルの気象カードは 春平年 夏渇水 秋少雨 冬平年に固定されている`` () =
    let tut = byName "チュートリアル"

    for seed in [ 1 .. 20 ] do
        let names = Weather.generate tut seed |> List.map (fun x -> x.Card.Name)
        Assert.Equal<string list>([ "平年の春"; "渇水の夏"; "少雨の秋"; "平年の冬" ], names)

[<Fact>]
let ``チュートリアルは 何もしないより 案内どおりに遊んだほうが総合点が高い`` () =
    let tut = byName "チュートリアル"

    let play seed guided =
        let w = Weather.generate tut seed
        let mutable st = Turn.initialState tut

        for t in 0 .. tut.Turns - 1 do
            let season = Season.ofTurn t

            let action =
                if guided && t = 0 then Survey(Farm, Reconnaissance)
                elif guided && t = 1 then BuildWell Farm
                else NoAction

            let pump =
                tut.Zones
                |> List.map (fun z -> z.Id, (if guided && st.Wells[z.Id] = WellBuilt then 1500.0 else 0.0))
                |> Map.ofList

            let prio = if guided && t = 0 then [ Municipal; Agriculture; Environment ] else st.Priority

            let p =
                { policyFor tut st with
                    Priority = prio
                    PumpLimitWU = pump
                    Action = action }

            st <- fst (Turn.resolve tut st p w[t])

        (Evaluation.summarize tut st).Score

    let seeds = [ 1 .. 10 ]
    let mean guided = seeds |> List.averageBy (fun s -> play s guided)
    Assert.True(mean true > mean false + 0.05, $"guided {mean true} passive {mean false}")
    // 初期設定のままでは、高ランク (S) にならない
    Assert.True(mean false < 0.85)
