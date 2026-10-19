module WaterGame.Tests.InvariantTests

open Xunit
open WaterGame.Core

// 資金は十分に与える (ここで確かめたいのは水循環と調査の不変条件で、資金制約は別のテストで扱う)
let scenario =
    { Scenarios.normalYear with
        Finance = { Scenarios.normalYear.Finance with InitialBalance = 20000.0 } }

/// 地下水に関する行動計画。ターンごとの主要行動と揚水上限を決める
type Plan =
    { Name: string
      PumpLimit: float
      Action: int -> MajorAction }

let plans =
    [ { Name = "なし"; PumpLimit = 0.0; Action = fun _ -> NoAction }
      { Name = "控えめ"
        PumpLimit = 900.0
        Action =
          function
          | 0 -> Survey(Farm, Reconnaissance)
          | 1 -> BuildWell Farm
          | 2 -> Survey(Farm, TestDrilling)
          | _ -> NoAction }
      { Name = "使い切る"
        PumpLimit = 5000.0
        Action =
          function
          | 0 -> Survey(Farm, Reconnaissance)
          | 1 -> BuildWell Farm
          | 2 -> Survey(Forest, Reconnaissance)
          | 3 -> BuildWell Forest
          | 4 -> Survey(Wetland, Reconnaissance)
          | 5 -> BuildWell Wetland
          | _ -> NoAction } ]

let policyOf agFraction envExtra priority (plan: Plan) (state: GameState) =
    let season = Season.ofTurn state.Turn

    { AgTargetWU = scenario.AgDemandWU[season] * agFraction
      UrbanTargetWU = scenario.UrbanDemandWU[season]
      EnvTargetWU = scenario.MinEnvFlowWU[season] + envExtra
      Priority = priority
      PumpLimitWU = scenario.Zones |> List.map (fun z -> z.Id, plan.PumpLimit) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = Accurate
      Action = plan.Action state.Turn }

/// 全ターンを指定の政策関数で最後まで進める
let playGame (seed: int) (policyOf: GameState -> Policy) =
    Weather.generate scenario seed
    |> List.fold (fun state w -> Turn.resolve scenario state (policyOf state) w |> fst) (Turn.initialState scenario)

/// 政策の組み合わせを網羅的に走らせるための入力
let policyGrid =
    [ for ag in [ 0.0; 0.5; 1.0 ] do
        for env in [ 0.0; 5000.0 ] do
          for prio in [ [ Municipal; Agriculture; Environment ]; [ Environment; Agriculture; Municipal ] ] do
            for plan in plans -> ag, env, prio, plan ]

let seeds = [ 1 .. 40 ]

let private gamesLazy =
    lazy
        [ for seed in seeds do
            for ag, env, prio, plan in policyGrid do
              yield playGame seed (policyOf ag env prio plan) ]

let games () = gamesLazy.Value
let allResults () = games () |> List.collect _.History

let near (a: float) (b: float) = abs (a - b) <= 1e-6 * max 1.0 (abs a)

// ---------- v0.1 から続く不変条件 ----------

[<Fact>]
let ``1 降水入力は地表流出と浸透の合計に一致する`` () =
    for r in allResults () do
        for z in r.Zones do
            Assert.True(near z.PrecipWU (z.RunoffWU + z.InfiltrationWU))

[<Fact>]
let ``2 土壌水分と地下水貯留量は負にならない`` () =
    for r in allResults () do
        for z in r.Zones do
            Assert.True(z.SoilEndWU >= 0.0)
            Assert.True(z.SoilEndWU <= z.SoilCapacityWU + 1e-6)
            Assert.True(z.DeepDrainageWU >= 0.0)
            Assert.True(z.GwStartWU >= 0.0)
            Assert.True(z.GwEndWU >= -1e-9, $"{z.Zone} G={z.GwEndWU}")

[<Fact>]
let ``3 実蒸発散は需要と利用可能な土壌水分を超えない`` () =
    for r in allResults () do
        for z in r.Zones do
            Assert.True(z.ActualEtWU <= z.EtDemandWU + 1e-9)
            Assert.True(z.ActualEtWU <= z.SoilStartWU + z.InfiltrationWU + 1e-9)

[<Fact>]
let ``4 実際の揚水量は井戸能力と設定上限と利用可能な地下水量を超えない`` () =
    for state in games () do
        for r in state.History do
            for z in r.Zones do
                let cap = (Groundwater.spec scenario z.Zone).WellCapacityWU
                Assert.True(z.PumpedWU >= 0.0)
                Assert.True(z.PumpedWU <= cap + 1e-9)
                Assert.True(z.PumpedWU <= r.Policy.PumpLimitWU[z.Zone] + 1e-9)
                Assert.True(z.PumpedWU <= z.GwStartWU - z.DischargeWU + 1e-9)

[<Fact>]
let ``4b 井戸がない区域では揚水できず 建設した季節にも揚水できない`` () =
    for state in games () do
        // 井戸の完成前の状態を再現するため、履歴の行動から建設ターンを求める
        let builtAt =
            state.History
            |> List.choose (fun r ->
                match r.ActionTaken with
                | BuildWell z -> Some(z, r.Turn)
                | _ -> None)
            |> Map.ofList

        for r in state.History do
            for z in r.Zones do
                match Map.tryFind z.Zone builtAt with
                | Some t when r.Turn > t -> ()
                | _ -> Assert.Equal(0.0, z.PumpedWU)

[<Fact>]
let ``5 河川水の供給量は配分可能な水量を超えず目標も超えない`` () =
    for r in allResults () do
        let a = r.Allocation
        Assert.True(a.AgSupplyWU >= 0.0)
        Assert.True(a.AgSupplyWU <= a.AvailableWU + 1e-9)
        Assert.True(a.AgSupplyWU <= r.Policy.AgTargetWU + 1e-9)
        Assert.True(a.AgSupplyWU <= r.AgDemandWU + 1e-9)

[<Fact>]
let ``5b 河川水と地下水の合計は農業需要と生活産業需要をそれぞれ超えない`` () =
    for r in allResults () do
        Assert.True(r.Allocation.AgSupplyWU + r.AgGwSupplyWU <= r.AgDemandWU + 1e-6)
        Assert.True(r.Allocation.UrbanSupplyWU + r.UrbanGwSupplyWU <= r.UrbanDemandWU + 1e-6)
        Assert.True(near r.GwSupplyWU (r.AgGwSupplyWU + r.UrbanGwSupplyWU))
        Assert.InRange(r.AgFillRate, 0.0, 1.0)
        Assert.InRange(r.UrbanFillRate, 0.0, 1.0)

[<Fact>]
let ``6 環境流水の予約量は用水として二重に供給されない`` () =
    for r in allResults () do
        let a = r.Allocation
        Assert.True(a.EnvReserveWU + a.AgSupplyWU + a.UrbanSupplyWU <= a.AvailableWU + 1e-9)
        Assert.True(near a.DownstreamFlowWU (a.AvailableWU - a.AgSupplyWU - a.UrbanSupplyWU + a.BelowContributionWU))

[<Fact>]
let ``6b 地下水の揚水は下流流量に加算されない`` () =
    // 地下水は農業の未充足需要にだけ供給され、環境流水の達成度には加算しない
    for r in allResults () do
        let a = r.Allocation
        Assert.True(near a.DownstreamFlowWU (a.AvailableWU - a.AgSupplyWU - a.UrbanSupplyWU + a.BelowContributionWU))

[<Fact>]
let ``6c 環境流水が最優先なら予約できた分は農業に取られない`` () =
    for r in allResults () do
        if r.EffectivePriority.Head = Environment then
            let a = r.Allocation
            let reserveTarget = max 0.0 (r.EnvTargetWU - a.BelowContributionWU)

            if a.AvailableWU >= reserveTarget then
                Assert.True(a.DownstreamFlowWU >= r.EnvTargetWU - 1e-6)

[<Fact>]
let ``7 地下水を含む流域全体の水収支残差が許容誤差内に収まる`` () =
    for r in allResults () do
        Assert.True(r.Balance.IsWithinTolerance, Turn.balanceLogLine r)

        for z in r.Zones do
            Assert.True(abs z.ResidualWU <= 1e-6 * max 1.0 z.PrecipWU)

[<Fact>]
let ``8 同一シナリオ同一シードなら気象実績が再現され 隠しパラメータはシードに依存しない`` () =
    for seed in seeds do
        Assert.Equal<ActualWeather list>(Weather.generate scenario seed, Weather.generate scenario seed)

    // 乱数は気象だけ。隠しパラメータ (真の値) は、シナリオだけから決まる
    Assert.Equal<Map<ZoneId, HiddenAquifer>>(Groundwater.hiddenOf scenario, Groundwater.hiddenOf scenario)

[<Fact>]
let ``8b 政策を変えても気象実績は変わらない`` () =
    for seed in [ 1; 7; 42 ] do
        let weathers =
            [ for ag, env, prio, plan in policyGrid ->
                  (playGame seed (policyOf ag env prio plan)).History |> List.map _.Weather ]

        Assert.All(weathers, fun w -> Assert.Equal<ActualWeather list>(weathers.Head, w))

[<Fact>]
let ``8c 異なるシードで変わるのは気象だけで 初期状態は変わらない`` () =
    Assert.NotEqual<ActualWeather list>(Weather.generate scenario 1, Weather.generate scenario 2)

    // ゲームの初期状態は、シードの引数すら取らず、シナリオだけで決まる (運の要素は気象だけ)
    for sc in Scenarios.all do
        Assert.Equal(Turn.initialState sc, Turn.initialState sc)

[<Fact>]
let ``隠しパラメータは公開された範囲に収まる`` () =
    for sc in Scenarios.all do
        let h = Groundwater.hiddenOf sc

        for a in sc.Aquifers do
            Assert.InRange(h[a.Zone].InitialG, fst a.InitialGRangeWU, snd a.InitialGRangeWU)
            Assert.InRange(h[a.Zone].Yield, fst a.YieldRangeWU, snd a.YieldRangeWU)
            Assert.InRange(h[a.Zone].ReconBias, -1.0, 1.0)
            Assert.InRange(h[a.Zone].DrillBias, -1.0, 1.0)

[<Fact>]
let ``気象実績はカードの予報範囲に収まる`` () =
    for seed in seeds do
        for w in Weather.generate scenario seed do
            let lo, hi = w.Card.PrecipMm
            Assert.InRange(w.PrecipMm, lo, hi)
            Assert.Equal(Season.ofTurn w.Turn, w.Card.Season)

// ---------- v0.2: 地下水 ----------

[<Fact>]
let ``地下水の湧出は季節開始時の貯留量だけで決まり 当季の涵養は反映されない`` () =
    for r in allResults () do
        for z in r.Zones do
            let k = (Groundwater.spec scenario z.Zone).DischargeK
            Assert.True(near z.DischargeWU (z.GwStartWU * k))

[<Fact>]
let ``地下水貯留量の更新は G(t+1) = G(t) + R + 河川からの浸透 - P - B に従う`` () =
    for r in allResults () do
        for z in r.Zones do
            Assert.True(near z.GwEndWU (z.GwStartWU + z.DeepDrainageWU + z.SeepageWU - z.PumpedWU - z.DischargeWU))

[<Fact>]
let ``涵養は1ターン遅れて効く 当季の降水が違っても当季の湧出は変わらない`` () =
    // 同じ期首状態で降水だけが異なる2つの天気を解決し、涵養が違っても河川への湧出が同じことを確かめる
    let state = Turn.initialState scenario
    let weather = Weather.generate scenario 5
    let wet = { weather[0] with PrecipMm = 400.0 }
    let dry = { weather[0] with PrecipMm = 100.0 }
    let policy = policyOf 1.0 0.0 Turn.defaultPriority plans[0] state
    let _, rWet = Turn.resolve scenario state policy wet
    let _, rDry = Turn.resolve scenario state policy dry

    for (a, b) in List.zip rWet.Zones rDry.Zones do
        Assert.Equal(a.DischargeWU, b.DischargeWU)

[<Fact>]
let ``揚水は河川水で満たせなかった農業と生活産業の需要の範囲内に限られる`` () =
    for r in allResults () do
        let unmet =
            max 0.0 (r.AgDemandWU - r.Allocation.AgSupplyWU)
            + max 0.0 (r.UrbanDemandWU - r.Allocation.UrbanSupplyWU)

        Assert.True(r.GwSupplyWU <= unmet + 1e-6)

[<Fact>]
let ``最終評価の農業充足度には地下水の供給も含まれる`` () =
    for state in games () |> List.truncate 60 do
        let s = Evaluation.summarize scenario state
        let supply = state.History |> List.sumBy (fun r -> r.Allocation.AgSupplyWU + r.AgGwSupplyWU)
        let demand = state.History |> List.sumBy _.AgDemandWU
        Assert.True(near s.AgFillRate (supply / demand))

// ---------- v0.2: 不可逆性 ----------

[<Fact>]
let ``揚水コスト上昇フラグは一度有効になったら解除されない`` () =
    for state in games () do
        let mutable flagged = Set.empty

        for r in state.History do
            // 新たに有効になった区域は、それまでに有効でなかった区域だけ
            for z in r.NewCostFlags do
                Assert.DoesNotContain(z, flagged)

            flagged <- Set.union flagged (Set.ofList r.NewCostFlags)

        for z in flagged do
            Assert.True(state.CostFlags[z])

[<Fact>]
let ``閾値を下回るとフラグが有効になり 回復後も揚水費の倍率が続く`` () =
    // 森林の貯留量を閾値直上にした状態から井戸で汲み、閾値を割らせる
    let seed = 2
    let spec = Groundwater.spec scenario Forest
    let start = Turn.initialState scenario

    let primed =
        { start with
            GwWU = start.GwWU |> Map.add Forest (spec.ThresholdWU + 100.0)
            Survey = start.Survey |> Map.add Forest Reconnoitered
            Wells = start.Wells |> Map.add Forest WellBuilt }

    let weather = Weather.generate scenario seed
    let plan = { Name = "森林を汲む"; PumpLimit = 600.0; Action = fun _ -> NoAction }
    let policy (s: GameState) = policyOf 0.0 0.0 Turn.defaultPriority plan s

    let s1, r1 = Turn.resolve scenario primed (policy primed) weather[0]
    Assert.Contains(Forest, r1.NewCostFlags)
    Assert.True(s1.CostFlags[Forest])

    // 貯留量が閾値を大きく超えるまで回復させても、フラグは残る
    let recovered = { s1 with GwWU = s1.GwWU |> Map.add Forest (spec.ThresholdWU * 10.0) }
    let s2, r2 = Turn.resolve scenario recovered (policy recovered) weather[1]
    Assert.True(s2.CostFlags[Forest])
    Assert.DoesNotContain(Forest, r2.NewCostFlags)

    // 同じ揚水量でも、フラグ有効後の揚水費は倍率分だけ高い
    let cost flagged =
        Groundwater.pumpingCost scenario.Costs spec start.Hidden[Forest] flagged 100.0

    Assert.True(near (cost true) (cost false * spec.CostMultiplier))

// ---------- v0.2: 調査・井戸・推定値 ----------

[<Fact>]
let ``実行できない主要行動は無視され 1ターンの行動は最大1つ`` () =
    let state = Turn.initialState scenario
    let base' = policyOf 1.0 0.0 Turn.defaultPriority plans[0] state
    let normalized action = (Turn.normalizePolicy scenario state { base' with Action = action }).Action

    // 未調査の区域への試掘・井戸建設は実行できない
    Assert.Equal(NoAction, normalized (Survey(Farm, TestDrilling)))
    Assert.Equal(NoAction, normalized (BuildWell Farm))
    Assert.Equal(Survey(Farm, Reconnaissance), normalized (Survey(Farm, Reconnaissance)))

    for state in games () do
        for r in state.History do
            let n =
                [ r.Costs.Survey > 0.0; r.Costs.Build > 0.0; r.Costs.Invest > 0.0 ]
                |> List.filter id
                |> List.length
            Assert.True(n <= 1)

[<Fact>]
let ``調査状態と井戸建設状態は別々に管理され 井戸を建てても調査情報は失われない`` () =
    let seed = 3
    let w = Weather.generate scenario seed
    let plan = plans[1]
    let step s i = Turn.resolve scenario s (policyOf 1.0 0.0 Turn.defaultPriority plan s) w[i] |> fst
    let s1 = step (Turn.initialState scenario) 0
    Assert.Equal(Reconnoitered, s1.Survey[Farm])
    Assert.Equal(NoWell, s1.Wells[Farm])
    let s2 = step s1 1
    Assert.Equal(WellBuilt, s2.Wells[Farm])
    Assert.Equal(Reconnoitered, s2.Survey[Farm])
    // 試掘済みで、かつ井戸が建設されている状態も表現できる
    let s3 = step s2 2
    Assert.Equal(TestDrilled, s3.Survey[Farm])
    Assert.Equal(WellBuilt, s3.Wells[Farm])

[<Fact>]
let ``調査すると推定の幅が狭まり 推定値は真の値の誤差範囲に入る`` () =
    for seed in seeds do
        let w = Weather.generate scenario seed
        let plan = plans[1]
        let step s i = Turn.resolve scenario s (policyOf 1.0 0.0 Turn.defaultPriority plan s) w[i]
        let s0 = Turn.initialState scenario
        let prior = s0.Estimates[Farm]

        let s1, r1 = step s0 0
        let recon = s1.Estimates[Farm]
        Assert.True(recon.RelErr < prior.RelErr)
        Assert.Equal(1, r1.SurveyReports.Length)
        // 概査の推定は、真の貯留量の ±(概査の相対誤差) に収まる
        let actual = s1.GwWU[Farm]
        Assert.True(abs (recon.GwWU - actual) <= actual * scenario.Costs.ReconRelErr + 1e-6)

        let s2, _ = step s1 1
        let s3, _ = step s2 2
        let drill = s3.Estimates[Farm]
        Assert.True(drill.RelErr < recon.RelErr)
        let actual3 = s3.GwWU[Farm]
        Assert.True(abs (drill.GwWU - actual3) <= actual3 * scenario.Costs.TestDrillRelErr + 1e-6)

[<Fact>]
let ``推定値は公開情報だけで更新され 隠しの真の値には依存しない`` () =
    // 真の地下水貯留量だけが異なる2つの状態から同じ政策を進めても、未調査の区域の推定値は同じになる
    let seed = 4
    let w = Weather.generate scenario seed
    let a = Turn.initialState scenario
    let b = { a with GwWU = a.GwWU |> Map.map (fun _ g -> g * 1.7) }
    let plan = plans[0]
    let _, ra = Turn.resolve scenario a (policyOf 1.0 0.0 Turn.defaultPriority plan a) w[0]
    let _, rb = Turn.resolve scenario b (policyOf 1.0 0.0 Turn.defaultPriority plan b) w[0]
    Assert.NotEqual(ra.ActualGwAfter[Farm], rb.ActualGwAfter[Farm])
    Assert.Equal(ra.EstimatesAfter[Farm].GwWU, rb.EstimatesAfter[Farm].GwWU)

[<Fact>]
let ``費用は調査費と建設費と揚水費に集計され 負にならない`` () =
    for state in games () do
        let t = state.CostTotals
        Assert.True(t.Survey >= 0.0 && t.Build >= 0.0 && t.Invest >= 0.0 && t.Maintenance >= 0.0 && t.Pumping >= 0.0)
        Assert.True(near t.Survey (state.History |> List.sumBy _.Costs.Survey))
        Assert.True(near t.Build (state.History |> List.sumBy _.Costs.Build))
        Assert.True(near t.Invest (state.History |> List.sumBy _.Costs.Invest))
        Assert.True(near t.Maintenance (state.History |> List.sumBy _.Costs.Maintenance))
        Assert.True(near t.Pumping (state.History |> List.sumBy _.Costs.Pumping))

// ---------- 政策の入力範囲 ----------

[<Fact>]
let ``環境流水の目標はシナリオの最低基準を下回れない`` () =
    let state = Turn.initialState scenario
    let p = Turn.normalizePolicy scenario state (policyOf 0.0 (-5000.0) Turn.defaultPriority plans[0] state)
    Assert.Equal(scenario.MinEnvFlowWU[Spring], p.EnvTargetWU)

[<Fact>]
let ``農業の取水目標は需要を超えられず 揚水上限は負にならない`` () =
    let state = Turn.initialState scenario
    let raw = policyOf 1.0 0.0 Turn.defaultPriority plans[0] state

    let p =
        Turn.normalizePolicy
            scenario
            state
            { raw with
                AgTargetWU = 1e9
                PumpLimitWU = Map.ofList [ Farm, -50.0 ] }

    Assert.Equal(scenario.AgDemandWU[Spring], p.AgTargetWU)
    Assert.Equal(0.0, p.PumpLimitWU[Farm])
    Assert.Equal(0.0, p.PumpLimitWU[Forest])

[<Fact>]
let ``配分優先順位は春ターンにしか変更できない`` () =
    let flipped = [ Environment; Agriculture; Municipal ]
    let weather = Weather.generate scenario 1
    let s0 = Turn.initialState scenario
    let policy = policyOf 0.0 0.0 flipped plans[0] s0

    let s1, _ = Turn.resolve scenario s0 policy weather[0]
    Assert.Equal<Allocatee list>(flipped, s1.Priority)

    let s2, r2 = Turn.resolve scenario s1 { policy with Priority = Turn.defaultPriority } weather[1]
    Assert.Equal<Allocatee list>(flipped, s2.Priority)
    Assert.Equal<Allocatee list>(flipped, r2.EffectivePriority)

[<Fact>]
let ``全ターンで終了する`` () =
    let final = playGame 1 (policyOf 1.0 0.0 Turn.defaultPriority plans[1])
    Assert.Equal(12, scenario.Turns)
    Assert.Equal(scenario.Turns, final.Turn)
    Assert.Equal(scenario.Turns, final.History.Length)
    Assert.True(Turn.isFinished scenario final)

[<Fact>]
let ``最終評価の集計が結果と整合する`` () =
    let final = playGame 3 (policyOf 1.0 0.0 Turn.defaultPriority plans[1])
    let s = Evaluation.summarize scenario final
    Assert.InRange(s.AgFillRate, 0.0, 1.0)
    Assert.InRange(s.EnvAchievement, 0.0, 1.0)
    Assert.True(s.ResidualOk)
