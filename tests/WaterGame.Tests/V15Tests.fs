/// 計画経済版 (v2.0-a): 指令・上司の信頼・計画達成度のテスト。
/// 特に、組織の層が水文学の状態を変えないことを確かめる。
module WaterGame.Tests.V15Tests

open Xunit
open WaterGame.Core

let withDirectives = Scenarios.all |> List.filter (fun s -> s.Directives.IsSome)

let policyFor (sc: Scenario) (state: GameState) (agFrac: float) =
    let season = Season.ofTurn state.Turn

    { AgTargetWU = Turn.effectiveAgDemand sc state season * agFrac
      UrbanTargetWU = Turn.effectiveUrbanDemand sc state state.Turn
      EnvTargetWU = sc.MinEnvFlowWU[season]
      Priority = state.Priority
      PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 0.0) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = Accurate
      Action = NoAction }

let play (sc: Scenario) (seed: int) (agFrac: float) =
    let w = Weather.generate sc seed
    let mutable s = Turn.initialState sc

    for t in 0 .. sc.Turns - 1 do
        s <- fst (Turn.resolve sc s (policyFor sc s agFrac) w[t])

    s

[<Fact>]
let ``入門には指令がなく それ以外のシナリオには指令がある`` () =
    Assert.True(Scenarios.tutorial.Directives.IsNone)
    Assert.Empty((Turn.initialState Scenarios.tutorial).Organization.Directives)
    Assert.Equal(5, withDirectives.Length)

    for sc in withDirectives do
        Assert.NotEmpty((Turn.initialState sc).Organization.Directives)

[<Fact>]
let ``指令の列は シナリオだけで決まり 乱数を使わず 種類と目標が変化に富む`` () =
    for sc in withDirectives do
        Assert.Equal<Directive list>(Organization.generate sc, Organization.generate sc)
        let ds = Organization.generate sc
        // 種類は両方が出て、目標も1通りではない
        Assert.True(ds |> List.map _.Kind |> List.distinct |> List.length >= 2, sc.Name)
        Assert.True(ds |> List.map _.TargetFill |> List.distinct |> List.length >= 3, sc.Name)
        // 最初の指令は、範囲のいちばん下
        Assert.Equal(sc.Directives.Value.TargetMin, ds.Head.TargetFill)

[<Fact>]
let ``指令の期間は 隙間なく 最初のターンから 最後まで続き 2〜3ターンずつである`` () =
    for sc in withDirectives do
        let ds = Organization.generate sc
        Assert.Equal(0, ds.Head.IssuedTurn)

        for (a, b) in List.pairwise ds do
            Assert.Equal(a.DeadlineTurn + 1, b.IssuedTurn)

        for d in ds do
            Assert.InRange(d.DeadlineTurn - d.IssuedTurn + 1, 1, 3)
            Assert.InRange(d.TargetFill, 0.5, 1.0)
            Assert.True(d.DeadlineTurn <= sc.Turns - 1)

        // 最後のターンだけの指令は出さない
        Assert.True(ds |> List.forall (fun d -> d.IssuedTurn < sc.Turns - 1))

[<Fact>]
let ``綿花の指令は 農業の需要がある期間にだけ出る`` () =
    for sc in withDirectives do
        for d in Organization.generate sc do
            if d.Kind = CottonQuota then
                let demand = [ d.IssuedTurn .. d.DeadlineTurn ] |> List.sumBy (fun t -> sc.AgDemandWU[Season.ofTurn t])
                Assert.True(demand > 0.0)

[<Fact>]
let ``組織の層は 水文学の状態を変えない: 指令の有無で 水量の結果が同じ`` () =
    for sc in withDirectives do
        let without = { sc with Directives = None }

        for seed in [ 1; 2; 3 ] do
            let a = play sc seed 0.9
            let b = play without seed 0.9

            // 水循環・地下水・配分は、まったく同じ (交付金の倍率で財政だけが変わりうる)
            Assert.Equal<Map<ZoneId, float>>(b.GwWU, a.GwWU)
            Assert.Equal<Map<ZoneId, float>>(b.SoilWU, a.SoilWU)

            for (x, y) in List.zip a.History b.History do
                Assert.Equal<ZoneResult list>(y.Zones, x.Zones)
                Assert.Equal(y.Allocation, x.Allocation)
                Assert.Equal(y.AgFillRate, x.AgFillRate)
                Assert.Equal(y.UrbanFillRate, x.UrbanFillRate)
                Assert.True(x.Balance.IsWithinTolerance)

[<Fact>]
let ``指令は 期限のターンに 1度だけ判定され 判定済みの記録と結果が一致する`` () =
    for sc in withDirectives do
        let s = play sc 5 1.0
        let ds = s.Organization.Directives
        Assert.Equal(ds.Length, s.Organization.Resolved.Length)
        Assert.Equal<int list>(ds |> List.map _.Id, s.Organization.Resolved |> List.map (fun r -> r.Directive.Id))

        for r in s.History do
            let expected = ds |> List.filter (fun d -> d.DeadlineTurn = r.Turn) |> List.map _.Id
            Assert.Equal<int list>(expected, r.Plan.Resolved |> List.map (fun x -> x.Directive.Id))
            let issued = ds |> List.filter (fun d -> d.IssuedTurn = r.Turn) |> List.map _.Id
            Assert.Equal<int list>(issued, r.Plan.Issued |> List.map _.Id)

[<Fact>]
let ``指令の判定は 期間の充足率と目標の比で決まり 達成のときだけ報酬が出る`` () =
    for sc in withDirectives do
        let s = play sc 4 1.0

        for r in s.Organization.Resolved do
            let d = r.Directive
            let window = s.History |> List.filter (fun h -> h.Turn >= d.IssuedTurn && h.Turn <= d.DeadlineTurn)

            let supply, demand =
                match d.Kind with
                | BuildOrder -> 0.0, 0.0 // 建設期限は、充足率ではなく、完成で判定する (V17Tests)
                | CottonQuota -> window |> List.sumBy (fun h -> h.Allocation.AgSupplyWU + h.AgGwSupplyWU), window |> List.sumBy _.AgDemandWU
                | IndustrialSupply -> window |> List.sumBy (fun h -> h.Allocation.UrbanSupplyWU + h.UrbanGwSupplyWU), window |> List.sumBy _.UrbanDemandWU

            let measured = if demand <= 0.0 then 1.0 else supply / demand
            if d.Kind <> BuildOrder then Assert.True(abs (measured - r.Measured) < 1e-9)
            Assert.True(abs (r.Ratio - r.Measured / d.TargetFill) < 1e-9)

            match r.Outcome with
            | Achieved ->
                Assert.True(r.Ratio >= 1.0)
                Assert.True(r.Reward > 0.0 && r.TrustChange > 0.0)
            | Partial ->
                Assert.True(r.Ratio >= 0.9 && r.Ratio < 1.0)
                Assert.Equal(0.0, r.Reward)
                Assert.Equal(0.0, r.TrustChange)
            | Failed ->
                Assert.True(r.Ratio < 0.9)
                Assert.Equal(0.0, r.Reward)
                Assert.True(r.TrustChange < 0.0)

[<Fact>]
let ``上司の信頼は 0〜100に収まり 前のターンの信頼と報酬から 期末残高が決まる`` () =
    for sc in withDirectives do
        for seed in 1..6 do
            let s = play sc seed (if seed % 2 = 0 then 1.0 else 0.3)
            Assert.InRange(s.Organization.Trust, 0.0, 100.0)

            for (a, b) in List.pairwise s.History do
                Assert.Equal(a.Plan.TrustAfter, b.Plan.TrustBefore)

            for r in s.History do
                Assert.InRange(r.Plan.TrustAfter, 0.0, 100.0)
                Assert.True(abs (r.BalanceEnd - (r.BalanceStart + r.Grant + r.Revenue + r.Plan.Reward - r.Plan.Fine - r.Costs.Total)) < 1e-6)

[<Fact>]
let ``年度の交付金は 上司の信頼が高いほど多い`` () =
    let sc = Scenarios.normalYear
    let s0 = Turn.initialState sc
    let at trust = Turn.grantThisTurn sc { s0 with Organization = { s0.Organization with Trust = trust } }
    Assert.True(at 0.0 < at 50.0 && at 50.0 < at 100.0)
    Assert.True(abs (at 50.0 - sc.Finance.AnnualGrant) < 1e-9)

    // 入門は、信頼に関係なく変わらない
    let t0 = Turn.initialState Scenarios.tutorial
    let g trust = Turn.grantThisTurn Scenarios.tutorial { t0 with Organization = { t0.Organization with Trust = trust } }
    Assert.Equal(g 0.0, g 100.0)

[<Fact>]
let ``計画達成度は 判定済みの指令の達成率の平均で 入門では None`` () =
    let sc = Scenarios.normalYear
    let s = play sc 2 1.0
    let expected = s.Organization.Resolved |> List.averageBy (fun r -> min 1.0 r.Ratio)
    let plan = (Evaluation.summarize sc s).Plan.Value
    Assert.True(abs (plan.Achievement.Value - expected) < 1e-9)
    Assert.Equal(s.Organization.Trust, plan.Trust)

    let tut = Scenarios.tutorial
    Assert.True((Evaluation.summarize tut (play tut 2 1.0)).Plan.IsNone)

[<Fact>]
let ``指令の判定の前後で 同じ状態に同じ入力なら 結果は同じである`` () =
    let sc = Scenarios.drought
    let a = play sc 9 0.8
    let b = play sc 9 0.8
    Assert.Equal<DirectiveResult list>(a.Organization.Resolved, b.Organization.Resolved)
    Assert.Equal(a.Organization.Trust, b.Organization.Trust)
