module WaterGame.Tests.V14Tests

open Xunit
open WaterGame.Core
open WaterGame.App
open WaterGame.App.Model

let normal = Scenarios.normalYear
let now = System.DateTime(2025, 1, 1)

let withFunds (s: GameState) = { s with Balance = 1000000.0 }

let built (inv: Investment list) (s: GameState) = { s with Invested = Set.union s.Invested (Set.ofList inv) }

// ---------- 「作成する」と「拡張する」 ----------

[<Fact>]
let ``拡張は 作成のあとにだけ 1度だけ実施できる`` () =
    let s0 = Turn.initialState normal |> withFunds

    for (b, u) in [ Reservoir, ReservoirExpansion; IrrigationEfficiency, IrrigationUpgrade; EffluentReuse, EffluentUpgrade ] do
        Assert.False(Turn.isValidAction normal s0 (Invest u), $"{u} は作成の前には実施できない")
        Assert.True(Turn.isValidAction normal s0 (Invest b))
        let s1 = s0 |> built [ b ]
        Assert.False(Turn.isValidAction normal s1 (Invest b), $"{b} は1度だけ")
        Assert.True(Turn.isValidAction normal s1 (Invest u))
        let s2 = s1 |> built [ u ]
        Assert.False(Turn.isValidAction normal s2 (Invest u), $"{u} は1度だけ")

[<Fact>]
let ``入門のシナリオでは 貯水池も その拡張も実施できない`` () =
    let tut = Scenarios.tutorial
    let s = Turn.initialState tut |> withFunds |> built [ Reservoir ]
    Assert.False(Turn.isValidAction tut s (Invest Reservoir))
    Assert.False(Turn.isValidAction tut s (Invest ReservoirExpansion))

[<Fact>]
let ``貯水池の容量は 建設で小さく 拡張で大きくなり 拡張後の容量は 建設直後の2倍`` () =
    let r = normal.Reservoir.Value
    let s0 = Turn.initialState normal
    Assert.Equal(0.0, Turn.reservoirCapacity normal s0)
    Assert.Equal(r.CapacityWU, Turn.reservoirCapacity normal (s0 |> built [ Reservoir ]))
    Assert.Equal(r.ExpandedCapacityWU, Turn.reservoirCapacity normal (s0 |> built [ Reservoir; ReservoirExpansion ]))
    Assert.True(r.CapacityWU < r.ExpandedCapacityWU)
    Assert.Equal(2.0 * r.CapacityWU, r.ExpandedCapacityWU)

[<Fact>]
let ``灌漑効率は 作成で需要が減り 拡張でさらに減る`` () =
    let s0 = Turn.initialState normal
    let ag (s: GameState) = Turn.effectiveAgDemand normal s Summer
    let baseD = normal.AgDemandWU[Summer]
    Assert.True(abs (ag s0 - baseD) < 1e-9)
    let s1 = s0 |> built [ IrrigationEfficiency ]
    Assert.True(abs (ag s1 - baseD * (1.0 - normal.Invest.IrrigationDemandReduction)) < 1e-9)
    let s2 = s1 |> built [ IrrigationUpgrade ]
    Assert.True(abs (ag s2 - baseD * (1.0 - normal.Invest.IrrigationUpgradeReduction)) < 1e-9)
    Assert.True(ag s2 < ag s1 && ag s1 < ag s0)

[<Fact>]
let ``再生水の還元率は 作成より拡張のほうが大きい`` () =
    let s0 = Turn.initialState normal
    Assert.Equal(0.0, Turn.effluentReturnRate normal s0)
    let r1 = Turn.effluentReturnRate normal (s0 |> built [ EffluentReuse ])
    let r2 = Turn.effluentReturnRate normal (s0 |> built [ EffluentReuse; EffluentUpgrade ])
    Assert.Equal(normal.Invest.EffluentReturnRate, r1)
    Assert.Equal(normal.Invest.EffluentUpgradeReturnRate, r2)
    Assert.True(r1 < r2)

[<Fact>]
let ``拡張の費用は 長期の試練でも同じ倍率で高くなり どの拡張も正の費用である`` () =
    for sc in Scenarios.all do
        for i in [ IrrigationUpgrade; EffluentUpgrade ] do
            Assert.True(Turn.actionCost sc (Invest i) > 0.0, $"{sc.Name} {i}")

        match sc.Reservoir with
        | Some r ->
            Assert.True(r.ExpansionCost > 0.0 && r.ExpandedCapacityWU > r.CapacityWU, sc.Name)
        | None -> ()

    let long = Scenarios.longTrial
    Assert.True(Turn.actionCost long (Invest IrrigationUpgrade) > Turn.actionCost normal (Invest IrrigationUpgrade))

[<Fact>]
let ``拡張を含む全ての設備を使っても 水収支の残差は許容誤差内に収まる`` () =
    for sc in Scenarios.all do
        let w = Weather.generate sc 3
        let mutable s = Turn.initialState sc |> withFunds

        let all =
            [ IrrigationEfficiency; IrrigationUpgrade; EffluentReuse; EffluentUpgrade; RainwaterHarvest; WaterSaving ]
            @ (if sc.Reservoir.IsSome then [ Reservoir; ReservoirExpansion ] else [])

        s <- s |> built all

        for t in 0 .. sc.Turns - 1 do
            let season = Season.ofTurn t

            let policy =
                { AgTargetWU = Turn.effectiveAgDemand sc s season
                  UrbanTargetWU = Turn.effectiveUrbanDemand sc s t
                  EnvTargetWU = sc.MinEnvFlowWU[season]
                  Priority = s.Priority
                  PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 0.0) |> Map.ofList
                  ReservoirOpWU = (if t % 2 = 0 then 500.0 else -500.0)
                  Report = Accurate
                  Action = NoAction }

            let next, r = Turn.resolve sc s policy w[t]
            Assert.True(r.Balance.IsWithinTolerance, $"{sc.Name} turn {t}: {Turn.balanceLogLine r}")
            s <- next
