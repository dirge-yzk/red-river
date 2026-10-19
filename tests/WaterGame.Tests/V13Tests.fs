/// 費用対効果の回帰テスト: 「設備を何も使わない」が、最良の戦略にならないことを、簡易プレイヤーで確かめる。
/// 各設備について、最初から (費用なしで) 持っていた場合の効果と、実際に費用を払って買い進めた場合の総合点を見る。
module WaterGame.Tests.V13Tests

open Xunit
open WaterGame.Core

type private Cfg =
    { Prio: Allocatee list
      AgFrac: float
      PumpFrac: float }

/// 小さな探索の範囲 (優先順位 2 通り × 農業の取水の絞り 2 通り × 揚水 2 通り)
let private grid =
    [ for prio in [ [ Municipal; Agriculture; Environment ]; [ Agriculture; Municipal; Environment ] ] do
          for ag in [ 1.0; 0.8 ] do
              for pf in [ 0.0; 0.6 ] do
                  yield { Prio = prio; AgFrac = ag; PumpFrac = pf } ]

let private seeds = [ 1 .. 8 ]

/// 1回のプレイ。init で開始時の状態 (設備の贈与) を変え、queue の順に、買えるときに買い進める
let private play (sc: Scenario) (seed: int) (c: Cfg) (init: GameState -> GameState) (queue: MajorAction list) =
    let w = Weather.generate sc seed
    let mutable st = init (Turn.initialState sc)

    for t in 0 .. sc.Turns - 1 do
        let season = Season.ofTurn t
        let action = queue |> List.tryFind (Turn.isValidAction sc st) |> Option.defaultValue NoAction

        let pump =
            sc.Zones
            |> List.map (fun z ->
                z.Id,
                if st.Wells[z.Id] = WellBuilt then
                    min (Turn.wellCapacity sc st z.Id) (c.PumpFrac * st.Estimates[z.Id].YieldWU)
                else
                    0.0)
            |> Map.ofList

        let policy =
            { AgTargetWU = Turn.effectiveAgDemand sc st season * c.AgFrac
              UrbanTargetWU = Turn.effectiveUrbanDemand sc st t
              EnvTargetWU = sc.MinEnvFlowWU[season]
              Priority = (if t % 4 = 0 then c.Prio else st.Priority)
              PumpLimitWU = pump
              ReservoirOpWU = 0.0
              Report = Accurate
              Action = action }

        // 貯水池の操作: 不足する見込みなら放流し、余る見込みなら余りの 9 割を貯める
        let policy =
            if st.Invested.Contains Reservoir then
                let o = Forecast.outlook sc st w[t].Card
                let a = Forecast.allocation policy o.Mid
                let demand = policy.AgTargetWU + policy.UrbanTargetWU

                let short =
                    a.AgSupplyWU + a.UrbanSupplyWU < demand - 1.0 || a.DownstreamFlowWU < sc.MinEnvFlowWU[season]

                let op =
                    if short then
                        -(min st.ReservoirWU 3000.0)
                    else
                        max 0.0 (min 4000.0 ((a.DownstreamFlowWU - sc.MinEnvFlowWU[season]) * 0.9))

                { policy with ReservoirOpWU = op }
            else
                policy

        st <- fst (Turn.resolve sc st policy w[t])

    (Evaluation.summarize sc st).Score

/// 探索の範囲の中の最良の平均点
let private best (sc: Scenario) (init: GameState -> GameState) (queue: MajorAction list) =
    grid |> List.map (fun c -> seeds |> List.averageBy (fun s -> play sc s c init queue)) |> List.max

let private withInv (l: Investment list) (s: GameState) = { s with Invested = Set.union s.Invested (Set.ofList l) }

let private withWell (z: ZoneId) (s: GameState) =
    { s with
        Wells = s.Wells |> Map.add z WellBuilt
        Survey = s.Survey |> Map.add z Reconnoitered }

/// 設備の費用対効果を、経済の設計として確かめるので、工事が発注した季節に完成するもの (資材・建設の影響は V17Tests で確かめる)
let private standard =
    Scenarios.all
    |> List.filter (fun s -> s.Name <> "チュートリアル")
    |> List.map (fun s -> { s with Construction = None })

[<Fact>]
let ``再生水 雨水利用 節水 灌漑効率は 多くのシナリオで 持っていないより総合点が高い`` () =
    let gifts =
        [ "再生水", withInv [ EffluentReuse ]
          "雨水利用", withInv [ RainwaterHarvest ]
          "節水キャンペーン", withInv [ WaterSaving ]
          "灌漑効率", withInv [ IrrigationEfficiency ] ]

    for name, gift in gifts do
        let wins =
            standard
            |> List.filter (fun sc -> best sc gift [] > best sc id [] + 0.01)
            |> List.length

        Assert.True(wins >= 4, $"{name}: 効果が +0.01 以上のシナリオが {wins}/{standard.Length}")

[<Fact>]
let ``井戸 (農地) は 多くのシナリオで 持っていないより総合点が高い`` () =
    let wins =
        standard
        |> List.filter (fun sc -> best sc (withWell Farm) [] > best sc id [] + 0.01)
        |> List.length

    Assert.True(wins >= 4, $"井戸の効果が +0.01 以上のシナリオが {wins}/{standard.Length}")

[<Fact>]
let ``貯水池は 多くのシナリオで 持っていないより総合点が高い`` () =
    let wins =
        standard
        |> List.filter (fun sc -> sc.Reservoir.IsSome && best sc (withInv [ Reservoir ]) [] > best sc id [] + 0.005)
        |> List.length

    Assert.True(wins >= 4, $"貯水池の効果が +0.005 以上のシナリオが {wins}/{standard.Length}")

[<Fact>]
let ``費用を払って買い進めた最良の戦略は 何も買わない最良の戦略より 総合点が高い`` () =
    // 候補の買い方 (どれも、資金の範囲で、費用を払う)
    let candidates =
        [ [ Invest EffluentReuse; Invest WaterSaving; Invest RainwaterHarvest ]
          [ Invest EffluentReuse; Invest IrrigationEfficiency ]
          [ Survey(Farm, Reconnaissance); BuildWell Farm; Invest EffluentReuse; Invest WaterSaving ] ]

    for sc in standard do
        let bought = candidates |> List.map (best sc id) |> List.max
        let none = best sc id []
        Assert.True(bought > none + 0.04, $"{sc.Name}: 最良の買い方 {bought:F3} / 何も買わない {none:F3}")

[<Fact>]
let ``井戸 (概査から建設まで) は 費用を払っても 再生水だけの場合と同程度かそれ以上で 半数近くのシナリオで上回る`` () =
    let without = [ Invest EffluentReuse ]
    let withWell = [ Survey(Farm, Reconnaissance); BuildWell Farm; Invest EffluentReuse ]

    let diffs = standard |> List.map (fun sc -> best sc id withWell - best sc id without)
    // どのシナリオでも、大きく悪くはならない
    Assert.True(diffs |> List.forall (fun d -> d > -0.02), $"{diffs}")
    // 上回るシナリオがある
    Assert.True((diffs |> List.filter (fun d -> d > 0.005) |> List.length) >= 2, $"{diffs}")

[<Fact>]
let ``何でも買う戦略は 費用で財政が悪化し 厳選した戦略より総合点が低い`` () =
    // 資金には限りがある (どれも買えば良いわけではない)
    let selective = [ Invest EffluentReuse; Invest RainwaterHarvest; Invest WaterSaving ]

    let everything =
        selective
        @ [ Survey(Farm, Reconnaissance)
            BuildWell Farm
            Survey(Wetland, Reconnaissance)
            BuildWell Wetland
            Invest(ObservationWell Farm)
            Invest(ObservationWell Wetland)
            Invest IrrigationEfficiency
            Invest Reservoir ]

    let sc = { Scenarios.normalYear with Construction = None }
    Assert.True(best sc id selective > best sc id everything)

[<Fact>]
let ``再生水の拡張は 費用を払っても 作成だけの場合より総合点が高い`` () =
    let baseQ = [ Invest EffluentReuse; Invest WaterSaving; Invest RainwaterHarvest ]
    let upQ = baseQ @ [ Invest EffluentUpgrade ]

    for sc in standard do
        let d = best sc id upQ - best sc id baseQ
        Assert.True(d > 0.003, $"{sc.Name}: 拡張の効果 {d:F3}")

[<Fact>]
let ``貯水池の拡張は 費用を払っても 作成だけの場合より悪くならず 貯水池の作成は 再生水だけの場合より高い`` () =
    let reuse = [ Invest EffluentReuse ]
    let small = reuse @ [ Invest Reservoir ]
    let expanded = small @ [ Invest ReservoirExpansion ]

    for sc in standard |> List.filter (fun s -> s.Reservoir.IsSome) do
        Assert.True(best sc id small > best sc id reuse + 0.003, $"{sc.Name}: 貯水池(小)")
        Assert.True(best sc id expanded >= best sc id small - 0.002, $"{sc.Name}: 拡張")
