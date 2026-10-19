module WaterGame.Tests.V11Tests

open System
open Xunit
open WaterGame.Core
open WaterGame.App
open WaterGame.App.Model

let near (a: float) (b: float) = abs (a - b) <= 1e-6 * max 1.0 (abs a)

let normal = Scenarios.normalYear
let tutorial = Scenarios.tutorial
let now = DateTime(2026, 1, 1, 0, 0, 0, DateTimeKind.Utc)

let policyFor (sc: Scenario) (state: GameState) =
    let season = Season.ofTurn state.Turn

    { AgTargetWU = Turn.effectiveAgDemand sc state season
      UrbanTargetWU = Turn.urbanDemand sc state.Turn
      EnvTargetWU = sc.MinEnvFlowWU[season]
      Priority = state.Priority
      PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 0.0) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = Accurate
      Action = NoAction }

// ---------- 今季の見込み ----------

/// ゲームを途中まで進めた状態を、いろいろな場面で作る
let midStates () =
    [ for sc in Scenarios.all do
        for seed in [ 1; 2; 3 ] do
            let w = Weather.generate sc seed

            let states =
                w
                |> List.truncate (min 8 sc.Turns)
                |> List.scan (fun s x -> Turn.resolve sc s (policyFor sc s) x |> fst) (Turn.initialState sc)

            for s in states do
                if not (Turn.isFinished sc s) then
                    yield sc, w, s ]

[<Fact>]
let ``見込みは 降水の下限 中央 上限の順に利用可能量が増える`` () =
    for sc, w, s in midStates () do
        let o = Forecast.outlook sc s w[s.Turn].Card
        Assert.True(o.Low.AvailableWU <= o.Mid.AvailableWU + 1e-9)
        Assert.True(o.Mid.AvailableWU <= o.High.AvailableWU + 1e-9)
        Assert.True(o.Low.BelowWU <= o.High.BelowWU + 1e-9)

[<Fact>]
let ``見込みは 隠しの真の地下水貯留量に依存せず 推定値だけで決まる`` () =
    for sc, w, s in midStates () |> List.truncate 30 do
        let doubled = { s with GwWU = s.GwWU |> Map.map (fun _ g -> g * 2.0) }
        let a = Forecast.outlook sc s w[s.Turn].Card
        let b = Forecast.outlook sc doubled w[s.Turn].Card
        Assert.Equal(a.Mid.AvailableWU, b.Mid.AvailableWU)

[<Fact>]
let ``見込みは 確定済みの気象実績を使わない 予報のカードだけから計算する`` () =
    // 実績の降水を変えても、見込み(カードだけが入力)は変わらない
    for sc, w, s in midStates () |> List.truncate 20 do
        let card = w[s.Turn].Card
        let a = Forecast.outlook sc s card
        let other = { card with Name = card.Name + "(別名)" }
        let b = Forecast.outlook sc s other
        Assert.Equal(a.Low.AvailableWU, b.Low.AvailableWU)

[<Fact>]
let ``推定が真の値と同じで 降水と蒸発散が見込みどおりなら 見込みは実際の計算と一致する`` () =
    for sc in Scenarios.all do
        let seed = 5
        let w = Weather.generate sc seed
        let s0 = Turn.initialState sc
        // 推定を真の貯留量に合わせる
        let s = { s0 with Estimates = s0.Estimates |> Map.map (fun z e -> { e with GwWU = s0.GwWU[z] }) }
        let card = w[0].Card
        // 予報の幅を実績の1点に絞り、蒸発散も見込み(係数帯の中央)に合わせる
        let narrow = { card with PrecipMm = (w[0].PrecipMm, w[0].PrecipMm) }
        let o = Forecast.outlook sc s narrow
        let actual = { w[0] with EtDemandMm = Forecast.etMm sc narrow }
        let _, r = Turn.resolve sc s (policyFor sc s) actual
        Assert.True(near o.Mid.AvailableWU r.Allocation.AvailableWU)
        Assert.True(near o.Mid.BelowWU r.Allocation.BelowContributionWU)

[<Fact>]
let ``見込みの配分は 実際の配分関数と同じ結果になる`` () =
    for sc, w, s in midStates () |> List.truncate 20 do
        let o = Forecast.outlook sc s w[s.Turn].Card
        let p = policyFor sc s
        let a = Forecast.allocation p o.Mid
        let b = Hydrology.allocate p.Priority o.Mid.AvailableWU o.Mid.BelowWU p.AgTargetWU p.UrbanTargetWU p.EnvTargetWU
        Assert.Equal(b, a)

[<Fact>]
let ``揚水の見込み 井戸がなければ揚水は0 井戸があれば上限と能力の範囲`` () =
    let sc = normal
    let w = Weather.generate sc 3
    let s0 = Turn.initialState sc
    let o = Forecast.outlook sc s0 w[0].Card
    let z = o.Mid.Zones |> List.find (fun x -> x.Zone = Farm)
    // 井戸なし: G + R - B
    Assert.True(near (Forecast.gwEndIfPumpedToLimit sc s0 o.Mid Farm 5000.0) (z.GwStartWU + z.DeepDrainageWU + z.SeepageWU - z.DischargeWU))
    // 井戸あり: 上限が小さければ、その分だけ減る
    let wells = { s0 with Wells = s0.Wells |> Map.add Farm WellBuilt }
    let small = Forecast.gwEndIfPumpedToLimit sc wells o.Mid Farm 100.0
    let large = Forecast.gwEndIfPumpedToLimit sc wells o.Mid Farm 5000.0
    Assert.True(near small (z.GwStartWU + z.DeepDrainageWU + z.SeepageWU - 100.0 - z.DischargeWU))
    Assert.True(large <= small)
    Assert.True(large >= 0.0)

[<Fact>]
let ``最終ターンの後は 自由に使える資金に翌年の交付金を足さない`` () =
    let final =
        Weather.generate tutorial 1
        |> List.fold (fun s x -> Turn.resolve tutorial s (policyFor tutorial s) x |> fst) (Turn.initialState tutorial)

    Assert.True(Turn.isFinished tutorial final)
    Assert.Equal(final.Balance, Turn.availableFunds tutorial final)

// ---------- Model (画面の状態遷移) ----------

let private send msgs (m: Model) =
    msgs |> List.fold (fun (m: Model) msg -> Model.update msg m |> fst) m

let private fresh (sc: Scenario) =
    { Model.newGame sc 1 now with Overlay = NoOverlay }

let private nextTurn (m: Model) =
    m |> send [ EndTurn now; CloseOverlay now ]

[<Fact>]
let ``前の季節の設定が 次の季節の初期値に引き継がれる`` () =
    let m0 = fresh normal
    // 農業は需要の半分、環境流水は最低基準に 500 上乗せ
    let demand = Turn.effectiveAgDemand normal m0.Game Spring
    let envMin = normal.MinEnvFlowWU[Spring]
    let m1 = m0 |> send [ SetAgTarget(demand / 2.0); SetEnvTarget(envMin + 500.0) ]
    let m2 = nextTurn m1
    // 夏: 需要が変わっても割合 (半分) と上乗せ (500) を引き継ぐ
    let demandSummer = Turn.effectiveAgDemand normal m2.Game Summer
    Assert.True(abs (m2.AgTargetWU - demandSummer / 2.0) <= 25.0)
    Assert.Equal(normal.MinEnvFlowWU[Summer] + 500.0, m2.EnvTargetWU)

[<Fact>]
let ``揚水上限は 井戸のある区域で前の値が引き継がれる`` () =
    let m0 = fresh normal
    let wells = { m0.Game with Wells = m0.Game.Wells |> Map.add Farm WellBuilt; Survey = m0.Game.Survey |> Map.add Farm Reconnoitered }
    let m1 = { m0 with Game = wells }
    let m2 = m1 |> send [ SetPumpLimit(Farm, 300.0) ] |> nextTurn
    Assert.Equal(300.0, m2.PumpLimits[Farm])
    // 井戸のない区域は 0
    Assert.Equal(0.0, m2.PumpLimits[Forest])

[<Fact>]
let ``優先順位の並べ替えは春だけで 位置の入れ替えになる`` () =
    let m0 = fresh normal
    let moved = m0 |> send [ MovePriority(0, 1) ]
    // 初期値 (農業・環境流水・生活産業) の先頭2つを入れ替えた並び
    Assert.Equal<Allocatee list>([ Environment; Agriculture; Municipal ], moved.Priority)
    Assert.Equal<Allocatee list>([ Agriculture; Environment; Municipal ], Turn.defaultPriority)
    // 範囲外は無視
    Assert.Equal<Allocatee list>(m0.Priority, (m0 |> send [ MovePriority(0, -1) ]).Priority)
    // 夏は並べ替えられない
    let summer = nextTurn moved
    Assert.Equal(Summer, Season.ofTurn summer.Game.Turn)
    Assert.Equal<Allocatee list>(moved.Priority, (summer |> send [ MovePriority(0, 1) ]).Priority)

[<Fact>]
let ``主要行動は 選ぶと取り消せて 実行できない行動は選べない`` () =
    let m0 = fresh normal
    let recon = Survey(Farm, Reconnaissance)
    Assert.Equal(recon, (m0 |> send [ ChooseAction recon ]).Action)
    Assert.Equal(NoAction, (m0 |> send [ ChooseAction recon; ChooseAction recon ]).Action)
    // 概査の前の試掘・井戸は選べない
    Assert.Equal(NoAction, (m0 |> send [ ChooseAction(Survey(Farm, TestDrilling)) ]).Action)
    Assert.Equal(NoAction, (m0 |> send [ ChooseAction(BuildWell Farm) ]).Action)

[<Fact>]
let ``区域を選ぶと その区域の行動が選ばれる (区域を切り替えても前の区域の行動にならない)`` () =
    let m0 = fresh normal
    let m1 = m0 |> send [ SelectZone Urban; ChooseAction(Survey(Urban, Reconnaissance)) ]
    Assert.Equal(Survey(Urban, Reconnaissance), m1.Action)
    let m2 = m1 |> send [ EndTurn now ]

    match m2.Overlay with
    | ShowResult r -> Assert.Equal(Survey(Urban, Reconnaissance), r.ActionTaken)
    | _ -> failwith "結果が出ていない"

[<Fact>]
let ``最終結果は閉じて 開き直せる`` () =
    let finishedModel =
        let m0 = fresh tutorial
        // 最後の結果カードを閉じる直前まで進め、閉じて最終結果にする
        let rec go (m: Model) =
            if Turn.isFinished m.Scenario m.Game then m
            else go (m |> send [ EndTurn now ] |> fun x -> if Turn.isFinished x.Scenario x.Game then x else x |> send [ CloseOverlay now ])

        go m0 |> send [ CloseOverlay now ]

    match finishedModel.Overlay with
    | ShowFinal _ -> ()
    | o -> failwith $"最終結果になっていない: {o}"

    let closed = finishedModel |> send [ CloseFinal ]
    Assert.Equal(NoOverlay, closed.Overlay)
    let reopened = closed |> send [ ReopenFinal ]

    match reopened.Overlay with
    | ShowFinal _ -> ()
    | _ -> failwith "開き直せていない"
    // 終了後は、季節を進める操作は無視される
    Assert.Equal(closed.Game.Turn, (closed |> send [ EndTurn now ]).Game.Turn)

[<Fact>]
let ``結果カードを閉じるまで 次のターンの方針は始まらない`` () =
    let m0 = fresh normal
    let m1 = m0 |> send [ EndTurn now ]
    match m1.Overlay with
    | ShowResult r -> Assert.Equal(0, r.Turn)
    | _ -> failwith "結果が出ていない"
    // 結果を見ている間に、もう一度 EndTurn を送っても進まない
    Assert.Equal(m1.Game.Turn, (m1 |> send [ EndTurn now ]).Game.Turn)

[<Fact>]
let ``起動オプションを解釈する`` () =
    let o = Model.parseArgs [| "--scenario"; "渇水"; "--seed"; "42"; "--skip-intro" |]
    Assert.Equal(Some "渇水", o.Scenario)
    Assert.Equal(Some 42, o.Seed)
    Assert.True(o.SkipIntro)
    let none = Model.parseArgs [||]
    Assert.Equal(None, none.Scenario)
    // 不正なシードは無視される
    Assert.Equal(None, (Model.parseArgs [| "--seed"; "abc" |]).Seed)


// ---------- ラウンド2の改善 ----------

[<Fact>]
let ``スライダーは 右端の近くで需要そのものに丸められ 環境流水は最低基準から刻む`` () =
    let m0 = fresh normal
    let demand = Turn.effectiveAgDemand normal m0.Game Spring
    // 需要が50の倍数でなくても、右端付近では需要ぴったり
    let m1 = m0 |> send [ SetAgTarget(demand - 10.0) ]
    Assert.Equal(demand, m1.AgTargetWU)
    Assert.Equal(1.0, m1.AgShare)
    // 右端を超えても需要まで
    Assert.Equal(demand, (m0 |> send [ SetAgTarget(demand + 1000.0) ]).AgTargetWU)
    // 環境流水は、最低基準 + 刻みの倍数
    let envMin = normal.MinEnvFlowWU[Spring]
    let m2 = m0 |> send [ SetEnvTarget(envMin + 120.0) ]
    Assert.True(near (m2.EnvTargetWU - envMin) 100.0 || near (m2.EnvTargetWU - envMin) 150.0)
    Assert.Equal(envMin, (m0 |> send [ SetEnvTarget(envMin - 500.0) ]).EnvTargetWU)

[<Fact>]
let ``優先順位は 春だけ 指定した項目を1位へ移せる`` () =
    let m0 = fresh normal
    let top = m0 |> send [ PriorityToTop 2 ]
    Assert.Equal<Allocatee list>([ Municipal; Agriculture; Environment ], top.Priority)
    Assert.Equal<Allocatee list>(m0.Priority, (m0 |> send [ PriorityToTop 0 ]).Priority)
    Assert.Equal<Allocatee list>(m0.Priority, (m0 |> send [ PriorityToTop 7 ]).Priority)
    let summer = nextTurn top
    Assert.Equal<Allocatee list>(top.Priority, (summer |> send [ PriorityToTop 1 ]).Priority)

[<Fact>]
let ``揚水コスト上昇フラグは 井戸のある区域だけに立つ`` () =
    // 貯留量が閾値を下回るほど少ない状態で 井戸なしなら立たず 井戸ありなら立つ
    let sc = normal
    let st0 = Turn.initialState sc
    let low = { st0 with GwWU = st0.GwWU |> Map.map (fun z _ -> (Groundwater.spec sc z).ThresholdWU * 0.1) }
    let w = (Weather.generate sc 1)[0]
    let after, r = Turn.resolve sc low (policyFor sc low) w
    Assert.True(after.CostFlags |> Map.forall (fun _ f -> not f))
    Assert.True(r.NewCostFlags.IsEmpty)
    let withWell = { low with Wells = low.Wells |> Map.add Farm WellBuilt; Survey = low.Survey |> Map.add Farm Reconnoitered }
    let after2, _ = Turn.resolve sc withWell (policyFor sc withWell) w
    Assert.True(after2.CostFlags[Farm])
    Assert.False(after2.CostFlags[Forest])

[<Fact>]
let ``揚水上限を0にすると 次の季節も0のまま 完成直後の井戸だけ初期値から始まる`` () =
    let m0 = fresh normal
    let wells = { m0.Game with Wells = m0.Game.Wells |> Map.add Farm WellBuilt; Survey = m0.Game.Survey |> Map.add Farm Reconnoitered }
    let m1 = { m0 with Game = wells }
    let m2 = m1 |> send [ SetPumpLimit(Farm, 0.0) ] |> nextTurn
    Assert.Equal(0.0, m2.PumpLimits[Farm])
    // 井戸を建てた季節の次は、初期値 (推定の持続量を丸めたもの) から
    let m3 = fresh normal |> send [ ChooseAction(Survey(Farm, Reconnaissance)) ] |> nextTurn
    let m4 = m3 |> send [ ChooseAction(BuildWell Farm) ] |> nextTurn
    Assert.Equal(WellBuilt, m4.Game.Wells[Farm])
    Assert.True(m4.PumpLimits[Farm] > 0.0)

[<Fact>]
let ``結果カードの表示中も 方針の目標は次の季節の需要の範囲に収まっている`` () =
    let m0 = fresh normal
    let r = m0 |> send [ SetAgTarget 99999.0; SetUrbanTarget 99999.0 ] |> send [ EndTurn now ]
    let season = Season.ofTurn r.Game.Turn
    Assert.True(r.AgTargetWU <= Turn.effectiveAgDemand normal r.Game season + 1e-9)
    Assert.True(r.UrbanTargetWU <= Turn.urbanDemand normal r.Game.Turn + 1e-9)

[<Fact>]
let ``地下水がほぼ空でも 振り返りに桁外れの誤差の割合を出さない`` () =
    let sc = normal
    let w = Weather.generate sc 1
    let st0 = Turn.initialState sc
    let final = w |> List.fold (fun s x -> Turn.resolve sc s (policyFor sc s) x |> fst) st0
    // 最終ターンの実際の貯留量がほぼ空だった状態にする
    let emptied =
        { final with
            History =
                final.History
                |> List.mapi (fun i r ->
                    if i = final.History.Length - 1 then
                        { r with ActualGwAfter = r.ActualGwAfter |> Map.map (fun _ _ -> 1e-13) }
                    else
                        r) }

    // 割合などに 8 桁以上の数字が出ていない
    let huge (t: string) = System.Text.RegularExpressions.Regex.IsMatch(t, "[0-9]{8}")

    for l in Evaluation.debrief sc emptied do
        Assert.False(huge l, l)

    for (_, _, body) in Evaluation.reflections sc emptied (Evaluation.summarize sc emptied) do
        Assert.False(huge body, body)
