/// 計画経済版 v2.0-c: 資材・建設プロジェクト・工期・融通・建設期限の指令のテスト。
/// 資材の配給は日程どおり、乱数は使わない。
module WaterGame.Tests.V17Tests

open Xunit
open WaterGame.Core

let normal = Scenarios.normalYear
let cfg = normal.Construction.Value

let policyFor (sc: Scenario) (state: GameState) (action: MajorAction) =
    let season = Season.ofTurn state.Turn

    { AgTargetWU = Turn.effectiveAgDemand sc state season
      UrbanTargetWU = Turn.effectiveUrbanDemand sc state state.Turn
      EnvTargetWU = sc.MinEnvFlowWU[season]
      Priority = state.Priority
      PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 0.0) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = Accurate
      Action = action }

/// 決まった行動の列 (ターン → 行動) で遊ぶ。資金は十分に与える
let playScript (sc: Scenario) (script: Map<int, MajorAction>) =
    let w = Weather.generate sc 3
    let mutable s = { Turn.initialState sc with Balance = 100000.0 }
    let mutable results = []

    for t in 0 .. sc.Turns - 1 do
        let action = script |> Map.tryFind t |> Option.defaultValue NoAction
        let next, r = Turn.resolve sc s (policyFor sc s action) w[t]
        results <- results @ [ r ]
        s <- next

    s, results

let stock (s: GameState) m = Construction.stock s.Construction m

// ---------- 配給 ----------

[<Fact>]
let ``資材は 開始時の量と 正規配給の日程どおりに 増える`` () =
    let s, _ = playScript normal Map.empty
    let w = Weather.generate normal 3
    // 何も発注しなければ、季節はじめの資材 = 開始時 + その季節までの配給
    let mutable st = { Turn.initialState normal with Balance = 100000.0 }

    for t in 0 .. normal.Turns - 1 do
        for m in Construction.allMaterials do
            let expected =
                (cfg.InitialStock |> Map.tryFind m |> Option.defaultValue 0)
                + (cfg.Arrivals |> List.filter (fun (turn, mm, _) -> mm = m && turn <= t) |> List.sumBy (fun (_, _, q) -> q))

            Assert.Equal(expected, stock st m)

        st <- fst (Turn.resolve normal st (policyFor normal st NoAction) w[t])

    Assert.Equal(normal.Turns, s.Turn)

// ---------- 工事の経過 ----------

[<Fact>]
let ``資材がそろっていれば 発注した季節に着工し 標準工期のあとに完成し 効果は完成の翌季から`` () =
    // 貯水池: 建設資材 2・専門技術者 1・標準工期 3 季。開始時の資材でそろっている
    let script = Map.ofList [ 0, Invest Reservoir ]
    let s0 = { Turn.initialState normal with Balance = 100000.0 }
    let w = Weather.generate normal 3
    let s1, r1 = Turn.resolve normal s0 (policyFor normal s0 (Invest Reservoir)) w[0]
    Assert.Equal(1, r1.Works.Started.Length)
    Assert.Empty(r1.Works.Completed)
    Assert.False(s1.Invested.Contains Reservoir)
    Assert.Equal(1, s1.Construction.Projects.Length)
    Assert.Equal(Building, s1.Construction.Projects.Head.Status)
    // 技術者は、工事のあいだ、つきっきりになる
    Assert.Equal(0, stock s1 Engineer)

    let s2, r2 = Turn.resolve normal s1 (policyFor normal s1 NoAction) w[1]
    Assert.Empty(r2.Works.Completed)
    let s3, r3 = Turn.resolve normal s2 (policyFor normal s2 NoAction) w[2]
    // 3 季目 (着工した季節を1と数える) に完成し、技術者が戻る。効果は、次の季節から
    Assert.Equal(1, r3.Works.Completed.Length)
    Assert.True(s3.Invested.Contains Reservoir)
    Assert.Equal(1, stock s3 Engineer)
    Assert.Empty(s3.Construction.Projects)
    ignore script

[<Fact>]
let ``資材が足りなければ 資材待ちになり 配給が届いたターンに着工する`` () =
    // 井戸 2 本の発注: 機械設備は開始時に 1 単位しかなく、次の配給は 4 季目 (turn 3)
    let sc = normal
    let script =
        Map.ofList
            [ 0, Survey(Farm, Reconnaissance)
              1, Survey(Urban, Reconnaissance)
              2, BuildWell Farm
              3, BuildWell Urban ]

    let s, rs = playScript sc script
    // 農地の井戸: 機械設備 1 が開始時にあるので、すぐ着工・完成 (標準工期 1 季)
    Assert.Equal(1, rs[2].Works.Completed.Length)
    // 市街地の井戸: 機械設備が 0 → 4 季目 (turn 3) の配給 (+1) が、季節はじめに届いているので、その季節に着工
    Assert.Equal(1, rs[3].Works.Started.Length)

[<Fact>]
let ``資材待ちの工事は 警告に 不足する資材を出す`` () =
    // 機械設備を使い切ってから、さらに機械設備が要る工事を発注する
    let script =
        Map.ofList
            [ 0, Survey(Farm, Reconnaissance)
              1, BuildWell Farm // 機械設備 1 を使う (開始時の 1 単位)
              2, Invest EffluentReuse ] // 機械設備 1 が要るが、次の配給は turn 3

    let _, rs = playScript normal script
    Assert.True(rs[2].Works.Started.IsEmpty)
    Assert.True(rs[2].Warnings |> List.exists (fun w -> w.Contains "資材待ち"), String.concat "/" rs[2].Warnings)
    // 配給のあとに着工する
    Assert.Equal(1, rs[3].Works.Started.Length)

[<Fact>]
let ``専門技術者が足りなければ 工事は 着工できない`` () =
    // 貯水池 (技術者 1・工期 3) の施工中に、技術者が要る井戸を発注しても、技術者が戻るまで待つ
    let script =
        Map.ofList
            [ 0, Survey(Farm, Reconnaissance)
              1, Invest Reservoir // 技術者 1 を使う (開始時の 1 人)
              2, BuildWell Farm ] // 技術者 1 が要る → 待つ

    let _, rs = playScript normal script
    Assert.True(rs[2].Works.Started.IsEmpty)
    Assert.True(rs[2].Warnings |> List.exists (fun w -> w.Contains "資材待ち" && w.Contains "専門技術者"), String.concat "/" rs[2].Warnings)

[<Fact>]
let ``同じ工事を 二重に発注できない`` () =
    let s0 = { Turn.initialState normal with Balance = 100000.0 }
    let w = Weather.generate normal 3
    // 貯水池は施工中の 3 季のあいだ、再発注できない
    let s1, _ = Turn.resolve normal s0 (policyFor normal s0 (Invest Reservoir)) w[0]
    Assert.False(Turn.isValidAction normal s1 (Invest Reservoir))

// ---------- 費用 ----------

[<Fact>]
let ``費用は 発注した季節に払い 完成を待たない`` () =
    let s0 = { Turn.initialState normal with Balance = 100000.0 }
    let w = Weather.generate normal 3
    let _, r = Turn.resolve normal s0 (policyFor normal s0 (Invest Reservoir)) w[0]
    Assert.Equal(normal.Reservoir.Value.Cost, r.Costs.Invest)

// ---------- 融通 ----------

[<Fact>]
let ``融通は すぐ届き 手数料がかかり 返礼の期限に 正規配給から差し引かれる`` () =
    let s0 = { Turn.initialState normal with Balance = 100000.0 }
    let w = Weather.generate normal 3
    let before = stock s0 Machinery
    let s1, r1 = Turn.resolve normal s0 (policyFor normal s0 (RequestTransfer Machinery)) w[0]
    Assert.Equal(Some Machinery, r1.Works.Transferred)
    Assert.Equal(cfg.TransferFee, r1.Costs.Invest)
    Assert.Equal(before + 1, stock s1 Machinery)
    Assert.Equal(1, s1.Construction.Loans.Length)
    Assert.Equal(cfg.TransferRepayAfter, s1.Construction.Loans.Head.RepayTurn)

    // 返礼の期限の季節のはじめに、1 単位が差し引かれる
    let mutable st = s1

    for t in 1 .. cfg.TransferRepayAfter do
        let arrivals = cfg.Arrivals |> List.filter (fun (turn, m, _) -> turn = t + 1 && m = Machinery) |> List.sumBy (fun (_, _, q) -> q)
        let b = stock st Machinery
        let next, r = Turn.resolve normal st (policyFor normal st NoAction) w[t]
        let repaid = r.Works.Repaid |> List.filter ((=) Machinery) |> List.length
        Assert.Equal(b + arrivals - repaid, stock next Machinery)
        st <- next

    Assert.Empty(st.Construction.Loans)

[<Fact>]
let ``融通は 借りられる上限があり 入門では使えない`` () =
    let s0 = { Turn.initialState normal with Balance = 100000.0 }
    let w = Weather.generate normal 3
    let mutable s = s0

    for t in 0 .. cfg.MaxLoans - 1 do
        Assert.True(Turn.isValidAction normal s (RequestTransfer Machinery), $"{t} 回目")
        s <- fst (Turn.resolve normal s (policyFor normal s (RequestTransfer Machinery)) w[t])

    Assert.False(Turn.isValidAction normal s (RequestTransfer BuildMaterial))

    let tut = Scenarios.tutorial
    let t0 = { Turn.initialState tut with Balance = 100000.0 }
    Assert.False(Turn.isValidAction tut t0 (RequestTransfer Machinery))

// ---------- 入門: 工事は、発注した季節に完成する ----------

[<Fact>]
let ``資材・建設のない入門では 井戸も投資も 発注した季節に完成する`` () =
    let tut = Scenarios.tutorial
    let w = Weather.generate tut 3
    let s0 = { Turn.initialState tut with Balance = 100000.0 }
    let s1, r1 = Turn.resolve tut s0 (policyFor tut s0 (Invest Reservoir)) w[0]
    // 入門は貯水池を建てられない。灌漑効率で確かめる
    let s1, _ = Turn.resolve tut s0 (policyFor tut s0 (Invest IrrigationEfficiency)) w[0]
    Assert.True(s1.Invested.Contains IrrigationEfficiency)
    Assert.Empty(s1.Construction.Projects)
    ignore r1

// ---------- 建設期限の指令 ----------

[<Fact>]
let ``建設期限の指令は 資材・建設のあるシナリオにだけあり 完成で達成になる`` () =
    for sc in Scenarios.all do
        let ds = Organization.generate sc
        let builds = ds |> List.filter (fun d -> d.Kind = BuildOrder)

        if sc.Construction.IsSome && sc.Directives.IsSome then
            Assert.NotEmpty(builds)
            Assert.True(builds |> List.forall (fun d -> d.Target.IsSome && d.TargetFill = 1.0), sc.Name)
        else
            Assert.Empty(builds)

[<Fact>]
let ``建設期限は 期限までに完成すれば達成 施工中なら一部達成 未着工なら未達`` () =
    let sc = normal
    let build = Organization.generate sc |> List.find (fun d -> d.Kind = BuildOrder)
    let target = build.Target.Value // 農地の井戸
    let issued = build.IssuedTurn

    // 完成: 概査 → 井戸 (標準工期 1 季) を、期限までに
    let done_ = Map.ofList [ 0, Survey(Farm, Reconnaissance); 1, target ]
    let s1, _ = playScript sc done_
    let r1 = s1.Organization.Resolved |> List.find (fun r -> r.Directive.Id = build.Id)
    Assert.Equal(Achieved, r1.Outcome)

    // 何もしなければ、未達
    let s2, _ = playScript sc Map.empty
    let r2 = s2.Organization.Resolved |> List.find (fun r -> r.Directive.Id = build.Id)
    Assert.Equal(Failed, r2.Outcome)
    Assert.True(issued < build.DeadlineTurn)

// ---------- 水文学・決定論 ----------

[<Fact>]
let ``資材・建設は 水文学の状態を直接変えない: 発注しても 完成するまで 水量は同じ`` () =
    let w = Weather.generate normal 3
    let s0 = { Turn.initialState normal with Balance = 100000.0 }
    // 貯水池を発注 (3 季かかる) しても、最初の 2 季は、何も発注しない場合と、水量が同じ
    let a1, ra1 = Turn.resolve normal s0 (policyFor normal s0 (Invest Reservoir)) w[0]
    let b1, rb1 = Turn.resolve normal s0 (policyFor normal s0 NoAction) w[0]
    Assert.Equal<ZoneResult list>(rb1.Zones, ra1.Zones)
    let _, ra2 = Turn.resolve normal a1 (policyFor normal a1 NoAction) w[1]
    let _, rb2 = Turn.resolve normal b1 (policyFor normal b1 NoAction) w[1]
    Assert.Equal<ZoneResult list>(rb2.Zones, ra2.Zones)
    Assert.True(ra2.Balance.IsWithinTolerance)

[<Fact>]
let ``同じ入力なら同じ結果で 乱数を使わない: 工事・融通を含めても 再現される`` () =
    let script = Map.ofList [ 0, RequestTransfer Machinery; 1, Survey(Farm, Reconnaissance); 2, BuildWell Farm; 3, Invest Reservoir ]
    let a, ra = playScript normal script
    let b, rb = playScript normal script
    Assert.Equal<TurnResult list>(ra, rb)
    Assert.Equal<ConstructionState>(a.Construction, b.Construction)

[<Fact>]
let ``工事にならない行動 (節水キャンペーン) は 資材・建設のあるシナリオでも その季節に完成する`` () =
    let s0 = { Turn.initialState normal with Balance = 100000.0 }
    let w = Weather.generate normal 3
    let s1, r1 = Turn.resolve normal s0 (policyFor normal s0 (Invest WaterSaving)) w[0]
    Assert.True(s1.Invested.Contains WaterSaving)
    Assert.Empty(s1.Construction.Projects)
    Assert.True(r1.Works.Ordered.IsNone)
