/// 計画経済版 v2.0-b: 年次報告・整合性リスク・監査のテスト。
/// 監査は、決まった規則だけで動き (整合性リスクが閾値に達したら、次の季節に入る)、乱数は使わない。
module WaterGame.Tests.V16Tests

open Xunit
open WaterGame.Core

let normal = Scenarios.normalYear

let policyFor (sc: Scenario) (state: GameState) (agFrac: float) (stance: ReportStance) =
    let season = Season.ofTurn state.Turn

    { AgTargetWU = Turn.effectiveAgDemand sc state season * agFrac
      UrbanTargetWU = Turn.effectiveUrbanDemand sc state state.Turn
      EnvTargetWU = sc.MinEnvFlowWU[season]
      Priority = state.Priority
      PumpLimitWU = sc.Zones |> List.map (fun z -> z.Id, 0.0) |> Map.ofList
      ReservoirOpWU = 0.0
      Report = stance
      Action = NoAction }

let play (sc: Scenario) (seed: int) (agFrac: float) (stance: ReportStance) =
    let w = Weather.generate sc seed
    let mutable s = Turn.initialState sc

    for t in 0 .. sc.Turns - 1 do
        s <- fst (Turn.resolve sc s (policyFor sc s agFrac stance) w[t])

    s

let item id actual reported gain reward =
    { DirectiveId = id
      Actual = actual
      Reported = reported
      TrustGain = gain
      RewardGain = reward }

// ---------- 報告の方針 ----------

[<Fact>]
let ``報告の方針ごとの 報告する判定は 実際を そのまま 一段よく 未達を外す`` () =
    let cfg = normal.Directives.Value

    let result outcome =
        { Directive = { Id = 0; Kind = CottonQuota; IssuedTurn = 0; DeadlineTurn = 2; TargetFill = 0.9; Target = None }
          Measured = 0.5
          Ratio = 0.5
          Outcome = outcome
          TrustChange = 0.0
          Reward = 0.0 }

    let items stance = Organization.reportItems normal cfg stance [ result Failed; result Partial; result Achieved ]
    let reported stance = items stance |> List.map _.Reported

    Assert.Equal<DirectiveOutcome option list>([ Some Failed; Some Partial; Some Achieved ], reported Accurate)
    Assert.Equal<DirectiveOutcome option list>([ Some Partial; Some Achieved; Some Achieved ], reported Optimistic)
    Assert.Equal<DirectiveOutcome option list>([ None; Some Partial; Some Achieved ], reported Conceal)

    // 正確な報告は、信頼も補助金も、実際のまま
    Assert.True(items Accurate |> List.forall (fun i -> i.TrustGain = 0.0 && i.RewardGain = 0.0 && not (Organization.isMisreport i)))

    // 前向きな報告: 未達→一部達成で、未達の分の信頼 (−10) を取り戻し、一部達成→達成で、+8 と補助金
    let o = items Optimistic
    Assert.Equal(cfg.TrustFailed, o[0].TrustGain)
    Assert.Equal(cfg.TrustAchieved, o[1].TrustGain)
    Assert.True(o[1].RewardGain > 0.0)
    Assert.False(Organization.isMisreport o[2])

    // 隠した未達は、信頼の低下がなかったことになる
    let c = items Conceal
    Assert.Equal(cfg.TrustFailed, c[0].TrustGain)
    Assert.Equal(0.0, c[0].RewardGain)
    Assert.True(Organization.isMisreport c[0])

// ---------- 報告のタイミングと記録 ----------

[<Fact>]
let ``年次報告は 冬の季節だけで 報告済みの指令は 二度報告しない`` () =
    for sc in Scenarios.all |> List.filter (fun s -> s.Directives.IsSome) do
        let s = play sc 3 1.0 Accurate

        for r in s.History do
            match r.Plan.Report with
            | Some rep -> Assert.Equal(Season.Winter, Season.ofTurn r.Turn)
            | None -> ()

        let ids = s.Organization.Reports |> List.collect (fun r -> r.Items |> List.map _.DirectiveId)
        Assert.Equal(ids.Length, (List.distinct ids).Length)
        // 12/20 ターンは冬で終わるので、すべての判定済みの指令が報告済みになる
        Assert.Equal<Set<int>>(s.Organization.Resolved |> List.map (fun r -> r.Directive.Id) |> Set.ofList, s.Organization.Reported)

[<Fact>]
let ``冬以外の季節では 報告の方針は 何も変えない`` () =
    let sc = normal
    let w = Weather.generate sc 4
    let s0 = Turn.initialState sc
    let a = Turn.resolve sc s0 (policyFor sc s0 1.0 Accurate) w[0]
    let b = Turn.resolve sc s0 (policyFor sc s0 1.0 Conceal) w[0]
    // 記録された政策 (報告の方針) だけが違い、結果は同じ
    Assert.Equal({ snd a with Policy = (snd b).Policy }, snd b)

[<Fact>]
let ``指令のない入門では 報告の方針は 何も変えず 整合性リスクも動かない`` () =
    let tut = Scenarios.tutorial
    let a = play tut 2 0.3 Accurate
    let b = play tut 2 0.3 Conceal
    Assert.Equal(a.Balance, b.Balance)
    Assert.Equal(0.0, b.Organization.Risk)
    Assert.Empty(b.Organization.Reports)

// ---------- 正確な報告 ----------

[<Fact>]
let ``正確な報告だけなら 整合性リスクは 上がらず 監査は入らない`` () =
    for sc in Scenarios.all |> List.filter (fun s -> s.Directives.IsSome) do
        for agFrac in [ 1.0; 0.3 ] do
            let s = play sc 5 agFrac Accurate
            Assert.True(s.History |> List.forall (fun r -> r.Plan.RiskAfter = 0.0 && not r.Plan.AuditNotice), sc.Name)
            Assert.Empty(s.Organization.Audits)
            Assert.Empty(s.Organization.Misreports)

// ---------- 虚偽の報告・整合性リスク ----------

[<Fact>]
let ``虚偽の報告は 整合性リスクを上げ 残っているあいだ 毎季節 上がり続ける`` () =
    // 未達が出るように、農業の取水を絞って、前向きに報告する
    let sc = normal
    let s = play sc 5 0.3 Optimistic
    let firstReport = s.History |> List.find (fun r -> r.Plan.Report.IsSome)
    let rep = firstReport.Plan.Report.Value
    let mis = rep.Items |> List.filter Organization.isMisreport
    Assert.NotEmpty(mis)
    Assert.Equal(float mis.Length * Organization.riskOptimistic, rep.RiskDelta)
    // 報告のターンのリスク = 報告の増加 + 残っている虚偽 1件あたり +3
    Assert.Equal(rep.RiskDelta + Organization.riskDrift * float mis.Length, firstReport.Plan.RiskAfter)
    // 次の季節 (監査がなければ) さらに上がる
    let next = s.History |> List.find (fun r -> r.Turn = firstReport.Turn + 1)

    if next.Plan.Audit.IsNone then
        Assert.Equal(firstReport.Plan.RiskAfter + Organization.riskDrift * float mis.Length, next.Plan.RiskAfter)

[<Fact>]
let ``隠す報告は 前向きな報告より 整合性リスクが大きい`` () =
    let sc = normal
    let risk stance =
        let s = play sc 5 0.3 stance
        (s.History |> List.find (fun r -> r.Plan.Report.IsSome)).Plan.Report.Value.RiskDelta

    Assert.True(risk Conceal > risk Optimistic)
    Assert.True(risk Optimistic > risk Accurate)

// ---------- 監査 (決まった規則。乱数なし) ----------

let orgWith (s: GameState) risk misreports due =
    { s.Organization with Risk = risk; Misreports = misreports; AuditDue = due }

[<Fact>]
let ``整合性リスクが閾値に達すると 予兆が出て 次の季節に監査が入り 虚偽の報告が露見する`` () =
    let sc = normal
    let s0 = Turn.initialState sc
    let mis = [ item 0 Failed (Some Partial) 10.0 0.0 ]
    let turn = 1 // 夏: 期限の来る指令も、年次報告もない季節

    // リスク 58 + 虚偽 1件の +3 = 61 で、閾値 (60) 以上 → 予兆
    let org0 = orgWith s0 58.0 mis false
    let plan1 = Organization.step sc org0 turn [] Accurate
    Assert.Equal(61.0, plan1.RiskAfter)
    Assert.True(plan1.AuditNotice)
    Assert.True(plan1.Audit.IsNone)

    // 予兆の次の季節: 監査が入る。露見で、報告で得た信頼 (+10) が取り消され、さらに −15。予算も削減される
    let org1 = Organization.advance org0 plan1
    Assert.True(org1.AuditDue)
    let plan2 = Organization.step sc org1 (turn + 1) [] Accurate
    let audit = plan2.Audit.Value
    Assert.Equal<ReportItem list>(mis, audit.Exposed)
    Assert.Equal(-10.0 - Organization.auditTrustPenalty, audit.TrustChange)
    Assert.Equal(sc.Finance.AnnualGrant * Organization.auditFineFraction, audit.Fine)
    Assert.Equal(plan2.Fine, audit.Fine)
    Assert.Equal(Organization.riskAfterExposure, plan2.RiskAfter)
    Assert.False(plan2.AuditNotice)

    // 露見した虚偽の報告は、記録から消える
    let org2 = Organization.advance org1 plan2
    Assert.Empty(org2.Misreports)
    Assert.False(org2.AuditDue)

[<Fact>]
let ``予兆の出ていない季節に 監査は入らない`` () =
    let sc = normal
    let s0 = Turn.initialState sc
    let mis = [ item 0 Failed None 10.0 0.0 ]
    // リスクが低ければ、虚偽の報告が残っていても、監査は入らず 予兆も出ない
    let plan = Organization.step sc (orgWith s0 10.0 mis false) 1 [] Accurate
    Assert.True(plan.Audit.IsNone && not plan.AuditNotice)
    Assert.Equal(13.0, plan.RiskAfter)

[<Fact>]
let ``虚偽の報告が残っていない監査は 問題なしで 信頼が少し上がる`` () =
    let sc = normal
    let s0 = Turn.initialState sc
    let plan = Organization.step sc (orgWith s0 70.0 [] true) 1 [] Accurate
    let audit = plan.Audit.Value
    Assert.Empty(audit.Exposed)
    Assert.Equal(Organization.cleanAuditTrust, audit.TrustChange)
    Assert.Equal(0.0, audit.Fine)
    Assert.True(plan.RiskAfter <= Organization.riskAfterCleanAudit)

[<Fact>]
let ``同じ入力なら 同じ結果で 乱数を使わない: 虚偽の報告をしても 結果は再現される`` () =
    for stance in [ Optimistic; Conceal ] do
        let a = play normal 8 0.3 stance
        let b = play normal 8 0.3 stance
        Assert.Equal<TurnResult list>(a.History, b.History)
        Assert.Equal<OrganizationState>(a.Organization, b.Organization)

// ---------- 最終の監査 ----------

[<Fact>]
let ``終了時の監査は 虚偽の報告が残り かつ 整合性リスクが閾値以上のときだけ入る`` () =
    let sc = normal
    let s = play sc 5 0.3 Optimistic
    let mis = [ item 0 Failed (Some Partial) 10.0 0.0 ]

    // 虚偽が残っていて、リスクが閾値以上: 露見して、最終の信頼が下がる
    let high = { s with Organization = { s.Organization with Misreports = mis; Risk = 65.0 } }
    let planHigh = (Evaluation.summarize sc high).Plan.Value
    let final = planHigh.FinalAudit.Value
    Assert.Equal<ReportItem list>(mis, final.Exposed)
    Assert.Equal(-10.0 - Organization.auditTrustPenalty, final.TrustChange)
    Assert.True(planHigh.Trust < high.Organization.Trust)
    Assert.Empty(planHigh.Undetected)

    // 虚偽が残っていても、リスクが閾値に届かなければ、露見しないまま終わる
    let low = { s with Organization = { s.Organization with Misreports = mis; Risk = 40.0 } }
    let planLow = (Evaluation.summarize sc low).Plan.Value
    Assert.True(planLow.FinalAudit.IsNone)
    Assert.Equal<ReportItem list>(mis, planLow.Undetected)
    Assert.Equal(low.Organization.Trust, planLow.Trust)

    // 正確に報告していれば、最終の監査も、露見しなかった虚偽もない
    let honest = play sc 5 0.3 Accurate
    let planHonest = (Evaluation.summarize sc honest).Plan.Value
    Assert.True(planHonest.FinalAudit.IsNone)
    Assert.Empty(planHonest.Undetected)

[<Fact>]
let ``露見したとき 虚偽の報告で得た補助金も 予算削減に加えて返す`` () =
    let sc = normal
    let s0 = Turn.initialState sc
    let mis = [ item 0 Partial (Some Achieved) 8.0 280.0 ]
    let plan = Organization.step sc (orgWith s0 70.0 mis true) 1 [] Accurate
    Assert.Equal(sc.Finance.AnnualGrant * Organization.auditFineFraction + 280.0, plan.Audit.Value.Fine)

// ---------- 水文学への影響がないこと ----------

[<Fact>]
let ``報告の方針で 水量の結果は変わらない: 虚偽の報告は 実際の水の動きを書き換えない`` () =
    for sc in Scenarios.all |> List.filter (fun s -> s.Directives.IsSome) do
        let a = play sc 6 0.5 Accurate
        let b = play sc 6 0.5 Conceal

        Assert.Equal<Map<ZoneId, float>>(a.GwWU, b.GwWU)
        Assert.Equal<Map<ZoneId, float>>(a.SoilWU, b.SoilWU)

        for (x, y) in List.zip a.History b.History do
            Assert.Equal<ZoneResult list>(x.Zones, y.Zones)
            Assert.Equal(x.Allocation, y.Allocation)
            Assert.Equal(x.AgFillRate, y.AgFillRate)
            Assert.True(y.Balance.IsWithinTolerance)

        // 実際の判定は、報告の方針で変わらない
        Assert.Equal<DirectiveOutcome list>(a.Organization.Resolved |> List.map _.Outcome, b.Organization.Resolved |> List.map _.Outcome)

// ---------- 範囲 ----------

[<Fact>]
let ``信頼と整合性リスクは 0〜100に収まり 期末残高は 報酬と予算削減を含めて一致する`` () =
    for sc in Scenarios.all |> List.filter (fun s -> s.Directives.IsSome) do
        for stance in [ Accurate; Optimistic; Conceal ] do
            let s = play sc 7 0.4 stance

            for r in s.History do
                Assert.InRange(r.Plan.TrustAfter, 0.0, 100.0)
                Assert.InRange(r.Plan.RiskAfter, 0.0, 100.0)
                Assert.True(abs (r.BalanceEnd - (r.BalanceStart + r.Grant + r.Revenue + r.Plan.Reward - r.Plan.Fine - r.Costs.Total)) < 1e-6)
