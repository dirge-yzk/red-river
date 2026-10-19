namespace WaterGame.Core

/// 組織・政治の層 (計画経済版 v2.0-a): 中央からの計画指令 (ノルマ)、達成度、上司の信頼。
/// 水文学の状態は読むだけで、水量・地下水・河川流量には一切影響しない。
/// 影響が及ぶのは、上司の信頼を通じた年度の交付金と、達成報酬の補助金 (財政) だけである。
/// 指令の列は、シナリオから固定で決まり、乱数は使わない。
module Organization =

    let private clamp (lo: float) (hi: float) (x: float) = max lo (min hi x)

    let private roundTo (step: float) (x: float) = System.Math.Round(System.Math.Round(x / step) * step, 4)

    let kindLabel =
        function
        | CottonQuota -> "生産ノルマ (綿花の増産)"
        | IndustrialSupply -> "工業用水の供給"
        | BuildOrder -> "建設期限"

    /// 指令の対象 (農業用水か、生活・産業用水か) の短い名前
    let targetLabel =
        function
        | CottonQuota -> "農業用水"
        | IndustrialSupply -> "生活・産業用水"
        | BuildOrder -> "設備の完成"

    let outcomeLabel =
        function
        | Achieved -> "達成"
        | Partial -> "一部達成"
        | Failed -> "未達"

    // 指令の並びは、決まったパターンの繰り返しで、乱数は使わない (運の要素は、気象だけにする)。
    // 期間 (季節数)、種類、目標の位置 (範囲の下端 0 〜 上端 1) の順に、繰り返す。
    let private spans = [| 3; 2 |]
    let private kinds = [| CottonQuota; IndustrialSupply; CottonQuota; CottonQuota; IndustrialSupply |]
    /// 建設期限の指令 (資材・建設のあるシナリオで、4 件目と 7 件目)。完成させる設備の順
    let private buildTargets = [| BuildWell Farm; Invest Reservoir; Invest IrrigationEfficiency |]
    let private positions = [| 0.0; 0.5; 1.0; 0.25; 0.75 |]

    /// 全期間の指令を、シナリオから確定する (乱数なし)。指令のないシナリオでは空。
    /// 発令は、最初のターンから、2〜3ターンおき。期限は、次の発令の直前 (最後の指令は終了まで)。
    /// 最初の指令の目標は、準備の時間がないので、範囲のいちばん下。
    let generate (scenario: Scenario) : Directive list =
        match scenario.Directives with
        | None -> []
        | Some cfg ->
            let cottonPossible (issued: int) (deadline: int) =
                [ issued..deadline ] |> List.sumBy (fun t -> scenario.AgDemandWU[Season.ofTurn t]) > 0.0

            let hasConstruction = scenario.Construction.IsSome

            let rec go (turn: int) (id: int) (acc: Directive list) =
                // 最後のターンだけの指令は出さない
                if turn >= scenario.Turns - 1 then
                    List.rev acc
                else
                    let deadline = min (scenario.Turns - 1) (turn + spans[id % spans.Length] - 1)
                    // 資材・建設のあるシナリオでは、4 件目 (id 3) と 7 件目 (id 6) が、建設期限
                    let buildIndex = if id = 3 then Some 0 elif id = 6 then Some 1 else None

                    let kind =
                        if hasConstruction && buildIndex.IsSome then BuildOrder
                        elif kinds[id % kinds.Length] = CottonQuota && not (cottonPossible turn deadline) then IndustrialSupply
                        else kinds[id % kinds.Length]

                    let target = roundTo 0.05 (cfg.TargetMin + (cfg.TargetMax - cfg.TargetMin) * positions[id % positions.Length])

                    let d =
                        { Id = id
                          Kind = kind
                          IssuedTurn = turn
                          DeadlineTurn = deadline
                          TargetFill = (if kind = BuildOrder then 1.0 else clamp 0.0 1.0 target)
                          Target = (if kind = BuildOrder then Some buildTargets[buildIndex.Value] else None) }

                    go (deadline + 1) (id + 1) (d :: acc)

            go 0 0 []

    let initialState (scenario: Scenario) : OrganizationState =
        { Directives = generate scenario
          Trust = scenario.Directives |> Option.map _.InitialTrust |> Option.defaultValue 50.0
          Resolved = []
          Risk = 0.0
          Reported = Set.empty
          Misreports = []
          AuditDue = false
          Reports = []
          Audits = [] }

    /// 建設期限の指令の、ここまでの状況: 完成 = 1、施工中 = 0.9、それ以外 (未発注・資材待ち) = 0
    let buildState (d: Directive) (history: TurnResult list) : float =
        match d.Target with
        | Some target ->
            if history |> List.exists (fun r -> r.Works.Completed |> List.exists (fun p -> p.Target = target)) then
                1.0
            elif history |> List.exists (fun r -> r.Works.Started |> List.exists (fun p -> p.Target = target)) then
                0.9
            else
                0.0
        | None -> 0.0

    /// 建設期限の指令の状況の名前
    let buildStateLabel (v: float) =
        if v >= 1.0 then "完成"
        elif v > 0.0 then "施工中"
        else "未着工"

    /// 指令の期間のうち、history に含まれるターンの (供給量の合計, 需要量の合計)
    let private window (d: Directive) (history: TurnResult list) =
        let inWindow = history |> List.filter (fun r -> r.Turn >= d.IssuedTurn && r.Turn <= d.DeadlineTurn)

        match d.Kind with
        | BuildOrder -> 0.0, 0.0
        | CottonQuota ->
            inWindow |> List.sumBy (fun r -> r.Allocation.AgSupplyWU + r.AgGwSupplyWU),
            inWindow |> List.sumBy _.AgDemandWU
        | IndustrialSupply ->
            inWindow |> List.sumBy (fun r -> r.Allocation.UrbanSupplyWU + r.UrbanGwSupplyWU),
            inWindow |> List.sumBy _.UrbanDemandWU

    /// 指令の期間の、ここまでの充足率 (需要がまだなければ None)
    let progress (d: Directive) (history: TurnResult list) : float option =
        if d.Kind = BuildOrder then
            Some(buildState d history)
        else
            let supply, demand = window d history
            if demand <= 0.0 then None else Some(supply / demand)

    let private judge (cfg: DirectiveConfig) (scenario: Scenario) (history: TurnResult list) (d: Directive) : DirectiveResult =
        let measured = progress d history |> Option.defaultValue 1.0
        let ratio = if d.TargetFill <= 0.0 then 1.0 else measured / d.TargetFill

        let outcome =
            if ratio >= 1.0 then Achieved
            elif ratio >= 0.9 then Partial
            else Failed

        { Directive = d
          Measured = measured
          Ratio = ratio
          Outcome = outcome
          TrustChange =
            (match outcome with
             | Achieved -> cfg.TrustAchieved
             | Partial -> 0.0
             | Failed -> -cfg.TrustFailed)
          Reward = (if outcome = Achieved then scenario.Finance.AnnualGrant * cfg.RewardFraction else 0.0) }

    // ---- 年次報告・整合性リスク・監査 (v2.0-b)。すべて決まった規則で、乱数は使わない ----

    /// 虚偽の報告 1 件あたりの整合性リスクの増加: 結果を実際より良く見せる / 未達を報告から外す
    let riskOptimistic = 15.0
    let riskConceal = 30.0
    /// 正確な報告で、整合性リスクが下がる量
    let riskDecayAccurate = 10.0
    /// 毎季節の、整合性リスクの変化: 虚偽の報告が残っていると、1件につき +3 (記録・観測・証言との矛盾が積み上がる)。なければ −3
    let riskDrift = 3.0
    /// 整合性リスクがこの値以上になると、監査の予兆が出て、次の季節に監査が入る
    let auditThreshold = 60.0
    /// 監査のあとの整合性リスク (露見したとき / 問題なしのときの上限)
    let riskAfterExposure = 30.0
    let riskAfterCleanAudit = 20.0
    /// 露見したとき、報告で得た信頼を取り消したうえで、さらに下がる信頼
    let auditTrustPenalty = 15.0
    /// 問題なしの監査で、少し上がる信頼
    let cleanAuditTrust = 3.0
    /// 露見したときの予算削減 = 年度の交付金 × この値
    let auditFineFraction = 0.5

    let private trustValue (cfg: DirectiveConfig) =
        function
        | Achieved -> cfg.TrustAchieved
        | Partial -> 0.0
        | Failed -> -cfg.TrustFailed

    let private rewardValue (scenario: Scenario) (cfg: DirectiveConfig) =
        function
        | Achieved -> scenario.Finance.AnnualGrant * cfg.RewardFraction
        | _ -> 0.0

    /// 実際の判定と報告した判定が食い違う (虚偽の報告) か
    let isMisreport (i: ReportItem) = i.Reported <> Some i.Actual

    let private riskOf (i: ReportItem) =
        match i.Reported with
        | None -> riskConceal
        | Some r when r <> i.Actual -> riskOptimistic
        | _ -> 0.0

    let stanceLabel =
        function
        | Accurate -> "正確に報告"
        | Optimistic -> "前向きに報告"
        | Conceal -> "重大な問題を隠す"

    /// 方針ごとの、報告する判定。結果の実値と、報告値を並べて返す (UI の予告にも、実際の報告にも使う)
    let reportItems (scenario: Scenario) (cfg: DirectiveConfig) (stance: ReportStance) (results: DirectiveResult list) : ReportItem list =
        results
        |> List.map (fun r ->
            let reported =
                match stance with
                | Accurate -> Some r.Outcome
                | Optimistic ->
                    Some(
                        match r.Outcome with
                        | Failed -> Partial
                        | _ -> Achieved
                    )
                | Conceal -> if r.Outcome = Failed then None else Some r.Outcome

            { DirectiveId = r.Directive.Id
              Actual = r.Outcome
              Reported = reported
              TrustGain = (reported |> Option.map (trustValue cfg) |> Option.defaultValue 0.0) - trustValue cfg r.Outcome
              RewardGain = (reported |> Option.map (rewardValue scenario cfg) |> Option.defaultValue 0.0) - rewardValue scenario cfg r.Outcome })

    /// 判定済みで、まだ報告していない指令
    let pendingReport (org: OrganizationState) : DirectiveResult list =
        org.Resolved |> List.filter (fun r -> not (org.Reported.Contains r.Directive.Id))

    /// 指令のない場合・結果の組み立て用の、何も起こらない動き
    let noPlan (org: OrganizationState) : DirectiveTurn =
        { Issued = []
          Resolved = []
          TrustBefore = org.Trust
          TrustAfter = org.Trust
          Reward = 0.0
          Report = None
          Audit = None
          Fine = 0.0
          RiskBefore = org.Risk
          RiskAfter = org.Risk
          AuditNotice = false }

    /// 今季の結果 (history の末尾) を受けて、今季が期限の指令を判定し、冬の季節なら年次報告を受け付け、
    /// 整合性リスクと監査 (予兆の翌季に入る) を進めて、信頼・報酬・予算削減を決める。
    /// 水文学の状態は変更しない。
    let step (scenario: Scenario) (org: OrganizationState) (turn: int) (history: TurnResult list) (stance: ReportStance) : DirectiveTurn =
        match scenario.Directives with
        | None -> noPlan org
        | Some cfg ->
            let resolved =
                org.Directives
                |> List.filter (fun d -> d.DeadlineTurn = turn)
                |> List.map (judge cfg scenario history)

            // 年次報告: 冬の季節に、まだ報告していない判定済みの指令 (今季が期限のものを含む) を報告する
            let pending =
                (org.Resolved @ resolved)
                |> List.filter (fun r -> not (org.Reported.Contains r.Directive.Id))

            let report =
                if Season.ofTurn turn = Winter && not pending.IsEmpty then
                    let items = reportItems scenario cfg stance pending
                    let increase = items |> List.sumBy riskOf

                    Some
                        { Turn = turn
                          Stance = stance
                          Items = items
                          RiskDelta = (if stance = Accurate then -riskDecayAccurate else increase) }
                else
                    None

            let outstanding =
                org.Misreports
                @ (report |> Option.map (fun r -> r.Items |> List.filter isMisreport) |> Option.defaultValue [])

            let riskAfterReport = clamp 0.0 100.0 (org.Risk + (report |> Option.map _.RiskDelta |> Option.defaultValue 0.0))

            // 予兆の出た次の季節に、監査が入る。虚偽の報告が残っていれば露見し、なければ問題なし
            let audit, riskAfter =
                if org.AuditDue then
                    if outstanding.IsEmpty then
                        Some { Turn = turn; Exposed = []; TrustChange = cleanAuditTrust; Fine = 0.0 }, min riskAfterReport riskAfterCleanAudit
                    else
                        Some
                            { Turn = turn
                              Exposed = outstanding
                              TrustChange = -(outstanding |> List.sumBy _.TrustGain) - auditTrustPenalty
                              // 予算削減に加えて、虚偽の報告で得た補助金を返す
                              Fine = scenario.Finance.AnnualGrant * auditFineFraction + (outstanding |> List.sumBy _.RewardGain) },
                        riskAfterExposure
                else
                    let drift = if outstanding.IsEmpty then -riskDrift else riskDrift * float outstanding.Length
                    None, clamp 0.0 100.0 (riskAfterReport + drift)

            let reportTrust = report |> Option.map (fun r -> r.Items |> List.sumBy _.TrustGain) |> Option.defaultValue 0.0
            let reportReward = report |> Option.map (fun r -> r.Items |> List.sumBy _.RewardGain) |> Option.defaultValue 0.0
            let auditTrust = audit |> Option.map _.TrustChange |> Option.defaultValue 0.0

            { Issued = org.Directives |> List.filter (fun d -> d.IssuedTurn = turn)
              Resolved = resolved
              TrustBefore = org.Trust
              TrustAfter = clamp 0.0 100.0 (org.Trust + (resolved |> List.sumBy _.TrustChange) + reportTrust + auditTrust)
              Reward = (resolved |> List.sumBy _.Reward) + reportReward
              Report = report
              Audit = audit
              Fine = audit |> Option.map _.Fine |> Option.defaultValue 0.0
              RiskBefore = org.Risk
              RiskAfter = riskAfter
              AuditNotice = audit.IsNone && riskAfter >= auditThreshold }

    /// 次の状態
    let advance (org: OrganizationState) (plan: DirectiveTurn) : OrganizationState =
        let newMisreports =
            plan.Report |> Option.map (fun r -> r.Items |> List.filter isMisreport) |> Option.defaultValue []

        { org with
            Trust = plan.TrustAfter
            Resolved = org.Resolved @ plan.Resolved
            Risk = plan.RiskAfter
            Reported =
                match plan.Report with
                | Some r -> Set.union org.Reported (r.Items |> List.map _.DirectiveId |> Set.ofList)
                | None -> org.Reported
            // 監査が入れば、残っていた虚偽の報告はすべて露見して、消える
            Misreports = (if plan.Audit.IsSome then [] else org.Misreports @ newMisreports)
            AuditDue = plan.AuditNotice
            Reports = org.Reports @ Option.toList plan.Report
            Audits = org.Audits @ Option.toList plan.Audit }

    /// 今季に適用中の指令 (発令から期限まで)
    let active (org: OrganizationState) (turn: int) : Directive list =
        org.Directives |> List.filter (fun d -> d.IssuedTurn <= turn && turn <= d.DeadlineTurn)

    /// 来季はじめに示される指令の予告
    let upcoming (org: OrganizationState) (turn: int) : Directive list =
        org.Directives |> List.filter (fun d -> d.IssuedTurn = turn + 1)

    /// 上司の信頼による、年度の交付金の倍率 (信頼 50 で 1.0、0 で 0.8、100 で 1.2)。指令のないシナリオは 1.0
    let grantMultiplier (scenario: Scenario) (org: OrganizationState) =
        match scenario.Directives with
        | Some _ -> 0.8 + 0.4 * org.Trust / 100.0
        | None -> 1.0

    /// 計画達成度 (0〜1): 判定済みの指令の達成率 (充足率 / 目標、上限 1) の平均。判定済みがなければ None
    let achievement (org: OrganizationState) : float option =
        if org.Resolved.IsEmpty then
            None
        else
            Some(org.Resolved |> List.averageBy (fun r -> min 1.0 r.Ratio))

    /// 終了時の監査: 虚偽の報告が残っていて、整合性リスクが閾値以上のときだけ入る。
    /// リスクが閾値に届かなければ、虚偽は露見しないまま終わる (ごまかしが、常に損になるわけではない。乱数は使わない)
    let finalAudit (org: OrganizationState) : AuditRecord option =
        if org.Misreports.IsEmpty || org.Risk < auditThreshold then
            None
        else
            Some
                { Turn = -1
                  Exposed = org.Misreports
                  TrustChange = -(org.Misreports |> List.sumBy _.TrustGain) - auditTrustPenalty
                  Fine = 0.0 }

    let summary (org: OrganizationState) : PlanSummary =
        let final = finalAudit org

        { Achievement = achievement org
          Results = org.Resolved
          Trust = clamp 0.0 100.0 (org.Trust + (final |> Option.map _.TrustChange |> Option.defaultValue 0.0))
          Risk = org.Risk
          Reports = org.Reports
          Audits = org.Audits
          FinalAudit = final
          Undetected = (if final.IsNone then org.Misreports else []) }
