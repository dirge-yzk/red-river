namespace WaterGame.Core

type Rank =
    | S
    | A
    | B
    | C

type Indicator =
    { Name: string
      /// 0〜1
      Score: float
      Weight: float
      Detail: string }

/// 最終評価 (仕様書 10.2): 農業・生活産業・環境・地下水・財政の5指標。
type Summary =
    { /// 農業用水の累積充足率 (河川水 + 地下水の供給量合計 / 需要量合計)
      AgFillRate: float
      /// 生活・産業用水の累積充足率
      UrbanFillRate: float
      /// 環境流水の達成度 (ターン平均)
      EnvAchievement: float
      /// 環境流水の最低基準を満たしたターン数
      EnvMinMetTurns: int
      TotalTurns: int
      /// 終了時の地下水貯留量 / 開始時の地下水貯留量 (真の値の合計)
      GwRatio: float
      GwScore: float
      /// 揚水コスト上昇フラグが有効になった区域の数
      FlaggedZones: int
      FinalBalance: float
      MinBalance: float
      FinScore: float
      TotalCosts: TurnCosts
      TotalRevenue: float
      Indicators: Indicator list
      /// 重み付き平均と、最も低い指標の混合 (0.6 × 平均 + 0.4 × 最低) の総合点
      Score: float
      MaxAbsResidualWU: float
      ResidualOk: bool
      Rank: Rank
      /// 計画経済版: 計画達成度と上司の信頼 (指令のないシナリオでは None)。流域の実態 (上の指標・総合) とは混ぜない
      Plan: PlanSummary option }

/// デブリーフ用: 推定値と実際の値の推移。Turn = 0 は開始時
type GwPoint =
    { Turn: int
      Estimated: float
      Actual: float }

module Evaluation =

    let private clamp01 (x: float) = x |> max 0.0 |> min 1.0

    let rankOf (score: float) =
        if score >= 0.90 then S
        elif score >= 0.80 then A
        elif score >= 0.65 then B
        else C

    let rankLabel =
        function
        | S -> "S"
        | A -> "A"
        | B -> "B"
        | C -> "C"

    /// 地下水の評価: 残存量(終了時/開始時)を基準の比で割って満点を1とし、フラグが有効になった区域ごとに 0.15 を引く
    let gwScore (referenceRatio: float) (ratio: float) (flagged: int) =
        clamp01 (ratio / referenceRatio - 0.15 * float flagged)

    /// 財政の評価: 最終残高が基準以上なら満点。基準未満は比例で、一度でもマイナスになったら 0.2 引く
    let finScore (target: float) (finalBalance: float) (minBalance: float) =
        let basis = if target <= 0.0 then 1.0 else clamp01 (finalBalance / target)
        clamp01 (basis - (if minBalance < 0.0 then 0.2 else 0.0))

    /// どれか1つの指標だけを最大化しても高ランクにならないよう、重み付き平均と最低の指標を混ぜる
    let combine (indicators: Indicator list) =
        let totalWeight = indicators |> List.sumBy _.Weight
        let avg = (indicators |> List.sumBy (fun i -> i.Score * i.Weight)) / totalWeight
        let lowest = indicators |> List.map _.Score |> List.min
        // 平均と最低値の混合: 最も低い指標が足を引っ張る一方で、ほかの指標の改善も(上限で打ち切られず)総合に反映される
        0.6 * avg + 0.4 * lowest

    let summarize (scenario: Scenario) (state: GameState) : Summary =
        let history = state.History
        let demand = history |> List.sumBy _.AgDemandWU
        let supply = history |> List.sumBy (fun r -> r.Allocation.AgSupplyWU + r.AgGwSupplyWU)
        let agFill = if demand <= 0.0 then 1.0 else supply / demand
        let urbanDemand = history |> List.sumBy _.UrbanDemandWU
        let urbanSupply = history |> List.sumBy (fun r -> r.Allocation.UrbanSupplyWU + r.UrbanGwSupplyWU)
        let urbanFill = if urbanDemand <= 0.0 then 1.0 else urbanSupply / urbanDemand

        let envAch =
            if history.IsEmpty then 1.0 else history |> List.averageBy _.EnvAchievement

        let envMet = history |> List.filter _.EnvMinMet |> List.length
        let maxRes = history |> List.map (fun r -> abs r.Balance.ResidualWU) |> List.fold max 0.0

        let initialGw = state.Hidden |> Map.toList |> List.sumBy (fun (_, h) -> h.InitialG)
        let finalGw = state.GwWU |> Map.toList |> List.sumBy snd
        let ratio = if initialGw <= 0.0 then 1.0 else finalGw / initialGw
        let flagged = state.CostFlags |> Map.toList |> List.filter snd |> List.length
        let gw = gwScore scenario.Eval.GwReferenceRatio ratio flagged

        let minBalance =
            history |> List.map _.BalanceEnd |> List.fold min scenario.Finance.InitialBalance

        let fin = finScore scenario.Eval.TargetBalance state.Balance minBalance
        let e = scenario.Eval

        let indicators =
            [ { Name = "農業用水の充足度"
                Score = clamp01 agFill
                Weight = e.AgWeight
                Detail = $"供給 {supply:N0} / 需要 {demand:N0} WU" }
              { Name = "生活・産業用水の充足度"
                Score = clamp01 urbanFill
                Weight = e.UrbanWeight
                Detail = $"供給 {urbanSupply:N0} / 需要 {urbanDemand:N0} WU" }
              { Name = "環境流水の達成度"
                Score = clamp01 envAch
                Weight = e.EnvWeight
                Detail = $"最低基準を満たしたのは {envMet} / {history.Length} ターン" }
              { Name = "地下水の残存"
                Score = gw
                Weight = e.GwWeight
                Detail = $"貯留量は開始時の {ratio * 100.0:F0}%%、揚水コスト上昇 {flagged} 区域" }
              { Name = "財政の健全性"
                Score = fin
                Weight = e.FinWeight
                Detail = $"最終残高 {state.Balance:N0} (基準 {e.TargetBalance:N0})、最低残高 {minBalance:N0}" } ]

        let score = combine indicators

        { AgFillRate = agFill
          UrbanFillRate = urbanFill
          EnvAchievement = envAch
          EnvMinMetTurns = envMet
          TotalTurns = history.Length
          GwRatio = ratio
          GwScore = gw
          FlaggedZones = flagged
          FinalBalance = state.Balance
          MinBalance = minBalance
          FinScore = fin
          TotalCosts = state.CostTotals
          TotalRevenue = history |> List.sumBy _.Revenue
          Indicators = indicators
          Score = score
          MaxAbsResidualWU = maxRes
          ResidualOk = history |> List.forall _.Balance.IsWithinTolerance
          Rank = rankOf score
          Plan = (if scenario.Directives.IsSome then Some(Organization.summary state.Organization) else None) }

    /// 推定値と実際の値の推移 (開始時 + 各ターン終了後)
    let gwTrace (scenario: Scenario) (state: GameState) (zone: ZoneId) : GwPoint list =
        let start =
            { Turn = 0
              Estimated = (Groundwater.priorEstimate (Groundwater.spec scenario zone)).GwWU
              Actual = state.Hidden[zone].InitialG }

        start
        :: (state.History
            |> List.map (fun r ->
                { Turn = r.Turn + 1
                  Estimated = r.EstimatesAfter[zone].GwWU
                  Actual = r.ActualGwAfter[zone] }))

    /// 推定誤差を割合で言うときの、実際の貯留量の下限 (これ未満だと割合が極端になる)
    let private minRelBase = 100.0

    /// プレイ中に見せる、ここまでの評価の見込み (B3)。結果がまだなければ None。
    /// 地下水だけは、隠しの真の値ではなく、プレイヤーが知っている推定値と開始時の推定から計算する。
    let preview (scenario: Scenario) (state: GameState) : (Indicator list * float * Rank) option =
        if state.History.IsEmpty then
            None
        else
            let s = summarize scenario state

            let startEst =
                scenario.Zones |> List.sumBy (fun z -> (Groundwater.priorEstimate (Groundwater.spec scenario z.Id)).GwWU)

            let nowEst = scenario.Zones |> List.sumBy (fun z -> state.Estimates[z.Id].GwWU)
            let ratio = if startEst <= 0.0 then 1.0 else nowEst / startEst
            let flagged = state.CostFlags |> Map.toList |> List.filter snd |> List.length

            let indicators =
                s.Indicators
                |> List.map (fun i ->
                    if i.Name = "地下水の残存" then
                        { i with
                            Score = gwScore scenario.Eval.GwReferenceRatio ratio flagged
                            Detail = $"推定では開始時の約 {ratio * 100.0:F0}%%、揚水コスト上昇 {flagged} 区域" }
                    elif i.Name = "財政の健全性" then
                        // 残高は毎年の交付金などで積み上がるので、ここまでの1ターンあたりの増減が続くとして、最終残高を見込む
                        let played = float state.History.Length
                        let remaining = float (scenario.Turns - state.History.Length)
                        let perTurn = (state.Balance - scenario.Finance.InitialBalance) / played
                        let projected = state.Balance + perTurn * remaining
                        let minBalance = state.History |> List.map _.BalanceEnd |> List.fold min scenario.Finance.InitialBalance

                        { i with
                            Score = finScore scenario.Eval.TargetBalance projected minBalance
                            Detail = $"今の残高 {state.Balance:N0}。このペースが続くと最終残高は約 {projected:N0} (基準 {scenario.Eval.TargetBalance:N0})" }
                    else
                        i)

            let score = combine indicators
            Some(indicators, score, rankOf score)

    /// 調査の時期と推定精度の短い解説 (「調査を早く実施していれば」)
    let debrief (scenario: Scenario) (state: GameState) : string list =
        [ for z in scenario.Zones do
              let spec = Groundwater.spec scenario z.Id
              let prior = Groundwater.priorEstimate spec
              let name = ZoneId.label z.Id

              let firstSurvey =
                  state.History
                  |> List.tryPick (fun r ->
                      r.SurveyReports
                      |> List.tryFind (fun s -> s.Zone = z.Id)
                      |> Option.map (fun _ -> r.Turn))

              let errText =
                  match List.tryLast state.History with
                  | Some r when r.ActualGwAfter[z.Id] >= minRelBase ->
                      let e = (r.EstimatesAfter[z.Id].GwWU - r.ActualGwAfter[z.Id]) / r.ActualGwAfter[z.Id]
                      let signed = (e * 100.0).ToString("+0;-0;0")
                      $"終了時の推定は実際より {signed}%% でした。"
                  | Some r ->
                      // 実際の貯留量がほぼ空だと、割合が意味を持たない
                      let diff = r.EstimatesAfter[z.Id].GwWU - r.ActualGwAfter[z.Id]
                      $"終了時の実際の貯留量はほぼ空でした (推定との差 {diff:N0} WU)。"
                  | None -> ""

              match firstSurvey with
              | None ->
                  yield $"{name}: 最後まで調査しなかったため、推定の幅は ±{prior.RelErr * 100.0:F0}%% のままでした。{errText}"
              | Some t when t >= 3 ->
                  yield
                      $"{name}: 第{t + 1}ターンに初めて調査しました。それまでの{t}ターンは ±{prior.RelErr * 100.0:F0}%% の幅で判断しており、もっと早く調べていれば、井戸や揚水の判断材料が増えたはずです。{errText}"
              | Some t -> yield $"{name}: 第{t + 1}ターンに早めに調査し、判断材料を確保できました。{errText}" ]


    /// 学習目標 1〜5 それぞれの振り返り (最終デブリーフ)。返り値は (番号, 目標, 振り返り)
    let reflections (scenario: Scenario) (state: GameState) (s: Summary) : (int * string * string) list =
        let h = state.History
        let sum f = h |> List.sumBy f
        let zoneSum f = h |> List.sumBy (fun r -> r.Zones |> List.sumBy f)
        let share x total = if total <= 0.0 then 0.0 else x / total * 100.0

        // 1 水収支
        let precip = sum (fun r -> r.Balance.PrecipInWU)
        let et = sum (fun r -> r.Balance.ActualEtWU)
        let supply = sum (fun r -> r.Balance.ExternalSupplyWU)
        let outflow = sum (fun r -> r.Balance.OutflowWU)
        let storage = sum (fun r -> r.Balance.StorageChangeWU)

        let goal1 =
            $"{h.Length}ターンの降水 {precip:N0} WU は、蒸発散 {share et precip:F0}%%、用水供給 {share supply precip:F0}%%、下流端からの流出 {share outflow precip:F0}%%、貯留の増減 {share storage precip:F0}%% に配分されました。浸透や涵養は貯留の場所を移すだけで、流域外への損失ではありません。収支の残差は最大 {s.MaxAbsResidualWU:E1} WU でした。"

        // 2 地下水のストック性
        let recharge = zoneSum _.DeepDrainageWU
        let pumped = zoneSum _.PumpedWU
        let discharge = zoneSum _.DischargeWU

        let seepage = zoneSum _.SeepageWU

        let seepageText =
            if seepage > 0.5 then
                $"河川から地下水へ {seepage:N0} WU が浸透しました (地下水位が下がるほど、川の水が地下水に吸われます)。"
            else
                ""

        let goal2 =
            $"地下水の貯留量は開始時の {s.GwRatio * 100.0:F0}%% になりました。涵養 {recharge:N0} WU に対し、揚水 {pumped:N0} WU を汲み、河川へ {discharge:N0} WU が湧き出ました。{seepageText}地下水は、その季節の量ではなく、涵養と揚水と湧出の差が積み重なるストックです。"

        // 3 配分のトレードオフ
        let weakest =
            [ "農業用水", s.AgFillRate
              "生活・産業用水", s.UrbanFillRate
              "環境流水", s.EnvAchievement ]
            |> List.minBy snd

        let finalPriority =
            match List.tryLast h with
            | Some r -> r.EffectivePriority |> List.map Allocatee.label |> String.concat " → "
            | None -> ""

        let weakestText =
            if snd weakest >= 0.99 then
                "どの用途もほぼ満たせました。"
            else
                $"最も満たせなかったのは{fst weakest}({snd weakest * 100.0:F0}%%)です。"

        let goal3 =
            $"農業 {s.AgFillRate * 100.0:F0}%%、生活・産業 {s.UrbanFillRate * 100.0:F0}%%、環境流水は最低基準を {s.EnvMinMetTurns}/{s.TotalTurns} ターンで満たしました。{weakestText}最後の優先順位は「{finalPriority}」で、水が足りないときは下の項目から削られます。"

        // 4 情報の価値
        let surveys = h |> List.sumBy (fun r -> r.SurveyReports.Length)

        let errOf (z: ZoneId) =
            match List.tryLast h with
            | Some r when r.ActualGwAfter[z] >= minRelBase -> abs (r.EstimatesAfter[z].GwWU - r.ActualGwAfter[z]) / r.ActualGwAfter[z]
            | _ -> 0.0

        let surveyed, unsurveyed =
            scenario.Zones |> List.map _.Id |> List.partition (fun z -> state.Survey[z] <> Unsurveyed)

        let avg zs = if List.isEmpty zs then None else Some(zs |> List.averageBy errOf)

        let errText =
            match avg surveyed, avg unsurveyed with
            | Some a, Some b ->
                $"終了時の推定の誤差は、調査した区域で平均 {a * 100.0:F0}%%、未調査の区域で平均 {b * 100.0:F0}%% でした。"
            | Some a, None -> $"終了時の推定の誤差は、全区域で平均 {a * 100.0:F0}%% でした。"
            | None, Some b -> $"調査しなかったため、終了時の推定の誤差は平均 {b * 100.0:F0}%% のままでした。"
            | None, None -> ""

        let obsZones =
            scenario.Zones
            |> List.filter (fun z -> state.Invested.Contains(ObservationWell z.Id))
            |> List.map (fun z -> ZoneId.label z.Id)

        let obsText =
            if obsZones.IsEmpty then
                ""
            else
                let names = String.concat "、" obsZones
                $"観測井戸は {names} に設置し、季節ごとに推定の幅を狭めました。"

        let goal4 =
            $"調査は {surveys} 回で、調査費は {s.TotalCosts.Survey:N0} でした。{obsText}{errText}調査は費用がかかりますが、推定の幅を狭めて、井戸や揚水の判断を変えられます。"

        // 5 不可逆性
        let flaggedZones =
            state.CostFlags |> Map.toList |> List.filter snd |> List.map (fst >> ZoneId.label)

        let flaggedNames = String.concat "、" flaggedZones

        let goal5 =
            if flaggedZones.IsEmpty then
                $"どの区域も閾値を下回らず、揚水コストは上昇しませんでした(揚水費の合計 {s.TotalCosts.Pumping:N0})。閾値を割ると、貯留量が回復しても揚水コストは元に戻りません。"
            else
                $"{flaggedNames}で貯留量が閾値を下回り、揚水コストが恒久的に上昇しました(揚水費の合計 {s.TotalCosts.Pumping:N0})。回復に時間がかかる資源は、使い切る前に歯止めをかける必要があります。"

        [ 1, "水収支", goal1
          2, "地下水のストック性", goal2
          3, "配分のトレードオフ", goal3
          4, "情報の価値", goal4
          5, "不可逆性", goal5 ]
