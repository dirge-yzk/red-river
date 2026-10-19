namespace WaterGame.Core

/// 1ターンの解決処理。UI から独立した純粋関数: (シナリオ, 前ターン状態, 政策, 気象実績) -> 次ターン状態・結果
module Turn =

    /// 配分優先順位の初期値 (農業 → 環境流水 → 生活・産業)。何も考えずに進めると、生活・産業が最初に不足する。
    /// 3項目の並べ替えは各年の春に1回だけできる
    let defaultPriority = [ Agriculture; Environment; Municipal ]

    let allAllocatees = [ Agriculture; Municipal; Environment ]

    /// 3項目をちょうど1つずつ含む並びか
    let isValidPriority (priority: Allocatee list) =
        List.sort priority = List.sort allAllocatees

    let private zeroCosts =
        { Survey = 0.0
          Build = 0.0
          Invest = 0.0
          Maintenance = 0.0
          Pumping = 0.0 }

    /// 開始時の状態。真の地下水貯留量などの隠しパラメータはシナリオとシードから決まる
    /// 初期状態。乱数は使わず、シナリオだけから決まる (気象は、シードから別に生成する)
    let initialState (scenario: Scenario) : GameState =
        let hidden = Groundwater.hiddenOf scenario
        let ids = scenario.Zones |> List.map _.Id
        let perZone f = ids |> List.map (fun z -> z, f z) |> Map.ofList

        { Turn = 0
          SoilWU =
            scenario.Zones
            |> List.map (fun z -> z.Id, z.InitialSoilMm * z.AreaKm2)
            |> Map.ofList
          GwWU = perZone (fun z -> hidden[z].InitialG)
          Hidden = hidden
          Survey = perZone (fun _ -> Unsurveyed)
          Wells = perZone (fun _ -> NoWell)
          Estimates = perZone (fun z -> Groundwater.priorEstimate (Groundwater.spec scenario z))
          CostFlags = perZone (fun _ -> false)
          Overdraft = perZone (fun _ -> 0.0)
          CostTotals = zeroCosts
          Balance = scenario.Finance.InitialBalance
          Invested = Set.empty
          Priority = defaultPriority
          ReservoirWU = 0.0
          Events = Events.generate scenario
          Organization = Organization.initialState scenario
          Construction = Construction.initialState scenario
          History = [] }

    /// 配分優先順位は各年の春ターンに限り変更できる
    let canChangePriority (turn: int) = Season.ofTurn turn = Spring

    let isFinished (scenario: Scenario) (state: GameState) = state.Turn >= scenario.Turns

    /// 主要行動の費用
    let actionCost (scenario: Scenario) =
        function
        | NoAction -> 0.0
        | Survey(_, Reconnaissance) -> scenario.Costs.ReconCost
        | Survey(_, TestDrilling) -> scenario.Costs.TestDrillCost
        | BuildWell _ -> scenario.Costs.WellCost
        | Invest IrrigationEfficiency -> scenario.Invest.IrrigationCost
        | Invest IrrigationUpgrade -> scenario.Invest.IrrigationUpgradeCost
        | Invest(ObservationWell _) -> scenario.Invest.ObservationCost
        | Invest Reservoir -> scenario.Reservoir |> Option.map _.Cost |> Option.defaultValue 0.0
        | Invest ReservoirExpansion -> scenario.Reservoir |> Option.map _.ExpansionCost |> Option.defaultValue 0.0
        | Invest RainwaterHarvest -> scenario.Invest.RainwaterCost
        | Invest EffluentReuse -> scenario.Invest.EffluentCost
        | Invest EffluentUpgrade -> scenario.Invest.EffluentUpgradeCost
        | Invest WaterSaving -> scenario.Invest.WaterSavingCost
        // 融通の手数料 (融通ができないシナリオでは 0)
        | RequestTransfer _ -> Construction.transferFee scenario

    /// 今季に受け取る管理交付金 (春だけ)。できごとで増減する。臨時の補助金も、その季節に加わる
    let grantThisTurn (scenario: Scenario) (state: GameState) =
        if isFinished scenario state then
            0.0
        else
            (if canChangePriority state.Turn then
                 scenario.Finance.AnnualGrant * Events.grantMultiplier state * Organization.grantMultiplier scenario state.Organization
             else
                 0.0)
            + Events.extraFunds state

    /// 今季の開始時点で自由に使える資金: 残高に、今季に受け取る交付金・補助金を加えたもの
    let availableFunds (scenario: Scenario) (state: GameState) = state.Balance + grantThisTurn scenario state

    /// 実行できる主要行動か。調査・井戸・投資の条件を満たし、かつ資金の範囲内であること
    /// (資金不足のときは任意行動を選べない。仕様書 10.1)
    let isValidAction (scenario: Scenario) (state: GameState) (action: MajorAction) =
        let allowed =
            match action with
            | Invest i ->
                not (state.Invested.Contains i)
                && (match i with
                    | Reservoir -> scenario.Reservoir.IsSome
                    // 拡張は、もとの設備を作ったあとに、1度だけ
                    | ReservoirExpansion -> scenario.Reservoir.IsSome && state.Invested.Contains Reservoir
                    | IrrigationUpgrade -> state.Invested.Contains IrrigationEfficiency
                    | EffluentUpgrade -> state.Invested.Contains EffluentReuse
                    | _ -> true)
            | RequestTransfer _ -> Construction.canTransfer scenario state.Construction
            | other -> Groundwater.isValidAction state other

        // 工事は、同じものを二重に発注できない (発注済みで、まだ完成していないもの)
        let notQueued = not (Construction.isProject scenario action && Construction.inQueue state.Construction action)

        allowed && notQueued && actionCost scenario action <= availableFunds scenario state + 1e-9

    /// 投資の効果とできごとを反映した、今季の農業需要 (灌漑効率改善で減る)
    let effectiveAgDemand (scenario: Scenario) (state: GameState) (season: Season) =
        let k =
            if state.Invested.Contains IrrigationUpgrade then
                1.0 - scenario.Invest.IrrigationUpgradeReduction
            elif state.Invested.Contains IrrigationEfficiency then
                1.0 - scenario.Invest.IrrigationDemandReduction
            else
                1.0

        let saving = if state.Invested.Contains WaterSaving then 1.0 - scenario.Invest.WaterSavingDemandReduction else 1.0
        scenario.AgDemandWU[season] * k * saving * Events.agDemandMultiplier state

    /// 今季の生活・産業用水の需要 (基本の値)。シナリオによっては年ごとに伸びる (市街地拡大)
    let urbanDemand (scenario: Scenario) (turn: int) =
        let year = turn / 4
        scenario.UrbanDemandWU[Season.ofTurn turn] * pown (1.0 + scenario.UrbanDemandGrowthPerYear) year

    /// 雨水利用の効果とできごとを反映した、今季の生活・産業用水の需要
    let effectiveUrbanDemand (scenario: Scenario) (state: GameState) (turn: int) =
        let k =
            if state.Invested.Contains RainwaterHarvest then
                1.0 - scenario.Invest.RainwaterDemandReduction
            else
                1.0

        let saving = if state.Invested.Contains WaterSaving then 1.0 - scenario.Invest.WaterSavingDemandReduction else 1.0
        urbanDemand scenario turn * k * saving * Events.urbanDemandMultiplier state

    /// 今季の井戸1本の揚水能力。劣化 (揚水コスト上昇フラグ)・停電などのできごとを反映する
    let wellCapacity (scenario: Scenario) (state: GameState) (zone: ZoneId) =
        let a = Groundwater.spec scenario zone
        // 揚水コスト上昇フラグが有効な区域は、水位の低下や井戸の劣化で、揚水能力も半分になる (回復しない)
        let degraded = if state.CostFlags[zone] then 0.5 else 1.0
        a.WellCapacityWU * degraded * Events.pumpCapacityMultiplier state

    /// 貯水池の貯留容量 (建設済みのときだけ。なければ 0)
    let reservoirCapacity (scenario: Scenario) (state: GameState) =
        match scenario.Reservoir with
        | Some r when state.Invested.Contains ReservoirExpansion -> r.ExpandedCapacityWU
        | Some r when state.Invested.Contains Reservoir -> r.CapacityWU
        | _ -> 0.0

    /// 再生水として河川へ戻る割合 (整備済みのときだけ。なければ 0)
    let effluentReturnRate (scenario: Scenario) (state: GameState) =
        if state.Invested.Contains EffluentUpgrade then scenario.Invest.EffluentUpgradeReturnRate
        elif state.Invested.Contains EffluentReuse then scenario.Invest.EffluentReturnRate
        else 0.0

    /// 貯水池の蒸発損失率 (今季)
    let reservoirEvapRate (scenario: Scenario) (season: Season) =
        scenario.Reservoir |> Option.map (fun r -> r.EvapRate[season]) |> Option.defaultValue 0.0

    /// その時点の区域の条件。市街地は舗装の拡大で流出係数が年々上がる (上限 0.9)
    let effectiveZone (scenario: Scenario) (state: GameState) (z: ZoneParams) =
        if z.Id = Urban then
            { z with RunoffCoef = min 0.9 (z.RunoffCoef + scenario.UrbanRunoffGrowthPerYear * float (state.Turn / 4)) }
        else
            z

    /// 政策を許容範囲に収める。農業は需要まで、環境流水はシナリオの最低基準以上、
    /// 揚水上限は 0 以上、実行できない主要行動は NoAction とする。
    let normalizePolicy (scenario: Scenario) (state: GameState) (policy: Policy) : Policy =
        let season = Season.ofTurn state.Turn

        { AgTargetWU = policy.AgTargetWU |> max 0.0 |> min (effectiveAgDemand scenario state season)
          UrbanTargetWU = policy.UrbanTargetWU |> max 0.0 |> min (effectiveUrbanDemand scenario state state.Turn)
          EnvTargetWU = max policy.EnvTargetWU scenario.MinEnvFlowWU[season]
          Priority =
            if canChangePriority state.Turn && isValidPriority policy.Priority then
                policy.Priority
            else
                state.Priority
          PumpLimitWU =
            scenario.Zones
            |> List.map (fun z -> z.Id, policy.PumpLimitWU |> Map.tryFind z.Id |> Option.defaultValue 0.0 |> max 0.0)
            |> Map.ofList
          Report = policy.Report
          ReservoirOpWU =
            if state.Invested.Contains Reservoir then
                policy.ReservoirOpWU |> max (-state.ReservoirWU) |> min (reservoirCapacity scenario state)
            else
                0.0
          Action = if isValidAction scenario state policy.Action then policy.Action else NoAction }

    let private toleranceFor (precipIn: float) = max 1e-6 (precipIn * 1e-9)

    let resolve (scenario: Scenario) (state: GameState) (rawPolicy: Policy) (weather: ActualWeather) : GameState * TurnResult =
        let season = Season.ofTurn state.Turn
        let policy = normalizePolicy scenario state rawPolicy

        // 2〜4. 降水を流出と浸透に分け、土壌水分・実蒸発散・深部排水を更新し、季節開始時の地下水から河川湧出を計算する
        let zones0 =
            scenario.Zones
            |> List.map (fun z ->
                let a = Groundwater.spec scenario z.Id

                Hydrology.zoneStep
                    (effectiveZone scenario state z)
                    state.SoilWU[z.Id]
                    state.GwWU[z.Id]
                    a.DischargeK
                    weather.PrecipMm
                    weather.EtDemandMm)

        // 5. 地表流出と地下水湧出を、上流から下流へ一度だけ集計する。川の水は、地下水位が低い区域で地下水へ浸透する
        let zonesR, inflow = Groundwater.routeRiver scenario (fun z -> state.GwWU[z]) zones0
        let allocIndex = scenario.Zones |> List.findIndex (fun z -> z.Id = scenario.AllocationAfter)
        let _, below = zonesR |> List.splitAt (allocIndex + 1)
        let belowContribution = below |> List.sumBy _.RiverContributionWU

        // 貯水池: 取水点の上流で、河川水を貯める・放流する
        let reservoirFlow =
            match scenario.Reservoir with
            | Some r when state.Invested.Contains Reservoir ->
                Some(Hydrology.reservoirStep (reservoirCapacity scenario state) r.EvapRate[season] state.ReservoirWU inflow policy.ReservoirOpWU)
            | _ -> None

        let available =
            match reservoirFlow with
            | Some rf -> inflow - rf.StoredWU + rf.ReleasedWU
            | None -> inflow

        // 6. 河川水を配分し、河川水で満たせなかった農業需要を地下水で補う
        let allocation0 =
            Hydrology.allocate
                policy.Priority
                available
                belowContribution
                policy.AgTargetWU
                policy.UrbanTargetWU
                policy.EnvTargetWU

        // 再生水: 河川水で供給した生活・産業用水の一部が、下流へ戻る (環境流水の予約には数えず、下流の流量に上積みされる)
        let returnFlow =
            allocation0.UrbanSupplyWU * effluentReturnRate scenario state

        let allocation =
            { allocation0 with
                ReturnFlowWU = returnFlow
                DownstreamFlowWU = allocation0.DownstreamFlowWU + returnFlow }

        let agDemand = effectiveAgDemand scenario state season
        let urbanDemandBase = urbanDemand scenario state.Turn
        let urbanDemand = effectiveUrbanDemand scenario state state.Turn
        let agUnmet = max 0.0 (agDemand - allocation.AgSupplyWU)
        let urbanUnmet = max 0.0 (urbanDemand - allocation.UrbanSupplyWU)

        let limits =
            zonesR
            |> List.map (fun z ->
                z.Zone, Groundwater.pumpLimit (wellCapacity scenario state z.Zone) state.Wells[z.Zone] policy.PumpLimitWU[z.Zone] z)

        let pumped = Groundwater.allocatePumping limits (agUnmet + urbanUnmet)
        let zones = zonesR |> List.map (fun z -> Hydrology.applyPumping pumped[z.Zone] z)
        let gwSupply = zones |> List.sumBy _.PumpedWU

        // 揚水した地下水は、優先順位の高い用水から順に未充足需要へ供給する (環境流水には供給しない)
        let _, agGw, urbanGw =
            policy.Priority
            |> List.fold
                (fun (remaining, ag, urban) item ->
                    match item with
                    | Agriculture ->
                        let s = min agUnmet remaining
                        remaining - s, s, urban
                    | Municipal ->
                        let s = min urbanUnmet remaining
                        remaining - s, ag, s
                    | Environment -> remaining, ag, urban)
                (gwSupply, 0.0, 0.0)

        // 7〜9. 環境流水の評価、状態更新、水収支残差
        let envMin = scenario.MinEnvFlowWU[season]
        let precipIn = zones |> List.sumBy _.PrecipWU
        let storageChange =
            (zones |> List.sumBy (fun z -> (z.SoilEndWU - z.SoilStartWU) + (z.GwEndWU - z.GwStartWU)))
            + (match reservoirFlow with
               | Some rf -> rf.EndWU - rf.StartWU
               | None -> 0.0)

        // 貯水池の蒸発も蒸発散に数える
        let actualEt =
            (zones |> List.sumBy _.ActualEtWU)
            + (match reservoirFlow with
               | Some rf -> rf.EvaporationWU
               | None -> 0.0)
        // 用水は流域の外へ供給するが、再生水として戻った分は差し引く (下流の流量に含まれているため)
        let externalSupply = allocation.AgSupplyWU + allocation.UrbanSupplyWU + gwSupply - returnFlow

        let balance =
            { PrecipInWU = precipIn
              ActualEtWU = actualEt
              ExternalSupplyWU = externalSupply
              OutflowWU = allocation.DownstreamFlowWU
              StorageChangeWU = storageChange
              ResidualWU = precipIn - actualEt - externalSupply - allocation.DownstreamFlowWU - storageChange
              ToleranceWU = toleranceFor precipIn }

        let envMinMet = allocation.DownstreamFlowWU >= envMin - 1e-9
        let fill demand supply = if demand <= 0.0 then 1.0 else min 1.0 (supply / demand)
        let agFill = fill agDemand (allocation.AgSupplyWU + agGw)
        let urbanFill = fill urbanDemand (allocation.UrbanSupplyWU + urbanGw)

        let envAchievement =
            if policy.EnvTargetWU <= 0.0 then 1.0
            else min 1.0 (allocation.DownstreamFlowWU / policy.EnvTargetWU)

        // 真の持続的揚水量を超えた揚水の累積 (超過の借り)。超えた分だけ増え、下回った分だけ減る
        let overdraft =
            zones
            |> List.map (fun z -> z.Zone, max 0.0 (state.Overdraft[z.Zone] + z.PumpedWU - state.Hidden[z.Zone].Yield))
            |> Map.ofList

        // 揚水コスト上昇フラグ: 井戸のある区域で、貯留量が閾値を下回るか、超過の借りが限度に達したら有効にし、以後は解除しない
        // (井戸がなければ揚水コストは発生しないので、自然な減少だけでは立てない)
        let newFlags =
            zones
            |> List.filter (fun z ->
                state.Wells[z.Zone] = WellBuilt
                && not state.CostFlags[z.Zone]
                && (z.GwEndWU < (Groundwater.spec scenario z.Zone).ThresholdWU
                    || overdraft[z.Zone] >= scenario.Costs.OverdraftLimit * state.Hidden[z.Zone].Yield))
            |> List.map _.Zone

        let costFlags = newFlags |> List.fold (fun m z -> Map.add z true m) state.CostFlags

        // 推定値の更新 (公開情報の涵養・揚水で進め、調査を行った区域は結果で更新する)
        let advanced =
            zones
            |> List.map (fun z ->
                z.Zone,
                Groundwater.advanceEstimate (Groundwater.spec scenario z.Zone) z.DeepDrainageWU z.PumpedWU state.Estimates[z.Zone])
            |> Map.ofList

        let actualGwAfter = zones |> List.map (fun z -> z.Zone, z.GwEndWU) |> Map.ofList

        // 観測井戸のある区域は、季節ごとに推定の誤差が縮む
        let observed, observations =
            scenario.Zones
            |> List.fold
                (fun (acc: Map<ZoneId, Estimate>, obs) z ->
                    if state.Invested.Contains(ObservationWell z.Id) then
                        let before = acc[z.Id]
                        let after = Groundwater.applyObservation scenario.Invest state.Hidden[z.Id] actualGwAfter[z.Id] before
                        Map.add z.Id after acc, obs @ [ z.Id, before.RelErr, after.RelErr ]
                    else
                        acc, obs)
                (advanced, [])

        let estimates, surveyStage, reports =
            match policy.Action with
            | Survey(z, kind) ->
                let est, report =
                    Groundwater.applySurvey scenario.Costs state.Hidden[z] actualGwAfter[z] kind observed[z]

                let stage = if kind = Reconnaissance then Reconnoitered else TestDrilled
                Map.add z est observed, Map.add z stage state.Survey, [ report ]
            | _ -> observed, state.Survey, []

        // 建設・資材: 発注 → 融通 → 着工 → 工事の進行 → 完成 → 来季の正規配給と返礼。
        // 資材・建設のないシナリオ (入門) では、井戸と投資は、実施した季節の終わりに完成する。
        // どちらも、完成した季節の翌季から効く。調査状態は変えない
        let construction, works = Construction.step scenario state.Construction state.Turn policy.Action

        let finished =
            if scenario.Construction.IsSome then
                // 工事にならない行動 (節水キャンペーンなど) は、その季節の終わりに完成する
                (works.Completed |> List.map _.Target)
                @ (if Construction.isProject scenario policy.Action then [] else [ policy.Action ])
            else
                [ policy.Action ]

        let wells =
            finished
            |> List.fold
                (fun (w: Map<ZoneId, WellState>) a ->
                    match a with
                    | BuildWell z -> Map.add z WellBuilt w
                    | _ -> w)
                state.Wells

        let invested =
            finished
            |> List.fold
                (fun (inv: Set<Investment>) a ->
                    match a with
                    | Invest i -> inv.Add i
                    | _ -> inv)
                state.Invested

        let pumpCostK = Events.pumpCostMultiplier state

        let pumpingCost =
            zones
            |> List.sumBy (fun z ->
                // 観測井戸のある区域は、水位を見ながら効率よく運転できるので、揚水費が下がる
                let obsK =
                    if state.Invested.Contains(ObservationWell z.Zone) then
                        1.0 - scenario.Invest.ObservationPumpSaving
                    else
                        1.0

                obsK
                * Groundwater.pumpingCost
                    scenario.Costs
                    (Groundwater.spec scenario z.Zone)
                    state.Hidden[z.Zone]
                    state.CostFlags[z.Zone]
                    z.PumpedWU)
            |> (*) pumpCostK

        let wellCount = state.Wells |> Map.toList |> List.filter (fun (_, w) -> w = WellBuilt) |> List.length
        let optionalCost = actionCost scenario policy.Action

        let costs =
            { Survey = (match policy.Action with Survey _ -> optionalCost | _ -> 0.0)
              Build = (match policy.Action with BuildWell _ -> optionalCost | _ -> 0.0)
              // 融通の手数料も、ここに含める
              Invest = (match policy.Action with Invest _ | RequestTransfer _ -> optionalCost | _ -> 0.0)
              Maintenance = float wellCount * scenario.Finance.WellMaintenance
              Pumping = pumpingCost }

        // 財政: 期末残高 = 期首残高 + 年度交付金 + 収入 - 各種費用。収入は農業生産(充足率)に比例する。
        // 灌漑効率改善で需要が減っても同じ生産を維持できるので、収入は元の需要を基準にする。
        let grant = grantThisTurn scenario state
        let revenue =
            agFill * scenario.AgDemandWU[season] * Events.agDemandMultiplier state * scenario.Finance.AgRevenuePerWU
            + urbanFill * (urbanDemandBase * Events.urbanDemandMultiplier state) * scenario.Finance.UrbanRevenuePerWU
        let balanceEnd = state.Balance + grant + revenue - costs.Total

        let goal0, line0 =
            Explain.explain
                { Priority = policy.Priority
                  Zones = zones
                  Allocation = allocation
                  AgDemandWU = agDemand
                  AgTargetWU = policy.AgTargetWU
                  AgRiverSupplyWU = allocation.AgSupplyWU
                  AgGwSupplyWU = agGw
                  UrbanDemandWU = urbanDemand
                  UrbanTargetWU = policy.UrbanTargetWU
                  UrbanRiverSupplyWU = allocation.UrbanSupplyWU
                  UrbanGwSupplyWU = urbanGw
                  GwSupplyWU = gwSupply
                  EnvMinMet = envMinMet
                  SurveyReports = reports
                  NewCostFlags = newFlags
                  Action = policy.Action }

        // 資材・建設のあるシナリオでは、発注した季節には、まだ完成しない
        let goal, line =
            match works.Ordered, works.Transferred with
            | Some p, _ ->
                let started = if works.Started |> List.exists (fun q -> q.Id = p.Id) then "資材がそろっているので、今季のうちに着工しました" else "資材がそろうのを待ちます"
                3, $"{Construction.targetLabel p.Target}を発注しました。{started}。標準工期 {p.Duration} 季のあとに完成し、効果は、完成した季節の翌季からです。資金があっても、資材と専門技術者がなければ、工事は進みません。"
            | None, Some m ->
                3, $"他地域から、{Construction.materialLabel m}を 1 単位、融通してもらいました。すぐ使えますが、{(scenario.Construction |> Option.map _.TransferRepayAfter |> Option.defaultValue 0)} 季後に、正規配給から 1 単位が差し引かれます。"
            | None, None -> goal0, line0

        let warnings =
            [ if not envMinMet then
                  "環境流水が最低基準を下回りました"
              if agDemand > 0.0 && agFill < 0.5 then
                  "農業用水の充足率が50%を下回りました"
              if urbanFill < 0.9 then
                  "生活・産業用水の充足率が90%を下回りました"
              for z in newFlags do
                  $"{ZoneId.label z}の地下水が限界を超えました (貯留量が閾値を下回ったか、持続的揚水量を超える揚水が積み重なりました)。揚水コストが恒久的に上昇します"
              for z in zones do
                  if z.PumpedWU > state.Estimates[z.Zone].YieldWU then
                      $"{ZoneId.label z.Zone}の揚水が、推定される持続的揚水量を超えました"
              if balanceEnd < 0.0 then
                  "残高がマイナスです。調査・建設・投資は残高が回復するまで選べません"
              if not balance.IsWithinTolerance then
                  "水収支の残差が許容誤差を超えています (内部計算の不整合)"
              for p in construction.Projects do
                  if p.Status = Waiting then
                      let lack =
                          Construction.missing construction p.Needs
                          |> List.map (fun (m, q) -> $"{Construction.materialLabel m} {q}")
                          |> String.concat "、"

                      // 来季のはじめに届く配給で、そろうなら、来季に着工できる
                      if lack = "" then
                          $"{Construction.targetLabel p.Target}は、資材待ちです (来季のはじめに資材が届き、着工できます)"
                      else
                          $"{Construction.targetLabel p.Target}は、資材待ちです (来季のはじめの配給を加えても、不足: {lack})" ]

        let result =
            { Turn = state.Turn
              Season = season
              Weather = weather
              Policy = policy
              EffectivePriority = policy.Priority
              Zones = zones
              Allocation = allocation
              ReservoirFlow = reservoirFlow
              Event = state.Events |> Map.tryFind state.Turn
              Observations = observations
              AgDemandWU = agDemand
              UrbanDemandWU = urbanDemand
              GwSupplyWU = gwSupply
              AgGwSupplyWU = agGw
              UrbanGwSupplyWU = urbanGw
              EnvTargetWU = policy.EnvTargetWU
              EnvMinWU = envMin
              AgFillRate = agFill
              UrbanFillRate = urbanFill
              EnvAchievement = envAchievement
              EnvMinMet = envMinMet
              Balance = balance
              Costs = costs
              Grant = grant
              Revenue = revenue
              BalanceStart = state.Balance
              BalanceEnd = balanceEnd
              ActionTaken = policy.Action
              SurveyReports = reports
              NewCostFlags = newFlags
              EstimatesAfter = estimates
              ActualGwAfter = actualGwAfter
              ExplanationGoal = goal
              Explanation = line
              Warnings = warnings
              Plan = Organization.noPlan state.Organization
              Works = works }

        // 組織・政治の層: 今季の結果を受けて、期限の来た指令を判定する。水文学の状態は変えず、
        // 達成報酬の補助金だけが、期末残高に加わる
        let plan = Organization.step scenario state.Organization state.Turn (state.History @ [ result ]) policy.Report

        let planWarnings =
            [ for r in plan.Resolved do
                  if r.Outcome = Failed then
                      $"指令第{r.Directive.Id + 1}号 ({Organization.kindLabel r.Directive.Kind}) を達成できませんでした。上司の信頼が下がります" ]
            @ (if plan.AuditNotice then
                   [ $"整合性リスクが {plan.RiskAfter:F0} になり、監査の閾値 ({Organization.auditThreshold:F0}) に達しました。次の季節に監査が入ります" ]
               else
                   [])
            @ (match plan.Audit with
               | Some a when not a.Exposed.IsEmpty ->
                   [ $"監査で、虚偽の報告が露見しました。報告で得た信頼と補助金が取り消され、予算が {a.Fine:N0} 削減されます" ]
               | Some _ -> [ "監査が入りましたが、問題は見つかりませんでした" ]
               | None -> [])

        let result =
            { result with
                Plan = plan
                BalanceEnd = result.BalanceEnd + plan.Reward - plan.Fine
                Warnings = result.Warnings @ planWarnings }

        let balanceEnd = result.BalanceEnd

        let totals = state.CostTotals

        let next =
            { state with
                Turn = state.Turn + 1
                SoilWU = zones |> List.map (fun z -> z.Zone, z.SoilEndWU) |> Map.ofList
                GwWU = actualGwAfter
                Survey = surveyStage
                Wells = wells
                Estimates = estimates
                CostFlags = costFlags
                Overdraft = overdraft
                CostTotals =
                    { Survey = totals.Survey + costs.Survey
                      Build = totals.Build + costs.Build
                      Invest = totals.Invest + costs.Invest
                      Maintenance = totals.Maintenance + costs.Maintenance
                      Pumping = totals.Pumping + costs.Pumping }
                Balance = balanceEnd
                Invested = invested
                Priority = policy.Priority
                ReservoirWU = (match reservoirFlow with Some rf -> rf.EndWU | None -> state.ReservoirWU)
                Organization = Organization.advance state.Organization plan
                Construction = construction
                History = state.History @ [ result ] }

        next, result

    /// 各ターン終了時にログへ出す1行。丸めによる差と内部計算の不整合を区別できるよう、残差は生の値を出す。
    let balanceLogLine (r: TurnResult) =
        let b = r.Balance
        let status = if b.IsWithinTolerance then "OK" else "WARN"

        $"[balance] turn={r.Turn + 1} {Season.label r.Season} precip={b.PrecipInWU:F3} et={b.ActualEtWU:F3} "
        + $"supply={b.ExternalSupplyWU:F3} outflow={b.OutflowWU:F3} dStorage={b.StorageChangeWU:F3} "
        + $"residual={b.ResidualWU:E3} tol={b.ToleranceWU:E3} {status}"
