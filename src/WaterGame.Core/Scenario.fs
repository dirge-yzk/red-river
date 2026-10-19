namespace WaterGame.Core

/// シナリオデータ。
/// 数値は仕様書に記載がないため、遊べるバランスを狙った【仮の値】。調整はこのファイルだけで完結させる。
module Scenarios =

    let private anyTurn = (0, 99)

    /// 平年の降水の倍率 (プレイテストで水が余りすぎたため、元の設定から 1 割減らした)
    let private precipK = 0.9

    let private cardT name season lo hi et weight (fromTurn, toTurn) =
        { Name = name
          Season = season
          PrecipMm = (lo * precipK, hi * precipK)
          EtLevel = et
          Weight = weight
          FromTurn = fromTurn
          ToTurn = toTurn }

    let private perSeason sp su au wi =
        Map.ofList [ Spring, sp; Summer, su; Autumn, au; Winter, wi ]

    let private normalCards =
        [ cardT "少雨の春" Spring 80.0 140.0 Mid 0.25 anyTurn
          cardT "平年の春" Spring 150.0 230.0 Mid 0.50 anyTurn
          cardT "多雨の春" Spring 240.0 340.0 Low 0.25 anyTurn
          cardT "渇水の夏" Summer 120.0 200.0 High 0.25 anyTurn
          cardT "平年の夏" Summer 220.0 320.0 High 0.50 anyTurn
          cardT "豪雨の夏" Summer 340.0 480.0 Mid 0.25 anyTurn
          cardT "少雨の秋" Autumn 70.0 130.0 Mid 0.25 anyTurn
          cardT "平年の秋" Autumn 140.0 220.0 Mid 0.50 anyTurn
          cardT "多雨の秋" Autumn 230.0 330.0 Low 0.25 anyTurn
          cardT "乾燥した冬" Winter 40.0 80.0 Low 0.25 anyTurn
          cardT "平年の冬" Winter 90.0 140.0 Low 0.50 anyTurn
          cardT "多雨の冬" Winter 150.0 220.0 Low 0.25 anyTurn ]

    /// できごとのカード。すべて季節のはじめに予告される。起こるターンは、シナリオの EventSchedule で固定する
    let private tightBudget =
        { Name = "財政の緊縮"
          Description = "今年の管理交付金が 40% 減ります"
          Kind = GrantMultiplier 0.6 }

    let private extraSubsidy =
        { Name = "臨時の補助金"
          Description = "国から臨時の補助金 600 が入ります"
          Kind = ExtraFunds 600.0 }

    let private heatWave =
        { Name = "猛暑"
          Description = "今季の農業用水の需要が 25% 増えます"
          Kind = AgDemandMultiplier 1.25 }

    let private newFactory =
        { Name = "工場の新設"
          Description = "今季の生活・産業用水の需要が 30% 増えます"
          Kind = UrbanDemandMultiplier 1.3 }

    let private blackout =
        { Name = "停電・ポンプの点検"
          Description = "今季は、すべての井戸の揚水能力が半分になります"
          Kind = PumpCapacityMultiplier 0.5 }

    let private powerPrice =
        { Name = "電力料金の高騰"
          Description = "今季の揚水費が 2 倍になります"
          Kind = PumpCostMultiplier 2.0 }

    /// 3年間・全12ターン。河川は 森林 -> 農地 -> 市街地 -> 湿地 の順に流れ、配分管理地点は市街地の下流端
    /// 計画指令の設定。目標の充足率の範囲だけがシナリオごとに違う (水が乏しいシナリオほど低い)
    let private directiveConfig (targetMin: float) (targetMax: float) : DirectiveConfig =
        { TargetMin = targetMin
          TargetMax = targetMax
          RewardFraction = 0.4
          TrustAchieved = 8.0
          TrustFailed = 10.0
          InitialTrust = 50.0 }

    let normalYear =
        { Name = "平年"
          Level = "標準"
          Summary = "3年間・12ターン。降水は平年並みで、全指標のバランスを学ぶ"
          Focus = "全指標のバランス。地下水の情報は未知"
          Hints = Map.empty
          UrbanDemandGrowthPerYear = 0.0
          UrbanRunoffGrowthPerYear = 0.0
          Turns = 12
          Zones =
            [ { Id = Forest
                Name = ZoneId.label Forest
                AreaKm2 = 60.0
                RunoffCoef = 0.15
                SoilCapacityMm = 300.0
                EtFactor = 0.9
                InitialSoilMm = 150.0
                RechargeFrac = 0.04 }
              { Id = Farm
                Name = ZoneId.label Farm
                AreaKm2 = 40.0
                RunoffCoef = 0.25
                SoilCapacityMm = 200.0
                EtFactor = 0.8
                InitialSoilMm = 100.0
                RechargeFrac = 0.19 }
              // 舗装が多く、降水の半分以上が地表流出になる。土壌は浅く蒸発散も小さい
              { Id = Urban
                Name = ZoneId.label Urban
                AreaKm2 = 15.0
                RunoffCoef = 0.55
                SoilCapacityMm = 100.0
                EtFactor = 0.6
                InitialSoilMm = 50.0
                RechargeFrac = 0.20 }
              { Id = Wetland
                Name = ZoneId.label Wetland
                AreaKm2 = 20.0
                RunoffCoef = 0.10
                SoilCapacityMm = 250.0
                EtFactor = 1.1
                InitialSoilMm = 150.0
                RechargeFrac = 0.11 } ]
          AllocationAfter = Urban
          Cards = normalCards
          AgDemandWU = perSeason 3500.0 5600.0 2100.0 0.0
          UrbanDemandWU = perSeason 1800.0 2640.0 1800.0 1560.0
          MinEnvFlowWU = perSeason 2400.0 2160.0 1800.0 1800.0
          BaseEtMm = perSeason 140.0 280.0 120.0 40.0
          // 森林は風化岩で小さく、農地の沖積層が最も大きい。市街地は小さめ、湿地は河川に近く中程度
          Aquifers =
            [ { Zone = Forest
                InitialGRangeWU = (4800.0, 11200.0)
                YieldRangeWU = (200.0, 500.0)
                DischargeK = 0.05
                WellCapacityWU = 600.0
                ThresholdWU = 2500.0
                CostMultiplier = 2.0
                RiverSeepageK = 0.0
                // 隠しの真の値 (固定)
                TrueInitialGFrac = 0.5
                TrueYieldFrac = 0.3
                ReconBias = -0.4
                DrillBias = 0.1 }
              { Zone = Farm
                InitialGRangeWU = (7200.0, 19200.0)
                YieldRangeWU = (700.0, 1500.0)
                DischargeK = 0.05
                WellCapacityWU = 1800.0
                ThresholdWU = 4000.0
                CostMultiplier = 2.0
                // 農地の沖積層は川の水を吸いやすい
                RiverSeepageK = 0.16
                // 隠しの真の値 (固定)
                TrueInitialGFrac = 0.6
                TrueYieldFrac = 0.55
                ReconBias = 0.3
                DrillBias = -0.2 }
              { Zone = Wetland
                InitialGRangeWU = (2400.0, 6400.0)
                YieldRangeWU = (300.0, 700.0)
                DischargeK = 0.05
                WellCapacityWU = 900.0
                ThresholdWU = 1200.0
                CostMultiplier = 2.0
                // 配分管理地点より下流なので、河川からの浸透は扱わない
                RiverSeepageK = 0.0
                // 隠しの真の値 (固定)
                TrueInitialGFrac = 0.45
                TrueYieldFrac = 0.25
                ReconBias = -0.2
                DrillBias = 0.2 }
              { Zone = Urban
                InitialGRangeWU = (3200.0, 8000.0)
                YieldRangeWU = (250.0, 600.0)
                DischargeK = 0.05
                WellCapacityWU = 800.0
                ThresholdWU = 1600.0
                CostMultiplier = 2.0
                // 舗装と護岸で、浸透は小さい
                RiverSeepageK = 0.06
                // 隠しの真の値 (固定)
                TrueInitialGFrac = 0.4
                TrueYieldFrac = 0.5
                ReconBias = 0.5
                DrillBias = -0.3 } ]
          Costs =
            { ReconCost = 200.0
              TestDrillCost = 800.0
              WellCost = 1000.0
              PumpCostPerWU = 0.20
              OverdraftFactor = 1.5
              ReconRelErr = 0.25
              TestDrillRelErr = 0.08
              OverdraftLimit = 0.75 }
          Finance =
            { InitialBalance = 800.0
              AnnualGrant = 700.0
              AgRevenuePerWU = 0.08
              UrbanRevenuePerWU = 0.10
              WellMaintenance = 50.0 }
          Invest =
            { IrrigationCost = 1100.0
              IrrigationDemandReduction = 0.12
              IrrigationUpgradeCost = 1500.0
              IrrigationUpgradeReduction = 0.22
              ObservationCost = 500.0
              ObservationShrink = 0.4
              ObservationFloor = 0.05
              ObservationPumpSaving = 0.6
              RainwaterCost = 800.0
              RainwaterDemandReduction = 0.12
              EffluentCost = 600.0
              EffluentReturnRate = 0.15
              EffluentUpgradeCost = 700.0
              EffluentUpgradeReturnRate = 0.30
              WaterSavingCost = 500.0
              WaterSavingDemandReduction = 0.08 }
          Reservoir =
            Some
                { Cost = 800.0
                  CapacityWU = 3000.0
                  ExpansionCost = 700.0
                  ExpandedCapacityWU = 6000.0
                  EvapRate = Map.ofList [ Spring, 0.012; Summer, 0.036; Autumn, 0.018; Winter, 0.006 ] }
          EventSchedule = []
          // 計画経済版: 入門を除くすべてのシナリオに、中央からの指令がある
          Directives = Some(directiveConfig 0.90 1.0)
          // 資材と建設 (v2.0-c): 正規配給は、日程どおり。機械設備は 4 季ごとに 1 単位、建設資材は 2 季ごとに 1 単位、
          // 専門技術者は 5 季に 1 人 (長期の試練は、13 季にもう 1 人) 増える
          Construction =
            Some
                { InitialStock = Map.ofList [ Machinery, 1; BuildMaterial, 2; Engineer, 1 ]
                  Arrivals =
                    [ for t in [ 3; 7; 11; 15; 19 ] -> t, Machinery, 1 ]
                    @ [ for t in [ 2; 4; 6; 8; 10; 12; 14; 16; 18 ] -> t, BuildMaterial, 1 ]
                    @ [ 5, Engineer, 1; 13, Engineer, 1 ]
                  TransferFee = 150.0
                  TransferRepayAfter = 4
                  MaxLoans = 2 }
          Eval =
            { AgWeight = 1.0
              UrbanWeight = 1.0
              EnvWeight = 1.0
              GwWeight = 1.0
              FinWeight = 1.0
              TargetBalance = 4500.0
              // 何もしなくても自然な湧出で貯留量は減る。その分を許容する基準 (これ以上なら満点)
              GwReferenceRatio = 0.70 } }

    /// 渇水: 前半(最初の6ターン)に少雨が続き、農業需要が大きい。農業用水と環境流水の対立を学ぶ
    let drought =
        let dryHalf = (0, 5)
        let recovery = (6, 99)

        let dryCards =
            [ cardT "少雨の春" Spring 60.0 110.0 Mid 0.55 dryHalf
              cardT "平年の春" Spring 150.0 230.0 Mid 0.40 dryHalf
              cardT "多雨の春" Spring 240.0 340.0 Low 0.05 dryHalf
              cardT "渇水の夏" Summer 100.0 170.0 High 0.55 dryHalf
              cardT "平年の夏" Summer 220.0 320.0 High 0.40 dryHalf
              cardT "豪雨の夏" Summer 340.0 480.0 Mid 0.05 dryHalf
              cardT "少雨の秋" Autumn 50.0 110.0 Mid 0.55 dryHalf
              cardT "平年の秋" Autumn 140.0 220.0 Mid 0.40 dryHalf
              cardT "多雨の秋" Autumn 230.0 330.0 Low 0.05 dryHalf
              cardT "乾燥した冬" Winter 30.0 70.0 Low 0.55 dryHalf
              cardT "平年の冬" Winter 90.0 140.0 Low 0.40 dryHalf
              cardT "多雨の冬" Winter 150.0 220.0 Low 0.05 dryHalf ]

        let recoveryCards = normalCards |> List.map (fun c -> { c with FromTurn = fst recovery; ToTurn = snd recovery })
        let scale k (m: Map<Season, float>) = m |> Map.map (fun _ v -> v * k)

        { normalYear with
            Name = "渇水"
            Level = "特殊"
            // 前半の少雨の2年目に、交付金の緊縮と猛暑が続く。終わり近くに臨時の補助金
            EventSchedule = [ 4, tightBudget; 5, heatWave; 10, extraSubsidy ]
            Directives = Some(directiveConfig 0.85 1.0)
            Summary = "3年間・12ターン。前半に少雨が続き、農業需要が大きい。農業と環境流水が対立する"
            Focus = "農業用水と環境流水の対立。前半に少雨が続き、農業需要が大きい"
            Zones = normalYear.Zones |> List.map (fun z -> { z with InitialSoilMm = z.InitialSoilMm * 0.7 })
            Cards = dryCards @ recoveryCards
            AgDemandWU = normalYear.AgDemandWU
            Aquifers =
                normalYear.Aquifers
                |> List.map (fun a ->
                    { a with InitialGRangeWU = (fst a.InitialGRangeWU * 0.85, snd a.InitialGRangeWU * 0.85) })
            Eval =
                { normalYear.Eval with
                    AgWeight = 2.0
                    EnvWeight = 2.0 } }

    let private scaleMap k (m: Map<Season, float>) = m |> Map.map (fun _ v -> v * k)

    let private scaleRange k (lo, hi) = (lo * k, hi * k)

    /// チュートリアル: 1年(4ターン)。気象カードは固定 (春は平年、夏は渇水、秋は少雨、冬は平年) で、
    /// 初期設定のままでは夏に不足が出る。優先順位・井戸・揚水上限が結果を変えることを、ヒントに沿って体験する
    let tutorial =
        let fixedCards =
            normalCards
            |> List.filter (fun c -> c.Name = "平年の春" || c.Name = "渇水の夏" || c.Name = "少雨の秋" || c.Name = "平年の冬")

        { normalYear with
            Name = "チュートリアル"
            Level = "入門"
            Summary = "1年間・4ターン。ヒントに従って、方針・調査・井戸・優先順位・結果の読み方を一通り体験する"
            Focus = "操作と1ターンの流れを覚える。夏の渇水は優先順位で、秋からは井戸で補う。評価は参考"
            Hints =
                Map.ofList
                    [ 0,
                      "【チュートリアル 1/4 春】ヘッダーの気象カードと、運転ページの降水予報・流量メーターで見通しを確認します。取水点・下流の流量メーターは、予報から計算した川の水の目安で、下流の赤い針が環境流水の最低基準です。\n★ 優先順位は、1年に1回、春だけ切り替えられます。この年は夏に渇水が来ます。水が足りないとき、順位の低い用途から削られるので、優先順位切替器を回して、いま最下位の生活・産業用水を上げておきましょう。\nさらに設備ページ (いま選ばれているのは中流農地。ほかの区域は地図をクリックして選びます) で「概査」(費用 200) を押してから、右下のナイフスイッチ「運転開始」を入れます。"
                      1,
                      "【2/4 夏】予報どおり渇水の夏です。農地の地下水の計器は、概査で推定の幅 (青いかっこ) が狭まりました。持続量メーターの針が高いほど、井戸を作る価値が大きい、という意味です。設備ページで「井戸を建設」(費用 1,000) を押して運転を進めてください。★ 井戸は完成した「次の季節」(秋) から使えます。調査・建設・投資は1ターンに1つまでです。"
                      2,
                      "【3/4 秋】秋も雨が少ない予報です。井戸が使えるので、運転ページの揚水ポンプのバルブを開けてみましょう。河川水で足りない農業・生活産業の用水を地下水が補います (閾値を割りそうなときは、ポンプのランプが点きます)。環境流水を守りたいときは、農業・生活産業のバルブを少し絞り、不足分を井戸に任せます。"
                      3,
                      "【4/4 冬】最後の季節です。ここまでの設定のまま進めて構いません。運転日報の水の流れと、最終報告書の学習目標の振り返りを読みましょう。同じシードで再挑戦すると、優先順位や井戸の使い方を変えて比べられます。" ]
            Turns = 4
            // 入門では貯水池・できごとは扱わない
            Reservoir = None
            EventSchedule = []
            // 入門には、指令を載せない。工事は、発注した季節に完成する
            Directives = None
            Construction = None
            Cards = fixedCards
            Zones = normalYear.Zones |> List.map (fun z -> { z with InitialSoilMm = z.InitialSoilMm * 1.0 })
            AgDemandWU = perSeason 2800.0 4400.0 2400.0 0.0
            UrbanDemandWU = perSeason 1500.0 2200.0 1500.0 1300.0
            MinEnvFlowWU = perSeason 2000.0 1800.0 1500.0 1500.0
            Aquifers =
                normalYear.Aquifers
                |> List.map (fun a ->
                    { a with InitialGRangeWU = scaleRange 1.25 a.InitialGRangeWU })
            // 入門は、操作を一通り試せるよう、収入と交付金を厚めにする (費用は標準と同じ)
            Costs = { normalYear.Costs with PumpCostPerWU = 0.12 }
            Finance =
                { normalYear.Finance with
                    InitialBalance = 1500.0
                    AnnualGrant = 1000.0
                    AgRevenuePerWU = 0.16
                    UrbanRevenuePerWU = 0.20
                    WellMaintenance = 40.0 }
            Eval =
                { normalYear.Eval with
                    TargetBalance = 500.0
                    GwReferenceRatio = 0.85 } }

    /// 市街地拡大: 都市の需要と舗装が年々増える。生活・産業用水と農業・環境流水の優先順位を、年ごとにどう見直すかを学ぶ
    let urbanGrowth =
        { normalYear with
            Name = "市街地拡大"
            Level = "特殊"
            Summary = "3年間・12ターン。都市の需要が年に20%増え、舗装で地表流出も増える。優先順位を見直す"
            Focus = "都市の成長。生活・産業用水の需要と舗装が年々増え、農業・環境流水との優先順位が問われる"
            UrbanDemandGrowthPerYear = 0.20
            UrbanRunoffGrowthPerYear = 0.05
            AgDemandWU = scaleMap 0.9 normalYear.AgDemandWU
            Eval =
                { normalYear.Eval with
                    UrbanWeight = 2.0
                    EnvWeight = 1.5 } }

    /// 乾燥地の地下水: 降水が少なく、河川は乏しい。地下水の貯留量は大きいが、範囲が広く不確かで、調査が判断を左右する
    let aridGroundwater =
        { normalYear with
            Name = "乾燥地"
            Directives = Some(directiveConfig 0.75 0.95)
            Level = "特殊"
            Summary = "3年間・12ターン。降水が少なく河川は乏しい。地下水が頼りだが、量は不確か。調査が鍵になる"
            Focus = "地下水が主な水源。情報の価値(調査)と、使いすぎの不可逆性が問われる"
            Cards =
                normalCards
                |> List.map (fun c ->
                    { c with
                        PrecipMm = scaleRange 0.385 c.PrecipMm
                        EtLevel =
                            match c.EtLevel with
                            | Low -> Mid
                            | _ -> High })
            Zones = normalYear.Zones |> List.map (fun z -> { z with InitialSoilMm = z.InitialSoilMm * 0.6 })
            AgDemandWU = scaleMap 1.1 normalYear.AgDemandWU
            MinEnvFlowWU = scaleMap 0.7 normalYear.MinEnvFlowWU
            Aquifers =
                normalYear.Aquifers
                |> List.map (fun a ->
                    { a with
                        // 貯留量も持続的揚水量も、範囲が広い (調査で幅が狭まる価値が大きい)
                        InitialGRangeWU = (fst a.InitialGRangeWU * 1.12, snd a.InitialGRangeWU * 2.8)
                        YieldRangeWU = (fst a.YieldRangeWU * 0.8, snd a.YieldRangeWU * 1.6)
                        // 河川が乏しいので、井戸の能力は大きい
                        WellCapacityWU = a.WellCapacityWU * 1.3 })
            Eval =
                { normalYear.Eval with
                    GwWeight = 2.0
                    TargetBalance = 4000.0
                    GwReferenceRatio = 0.5 } }

    /// 長期の試練: 5年(20ターン)。1年目は平年、その後に長い乾燥期と大渇水が続く。費用も厳しい
    let longTrial =
        let normalFirst = (0, 3)
        let dryPeriod = (4, 11)
        let severe = (12, 15)
        let recovery = (16, 99)
        let within (a, b) c = { c with FromTurn = a; ToTurn = b }

        let dryCardsOf weights range k =
            // 乾燥期のカード: 少雨・平年・多雨の重みを weights で与え、降水を k 倍する
            [ for (name, season, lo, hi, et) in
                  [ "少雨の春", Spring, 60.0, 110.0, Mid
                    "平年の春", Spring, 150.0, 230.0, Mid
                    "多雨の春", Spring, 240.0, 340.0, Low
                    "渇水の夏", Summer, 100.0, 170.0, High
                    "平年の夏", Summer, 220.0, 320.0, High
                    "豪雨の夏", Summer, 340.0, 480.0, Mid
                    "少雨の秋", Autumn, 50.0, 110.0, Mid
                    "平年の秋", Autumn, 140.0, 220.0, Mid
                    "多雨の秋", Autumn, 230.0, 330.0, Low
                    "乾燥した冬", Winter, 30.0, 70.0, Low
                    "平年の冬", Winter, 90.0, 140.0, Low
                    "多雨の冬", Winter, 150.0, 220.0, Low ] ->
                  let w =
                      if name.StartsWith "少雨" || name.StartsWith "渇水" || name.StartsWith "乾燥" then
                          fst weights
                      elif name.StartsWith "平年" then
                          snd weights
                      else
                          1.0 - fst weights - snd weights

                  cardT name season (lo * k) (hi * k) et w range ]

        let costs = normalYear.Costs
        let costK = 1.2

        { normalYear with
            Name = "長期の試練"
            Level = "高難度"
            // 5年の間に、交付金の緊縮・猛暑・停電・電力料金の高騰・工場の新設・臨時の補助金が、決まった時期に来る
            EventSchedule = [ 3, newFactory; 4, tightBudget; 6, powerPrice; 9, heatWave; 12, tightBudget; 13, blackout; 18, extraSubsidy ]
            Directives = Some(directiveConfig 0.75 0.95)
            Summary = "5年間・20ターン。長い乾燥期と大渇水が続き、費用も厳しい。計画性が問われる"
            Focus = "長期の計画。資金・地下水・配分のすべてで、少ない選択肢を使い切らずに最後まで保つ"
            UrbanDemandGrowthPerYear = 0.06
            UrbanRunoffGrowthPerYear = 0.02
            Turns = 20
            Cards =
                (normalCards |> List.map (within normalFirst))
                @ dryCardsOf (0.55, 0.40) dryPeriod 1.0
                @ dryCardsOf (0.70, 0.30) severe 0.8
                @ (normalCards |> List.map (within recovery))
            AgDemandWU = scaleMap 1.10 normalYear.AgDemandWU
            MinEnvFlowWU = scaleMap 1.05 normalYear.MinEnvFlowWU
            Zones = normalYear.Zones |> List.map (fun z -> { z with InitialSoilMm = z.InitialSoilMm * 0.8 })
            Aquifers =
                normalYear.Aquifers
                |> List.map (fun a ->
                    { a with
                        InitialGRangeWU = scaleRange 0.85 a.InitialGRangeWU
                        YieldRangeWU = scaleRange 0.9 a.YieldRangeWU })
            Costs =
                { costs with
                    ReconCost = costs.ReconCost * costK
                    TestDrillCost = costs.TestDrillCost * costK
                    WellCost = costs.WellCost * costK }
            Finance =
                { normalYear.Finance with
                    InitialBalance = 600.0
                    AnnualGrant = 560.0
                    WellMaintenance = 75.0 }
            Invest =
                { normalYear.Invest with
                    IrrigationCost = normalYear.Invest.IrrigationCost * costK
                    IrrigationUpgradeCost = normalYear.Invest.IrrigationUpgradeCost * costK
                    ObservationCost = normalYear.Invest.ObservationCost * costK
                    RainwaterCost = normalYear.Invest.RainwaterCost * costK
                    EffluentCost = normalYear.Invest.EffluentCost * costK
                    EffluentUpgradeCost = normalYear.Invest.EffluentUpgradeCost * costK
                    WaterSavingCost = normalYear.Invest.WaterSavingCost * costK }
            Reservoir = normalYear.Reservoir |> Option.map (fun r -> { r with Cost = r.Cost * costK; ExpansionCost = r.ExpansionCost * costK })
            Eval =
                { normalYear.Eval with
                    AgWeight = 1.5
                    UrbanWeight = 1.5
                    EnvWeight = 1.5
                    TargetBalance = 7000.0
                    // 5年では、何もしなくても自然な湧出で貯留量が大きく減る。その分を許容する
                    GwReferenceRatio = 0.45 } }

    /// 一覧の順序: 入門 → 標準 → 特殊 → 高難度
    let all = [ tutorial; normalYear; drought; urbanGrowth; aridGroundwater; longTrial ]
