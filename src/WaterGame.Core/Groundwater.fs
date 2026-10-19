namespace WaterGame.Core

/// 地下水・調査・井戸 (仕様書 6.4, 6.5, 8)
module Groundwater =

    let spec (scenario: Scenario) (zone: ZoneId) =
        scenario.Aquifers |> List.find (fun a -> a.Zone = zone)

    /// シナリオから、全区域の隠しパラメータ (真の値) を決める。乱数は使わない (運の要素は、気象だけ)。
    /// 真の値は、公開されている範囲の中の、シナリオが決めた位置にある。
    let hiddenOf (scenario: Scenario) : Map<ZoneId, HiddenAquifer> =
        let pick (lo, hi) (frac: float) = lo + (hi - lo) * frac

        scenario.Aquifers
        |> List.map (fun a ->
            a.Zone,
            { Zone = a.Zone
              InitialG = pick a.InitialGRangeWU a.TrueInitialGFrac
              Yield = pick a.YieldRangeWU a.TrueYieldFrac
              ReconBias = a.ReconBias
              DrillBias = a.DrillBias })
        |> Map.ofList

    /// 未調査の推定値: 公開されている範囲の中央値。誤差は範囲の半幅
    let priorEstimate (a: AquiferSpec) : Estimate =
        let mid (lo, hi) = (lo + hi) / 2.0
        let rel (lo, hi) = (hi - lo) / (hi + lo)

        { GwWU = mid a.InitialGRangeWU
          YieldWU = mid a.YieldRangeWU
          RelErr = max (rel a.InitialGRangeWU) (rel a.YieldRangeWU) }

    /// 調査結果の相対誤差
    let relErrOf (costs: CostParams) =
        function
        | Reconnaissance -> costs.ReconRelErr
        | TestDrilling -> costs.TestDrillRelErr

    /// 概査の結果から作る定性評価 (有望か乏しいか)
    let rating (yieldEstWU: float) =
        if yieldEstWU >= 800.0 then "有望"
        elif yieldEstWU >= 350.0 then "中程度"
        else "乏しい"

    /// 実行できる主要行動か (調査は概査 → 試掘の順。井戸は概査済み以上の区域に1本)
    let isValidAction (state: GameState) =
        function
        | NoAction -> true
        | Survey(z, Reconnaissance) -> state.Survey[z] = Unsurveyed
        | Survey(z, TestDrilling) -> state.Survey[z] = Reconnoitered
        | BuildWell z -> state.Survey[z] <> Unsurveyed && state.Wells[z] = NoWell
        // 設備投資の可否は財政と合わせて Turn.isValidAction で判定する
        | Invest _ -> true

    /// 区域ごとの揚水可能量: 設定上限、井戸能力、湧出後に残る地下水量の最小値 (井戸がなければ 0)
    let pumpLimit (wellCapacityWU: float) (well: WellState) (limitWU: float) (r: ZoneResult) =
        match well with
        | NoWell -> 0.0
        | WellBuilt -> [ max 0.0 limitWU; wellCapacityWU; max 0.0 (r.GwStartWU - r.DischargeWU) ] |> List.min

    /// 河川水で満たせなかった農業需要を、各区域の揚水可能量に比例して地下水で補う
    let allocatePumping (limits: (ZoneId * float) list) (unmetWU: float) : Map<ZoneId, float> =
        let total = limits |> List.sumBy snd
        let supply = min (max 0.0 unmetWU) total

        limits
        |> List.map (fun (z, l) -> z, (if total <= 0.0 then 0.0 else l * supply / total))
        |> Map.ofList

    /// 揚水費。持続的揚水量(真の値)を超えた分は割増し、コスト上昇フラグ有効後は倍率を掛ける
    let pumpingCost (costs: CostParams) (a: AquiferSpec) (hidden: HiddenAquifer) (flagged: bool) (pumpedWU: float) =
        let excess = max 0.0 (pumpedWU - hidden.Yield)
        let mult = if flagged then a.CostMultiplier else 1.0
        (pumpedWU + excess * (costs.OverdraftFactor - 1.0)) * costs.PumpCostPerWU * mult

    /// 河川から地下水への浸透の強さ (0〜1): 地下水位が公開されている初期貯留量の中央値の 1.5 倍以上なら 0、
    /// 中央値の 0.5 倍以下なら 1。地下水位が下がるほど、川の水が地下水に吸われる
    let seepageStrength (a: AquiferSpec) (gwWU: float) =
        let gref = (fst a.InitialGRangeWU + snd a.InitialGRangeWU) / 2.0
        if gref <= 0.0 then 0.0 else ((1.5 * gref - gwWU) / gref) |> max 0.0 |> min 1.0

    /// 区域を上流から順にたどり、河川から地下水への浸透を確定する。
    /// 浸透は区域に流れ込む河川水に対する割合で、配分管理地点までの区域でだけ起こる。戻り値は取水点の利用可能量。
    /// gwOf は、浸透の強さを決める地下水貯留量 (実際の計算では真の値、見込みでは推定値)。
    let routeRiver (scenario: Scenario) (gwOf: ZoneId -> float) (zones: ZoneResult list) : ZoneResult list * float =
        let allocIndex = scenario.Zones |> List.findIndex (fun z -> z.Id = scenario.AllocationAfter)

        let routed, flow =
            zones
            |> List.indexed
            |> List.fold
                (fun (acc, flowIn) (i, z) ->
                    if i <= allocIndex then
                        let a = spec scenario z.Zone
                        let seep = a.RiverSeepageK * seepageStrength a (gwOf z.Zone) * flowIn
                        let z' = Hydrology.applySeepage seep z
                        z' :: acc, flowIn - seep + z.RiverContributionWU
                    else
                        z :: acc, flowIn)
                ([], 0.0)

        List.rev routed, flow

    /// 推定値の更新。涵養と揚水は公開情報なので、推定側も同じ式で進める (湧出は推定貯留量から計算する)
    let advanceEstimate (a: AquiferSpec) (rechargeWU: float) (pumpedWU: float) (e: Estimate) : Estimate =
        let b = e.GwWU * a.DischargeK
        { e with GwWU = max 0.0 (e.GwWU + rechargeWU - pumpedWU - b) }

    /// 調査の実行結果で推定値を更新する。誤差の向きは隠しパラメータで決まり、幅は調査段階で狭まる
    let applySurvey
        (costs: CostParams)
        (hidden: HiddenAquifer)
        (actualGwWU: float)
        (kind: SurveyKind)
        (prev: Estimate)
        : Estimate * SurveyReport =
        let relErr = relErrOf costs kind

        let bias =
            match kind with
            | Reconnaissance -> hidden.ReconBias
            | TestDrilling -> hidden.DrillBias

        let surveyed =
            { GwWU = actualGwWU * (1.0 + bias * relErr)
              YieldWU = hidden.Yield * (1.0 + bias * relErr)
              RelErr = relErr }

        // 観測井戸のデータのほうがすでに精度が高ければ、調査の結果では更新しない
        let est = if prev.RelErr <= relErr then prev else surveyed

        est,
        { Zone = hidden.Zone
          Kind = kind
          GwEstWU = est.GwWU
          YieldEstWU = est.YieldWU
          PrevRelErr = prev.RelErr
          RelErr = est.RelErr }

    /// 観測井戸による推定の更新 (A2)。区域に観測井戸があれば、季節ごとに推定の誤差が縮み、貯留量の推定が実際に近づく。
    /// 誤差の向きは試掘と同じ隠しの偏り (観測器のくせ) で決まる。持続的揚水量も同じ割合で真の値に近づく。
    let applyObservation (invest: InvestParams) (hidden: HiddenAquifer) (actualGwWU: float) (e: Estimate) : Estimate =
        // 誤差は下限まで縮む (すでに下限より小さければ変えない)。貯留量の推定は、下限に達したあとも毎季節、観測で更新する
        let relErr = min e.RelErr (max invest.ObservationFloor (e.RelErr * (1.0 - invest.ObservationShrink)))
        let keep = if relErr < e.RelErr then 1.0 - invest.ObservationShrink else 1.0

        { GwWU = actualGwWU * (1.0 + hidden.DrillBias * relErr)
          YieldWU = hidden.Yield + (e.YieldWU - hidden.Yield) * keep
          RelErr = relErr }
