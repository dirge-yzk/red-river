namespace WaterGame.Core

/// 今季の見込み。プレイヤーが知っている情報(予報の降水範囲・蒸発散の段階・見えている土壌水分・
/// 推定の地下水・投資の状況)だけから、取水点の利用可能量や下流流量を計算して目安として見せる。
/// 真の地下水貯留量や、確定済みの気象実績は使わない (隠しの情報を漏らさない)。
module Forecast =

    /// 予報の降水量の下限・中央・上限それぞれの場合の見込み
    type Case =
        { PrecipMm: float
          Zones: ZoneResult list
          /// 取水点の上流を流れてくる河川水の量 (配分管理地点までの区域の、地表流出 + 地下水湧出 - 河川からの浸透)。貯水池の操作の前
          AvailableWU: float
          /// 配分地点より下流の区域から加わる量
          BelowWU: float
          /// 貯水池の今の貯留量・容量・今季の蒸発損失率 (貯水池がなければ 0)
          ReservoirWU: float
          ReservoirCapacityWU: float
          ReservoirEvapRate: float
          /// 再生水として戻る割合 (再生水がなければ 0)
          ReturnRate: float }

    type Outlook =
        { Low: Case
          Mid: Case
          High: Case }

    /// 気象カードの蒸発散の段階から、基準蒸発散需要 (mm) の見込みを求める (段階の係数帯の中央値)
    let etMm (scenario: Scenario) (card: WeatherCard) =
        let lo, hi = Weather.etBand card.EtLevel
        scenario.BaseEtMm[card.Season] * (lo + hi) / 2.0

    let private caseFor (scenario: Scenario) (state: GameState) (card: WeatherCard) (precipMm: float) : Case =
        let et = etMm scenario card

        // 地下水は、プレイヤーが知っている推定値で計算する
        let zones0 =
            scenario.Zones
            |> List.map (fun z ->
                let a = Groundwater.spec scenario z.Id

                Hydrology.zoneStep
                    (Turn.effectiveZone scenario state z)
                    state.SoilWU[z.Id]
                    state.Estimates[z.Id].GwWU
                    a.DischargeK
                    precipMm
                    et)

        let zones, inflow = Groundwater.routeRiver scenario (fun z -> state.Estimates[z].GwWU) zones0
        let allocIndex = scenario.Zones |> List.findIndex (fun z -> z.Id = scenario.AllocationAfter)
        let _, below = zones |> List.splitAt (allocIndex + 1)
        let cap = Turn.reservoirCapacity scenario state

        { PrecipMm = precipMm
          Zones = zones
          AvailableWU = inflow
          BelowWU = below |> List.sumBy _.RiverContributionWU
          ReservoirWU = (if cap > 0.0 then state.ReservoirWU else 0.0)
          ReservoirCapacityWU = cap
          ReservoirEvapRate = (if cap > 0.0 then Turn.reservoirEvapRate scenario card.Season else 0.0)
          ReturnRate = Turn.effluentReturnRate scenario state }

    let outlook (scenario: Scenario) (state: GameState) (card: WeatherCard) : Outlook =
        let lo, hi = card.PrecipMm

        { Low = caseFor scenario state card lo
          Mid = caseFor scenario state card ((lo + hi) / 2.0)
          High = caseFor scenario state card hi }

    /// 貯水池の操作 (正: 貯める / 負: 放流) をしたあとの、今季の貯水池の動きの見込み。貯水池がなければ None
    let reservoir (policy: Policy) (c: Case) : ReservoirResult option =
        if c.ReservoirCapacityWU <= 0.0 then
            None
        else
            Some(Hydrology.reservoirStep c.ReservoirCapacityWU c.ReservoirEvapRate c.ReservoirWU c.AvailableWU policy.ReservoirOpWU)

    /// 貯水池の操作を反映した、取水点の利用可能量の見込み
    let availableAfterReservoir (policy: Policy) (c: Case) =
        match reservoir policy c with
        | Some rf -> c.AvailableWU - rf.StoredWU + rf.ReleasedWU
        | None -> c.AvailableWU

    /// ある方針の目標と優先順位で河川水を配分した場合の見込み (貯水池の操作・再生水を含む)
    let allocation (policy: Policy) (c: Case) : AllocationResult =
        let a =
            Hydrology.allocate
                policy.Priority
                (availableAfterReservoir policy c)
                c.BelowWU
                policy.AgTargetWU
                policy.UrbanTargetWU
                policy.EnvTargetWU

        let ret = a.UrbanSupplyWU * c.ReturnRate
        { a with ReturnFlowWU = ret; DownstreamFlowWU = a.DownstreamFlowWU + ret }

    /// ある区域で、設定した揚水上限いっぱいまで汲んだ場合の、今季末の貯留量の見込み (推定値ベース)。
    /// 井戸がなければ揚水は 0。涵養と湧出は、その場合(c)の見込みを使う。
    let gwEndIfPumpedToLimit (scenario: Scenario) (state: GameState) (c: Case) (zone: ZoneId) (limitWU: float) : float =
        let z = c.Zones |> List.find (fun x -> x.Zone = zone)
        let pumped = Groundwater.pumpLimit (Turn.wellCapacity scenario state zone) state.Wells[zone] limitWU z
        max 0.0 (z.GwStartWU + z.DeepDrainageWU + z.SeepageWU - pumped - z.DischargeWU)

    /// 1年あたりの平年の降水量 (mm)。そのシナリオの気象カードを、重みで平均して季節ごとに合計する
    let annualMeanPrecipMm (scenario: Scenario) : float =
        [ Spring; Summer; Autumn; Winter ]
        |> List.sumBy (fun season ->
            let cards = scenario.Cards |> List.filter (fun c -> c.Season = season)
            let w = cards |> List.sumBy _.Weight
            if w <= 0.0 then 0.0 else (cards |> List.sumBy (fun c -> c.Weight * (fst c.PrecipMm + snd c.PrecipMm) / 2.0)) / w)

