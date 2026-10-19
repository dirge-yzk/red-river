namespace WaterGame.Core

module Hydrology =

    /// 1区域・1季節の水循環 (仕様書 6.1〜6.4)。
    /// 降水 = 地表流出 + 浸透 / 期末土壌水分 = 期首 + 浸透 - 実蒸発散 - 深部排水。
    /// 深部排水は当季の地下水涵養 R となり、期末更新でだけ地下水に加わる。
    /// 河川への湧出 B は季節開始時の地下水貯留量 G(t) だけから計算する (涵養が次季以降に効く「1ターン遅れ」)。
    /// 揚水量はこの時点では 0 で、配分後に applyPumping で確定する。
    let zoneStep
        (z: ZoneParams)
        (soilStartWU: float)
        (gwStartWU: float)
        (dischargeK: float)
        (precipMm: float)
        (baseEtMm: float)
        : ZoneResult =
        let precip = precipMm * z.AreaKm2
        let rawRunoff = precip * z.RunoffCoef
        let runoff = rawRunoff
        let infiltration = precip - rawRunoff
        // 浸透した水の一部は、土壌に留まらず直接、地下水に入る
        let direct = infiltration * z.RechargeFrac
        let toSoil = infiltration - direct
        let etDemand = baseEtMm * z.EtFactor * z.AreaKm2
        let available = soilStartWU + toSoil
        let actualEt = min etDemand available
        let afterEt = available - actualEt
        let capacity = z.SoilCapacityMm * z.AreaKm2
        let drainage = max 0.0 (afterEt - capacity)
        let soilEnd = afterEt - drainage
        // 地下水への涵養 R = 土壌からの深部排水 + 直接涵養
        let recharge = drainage + direct
        let discharge = gwStartWU * min 1.0 (max 0.0 dischargeK)
        let toRiver = runoff + discharge

        { Zone = z.Id
          PrecipWU = precip
          RunoffWU = runoff
          InfiltrationWU = infiltration
          EtDemandWU = etDemand
          ActualEtWU = actualEt
          DeepDrainageWU = recharge
          SoilStartWU = soilStartWU
          SoilEndWU = soilEnd
          SoilCapacityWU = capacity
          GwStartWU = gwStartWU
          DischargeWU = discharge
          PumpedWU = 0.0
          GwEndWU = gwStartWU + recharge - discharge
          RiverContributionWU = toRiver
          SeepageWU = 0.0
          ResidualWU = 0.0 }
        |> fun r -> { r with ResidualWU = precip - actualEt - toRiver - (soilEnd - soilStartWU) - (r.GwEndWU - gwStartWU) }

    /// 河川から地下水への浸透 (A6)。区域に流れ込む河川水から失われた分が、期末の地下水貯留量に加わる。
    /// 区域単独の収支は、降水 + 浸透(河川から) = 蒸発散 + 河川への流出 + 揚水 + 貯留の増減 になる
    let applySeepage (seepWU: float) (r: ZoneResult) : ZoneResult =
        { r with
            SeepageWU = seepWU
            GwEndWU = r.GwEndWU + seepWU }

    /// 揚水量を確定し、地下水貯留量 G(t+1) = G(t) + R + 河川からの浸透 - P - B と区域の収支残差を更新する。
    let applyPumping (pumpedWU: float) (r: ZoneResult) : ZoneResult =
        // 貯留量を使い切るまで汲んでも、浮動小数の誤差で負にならないようにする
        let gwEnd = max 0.0 (r.GwStartWU + r.DeepDrainageWU + r.SeepageWU - pumpedWU - r.DischargeWU)

        { r with
            PumpedWU = pumpedWU
            GwEndWU = gwEnd
            ResidualWU =
                r.PrecipWU + r.SeepageWU - r.ActualEtWU - r.RiverContributionWU - pumpedWU
                - (r.SoilEndWU - r.SoilStartWU)
                - (gwEnd - r.GwStartWU) }

    /// 貯水池の1季節の動き (A1)。季節開始時の貯留量から蒸発で失い、そのあと操作量 op (正: 貯める / 負: 放流) を実行する。
    /// 貯めるのは、取水点の上流を流れてくる河川水 (inflow) の範囲で、容量を超えない。放流は貯留量を超えない。
    let reservoirStep (capacityWU: float) (evapRate: float) (startWU: float) (inflowWU: float) (opWU: float) : ReservoirResult =
        let evap = startWU * min 1.0 (max 0.0 evapRate)
        let afterEvap = startWU - evap

        let stored, released =
            if opWU >= 0.0 then
                min opWU (min (max 0.0 inflowWU) (max 0.0 (capacityWU - afterEvap))), 0.0
            else
                0.0, min (-opWU) afterEvap

        { StartWU = startWU
          EvaporationWU = evap
          StoredWU = stored
          ReleasedWU = released
          EndWU = afterEvap + stored - released
          CapacityWU = capacityWU }

    /// 河川水の配分 (仕様書 7.3)。優先順位の高い項目から順に処理する。
    /// - 農業・生活産業: 目標量と残りの配分可能量の小さい方を供給し、配分可能量から差し引く。
    /// - 環境流水: 下流に残す予約量を確保する。予約量は用水として供給せず、優先順位が低い項目が使える水量からだけ差し引く。
    /// 下流管理地点の流量 = 配分地点の利用可能量 - 農業・生活産業の供給量 + 配分地点より下流の区域から加わる量。
    let allocate
        (priority: Allocatee list)
        (availableWU: float)
        (belowContributionWU: float)
        (agTargetWU: float)
        (urbanTargetWU: float)
        (envTargetWU: float)
        : AllocationResult =
        // 下流の区域が加わる分は、予約すべき量から差し引く
        let reserveTarget = max 0.0 (envTargetWU - belowContributionWU)

        let _, agSupply, urbanSupply, envReserve =
            priority
            |> List.fold
                (fun (remaining, ag, urban, env) item ->
                    match item with
                    | Agriculture ->
                        let s = min agTargetWU remaining
                        remaining - s, s, urban, env
                    | Municipal ->
                        let s = min urbanTargetWU remaining
                        remaining - s, ag, s, env
                    | Environment ->
                        let r = min reserveTarget remaining
                        remaining - r, ag, urban, r)
                (availableWU, 0.0, 0.0, 0.0)

        { AvailableWU = availableWU
          AgSupplyWU = agSupply
          UrbanSupplyWU = urbanSupply
          EnvReserveWU = envReserve
          BelowContributionWU = belowContributionWU
          ReturnFlowWU = 0.0
          DownstreamFlowWU = availableWU - agSupply - urbanSupply + belowContributionWU }
