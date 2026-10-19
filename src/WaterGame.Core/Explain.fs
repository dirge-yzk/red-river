namespace WaterGame.Core

/// 運転日報の「なぜこうなったか」の1行解説。学習目標 1 (水収支)・2 (地下水のストック性)・3 (配分のトレードオフ)・
/// 4 (情報の価値)・5 (不可逆性) に紐づける。
module Explain =

    type Input =
        { Priority: Allocatee list
          Zones: ZoneResult list
          Allocation: AllocationResult
          AgDemandWU: float
          AgTargetWU: float
          AgRiverSupplyWU: float
          AgGwSupplyWU: float
          UrbanDemandWU: float
          UrbanTargetWU: float
          UrbanRiverSupplyWU: float
          UrbanGwSupplyWU: float
          GwSupplyWU: float
          EnvMinMet: bool
          SurveyReports: SurveyReport list
          NewCostFlags: ZoneId list
          Action: MajorAction }

    let private firstLabel (priority: Allocatee list) =
        priority |> List.tryHead |> Option.map Allocatee.label |> Option.defaultValue ""

    let private pctText (x: float) = $"{x * 100.0:F0}%%"

    let private isInvest =
        function
        | Invest _ -> true
        | _ -> false

    let explain (i: Input) : int * string =
        let short demand supply = demand > 0.0 && supply < demand * 0.999
        let agShort = short i.AgDemandWU (i.AgRiverSupplyWU + i.AgGwSupplyWU)
        let urbanShort = short i.UrbanDemandWU (i.UrbanRiverSupplyWU + i.UrbanGwSupplyWU)
        let anyShort = agShort || urbanShort
        // 河川水が目標に届かなかった (= 河川水が足りなかった)
        let riverShortfall =
            i.AgRiverSupplyWU < i.AgTargetWU * 0.999 || i.UrbanRiverSupplyWU < i.UrbanTargetWU * 0.999

        // 不足した用水と、その充足率 (水が足りないと、優先順位が低い用途から削られる)
        let shortText =
            [ if agShort then
                  $"農業用水 (充足率 {pctText ((i.AgRiverSupplyWU + i.AgGwSupplyWU) / i.AgDemandWU)})"
              if urbanShort then
                  $"生活・産業用水 (充足率 {pctText ((i.UrbanRiverSupplyWU + i.UrbanGwSupplyWU) / i.UrbanDemandWU)})" ]
            |> String.concat "と"

        // 不足した用水のうち、優先順位が最も低いもの (水が足りないと、下の項目から削られる)
        let lowestShort =
            i.Priority
            |> List.rev
            |> List.tryFind (function
                | Agriculture -> agShort
                | Municipal -> urbanShort
                | Environment -> false)
            |> Option.map Allocatee.label
            |> Option.defaultValue ""

        let drained = i.Zones |> List.sumBy _.DeepDrainageWU
        let soilChange = i.Zones |> List.sumBy (fun z -> z.SoilEndWU - z.SoilStartWU)

        match i.NewCostFlags, i.SurveyReports with
        | z :: _, _ ->
            5,
            $"{ZoneId.label z}の地下水が限界を超えました (貯留量が閾値を下回ったか、持続的揚水量を超える揚水が積み重なりました)。この区域の揚水コストは、地下水位が回復しても元には戻りません。"
        | [], r :: _ ->
            4,
            $"{ZoneId.label r.Zone}の{SurveyKind.label r.Kind}で、推定の幅が±{pctText r.PrevRelErr}から±{pctText r.RelErr}に狭まりました。判断材料が増えます。"
        | [], [] when isInvest i.Action ->
            match i.Action with
            | Invest IrrigationEfficiency ->
                1, "灌漑効率の改善で、同じ生産に必要な水が減りました。来季から、河川や地下水から取る量を抑えられます。"
            | Invest(ObservationWell z) ->
                4,
                $"{ZoneId.label z}に観測井戸を設置しました。来季から季節ごとに、その区域の地下水の推定の幅が狭まり、実際の値に近づきます。"
            | Invest IrrigationUpgrade ->
                1, "高効率の灌漑方式に拡張しました。来季から、農業の需要がさらに減ります。"
            | Invest ReservoirExpansion ->
                3, "貯水池を拡張しました。来季から、より多くの河川水を貯められます。蒸発で失う量も、貯水量に応じて増えます。"
            | Invest EffluentUpgrade ->
                1, "再生水の処理能力を拡張しました。来季から、下流へ戻る水の割合が増えます。"
            | Invest Reservoir ->
                3,
                "貯水池が完成しました。来季から、河川水が豊かな季節に貯めて、不足する季節に放流できます。夏は蒸発で失う量が増えます。"
            | Invest RainwaterHarvest ->
                3, "雨水利用施設で、来季から市街地の用水の需要が減ります。河川水と地下水を、ほかの用途へ回せます。"
            | Invest EffluentReuse ->
                1,
                "再生水の放流を始めます。来季から、河川水で供給した生活・産業用水の一部が下流へ戻り、環境流水の流量を支えます。"
            | Invest WaterSaving ->
                3, "節水キャンペーンで、来季から農業・生活産業の需要が少し減ります。河川水や地下水を、ほかの用途へ回せます。"
            | _ -> 1, "設備が完成しました。来季から効果が出ます。"
        | [], [] ->
            match anyShort, i.EnvMinMet with
            | true, false ->
                3,
                $"河川水と地下水を合わせても用水と環境の両方は満たせず、優先順位の高い{firstLabel i.Priority}を先に確保したため、{shortText}が不足しました。"
            | false, false ->
                3, "取水後に下流へ残る流量が環境流水の最低基準を下回りました。取水を減らすと下流に残せますが、用水の供給は減ります。"
            | true, true when i.GwSupplyWU > 0.0 && riverShortfall ->
                3,
                $"河川水と地下水を使っても需要には届かず、{shortText}が不足しました。井戸の能力や揚水上限が、供給できる量を決めています。"
            | true, true when riverShortfall ->
                3,
                $"河川水が足りず、{shortText}が不足しました。水が足りないときは、優先順位の低い項目から削られます。環境流水を守ると、その分だけ使える水が減ります。"
            | true, true ->
                3, "取水目標を需要より低く設定したため、需要を満たせませんでした。下流に水が残る代わりに用水の供給が減ります。"
            | false, true when i.GwSupplyWU > 0.0 && i.GwSupplyWU > drained ->
                2, "揚水量が涵養量を上回り、地下水の貯留量が減っています。地下水は涵養と揚水の差が積み重なるストックです。"
            | false, true when i.GwSupplyWU > 0.0 ->
                2, "河川水で足りない分を地下水で補いました。涵養の範囲内の揚水なら、貯留量は大きく減りません。"
            | false, true when drained > 0.0 ->
                1, "降水の一部が、土壌があふれる、または割れ目を通って直接、地下水の涵養に回りました。地下水は次の季節から川への湧出として効いてきます。"
            | false, true when soilChange < 0.0 ->
                1, "蒸発散が降水の浸透量を上回り、土壌水分(貯留)が減りました。貯留の減少分が蒸発散を支えています。"
            | false, true -> 1, "降水は、蒸発散・取水・下流への流出・貯留(土壌と地下水)の変化に配分されます。収支の残差はほぼゼロです。"
