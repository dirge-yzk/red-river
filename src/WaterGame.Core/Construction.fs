namespace WaterGame.Core

/// 資材と建設 (計画経済版 v2.0-c)。工事は、発注 (主要行動) で始まり、必要な資材 (機械設備・建設資材・専門技術者) が
/// そろうと着工し、標準工期のあとに完成する。資金があっても、資材と技術者がなければ、工事は進まない。
/// 資材の正規配給は、シナリオの日程どおり。乱数は使わない。融通は、すぐ届くが、あとで正規配給から差し引かれる。
module Construction =

    let allMaterials = [ Machinery; BuildMaterial; Engineer ]

    let materialLabel =
        function
        | Machinery -> "機械設備"
        | BuildMaterial -> "建設資材"
        | Engineer -> "専門技術者"

    /// 工事になる行動の、必要な資材と標準工期。工事にならない行動 (調査・節水キャンペーンなど) は None。
    /// 資材の単位は抽象で、井戸 1 本・小さな設備 1 つが、おおむね 1 単位。
    let needsOf (action: MajorAction) : (MaterialNeeds * int) option =
        let n m c e =
            { Machinery = m
              BuildMaterial = c
              Engineers = e }

        match action with
        | BuildWell _ -> Some(n 1 1 1, 1)
        | Invest(ObservationWell _) -> Some(n 1 0 0, 1)
        | Invest Reservoir -> Some(n 0 2 1, 3)
        | Invest ReservoirExpansion -> Some(n 0 2 1, 2)
        | Invest IrrigationEfficiency -> Some(n 0 1 0, 2)
        | Invest IrrigationUpgrade -> Some(n 0 1 1, 2)
        | Invest RainwaterHarvest -> Some(n 0 1 0, 1)
        | Invest EffluentReuse -> Some(n 1 1 0, 2)
        | Invest EffluentUpgrade -> Some(n 1 0 0, 2)
        | _ -> None

    /// このシナリオで、その行動が工事 (資材と工期が要る) になるか
    let isProject (scenario: Scenario) (action: MajorAction) =
        scenario.Construction.IsSome && (needsOf action).IsSome

    /// 工事の名前
    let targetLabel =
        function
        | BuildWell z -> $"{ZoneId.label z}の井戸"
        | Invest i ->
            match i with
            | IrrigationEfficiency -> "灌漑効率の改善"
            | IrrigationUpgrade -> "灌漑効率の拡張"
            | ObservationWell z -> $"{ZoneId.label z}の観測井戸"
            | Reservoir -> "貯水池"
            | ReservoirExpansion -> "貯水池の拡張"
            | RainwaterHarvest -> "雨水利用施設"
            | EffluentReuse -> "再生水の放流"
            | EffluentUpgrade -> "再生水の拡張"
            | WaterSaving -> "節水キャンペーン"
        | Survey _ -> "調査"
        | RequestTransfer m -> $"{materialLabel m}の融通"
        | NoAction -> "なし"

    let private stockOf (stock: Map<Material, int>) (m: Material) = stock |> Map.tryFind m |> Option.defaultValue 0

    let private arrivalsAt (cfg: ConstructionConfig) (turn: int) : (Material * int) list =
        cfg.Arrivals |> List.filter (fun (t, _, _) -> t = turn) |> List.map (fun (_, m, q) -> m, q)

    let private addStock (stock: Map<Material, int>) (m: Material) (q: int) = Map.add m (stockOf stock m + q) stock

    /// 開始時の状態: 開始時の資材と、最初のターンに届く正規配給
    let initialState (scenario: Scenario) : ConstructionState =
        let stock =
            match scenario.Construction with
            | Some cfg ->
                let s0 = allMaterials |> List.map (fun m -> m, stockOf cfg.InitialStock m) |> Map.ofList
                arrivalsAt cfg 0 |> List.fold (fun s (m, q) -> addStock s m q) s0
            | None -> allMaterials |> List.map (fun m -> m, 0) |> Map.ofList

        { Stock = stock
          Projects = []
          Loans = []
          NextProjectId = 0 }

    let stock (state: ConstructionState) (m: Material) = stockOf state.Stock m

    /// すでに発注した (まだ完成していない) 工事か
    let inQueue (state: ConstructionState) (action: MajorAction) =
        state.Projects |> List.exists (fun (p: Project) -> p.Target = action)

    /// 融通の手数料 (融通ができないシナリオでは 0)
    let transferFee (scenario: Scenario) =
        scenario.Construction |> Option.map _.TransferFee |> Option.defaultValue 0.0

    /// 融通を頼めるか (借りている資材が、上限に達していないこと)
    let canTransfer (scenario: Scenario) (state: ConstructionState) =
        match scenario.Construction with
        | Some cfg -> state.Loans.Length < cfg.MaxLoans
        | None -> false

    /// 着工に必要な資材が、いまそろっているか
    let private canStart (state: ConstructionState) (n: MaterialNeeds) =
        stock state Machinery >= n.Machinery
        && stock state BuildMaterial >= n.BuildMaterial
        && stock state Engineer >= n.Engineers

    /// 足りない資材 (待っている理由)
    let missing (state: ConstructionState) (n: MaterialNeeds) : (Material * int) list =
        [ Machinery, n.Machinery - stock state Machinery
          BuildMaterial, n.BuildMaterial - stock state BuildMaterial
          Engineer, n.Engineers - stock state Engineer ]
        |> List.filter (fun (_, q) -> q > 0)

    /// 今季の処理: 発注 → 融通 → 着工 (発注順に、そろっているものから) → 工事の進行 → 完成 → 来季の正規配給と返礼。
    /// 水文学の状態には触れない。完成した工事の効果 (井戸・設備) は、呼び出し側が、翌季から反映する。
    let step (scenario: Scenario) (state: ConstructionState) (turn: int) (action: MajorAction) : ConstructionState * WorkTurn =
        match scenario.Construction with
        | None ->
            state,
            { Ordered = None
              Started = []
              Completed = []
              Transferred = None
              Repaid = []
              ArrivingNext = []
              StockAfter = state.Stock }
        | Some cfg ->
            // 1. 発注
            let ordered =
                match action, needsOf action with
                | (BuildWell _ | Invest _), Some(needs, duration) ->
                    Some(
                        { Id = state.NextProjectId
                          Target = action
                          Needs = needs
                          Duration = duration
                          OrderedTurn = turn
                          Status = Waiting
                          StartedTurn = None
                          Remaining = duration }
                        : Project
                    )
                | _ -> None

            let s1 =
                { state with
                    Projects = state.Projects @ Option.toList ordered
                    NextProjectId = state.NextProjectId + (if ordered.IsSome then 1 else 0) }

            // 2. 融通 (すぐ届く。返礼の期限が決まる)
            let transferred, s2 =
                match action with
                | RequestTransfer m ->
                    Some m,
                    { s1 with
                        Stock = addStock s1.Stock m 1
                        Loans = s1.Loans @ [ { Material = m; RepayTurn = turn + cfg.TransferRepayAfter } ] }
                | _ -> None, s1

            // 3. 着工: 発注順に、必要な資材がそろっているものから
            let s3, started =
                s2.Projects
                |> List.fold
                    (fun (st: ConstructionState, started) (p: Project) ->
                        if p.Status = Waiting && canStart st p.Needs then
                            let stock' =
                                st.Stock
                                |> fun s -> addStock s Machinery (-p.Needs.Machinery)
                                |> fun s -> addStock s BuildMaterial (-p.Needs.BuildMaterial)
                                |> fun s -> addStock s Engineer (-p.Needs.Engineers)

                            let p' =
                                { p with
                                    Status = Building
                                    StartedTurn = Some turn }

                            { st with
                                Stock = stock'
                                Projects = st.Projects |> List.map (fun (q: Project) -> if q.Id = p.Id then p' else q) },
                            started @ [ p' ]
                        else
                            st, started)
                    (s2, [])

            // 4. 工事の進行: 施工中の工事が、1季節ぶん進む (着工した季節も1と数える)。終わった工事は完成して、技術者が戻る
            let progressed = s3.Projects |> List.map (fun (p: Project) -> if p.Status = Building then { p with Remaining = p.Remaining - 1 } else p)
            let completed = progressed |> List.filter (fun (p: Project) -> p.Status = Building && p.Remaining <= 0)
            let running = progressed |> List.filter (fun (p: Project) -> not (List.exists (fun (c: Project) -> c.Id = p.Id) completed))
            let stock4 = completed |> List.fold (fun s (p: Project) -> addStock s Engineer p.Needs.Engineers) s3.Stock

            // 5. 来季のはじめに届く正規配給と、期限の来た返礼
            let next = turn + 1
            let arriving = arrivalsAt cfg next
            let repaid = s3.Loans |> List.filter (fun l -> l.RepayTurn <= next)
            let stock5 = arriving |> List.fold (fun s (m, q) -> addStock s m q) stock4
            let stock6 = repaid |> List.fold (fun s l -> addStock s l.Material -1) stock5

            { s3 with
                Stock = stock6
                Projects = running
                Loans = s3.Loans |> List.filter (fun l -> l.RepayTurn > next) },
            { Ordered = ordered
              Started = started
              Completed = completed
              Transferred = transferred
              Repaid = repaid |> List.map _.Material
              ArrivingNext = arriving
              StockAfter = stock6 }
