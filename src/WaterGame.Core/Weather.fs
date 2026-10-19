namespace WaterGame.Core

module Weather =

    /// 蒸発散需要の段階ごとの係数帯 (基準値に掛ける)
    let etBand =
        function
        | Low -> (0.70, 0.90)
        | Mid -> (0.90, 1.10)
        | High -> (1.10, 1.40)

    let private pickCard (cards: WeatherCard list) (r: Rng) =
        let total = cards |> List.sumBy _.Weight
        let u, r' = Rng.nextFloat r
        let target = u * total

        let rec go acc =
            function
            | [ c ] -> c
            | c :: rest -> if target < acc + c.Weight then c else go (acc + c.Weight) rest
            | [] -> failwith "気象カードが空です"

        go 0.0 cards, r'

    /// シナリオとシードから、全ターン分の気象実績をゲーム開始時に確定する。
    /// 政策には依存しないため、同じシナリオ・同じシードなら政策を変えても同一の気象になる。
    let generate (scenario: Scenario) (seed: int) : ActualWeather list =
        let step (r: Rng) turn =
            let season = Season.ofTurn turn
            let cards =
                scenario.Cards
                |> List.filter (fun c -> c.Season = season && turn >= c.FromTurn && turn <= c.ToTurn)
            let card, r = pickCard cards r
            let lo, hi = card.PrecipMm
            // 2つの一様乱数の平均で、範囲の中央寄りに分布させる
            let u1, r = Rng.nextFloat r
            let u2, r = Rng.nextFloat r
            let precip = lo + (hi - lo) * (u1 + u2) / 2.0
            let blo, bhi = etBand card.EtLevel
            let f, r = Rng.nextRange blo bhi r

            { Turn = turn
              Season = season
              Card = card
              PrecipMm = precip
              EtDemandMm = scenario.BaseEtMm[season] * f },
            r

        [ 0 .. scenario.Turns - 1 ]
        |> List.fold
            (fun (acc, r) turn ->
                let w, r' = step r turn
                w :: acc, r')
            ([], Rng.create seed)
        |> fst
        |> List.rev
