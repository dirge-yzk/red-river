namespace WaterGame.Core

/// 季節ごとのできごと (交付金の削減、停電、需要の急増など)。
/// シナリオの日程で、起こるターンが固定で決まり (乱数なし)、各季節のはじめに予告される。
module Events =

    /// シナリオの日程どおりの、ターンごとのできごと (乱数は使わない)
    let generate (scenario: Scenario) : Map<int, EventCard> = Map.ofList scenario.EventSchedule

    /// 今季のできごとの係数。できごとがなければ 1 (加算は 0)
    let private ofKind (state: GameState) (f: EventKind -> float option) =
        state.Events
        |> Map.tryFind state.Turn
        |> Option.bind (fun c -> f c.Kind)

    let grantMultiplier (state: GameState) =
        ofKind state (function GrantMultiplier k -> Some k | _ -> None) |> Option.defaultValue 1.0

    let extraFunds (state: GameState) =
        ofKind state (function ExtraFunds x -> Some x | _ -> None) |> Option.defaultValue 0.0

    let agDemandMultiplier (state: GameState) =
        ofKind state (function AgDemandMultiplier k -> Some k | _ -> None) |> Option.defaultValue 1.0

    let urbanDemandMultiplier (state: GameState) =
        ofKind state (function UrbanDemandMultiplier k -> Some k | _ -> None) |> Option.defaultValue 1.0

    let pumpCapacityMultiplier (state: GameState) =
        ofKind state (function PumpCapacityMultiplier k -> Some k | _ -> None) |> Option.defaultValue 1.0

    let pumpCostMultiplier (state: GameState) =
        ofKind state (function PumpCostMultiplier k -> Some k | _ -> None) |> Option.defaultValue 1.0
