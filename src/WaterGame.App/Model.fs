module WaterGame.App.Model

open System
open Elmish
open WaterGame.Core

type Overlay =
    | NoOverlay
    /// 起動時の遊び方
    | ShowIntro
    | ShowResult of TurnResult
    | ShowFinal of Summary

type Model =
    { Scenario: Scenario
      Seed: int
      /// ゲーム開始時に確定した気象実績。UI には予報の範囲しか出さない
      Weather: ActualWeather list
      Game: GameState
      AgTargetWU: float
      UrbanTargetWU: float
      EnvTargetWU: float
      Priority: Allocatee list
      /// 区域ごとの揚水上限 (井戸のある区域だけ意味を持つ)
      PumpLimits: Map<ZoneId, float>
      /// 貯水池の操作 (正: 貯める / 負: 放流)。季節ごとに 0 から始める
      ReservoirOp: float
      /// 年次報告の方針 (冬の季節だけ効く)。季節ごとに、正確な報告に戻す
      Report: ReportStance
      // 前の季節の設定を、次の季節の初期値に引き継ぐための記憶 (毎ターン設定し直さなくてよいように)
      /// 農業の河川目標が、需要に対して何割か
      AgShare: float
      UrbanShare: float
      /// 環境流水の目標が、最低基準を上回らせた量
      EnvExtraWU: float
      /// 今季に予定している主要行動 (調査・井戸建設・設備投資のいずれか1つ)
      Action: MajorAction
      Selected: ZoneId
      /// 右の盤のタブ (0: 運転, 1: 設備, 2: 指令, 3: 評価・財政, 4: マニュアル)。入門には、指令のタブがない
      RightTab: int
      Overlay: Overlay
      /// 重ねて表示するカード (遊び方・日報・最終報告書) の、いまのページ (0 始まり)。カードが替わると 0 に戻る
      OverlayPage: int
      /// マニュアルのページ (0 始まり)
      ManualPage: int
      // ---- プレイ時間の計測 (プレイテストの記録用。1プレイ15〜25分、1ターン45〜90秒が目標) ----
      StartedAt: DateTime
      /// 今のターンの方針決定を始めた時刻 (結果カードを読む時間は含めない)
      TurnStartedAt: DateTime
      /// 各ターンの方針決定に要した秒数
      DecisionSeconds: float list
      /// 結果カードまで含めた全体の所要時間 (最終結果の表示時に確定)
      Elapsed: TimeSpan option }

type Msg =
    | SelectZone of ZoneId
    | SelectTab of int
    /// カードのページを替える
    | SetPage of int
    | SetManualPage of int
    | SetAgTarget of float
    | SetUrbanTarget of float
    | SetEnvTarget of float
    | SetPumpLimit of ZoneId * float
    | SetReservoirOp of float
    /// 年次報告の方針を選ぶ
    | SetReport of ReportStance
    /// 優先順位の項目を (位置, 移動量) で動かす。春のターンだけ
    | MovePriority of int * int
    /// 優先順位の項目を1位へ移す (位置)
    | PriorityToTop of int
    /// 優先順位を並びごと指定する (ロータリースイッチ)。春のターンだけ
    | SetPriority of Allocatee list
    /// 主要行動を選ぶ。すでに選んでいる行動をもう一度選ぶと取り消す
    | ChooseAction of MajorAction
    /// シナリオ選択の画面を開く (最初のターンの前と、最終結果の画面から)
    | OpenMenu
    /// 最終結果を閉じて、地図や各区域を見直す / もう一度開く
    | CloseFinal
    | ReopenFinal
    // 時刻は、イベントを受け取った時点の値を View から渡す (update を純粋に保つため)
    | DismissIntro of DateTime
    | EndTurn of DateTime
    | CloseOverlay of DateTime
    /// 同じシードでやり直す (気象は同一になるので政策の違いを比較できる)
    | RestartSameSeed of DateTime
    | RestartNewSeed of DateTime
    /// シナリオを選んで、新しいシードで始める
    | StartScenario of Scenario * DateTime

let private step = 50.0

let private snap (v: float) = Math.Round(v / step) * step

/// 刻みに丸める。ただし上限の半刻み以内なら上限そのものにする (需要が刻みの倍数でなくても右端で「需要どおり」になる)
let private snapTo (maxV: float) (v: float) =
    if maxV - v < step / 2.0 then maxV else min maxV (max 0.0 (snap v))

/// 井戸のある区域の揚水上限の初期値: 推定される持続的揚水量 (ソフト上限) を、井戸能力の範囲で丸めたもの
let private defaultPumpLimit (m: Model) (zone: ZoneId) =
    match m.Game.Wells[zone] with
    | NoWell -> 0.0
    | WellBuilt ->
        let cap = Turn.wellCapacity m.Scenario m.Game zone
        // 推定を超えて警告が出ないよう、切り捨てで丸める
        min cap (Math.Floor(m.Game.Estimates[zone].YieldWU / step) * step)

/// 現在のターンの季節に合わせて、政策の初期値を決める (需要いっぱい、環境流水は最低基準、主要行動なし)
let private withDefaults (m: Model) : Model =
    let turn = min m.Game.Turn (m.Scenario.Turns - 1)
    let season = Season.ofTurn turn

    let agDemand = Turn.effectiveAgDemand m.Scenario m.Game season
    let urbanDemand = Turn.effectiveUrbanDemand m.Scenario m.Game turn

    // 農業・生活産業の目標は「需要の何割か」、環境流水は「最低基準にいくら上乗せか」、揚水上限は前の値を引き継ぐ
    { m with
        AgTargetWU = snapTo agDemand (m.AgShare * agDemand)
        UrbanTargetWU = snapTo urbanDemand (m.UrbanShare * urbanDemand)
        EnvTargetWU = m.Scenario.MinEnvFlowWU[season] + m.EnvExtraWU
        Priority = m.Game.Priority
        PumpLimits =
            m.Scenario.Zones
            |> List.map (fun z ->
                let cap = Turn.wellCapacity m.Scenario m.Game z.Id

                // 直前の季節に完成した井戸は初期値から始め、それ以外は前の値 (0 も含む) を引き継ぐ
                let justBuilt =
                    match m.Game.History |> List.tryLast with
                    | Some r -> r.ActionTaken = BuildWell z.Id
                    | None -> false

                let limit =
                    match m.Game.Wells[z.Id], m.PumpLimits |> Map.tryFind z.Id with
                    | WellBuilt, Some prev when not justBuilt -> min cap prev
                    | _ -> defaultPumpLimit m z.Id

                z.Id, limit)
            |> Map.ofList
        ReservoirOp = 0.0
        Report = Accurate
        Action = NoAction }

let newGame (scenario: Scenario) (seed: int) (now: DateTime) : Model =
    { Scenario = scenario
      Seed = seed
      Weather = Weather.generate scenario seed
      Game = Turn.initialState scenario
      AgTargetWU = 0.0
      UrbanTargetWU = 0.0
      EnvTargetWU = 0.0
      Priority = Turn.defaultPriority
      PumpLimits = Map.empty
      ReservoirOp = 0.0
      Report = Accurate
      AgShare = 1.0
      UrbanShare = 1.0
      EnvExtraWU = 0.0
      Action = NoAction
      Selected = Farm
      RightTab = 0
      Overlay = NoOverlay
      OverlayPage = 0
      ManualPage = 0
      StartedAt = now
      TurnStartedAt = now
      DecisionSeconds = []
      Elapsed = None }
    |> withDefaults

/// 起動オプション。プレイテストや不具合の再現で、シナリオとシードを指定して始めるために使う
///   --scenario <シナリオ名>   --seed <整数>   --skip-intro   --funds <整数> (試験用: 初期の残高を指定する)
type LaunchOptions =
    { Scenario: string option
      Seed: int option
      SkipIntro: bool
      Funds: float option }

let mutable launchOptions =
    { Scenario = None
      Seed = None
      SkipIntro = false
      Funds = None }

let parseArgs (args: string[]) : LaunchOptions =
    let rec go (opts: LaunchOptions) =
        function
        | "--scenario" :: name :: rest -> go { opts with Scenario = Some name } rest
        | "--seed" :: n :: rest ->
            match Int32.TryParse n with
            | true, v -> go { opts with Seed = Some v } rest
            | _ -> go opts rest
        | "--skip-intro" :: rest -> go { opts with SkipIntro = true } rest
        | "--funds" :: n :: rest ->
            match Double.TryParse(n, Globalization.NumberStyles.Float, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> go { opts with Funds = Some v } rest
            | _ -> go opts rest
        | _ :: rest -> go opts rest
        | [] -> opts

    go { Scenario = None; Seed = None; SkipIntro = false; Funds = None } (List.ofArray args)

let init () : Model * Cmd<Msg> =
    let now = DateTime.UtcNow
    let opts = launchOptions

    let scenario =
        opts.Scenario
        |> Option.bind (fun n -> Scenarios.all |> List.tryFind (fun s -> s.Name = n))
        |> Option.defaultValue Scenarios.normalYear

    let seed = opts.Seed |> Option.defaultValue (Environment.TickCount &&& 0x7FFFFFFF)
    // 起動時は遊び方を表示する。計測は「はじめる」を押した時刻から
    let m = newGame scenario seed now

    let m =
        match opts.Funds with
        | Some f -> { m with Game = { m.Game with Balance = f } }
        | None -> m

    { m with Overlay = (if opts.SkipIntro then NoOverlay else ShowIntro) }, Cmd.none

let private newSeed () = Environment.TickCount &&& 0x7FFFFFFF

let private updateCore (msg: Msg) (m: Model) : Model * Cmd<Msg> =
    match msg with
    // 区域をクリックすると、右パネルをその区域のタブに切り替える
    | SelectZone z -> { m with Selected = z; RightTab = 1 }, Cmd.none
    // タブの再構築中に、存在しないタブ (-1) の選択が通知されることがある。無視する
    | SelectTab i when i >= 0 && i <= 4 -> { m with RightTab = i }, Cmd.none
    | SelectTab _ -> m, Cmd.none
    | SetPage p -> { m with OverlayPage = max 0 p }, Cmd.none
    | SetManualPage p -> { m with ManualPage = max 0 p }, Cmd.none

    | SetAgTarget v ->
        let season = Season.ofTurn m.Game.Turn
        let demand = Turn.effectiveAgDemand m.Scenario m.Game season
        let target = snapTo demand v
        { m with AgTargetWU = target; AgShare = (if demand > 0.0 then target / demand else m.AgShare) }, Cmd.none

    | SetUrbanTarget v ->
        let season = Season.ofTurn m.Game.Turn
        let demand = Turn.effectiveUrbanDemand m.Scenario m.Game m.Game.Turn
        let target = snapTo demand v
        { m with UrbanTargetWU = target; UrbanShare = (if demand > 0.0 then target / demand else m.UrbanShare) }, Cmd.none

    | SetEnvTarget v ->
        let season = Season.ofTurn m.Game.Turn
        let envMin = m.Scenario.MinEnvFlowWU[season]
        // 最低基準が刻みの倍数でなくても、そこから刻みで上乗せする
        let target = envMin + snap (v - envMin) |> max envMin
        { m with EnvTargetWU = target; EnvExtraWU = target - envMin }, Cmd.none

    | SetPumpLimit(z, v) ->
        let cap = Turn.wellCapacity m.Scenario m.Game z
        { m with PumpLimits = m.PumpLimits |> Map.add z (v |> snap |> max 0.0 |> min cap) }, Cmd.none

    | SetReservoirOp v ->
        let cap = Turn.reservoirCapacity m.Scenario m.Game
        { m with ReservoirOp = (if cap > 0.0 then snap v |> max (-m.Game.ReservoirWU) |> min cap else 0.0) }, Cmd.none

    | SetReport stance -> { m with Report = stance }, Cmd.none

    | MovePriority(i, delta) when Turn.canChangePriority m.Game.Turn ->
        let j = i + delta

        if i < 0 || j < 0 || i >= m.Priority.Length || j >= m.Priority.Length then
            m, Cmd.none
        else
            let arr = List.toArray m.Priority
            let t = arr[i]
            arr[i] <- arr[j]
            arr[j] <- t
            { m with Priority = List.ofArray arr }, Cmd.none
    | MovePriority _ -> m, Cmd.none

    /// 指定した項目を1位へ (春のターンだけ)
    | PriorityToTop i when Turn.canChangePriority m.Game.Turn && i > 0 && i < m.Priority.Length ->
        { m with Priority = m.Priority[i] :: (m.Priority |> List.indexed |> List.filter (fun (k, _) -> k <> i) |> List.map snd) }, Cmd.none
    | PriorityToTop _ -> m, Cmd.none

    | SetPriority order when Turn.canChangePriority m.Game.Turn && Turn.isValidPriority order -> { m with Priority = order }, Cmd.none
    | SetPriority _ -> m, Cmd.none

    | ChooseAction a when m.Action = a || Turn.isValidAction m.Scenario m.Game a ->
        { m with Action = (if m.Action = a then NoAction else a) }, Cmd.none
    | ChooseAction _ -> m, Cmd.none

    | OpenMenu when m.Game.Turn = 0 || Turn.isFinished m.Scenario m.Game -> { m with Overlay = ShowIntro }, Cmd.none
    | OpenMenu -> m, Cmd.none

    | CloseFinal ->
        match m.Overlay with
        | ShowFinal _ -> { m with Overlay = NoOverlay }, Cmd.none
        | _ -> m, Cmd.none

    | ReopenFinal when m.Overlay = NoOverlay && Turn.isFinished m.Scenario m.Game ->
        { m with Overlay = ShowFinal(Evaluation.summarize m.Scenario m.Game) }, Cmd.none
    | ReopenFinal -> m, Cmd.none

    | DismissIntro now -> { m with Overlay = NoOverlay; StartedAt = now; TurnStartedAt = now }, Cmd.none

    | EndTurn now when m.Overlay = NoOverlay && not (Turn.isFinished m.Scenario m.Game) ->
        let policy =
            { AgTargetWU = m.AgTargetWU
              UrbanTargetWU = m.UrbanTargetWU
              EnvTargetWU = m.EnvTargetWU
              Priority = m.Priority
              PumpLimitWU = m.PumpLimits
              ReservoirOpWU = m.ReservoirOp
              Report = m.Report
              Action = m.Action }

        let next, result = Turn.resolve m.Scenario m.Game policy m.Weather[m.Game.Turn]
        let decision = (now - m.TurnStartedAt).TotalSeconds
        // 各ターン終了時に水収支残差と、方針決定に要した時間をログへ出す
        let log =
            Cmd.ofEffect (fun _ ->
                eprintfn "%s" (Turn.balanceLogLine result)
                eprintfn "[time] turn=%d decision=%.1fs" (result.Turn + 1) decision)

        let after =
            { m with
                Game = next
                Overlay = ShowResult result
                DecisionSeconds = m.DecisionSeconds @ [ decision ] }

        // 結果カードの背後の方針パネルも、次の季節の初期値にそろえておく (前季の目標が新しい需要を超えて見えないように)
        (if Turn.isFinished m.Scenario next then after else withDefaults after), log

    | EndTurn _ -> m, Cmd.none

    | CloseOverlay now ->
        match m.Overlay with
        | ShowResult _ when Turn.isFinished m.Scenario m.Game ->
            let summary = Evaluation.summarize m.Scenario m.Game
            let elapsed = now - m.StartedAt

            let log =
                Cmd.ofEffect (fun _ ->
                    eprintfn
                        "[time] total=%.1fmin avg-decision=%.1fs"
                        elapsed.TotalMinutes
                        (if m.DecisionSeconds.IsEmpty then 0.0 else List.average m.DecisionSeconds))

            { m with Overlay = ShowFinal summary; Elapsed = Some elapsed }, log
        | ShowResult _ -> { m with Overlay = NoOverlay; TurnStartedAt = now } |> withDefaults, Cmd.none
        | _ -> m, Cmd.none

    | RestartSameSeed now -> newGame m.Scenario m.Seed now, Cmd.none
    | RestartNewSeed now -> newGame m.Scenario (newSeed ()) now, Cmd.none
    | StartScenario(sc, now) -> newGame sc (newSeed ()) now, Cmd.none


/// カードの種類と中身が替わったかを見分ける印 (替わったらページを 0 に戻す)
let private overlayKey =
    function
    | NoOverlay -> ""
    | ShowIntro -> "intro"
    | ShowResult r -> $"result-{r.Turn}"
    | ShowFinal _ -> "final"

let update (msg: Msg) (m: Model) : Model * Cmd<Msg> =
    let m', cmd = updateCore msg m

    match msg with
    | SetPage _ -> m', cmd
    | _ ->
        if overlayKey m'.Overlay <> overlayKey m.Overlay then
            { m' with OverlayPage = 0 }, cmd
        else
            m', cmd
