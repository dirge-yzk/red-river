module WaterGame.App.View

open System
open System.Globalization
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open WaterGame.Core
open WaterGame.App.Model
open WaterGame.App.Instruments

// 画面は 1180 x 800 の固定の盤面で、Viewbox で窓に合わせて拡大・縮小する (スクロールは出さない)。
// 同じ情報を2か所に出さない: 時期・気象=ヘッダー / 残高=ヘッダー / 区域の状態=地図の計器 /
// 方針=運転ページ / 行動と資金=設備ページ / 評価・重み=評価・財政ページ / 効果の説明=マニュアル。

// ---------- 表示用ヘルパー ----------

let private inv = CultureInfo.InvariantCulture
let private wu (x: float) = x.ToString("N0", inv)
let private pct (x: float) = (x * 100.0).ToString("F0", inv) + "%"

/// 指令の目標の書き方 (充足率の指令と、建設期限の指令)
let private directiveGoal (d: Directive) =
    match d.Kind, d.Target with
    | BuildOrder, Some t -> $"「{Construction.targetLabel t}」の完成"
    | _ -> $"{Organization.targetLabel d.Kind}の充足率 {pct d.TargetFill} 以上"

/// 指令の実績の書き方
let private measuredText (d: Directive) (v: float) =
    if d.Kind = BuildOrder then Organization.buildStateLabel v else pct v

/// 通知灯などでの短い目標
let private goalShort (d: Directive) =
    match d.Kind, d.Target with
    | BuildOrder, Some t -> $"{Construction.targetLabel t}を完成"
    | _ -> $"目標 {pct d.TargetFill}"
let private mm (x: float) = x.ToString("F0", inv)
let private signed (x: float) = x.ToString("+#,##0;-#,##0;0", inv)
let private thousands (v: float) = if v = 0.0 then "0" else (v / 1000.0).ToString("0.#", inv) + "k"
let private plainNum (v: float) = v.ToString("0", inv)

let private brush (hex: string) : IBrush = SolidColorBrush(Color.Parse hex) :> IBrush

let private zoneBase =
    function
    | Forest -> "#4F9A57"
    | Farm -> "#D2B94F"
    | Urban -> "#A7B0BA"
    | Wetland -> "#4FA6A0"

let private okColor = "#2E7D32"
let private badColor = "#C62828"
let private mutedColor = "#5F6B76"

/// 紙 (報告書) の上の文字
let private styled (s: string) (size: float) (weight: FontWeight) (color: string) =
    TextBlock.create
        [ TextBlock.text s
          TextBlock.fontSize size
          TextBlock.fontWeight weight
          TextBlock.foreground (brush color)
          TextBlock.textWrapping TextWrapping.Wrap ]
    :> IView

let private text (s: string) = styled s 12.0 FontWeight.Normal "#20262B"
let private heading (s: string) = styled s 18.0 FontWeight.Bold "#1B2A38"
let private subheading (s: string) = styled s 13.0 FontWeight.Bold "#10202D"
let private muted (s: string) = styled s 11.0 FontWeight.Normal mutedColor

/// 盤 (暗い面) の上の文字
let private onDark (s: string) (size: float) (color: string) =
    TextBlock.create
        [ TextBlock.text s
          TextBlock.fontSize size
          TextBlock.foreground (brush color)
          TextBlock.textWrapping TextWrapping.Wrap ]
    :> IView

/// 計器の下の読み (数値や状態)。操作の効果の説明は書かない (マニュアルに書く)
let private caption (s: string) = onDark s 11.0 "#AAB4BD"

let private yesNo ok (good: string) (bad: string) =
    styled (if ok then good else bad) 12.0 FontWeight.SemiBold (if ok then okColor else badColor)

/// 表の1行 (列の幅つき)
let private row (cells: (string * float) list) =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.children
              [ for (s, w) in cells ->
                    TextBlock.create
                        [ TextBlock.text s
                          TextBlock.width w
                          TextBlock.fontSize 12.0
                          TextBlock.foreground (brush "#20262B") ] :> IView ] ]
    :> IView

let private at (x: float) (y: float) (v: IView) =
    Border.create [ Canvas.left x; Canvas.top y; Border.child v ] :> IView

let private lastResult (m: Model) = m.Game.History |> List.tryLast

let private lastZone (m: Model) (z: ZoneId) =
    lastResult m |> Option.bind (fun r -> r.Zones |> List.tryFind (fun x -> x.Zone = z))

let private actionText =
    function
    | NoAction -> "なし"
    | Survey(z, k) -> $"{ZoneId.label z}で{SurveyKind.label k}"
    | BuildWell z -> $"{ZoneId.label z}に井戸を建設"
    | Invest IrrigationEfficiency -> "灌漑効率の改善"
    | Invest(ObservationWell z) -> $"{ZoneId.label z}に観測井戸を設置"
    | Invest Reservoir -> "貯水池を建設"
    | Invest ReservoirExpansion -> "貯水池を拡張"
    | Invest IrrigationUpgrade -> "灌漑効率の拡張"
    | Invest EffluentUpgrade -> "再生水の拡張"
    | Invest RainwaterHarvest -> "雨水利用施設を建設"
    | Invest EffluentReuse -> "再生水の放流を開始"
    | Invest WaterSaving -> "節水キャンペーンを実施"
    | RequestTransfer mat -> $"{Construction.materialLabel mat}の融通を依頼"

/// 「1年目 冬」の形の、ターンの表記 (ヘッダーと同じ書き方にそろえる)
let private seasonText (turn: int) = $"{turn / 4 + 1}年目 {Season.label (Season.ofTurn turn)}"

let private costText (c: TurnCosts) =
    $"調査 {wu c.Survey} / 建設 {wu c.Build} / 投資 {wu c.Invest} / 維持 {wu c.Maintenance} / 揚水 {wu c.Pumping}"

let private marksOf (mx: float) (k: int) (fmt: float -> string) = [ for i in 0..k -> (mx * float i / float k, fmt (mx * float i / float k)) ]

// 盤面の寸法
let private boardW = 1180.0
let private boardH = 800.0
let private headerH = 56.0
let private mapW = 300.0
let private deskX = 300.0
let private deskW = 880.0

// ---------- ヘッダー ----------

let private header (m: Model) =
    // 日報を見ている間は、その日報のターンの表示にする (背景が次のターンに見えて紛らわしいため)
    let showingResult, turn =
        match m.Overlay with
        | ShowResult r -> true, r.Turn
        | _ -> false, min m.Game.Turn (m.Scenario.Turns - 1)

    let season = Season.ofTurn turn
    let year = turn / 4 + 1
    let w = m.Weather[turn]
    let suffix = if showingResult then " の日報" else ""
    // 資金 = 残高に、春は交付金を加えたもの (その季節に自由に使える額)。日報の間は、その季節の期末残高
    let balance =
        if showingResult then (m.Game.History |> List.find (fun r -> r.Turn = turn)).BalanceEnd
        elif Turn.isFinished m.Scenario m.Game then m.Game.Balance
        else Turn.availableFunds m.Scenario m.Game

    // 上司の信頼 (計画経済版のシナリオだけ)。日報の間は、その季節の終了後の値
    let trust =
        match m.Overlay with
        | ShowResult r -> r.Plan.TrustAfter
        | _ -> m.Game.Organization.Trust

    // 整合性リスク (監査リスク)。60 以上で監査の予兆が出る
    let risk =
        match m.Overlay with
        | ShowResult r -> r.Plan.RiskAfter
        | _ -> m.Game.Organization.Risk

    let politicsGauge =
        if m.Scenario.Directives.IsSome then
            [ at 790.0 18.0 (plate "上司の信頼")
              at 875.0 4.0 (meter 48.0 0.0 100.0 trust [] [ (0.0, 30.0, "#C94A3F"); (30.0, 70.0, "#E0A526"); (70.0, 100.0, "#4F9A57") ] None [])
              at 930.0 19.0 (miniCounter 3 trust)
              at 990.0 18.0 (plate "監査リスク")
              at 1072.0 4.0 (meter 48.0 0.0 100.0 risk [] [ (0.0, 30.0, "#4F9A57"); (30.0, Organization.auditThreshold, "#E0A526"); (Organization.auditThreshold, 100.0, "#C94A3F") ] None [ (Organization.auditThreshold, "#FFFFFF") ])
              at 1128.0 19.0 (miniCounter 3 risk) ]
        else
            []

    Border.create
        [ Canvas.left 0.0
          Canvas.top 0.0
          Border.width boardW
          Border.height headerH
          Border.background (brush "#1F3A52")
          Border.child (
              Canvas.create
                  [ Canvas.width boardW
                    Canvas.height headerH
                    Canvas.children
                        [ at 16.0 8.0 (styled $"{year}年目 {Season.label season}" 20.0 FontWeight.Bold "#FFFFFF")
                          at 16.0 33.0 (styled $"第 {turn + 1} 季 / 全 {m.Scenario.Turns} 季{suffix}" 12.0 FontWeight.Normal "#B8C7D6")
                          at 230.0 8.0 (styled $"気象カード: {w.Card.Name}" 16.0 FontWeight.SemiBold "#FFE082")
                          at 230.0 33.0 (styled $"蒸発散需要 {EtLevel.label w.Card.EtLevel}" 12.0 FontWeight.Normal "#FFFFFF")
                          at 430.0 8.0 (styled $"{m.Scenario.Name}【{m.Scenario.Level}】" 14.0 FontWeight.SemiBold "#FFFFFF")
                          at 430.0 33.0 (styled $"気象のシード {m.Seed}" 12.0 FontWeight.Normal "#B8C7D6")
                          at 608.0 18.0 (plate "資金")
                          at 658.0 14.0 (counter 6 balance "")
                          yield! politicsGauge ] ]
          ) ]
    :> IView

// ---------- 模擬盤 (流域の地図。区域ごとに観測計器を並べる) ----------

let private zoneW = 250.0
let private zoneH = 150.0

/// 区域の計器群: 土壌水分計・地下水の水位計 (推定)・持続的揚水量のメーター (推定)。クリックで、その区域を選ぶ
let private zoneCluster (m: Model) dispatch (z: ZoneParams) (x: float) (y: float) =
    let spec = Groundwater.spec m.Scenario z.Id
    let est = m.Game.Estimates[z.Id]
    let soil = m.Game.SoilWU[z.Id]
    let cap = z.SoilCapacityMm * z.AreaKm2
    let ratio = if cap <= 0.0 then 0.0 else soil / cap
    let selected = (m.Selected = z.Id)
    let flagged = m.Game.CostFlags[z.Id]
    let stage = m.Game.Survey[z.Id]
    let gMax = ceil ((snd spec.InitialGRangeWU * 1.3) / 1000.0) * 1000.0
    let yMax = ceil ((snd spec.YieldRangeWU * 1.2) / 500.0) * 500.0
    let lowE, highE = est.GwWU * (1.0 - est.RelErr), est.GwWU * (1.0 + est.RelErr)
    let lowY, highY = est.YieldWU * (1.0 - est.RelErr), est.YieldWU * (1.0 + est.RelErr)

    let planned =
        match m.Action with
        | Survey(az, Reconnaissance) when az = z.Id -> "★概査"
        | Survey(az, TestDrilling) when az = z.Id -> "★試掘"
        | BuildWell az when az = z.Id -> "★井戸"
        | Invest IrrigationEfficiency when z.Id = Farm -> "★灌漑"
        | Invest(ObservationWell az) when az = z.Id -> "★観測井戸"
        | Invest Reservoir when z.Id = Forest -> "★貯水池"
        | Invest ReservoirExpansion when z.Id = Forest -> "★貯水池拡張"
        | Invest IrrigationUpgrade when z.Id = Farm -> "★灌漑拡張"
        | Invest EffluentUpgrade when z.Id = Urban -> "★再生水拡張"
        | Invest RainwaterHarvest when z.Id = Urban -> "★雨水利用"
        | Invest EffluentReuse when z.Id = Urban -> "★再生水"
        | _ -> ""

    let title =
        Border.create
            [ Canvas.left 0.0
              Canvas.top 0.0
              Border.width zoneW
              Border.height 21.0
              Border.background (brush (zoneBase z.Id))
              Border.cornerRadius (CornerRadius(6.0, 6.0, 0.0, 0.0))
              Border.padding (Thickness(8.0, 2.0))
              Border.child (
                  StackPanel.create
                      [ StackPanel.orientation Orientation.Horizontal
                        StackPanel.spacing 8.0
                        StackPanel.children
                            [ styled z.Name 12.0 FontWeight.Bold "#10202D"
                              styled planned 11.0 FontWeight.Bold "#B3261E" ] ]
              ) ]
        :> IView

    let lamps =
        StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 7.0
              StackPanel.children
                  [ lampDot (stage <> Unsurveyed) okLamp "概査"
                    lampDot (stage = TestDrilled) okLamp "試掘"
                    lampDot (m.Game.Wells[z.Id] = WellBuilt) okLamp "井戸"
                    lampDot (m.Game.Invested.Contains(ObservationWell z.Id)) okLamp "観測"
                    lampDot flagged badLamp "警報" ] ]
        :> IView

    let body =
        Canvas.create
            [ Canvas.width zoneW
              Canvas.height zoneH
              Canvas.children
                  [ title
                    at 6.0 24.0 (soilMeter 76.0 ratio)
                    at 88.0 24.0 (levelGauge 56.0 84.0 0.0 gMax est.GwWU [ (0.0, "0"); (gMax, thousands gMax) ] (Some(lowE, highE)) [ (spec.ThresholdWU, "#FF5A4D") ] "#2E7DD1")
                    at 152.0 24.0 (meter 76.0 0.0 yMax est.YieldWU [ (0.0, "0"); (yMax, plainNum yMax) ] [] (Some(lowY, highY)) [ (350.0, warnLamp); (800.0, okLamp) ])
                    at 6.0 102.0 (gaugeLabel 76.0 "土壌水分")
                    at 86.0 102.0 (gaugeLabel 60.0 "地下水")
                    at 152.0 102.0 (gaugeLabel 76.0 "持続量")
                    at 24.0 114.0 (miniCounter 3 (ratio * 100.0))
                    at 86.0 114.0 (miniCounter 5 est.GwWU)
                    at 162.0 114.0 (miniCounter 4 est.YieldWU)
                    at 8.0 134.0 lamps ] ]
        :> IView

    Border.create
        [ Canvas.left x
          Canvas.top y
          Border.width zoneW
          Border.height zoneH
          Border.cornerRadius 6.0
          Border.background (brush "#2A3037")
          Border.borderBrush (brush (if selected then "#FFD54F" else "#4B555F"))
          Border.borderThickness (if selected then 3.0 else 1.0)
          Border.cursor (Cursor(StandardCursorType.Hand))
          Border.onPointerPressed ((fun _ -> dispatch (SelectZone z.Id)), SubPatchOptions.Always)
          Border.child body ]
    :> IView

let private mapView (m: Model) dispatch =
    let zone id = m.Scenario.Zones |> List.find (fun z -> z.Id = id)

    let seg left top width height =
        Border.create
            [ Canvas.left left
              Canvas.top top
              Border.width width
              Border.height height
              Border.cornerRadius 3.0
              Border.background (brush "#3D9BE0") ]
        :> IView

    let marker left top color =
        Border.create
            [ Canvas.left left
              Canvas.top top
              Border.width 18.0
              Border.height 18.0
              Border.cornerRadius 9.0
              Border.background (brush color)
              Border.borderBrush (brush "#FFFFFF")
              Border.borderThickness 2.0 ]
        :> IView

    let tag (x: float) (y: float) (s: string) = at x y (plate s)

    // 森林 -> 農地 -> 市街地 -> 湿地 の順に、左端を縦にまっすぐ流れる (各区域へは短い枝で入る)
    let riverX = 20.0
    let stub (y: float) = seg riverX y 22.0 6.0

    let river =
        [ seg riverX 80.0 6.0 600.0
          stub 80.0
          stub 242.0
          stub 404.0
          stub 592.0 ]

    Canvas.create
        [ Canvas.left 0.0
          Canvas.top headerH
          Canvas.width mapW
          Canvas.height (boardH - headerH)
          Canvas.background (brush "#222A31")
          Canvas.children
              [ yield! river
                yield zoneCluster m dispatch (zone Forest) 42.0 8.0
                yield zoneCluster m dispatch (zone Farm) 42.0 170.0
                yield zoneCluster m dispatch (zone Urban) 42.0 332.0
                yield zoneCluster m dispatch (zone Wetland) 42.0 520.0
                yield marker 14.0 486.0 "#F57C00"
                yield tag 42.0 488.0 "取水点 (配分管理地点)"
                yield marker 14.0 684.0 "#8E44AD"
                yield tag 42.0 684.0 "下流管理地点" ] ]
    :> IView

// ---------- 運転盤 (右の机) ----------

/// 盤の区画 (小さめの見出し帯つき)
let private deskPanel (title: string) (children: IView list) : IView =
    Border.create
        [ Border.background (brush panelColor)
          Border.borderBrush (brush panelEdge)
          Border.borderThickness 1.0
          Border.cornerRadius 5.0
          Border.margin (Thickness(0.0, 0.0, 0.0, 6.0))
          Border.child (
              StackPanel.create
                  [ StackPanel.children
                        [ Border.create
                              [ Border.background (brush "#3A434C")
                                Border.cornerRadius (CornerRadius(4.0, 4.0, 0.0, 0.0))
                                Border.padding (Thickness(8.0, 2.0))
                                Border.child (onDark title 11.0 "#DDE3E8") ]
                          :> IView
                          StackPanel.create
                              [ StackPanel.margin (Thickness(8.0, 6.0))
                                StackPanel.spacing 4.0
                                StackPanel.children children ]
                          :> IView ] ]
          ) ]
    :> IView

let private vcenter (v: IView) =
    Border.create [ Border.verticalAlignment VerticalAlignment.Center; Border.child v ] :> IView

/// 2列に並べる (押しボタンの一覧用)
let private twoCols (items: IView list) : IView =
    StackPanel.create
        [ StackPanel.spacing 4.0
          StackPanel.children
              [ for pair in List.chunkBySize 2 items ->
                    StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 4.0
                          StackPanel.children [ for it in pair -> Border.create [ Border.width 290.0; Border.child it ] :> IView ] ]
                    :> IView ] ]
    :> IView

// ---------- 運転ページ: 読む計器と、直す計器 ----------

let private emptyView = StackPanel.create [] :> IView

let private operationPage (m: Model) dispatch : IView * IView =
    if Turn.isFinished m.Scenario m.Game then
        deskPanel "運転" [ onDark "全季の運転が終了しました。最終報告書は、右下のナイフスイッチで開き直せます。" 12.0 "#E8EDF1" ], emptyView
    else
        let turn = min m.Game.Turn (m.Scenario.Turns - 1)
        let season = Season.ofTurn turn
        let demand = Turn.effectiveAgDemand m.Scenario m.Game season
        let urbanDemand = Turn.effectiveUrbanDemand m.Scenario m.Game turn
        let envMin = m.Scenario.MinEnvFlowWU[season]
        let canSwap = Turn.canChangePriority m.Game.Turn
        let weather = m.Weather[turn].Card
        let lo, hi = weather.PrecipMm
        let outlook = Forecast.outlook m.Scenario m.Game weather

        let policy =
            { AgTargetWU = m.AgTargetWU
              UrbanTargetWU = m.UrbanTargetWU
              EnvTargetWU = m.EnvTargetWU
              Priority = m.Priority
              PumpLimitWU = m.PumpLimits
              ReservoirOpWU = m.ReservoirOp
              Report = m.Report
              Action = m.Action }

        let aLow, aMid, aHigh =
            Forecast.allocation policy outlook.Low, Forecast.allocation policy outlook.Mid, Forecast.allocation policy outlook.High

        let down = [ aLow; aMid; aHigh ] |> List.map _.DownstreamFlowWU
        let downLo, downHi = List.min down, List.max down
        let availLo = Forecast.availableAfterReservoir policy outlook.Low
        let availMid = Forecast.availableAfterReservoir policy outlook.Mid
        let availHi = Forecast.availableAfterReservoir policy outlook.High

        // ---- 読む計器: 降水予報 (貯留型雨量計)・取水点の流量・下流の流量 ----
        let rainMax = ceil (max 400.0 (hi * 1.15) / 100.0) * 100.0
        let flowMax = ceil (max 1000.0 (max (availHi * 1.15) (m.AgTargetWU + m.UrbanTargetWU)) / 1000.0) * 1000.0
        let downMax = ceil (max 1000.0 (max (downHi * 1.2) (envMin * 1.4)) / 1000.0) * 1000.0

        let rainGauge =
            unitBox
                [ levelGauge 64.0 112.0 0.0 rainMax ((lo + hi) / 2.0) (marksOf rainMax 2 plainNum) (Some(lo, hi)) [] "#5AA9E6"
                  plate "降水予報 (mm)"
                  caption $"{mm lo}〜{mm hi}" ]

        let supplyMeter =
            unitBox
                [ meter 124.0 0.0 flowMax availMid (marksOf flowMax 4 thousands) [] (Some(availLo, availHi)) [ (m.AgTargetWU + m.UrbanTargetWU, warnLamp) ]
                  plate "取水点の流量 (WU)"
                  caption $"{wu availLo}〜{wu availHi}" ]

        let downMeter =
            unitBox
                [ meter 124.0 0.0 downMax aMid.DownstreamFlowWU (marksOf downMax 4 thousands) [ (0.0, envMin, "#C94A3F"); (envMin, downMax, "#4F9A57") ] (Some(downLo, downHi)) [ (envMin, "#C94A3F") ]
                  plate "下流の流量 (WU)"
                  caption $"{wu downLo}〜{wu downHi}" ]

        let envLamp =
            if downLo >= envMin then warningLampW 170.0 okLamp "環境流水: 基準を満たす見込み"
            elif downHi < envMin then warningLampW 170.0 badLamp "環境流水: 基準割れの見込み"
            else warningLampW 170.0 warnLamp "環境流水: 降水が少ないと基準割れ"

        let shortfalls =
            [ if aLow.AgSupplyWU < m.AgTargetWU - 1.0 then
                  $"農業 {wu (m.AgTargetWU - aLow.AgSupplyWU)}"
              if aLow.UrbanSupplyWU < m.UrbanTargetWU - 1.0 then
                  $"生活・産業 {wu (m.UrbanTargetWU - aLow.UrbanSupplyWU)}" ]

        let lamps =
            StackPanel.create
                [ StackPanel.spacing 8.0
                  StackPanel.width 180.0
                  StackPanel.children
                      [ yield envLamp
                        if not shortfalls.IsEmpty then
                            let shortText = String.concat " / " shortfalls
                            yield warningLampW 170.0 badLamp $"河川水の不足 (降水が少ない側): {shortText} WU" ] ]
            :> IView

        let readSection = deskPanel "予報と見込み" [ hbox 8.0 [ rainGauge; supplyMeter; downMeter; lamps ] ]

        // ---- 直す計器: 取水バルブ ×2・環境放流設定器・優先順位切替器 ----
        let valveMarks (mx: float) = [ for i in 0..4 -> (mx * float i / 4.0, string (i * 25)) ]

        let agValve =
            unitBox
                [ (if demand > 0.0 then
                       valve "valve-ag" 104.0 0.0 demand 50.0 m.AgTargetWU (valveMarks demand) [] (fun v -> dispatch (SetAgTarget v))
                   else
                       // この季節は農業の需要がないので、バルブは全閉のまま動かない
                       valve "valve-ag" 104.0 0.0 1.0 1.0 0.0 [ (0.0, "0") ] [] (fun _ -> ()))
                  counter 5 m.AgTargetWU ""
                  plate (if demand > 0.0 then $"農業用水\n需要 {wu demand}" else "農業用水\n今季は休止") ]

        let urbanValve =
            unitBox
                [ valve "valve-urban" 104.0 0.0 urbanDemand 50.0 m.UrbanTargetWU (valveMarks urbanDemand) [] (fun v -> dispatch (SetUrbanTarget v))
                  counter 5 m.UrbanTargetWU ""
                  plate $"生活・産業用水\n需要 {wu urbanDemand}" ]

        let envMax = envMin * 2.5

        let envDial =
            unitBox
                [ valve
                      "valve-env"
                      104.0
                      envMin
                      envMax
                      50.0
                      m.EnvTargetWU
                      [ for i in 0..3 -> (envMin + (envMax - envMin) * float i / 3.0, thousands (envMin + (envMax - envMin) * float i / 3.0)) ]
                      [ (envMin, envMax, "#4F9A57") ]
                      (fun v -> dispatch (SetEnvTarget v))
                  counter 5 m.EnvTargetWU ""
                  plate $"環境流水\n最低基準 {wu envMin}" ]

        let prioritySwitch =
            unitBox
                [ rankSelector "rank-priority" (m.Priority |> List.map Allocatee.label) (not canSwap) (fun i d -> dispatch (MovePriority(i, d)))
                  plate (if canSwap then "優先順位切替器" else "優先順位切替器 (固定)") ]

        let intakeSection = deskPanel "河川からの取水" [ hbox 22.0 [ agValve; urbanValve; envDial; vcenter prioritySwitch ] ]

        // ---- 揚水ポンプと貯水池 ----
        let wellZones = m.Scenario.Zones |> List.filter (fun z -> m.Game.Wells[z.Id] = WellBuilt)
        let pumpSize = 96.0

        let pumpUnit (z: ZoneParams) =
            let spec = Groundwater.spec m.Scenario z.Id
            let wellCap = Turn.wellCapacity m.Scenario m.Game z.Id
            let est = m.Game.Estimates[z.Id]
            let limit = m.PumpLimits |> Map.tryFind z.Id |> Option.defaultValue 0.0
            let gwEnd = Forecast.gwEndIfPumpedToLimit m.Scenario m.Game outlook.Mid z.Id limit
            let safe = max 0.0 (min wellCap (est.YieldWU * (1.0 - est.RelErr)))
            let likely = max safe (min wellCap (est.YieldWU * (1.0 + est.RelErr)))
            let flagged = m.Game.CostFlags[z.Id]
            let bands = [ (0.0, safe, "#4F9A57"); (safe, likely, "#E0A526"); (likely, wellCap, "#C94A3F") ]

            let status =
                if flagged then lampDot true badLamp "コスト上昇中"
                elif gwEnd < spec.ThresholdWU then lampDot true warnLamp "閾値割れ注意"
                else lampDot false okLamp "異常なし"

            unitBox
                [ valve $"valve-pump-{z.Id}" pumpSize 0.0 wellCap 50.0 limit [ (0.0, "0"); (wellCap / 2.0, plainNum (wellCap / 2.0)); (wellCap, plainNum wellCap) ] bands (fun v -> dispatch (SetPumpLimit(z.Id, v)))
                  counter 4 limit ""
                  plate $"{z.Name} ポンプ"
                  status ]

        let reservoirUnits =
            match m.Scenario.Reservoir with
            | None -> []
            | Some r when not (m.Game.Invested.Contains Reservoir) -> [ unitBox [ lampDot false okLamp "貯水池 未建設" ] ]
            | Some r ->
                let stored = m.Game.ReservoirWU
                let cap = Turn.reservoirCapacity m.Scenario m.Game

                let flags =
                    match Forecast.reservoir policy outlook.Mid with
                    | Some f -> [ (f.EndWU, warnLamp) ]
                    | None -> []

                [ unitBox
                      [ lever "lever-reservoir" 56.0 112.0 (-stored) cap 50.0 m.ReservoirOp 0.0 "貯める" "放流" (fun v -> dispatch (SetReservoirOp v))
                        counter 5 m.ReservoirOp ""
                        plate "貯水池ゲート" ]
                  unitBox
                      [ levelGauge 64.0 112.0 0.0 cap stored (marksOf cap 2 thousands) None flags "#2E7DD1"
                        miniCounter 5 stored
                        plate "貯水量" ] ]

        let pumpSection =
            deskPanel
                "揚水ポンプ・貯水池"
                (if wellZones.IsEmpty && reservoirUnits.IsEmpty then
                     [ lampDot false okLamp "井戸・貯水池なし" ]
                 else
                     [ yield!
                           [ for pair in List.chunkBySize 2 (wellZones |> List.map pumpUnit) -> hbox 12.0 pair ]
                       if not reservoirUnits.IsEmpty then
                           yield hbox 12.0 reservoirUnits ])

        StackPanel.create [ StackPanel.children [ readSection; intakeSection ] ] :> IView, pumpSection

// ---------- 設備ページ: 主要行動の押しボタンと資金 ----------

/// 押しボタン。操作の効果の説明は、マニュアルに書く
let private actionButton (m: Model) dispatch (a: MajorAction) (label: string) (doneLabel: string option) (blocked: string option) : IView =
    let chosen = (m.Action = a)
    let affordable = Turn.isValidAction m.Scenario m.Game a

    let queued =
        if Construction.isProject m.Scenario a then
            m.Game.Construction.Projects |> List.tryFind (fun (p: Project) -> p.Target = a)
        else
            None

    let text, enabled =
        match doneLabel, blocked with
        | Some d, _ -> $"✓ {d}", false
        | None, _ when queued.IsSome ->
            let p = queued.Value
            let state = if p.Status = Building then $"施工中 (残り {p.Remaining} 季)" else "資材待ち"
            $"◆ {Construction.targetLabel a} — 発注済み: {state}", false
        | None, Some why -> $"{label} — {why}", false
        | None, None ->
            if chosen then $"{label} (押すと取消)", true
            elif not affordable then $"{label} — 資金不足", false
            else label, true

    pushButtonW 250.0 text chosen enabled (fun () -> dispatch (ChooseAction a))

let private equipmentPage (m: Model) dispatch : IView * IView =
    let z = m.Scenario.Zones |> List.find (fun z -> z.Id = m.Selected)
    let costs = m.Scenario.Costs
    let inv = m.Scenario.Invest
    let stage = m.Game.Survey[z.Id]
    let well = m.Game.Wells[z.Id]
    let est = m.Game.Estimates[z.Id]
    let effRunoff = (Turn.effectiveZone m.Scenario m.Game z).RunoffCoef
    let btn = actionButton m dispatch

    // 区域が替わっても並びと位置が変わらないよう、全区域で同じ9個を同じ順に並べる。
    // その区域では実施できないものは、暗いボタンにして理由を書く
    let notHere = Some "この区域では実施できません"
    let isDone a = m.Game.Invested.Contains a

    // 「作成する」と「拡張する」: 作成済みなら、同じ位置のボタンが「拡張」に替わる
    let staged (baseInv: Investment) (upInv: Investment) (here: bool) (baseLabel: string) (baseCost: float) (upLabel: string) (upCost: float) (doneLabel: string) =
        if isDone upInv then btn (Invest upInv) "" (Some doneLabel) None
        elif isDone baseInv then btn (Invest upInv) $"{upLabel} (費用 {wu upCost})" None (if here then None else notHere)
        else btn (Invest baseInv) $"{baseLabel} (費用 {wu baseCost})" None (if here then None else notHere)

    let reservoirButton =
        match m.Scenario.Reservoir with
        | Some r -> staged Reservoir ReservoirExpansion (z.Id = Forest) "貯水池を建設" r.Cost "貯水池を拡張" r.ExpansionCost "貯水池 拡張済み"
        | None -> btn (Invest Reservoir) "貯水池を建設" None (Some "このシナリオでは建てられません")

    let buttons =
        [ btn (Survey(z.Id, Reconnaissance)) $"概査 (費用 {wu costs.ReconCost})" (if stage <> Unsurveyed then Some "概査 実施済み" else None) None
          btn (Survey(z.Id, TestDrilling)) $"試掘・揚水試験 (費用 {wu costs.TestDrillCost})" (if stage = TestDrilled then Some "試掘 実施済み" else None) (if stage = Unsurveyed then Some "先に概査が必要" else None)
          btn (BuildWell z.Id) $"井戸を建設 (費用 {wu costs.WellCost})" (if well = WellBuilt then Some "井戸 建設済み" else None) (if stage = Unsurveyed then Some "先に概査が必要" else None)
          btn (Invest(ObservationWell z.Id)) $"観測井戸を設置 (費用 {wu inv.ObservationCost})" (if isDone (ObservationWell z.Id) then Some "観測井戸 設置済み" else None) None
          staged IrrigationEfficiency IrrigationUpgrade (z.Id = Farm) "灌漑効率の改善" inv.IrrigationCost "灌漑効率を拡張" inv.IrrigationUpgradeCost "灌漑効率 拡張済み"
          btn (Invest RainwaterHarvest) $"雨水利用施設 (費用 {wu inv.RainwaterCost})" (if isDone RainwaterHarvest then Some "雨水利用 実施済み" else None) (if z.Id <> Urban then notHere else None)
          staged EffluentReuse EffluentUpgrade (z.Id = Urban) "再生水の放流" inv.EffluentCost "再生水を拡張" inv.EffluentUpgradeCost "再生水 拡張済み"
          reservoirButton
          btn (Invest WaterSaving) $"節水キャンペーン (費用 {wu inv.WaterSavingCost})" (if isDone WaterSaving then Some "節水キャンペーン 実施済み" else None) None ]

    let funds = Turn.availableFunds m.Scenario m.Game
    let actionCost = Turn.actionCost m.Scenario m.Action
    let ledger =
        deskPanel
            $"{z.Name}  設備台帳"
            [ caption $"面積 {z.AreaKm2:F0} km² / 流出係数 {effRunoff:F2} (開始時 {z.RunoffCoef:F2}) / 土壌保水容量 {z.SoilCapacityMm:F0} mm"
              (if stage <> Unsurveyed then caption $"概査の評価: {Groundwater.rating est.YieldWU}" else caption "概査の評価: —") ]

    let actions =
        deskPanel
            "主要行動 (押しボタン。1ターンに1つ。点灯が選択中)"
            [ twoCols buttons ]

    let finance =
        deskPanel
            "選んだ行動と資金"
            [ hbox 12.0 [ unitBox [ counter 5 actionCost ""; plate "選んだ行動の費用" ]; unitBox [ counter 6 (funds - actionCost) ""; plate "行動後の資金" ] ] ]

    match m.Scenario.Construction with
    | None -> StackPanel.create [ StackPanel.children [ ledger; actions ] ] :> IView, finance
    | Some cfg ->
        let cons = m.Game.Construction

        // 資材の融通 (他地域から借りる): 手数料がかかり、あとで返す
        let transferButton (mat: Material) =
            let a = RequestTransfer mat
            let blocked = if Construction.canTransfer m.Scenario cons then None else Some "借入の上限"
            actionButton m dispatch a $"{Construction.materialLabel mat}を融通 (手数料 {wu cfg.TransferFee})" None blocked

        let transfers =
            deskPanel "資材の融通 (他地域から借りる。すぐ届く)" [ twoCols (Construction.allMaterials |> List.map transferButton) ]

        let stockUnit (mat: Material) =
            unitBox [ miniCounter 2 (float (Construction.stock cons mat)); plate (Construction.materialLabel mat) ]

        let projectRow (p: Project) =
            let what = Construction.targetLabel p.Target

            let state =
                match p.Status with
                | Building -> $"施工中 (残り {p.Remaining} 季)"
                | Waiting ->
                    match Construction.missing cons p.Needs with
                    | [] -> "着工待ち"
                    | lack -> "資材待ち: " + (lack |> List.map (fun (mm, q) -> $"{Construction.materialLabel mm}{q}") |> String.concat "・")

            caption $"{what} — {state}"

        let loanRows =
            cons.Loans
            |> List.map (fun l -> caption $"{Construction.materialLabel l.Material} — {seasonText l.RepayTurn} に返す")

        // 今季のはじめにすでに届いた分は在庫に入っている。これから届く、直近の3件
        let upcoming =
            cfg.Arrivals
            |> List.filter (fun (t, _, _) -> t > m.Game.Turn)
            |> List.sortBy (fun (t, _, _) -> t)
            |> List.truncate 3
            |> List.map (fun (t, mat, q) -> caption $"{seasonText t}: {Construction.materialLabel mat} +{q}")

        let materials =
            deskPanel
                "資材と工事"
                [ hbox 8.0 (Construction.allMaterials |> List.map stockUnit)
                  caption "発注済みの工事"
                  (if cons.Projects.IsEmpty then caption "なし" else StackPanel.create [ StackPanel.children (cons.Projects |> List.map projectRow) ] :> IView)
                  caption "借りている資材"
                  (if loanRows.IsEmpty then caption "なし" else StackPanel.create [ StackPanel.children loanRows ] :> IView)
                  caption "正規配給の予定"
                  (if upcoming.IsEmpty then caption "なし" else StackPanel.create [ StackPanel.children upcoming ] :> IView) ]

        StackPanel.create [ StackPanel.children [ ledger; actions; transfers ] ] :> IView,
        StackPanel.create [ StackPanel.children [ finance; materials ] ] :> IView

// ---------- 指令ページ: 中央からの指令 (計画経済版) ----------

/// 暗い盤の上の表の1行
let private rowDark (cells: (string * float) list) =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.children
              [ for (s, w) in cells ->
                    TextBlock.create
                        [ TextBlock.text s
                          TextBlock.width w
                          TextBlock.fontSize 12.0
                          TextBlock.foreground (brush "#E8EDF1") ]
                    :> IView ] ]
    :> IView

let private headRowDark (cells: (string * float) list) =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.children
              [ for (s, w) in cells ->
                    TextBlock.create
                        [ TextBlock.text s
                          TextBlock.width w
                          TextBlock.fontSize 11.0
                          TextBlock.fontWeight FontWeight.Bold
                          TextBlock.foreground (brush "#AAB4BD") ]
                    :> IView ] ]
    :> IView


/// 紙の書類 (指令書)
let private paperCard (children: IView list) : IView =
    Border.create
        [ Border.background (brush paperColor)
          Border.borderBrush (brush "#C9BE9C")
          Border.borderThickness 1.0
          Border.cornerRadius 4.0
          Border.padding (Thickness(10.0, 8.0))
          Border.margin (Thickness(0.0, 0.0, 0.0, 4.0))
          Border.child (StackPanel.create [ StackPanel.spacing 3.0; StackPanel.children children ]) ]
    :> IView

let private outcomeColor =
    function
    | Achieved -> okColor
    | Partial -> "#8A6D00"
    | Failed -> badColor

let private directivePage (m: Model) dispatch : IView * IView =
    let org = m.Game.Organization
    let sc = m.Scenario
    let cfg = sc.Directives.Value
    let finished = Turn.isFinished sc m.Game
    let turn = min m.Game.Turn (sc.Turns - 1)
    let active = if finished then [] else Organization.active org m.Game.Turn

    // 期間の需要の目安 (今の設備で、期限までの季節の需要を足したもの。できごとは含めない)
    let demandEstimate (d: Directive) =
        [ max turn d.IssuedTurn .. d.DeadlineTurn ]
        |> List.sumBy (fun t ->
            match d.Kind with
            | CottonQuota -> Turn.effectiveAgDemand sc m.Game (Season.ofTurn t)
            | IndustrialSupply -> Turn.effectiveUrbanDemand sc m.Game t
            | BuildOrder -> 0.0)

    let card (d: Directive) =
        let progress = Organization.progress d m.Game.History
        let remaining = d.DeadlineTurn - turn
        let target = d.TargetFill

        let bands = [ (0.0, target * 0.9, "#C94A3F"); (target * 0.9, target, "#E0A526"); (target, 1.0, "#4F9A57") ]

        let gauge =
            unitBox
                [ meter 104.0 0.0 1.0 (progress |> Option.defaultValue 0.0) [ (0.0, "0"); (0.5, "50"); (1.0, "100") ] bands None [ (target, "#B3261E") ]
                  counter 3 ((progress |> Option.defaultValue 0.0) * 100.0) "%"
                  plate "ここまでの充足率" ]

        let reward = sc.Finance.AnnualGrant * cfg.RewardFraction
        let deadlineText = if remaining <= 0 then "今季" else $"あと {remaining} 季"

        // 建設期限の指令: 充足率ではなく、設備の完成 (状況・必要な資材・標準工期) を示す
        let buildBody =
            match d.Target with
            | Some t ->
                let needs, duration = (Construction.needsOf t) |> Option.defaultValue ({ Machinery = 0; BuildMaterial = 0; Engineers = 0 }, 0)
                let state = Organization.buildStateLabel (Organization.buildState d m.Game.History)

                let needText =
                    [ Machinery, needs.Machinery; BuildMaterial, needs.BuildMaterial; Engineer, needs.Engineers ]
                    |> List.filter (fun (_, q) -> q > 0)
                    |> List.map (fun (mat, q) -> $"{Construction.materialLabel mat} {q}")
                    |> String.concat "・"

                StackPanel.create
                    [ StackPanel.spacing 3.0
                      StackPanel.children
                          [ text $"状況: {state}   /   標準工期 {duration} 季 (着工した季節を 1 と数える)"
                            text $"必要な資材: {needText}"
                            styled $"完成: 補助金 {wu reward} (年度の交付金の{pct cfg.RewardFraction}) と、上司の信頼 +{cfg.TrustAchieved:F0}" 12.0 FontWeight.SemiBold okColor
                            styled $"期限までに未着工: 上司の信頼 −{cfg.TrustFailed:F0}" 12.0 FontWeight.SemiBold badColor
                            muted "期限までに着工していて、完成が間に合わない場合は、一部達成で、変化はありません。" ] ]
                :> IView
            | None -> StackPanel.create [] :> IView

        let fillBody =
            StackPanel.create
                [ StackPanel.orientation Orientation.Horizontal
                  StackPanel.spacing 14.0
                  StackPanel.children
                      [ gauge
                        StackPanel.create
                            [ StackPanel.spacing 3.0
                              StackPanel.verticalAlignment VerticalAlignment.Center
                              StackPanel.width 380.0
                              StackPanel.children
                                  [ text $"今の設備での、期限までの需要の目安: {wu (demandEstimate d)} WU"
                                    styled $"達成: 補助金 {wu reward} (年度の交付金の{pct cfg.RewardFraction}) と、上司の信頼 +{cfg.TrustAchieved:F0}" 12.0 FontWeight.SemiBold okColor
                                    styled $"目標の9割未満: 上司の信頼 −{cfg.TrustFailed:F0}" 12.0 FontWeight.SemiBold badColor
                                    muted "目標の9割以上で、目標に届かない場合は一部達成で、変化はありません。" ] ]
                        :> IView ] ]
            :> IView

        let goalLine =
            if d.Kind = BuildOrder then
                text $"目標: 期限の季節までに {directiveGoal d}"
            else
                text $"目標: {directiveGoal d} (期間の需要量に対する供給量。河川水と地下水の合計)"

        paperCard
            [ muted $"指令第{d.Id + 1}号  発令: 中央省庁"
              styled (Organization.kindLabel d.Kind) 15.0 FontWeight.Bold "#1B2A38"
              text $"期間: 第{d.IssuedTurn + 1}季〜第{d.DeadlineTurn + 1}季 (期限まで{deadlineText})"
              goalLine
              (if d.Kind = BuildOrder then buildBody else fillBody) ]

    let current =
        deskPanel
            "適用中の指令"
            (if active.IsEmpty then
                 [ lampDot false okLamp "適用中の指令はありません" ]
             else
                 active |> List.map card)

    // 右の列: 計画達成度と、判定済みの指令の結果
    let results = org.Resolved
    let count o = results |> List.filter (fun r -> r.Outcome = o) |> List.length

    let achievementPanel =
        deskPanel
            "計画達成度"
            [ unitBox
                  [ meter 104.0 0.0 1.0 (Organization.achievement org |> Option.defaultValue 0.0) [ (0.0, "0"); (0.5, "50"); (1.0, "100") ] [ (0.0, 0.5, "#C94A3F"); (0.5, 0.8, "#E0A526"); (0.8, 1.0, "#4F9A57") ] None []
                    counter 3 ((Organization.achievement org |> Option.defaultValue 0.0) * 100.0) "%"
                    plate "計画達成度" ]
              (if results.IsEmpty then
                   caption "まだ判定された指令はありません"
               else
                   caption $"判定済み {results.Length} 件: 達成 {count Achieved} / 一部達成 {count Partial} / 未達 {count Failed}") ]

    let resultRows =
        deskPanel
            "判定済みの指令"
            (if results.IsEmpty then
                 [ caption "—" ]
             else
                 [ for r in results |> List.rev |> List.truncate 8 ->
                       let goalText = if r.Directive.Kind = BuildOrder then "完成" else pct r.Directive.TargetFill
                       styled $"第{r.Directive.Id + 1}号 {Organization.targetLabel r.Directive.Kind} {measuredText r.Directive r.Measured} / 目標 {goalText}  {Organization.outcomeLabel r.Outcome}" 11.0 FontWeight.SemiBold (outcomeColor r.Outcome) ])

    // ---- 年次報告: 実際の判定 (実値) と、報告する判定 (報告値) を並べる。冬の季節に提出する ----
    let isReportTurn = not finished && Season.ofTurn m.Game.Turn = Winter
    let pending = Organization.pendingReport org
    // 今季が期限の指令は、今季の結果が出るまで判定できない
    let dueNow = if finished then [] else active |> List.filter (fun d -> d.DeadlineTurn = m.Game.Turn)
    let previewItems = Organization.reportItems sc cfg m.Report pending

    let outcomeText (o: DirectiveOutcome option) =
        match o with
        | Some x -> Organization.outcomeLabel x
        | None -> "報告から外す"

    // 虚偽の報告 1 件あたりの整合性リスクの増加 (報告した判定が実際と違う分)
    let riskOfItem (i: ReportItem) =
        match i.Reported with
        | None -> Organization.riskConceal
        | Some o when o <> i.Actual -> Organization.riskOptimistic
        | _ -> 0.0

    let totalRisk =
        if m.Report = Accurate then
            (if previewItems.IsEmpty then 0.0 else -Organization.riskDecayAccurate)
        else
            previewItems |> List.sumBy riskOfItem

    let reportRows =
        [ yield headRowDark [ "指令", 56.0; "実際の判定", 100.0; "報告する判定", 130.0; "信頼", 60.0; "補助金", 70.0; "リスク", 60.0 ]
          for i in previewItems ->
              let changed = Organization.isMisreport i
              rowDark
                  [ $"第{i.DirectiveId + 1}号", 56.0
                    Organization.outcomeLabel i.Actual, 100.0
                    (if changed then $"{outcomeText i.Reported} (虚偽)" else outcomeText i.Reported), 130.0
                    signed i.TrustGain, 60.0
                    signed i.RewardGain, 70.0
                    signed (riskOfItem i), 60.0 ]
          if not previewItems.IsEmpty then
              yield rowDark [ "合計", 56.0; "", 100.0; "", 130.0; signed (previewItems |> List.sumBy _.TrustGain), 60.0; signed (previewItems |> List.sumBy _.RewardGain), 70.0; signed totalRisk, 60.0 ]
          for d in dueNow ->
              rowDark [ $"第{d.Id + 1}号", 56.0; "今季の結果で判定", 130.0; "判定のあと、選んだ方針で報告", 250.0 ]
          if previewItems.IsEmpty && dueNow.IsEmpty then
              yield caption "報告する指令は、まだありません" ]

    let stanceButton (stance: ReportStance) =
        Border.create
            [ Border.width 186.0
              Border.child (pushButtonW 150.0 (Organization.stanceLabel stance) (m.Report = stance) isReportTurn (fun () -> dispatch (SetReport stance))) ]
        :> IView

    let reportPanel =
        deskPanel
            "年次報告 (冬の季節に提出。効き方は、マニュアルの「年次報告」「整合性リスクと監査」)"
            [ yield! reportRows
              yield hbox 4.0 [ stanceButton Accurate; stanceButton Optimistic; stanceButton Conceal ]
              if not isReportTurn then
                  yield lampDot false okLamp "報告は、冬の季節に提出します (今は選べません)" ]

    // 右の列: 年次報告・監査の記録
    let reportLog =
        deskPanel
            "報告と監査の記録"
            (if org.Reports.IsEmpty && org.Audits.IsEmpty then
                 [ caption "まだ提出した報告はありません" ]
             else
                 [ for r in org.Reports ->
                       let lies = r.Items |> List.filter Organization.isMisreport |> List.length
                       let ids = r.Items |> List.map (fun i -> $"{i.DirectiveId + 1}") |> String.concat "・"
                       let note = if lies = 0 then "" else $" / 虚偽 {lies} 件"
                       styled $"{seasonText r.Turn} {Organization.stanceLabel r.Stance} (第{ids}号){note}" 11.0 FontWeight.SemiBold (if lies = 0 then "#C5CDD4" else "#FFB74D")
                   for a in org.Audits ->
                       styled
                           (if a.Exposed.IsEmpty then
                                $"{seasonText a.Turn} 監査: 問題なし (信頼 {signed a.TrustChange})"
                            else
                                $"{seasonText a.Turn} 監査: 虚偽が露見 (信頼 {signed a.TrustChange}、予算 −{wu a.Fine})")
                           11.0
                           FontWeight.SemiBold
                           (if a.Exposed.IsEmpty then "#7BD88F" else "#FF6B6B") ]
             @ [ if not org.Misreports.IsEmpty then
                     yield caption $"露見していない虚偽の報告: {org.Misreports.Length} 件" ])

    StackPanel.create [ StackPanel.children [ current; reportPanel ] ] :> IView,
    StackPanel.create [ StackPanel.children [ achievementPanel; resultRows; reportLog ] ] :> IView

// ---------- 評価・財政ページ ----------

let private evaluationPage (m: Model) dispatch : IView * IView =
    // 計画経済版では、流域の実態・計画達成度・政治的評価を、混ぜずに別々に出す
    let evalTitle = if m.Scenario.Directives.IsSome then "流域の実態 (ここまでの評価・見込み)" else "ここまでの評価 (見込み)"
    let e = m.Scenario.Eval

    let evalPanel =
        match Evaluation.preview m.Scenario m.Game with
        | None -> deskPanel evalTitle [ caption "最初の運転のあとから表示されます" ]
        | Some(indicators, score, rank) ->
            let shortOf (i: Indicator) = i.Name.Replace("の充足度", "").Replace("の達成度", "").Replace("の残存", "").Replace("の健全性", "")

            let mini (i: Indicator) =
                unitBox
                    [ meter 104.0 0.0 1.0 i.Score [ (0.0, "0"); (0.5, "50"); (1.0, "100") ] [ (0.0, 0.5, "#C94A3F"); (0.5, 0.8, "#E0A526"); (0.8, 1.0, "#4F9A57") ] None []
                      counter 3 (i.Score * 100.0) "%"
                      plate $"{shortOf i} ×{i.Weight:F1}" ]

            deskPanel
                evalTitle
                [ hbox 2.0 (indicators |> List.map mini)
                  hbox 14.0
                      [ unitBox [ counter 3 (score * 100.0) "%"; plate "総合" ]
                        unitBox [ onDark (Evaluation.rankLabel rank) 28.0 "#FFE9A8"; plate "ランク" ] ] ]

    let totals = m.Game.CostTotals

    let costPanel =
        deskPanel
            "累計の費用"
            [ hbox 10.0
                  [ unitBox [ counter 5 totals.Survey ""; plate "調査" ]
                    unitBox [ counter 5 totals.Build ""; plate "建設" ]
                    unitBox [ counter 5 totals.Invest ""; plate "投資" ]
                    unitBox [ counter 5 totals.Maintenance ""; plate "維持" ]
                    unitBox [ counter 5 totals.Pumping ""; plate "揚水" ] ] ]

    let scenarioPanel =
        deskPanel
            $"シナリオ: {m.Scenario.Name}【{m.Scenario.Level}】"
            [ caption m.Scenario.Focus
              (if m.Scenario.UrbanDemandGrowthPerYear > 0.0 || m.Scenario.UrbanRunoffGrowthPerYear > 0.0 then
                   caption $"市街地の拡大: 需要 年+{pct m.Scenario.UrbanDemandGrowthPerYear} / 流出係数 年+{m.Scenario.UrbanRunoffGrowthPerYear:F2}"
               else
                   caption "")
              (if m.Game.Turn = 0 && m.Overlay = NoOverlay then
                   pushButton "シナリオを選び直す (第1季のみ)" false true (fun () -> dispatch OpenMenu)
               else
                   caption "") ]

    StackPanel.create [ StackPanel.children [ evalPanel; costPanel ] ] :> IView, scenarioPanel

// ---------- マニュアル (用語集をふくむ。計器の側には説明を置かず、ここに集める) ----------

let private manualEntries (m: Model) : (string * string) list =
    let sc = m.Scenario
    let inv = sc.Invest
    let costs = sc.Costs

    let constructionEntries =
        match sc.Construction with
        | None -> []
        | Some cfg ->
            let needs (a: MajorAction) =
                match Construction.needsOf a with
                | Some(n, d) ->
                    let parts =
                        [ Machinery, n.Machinery; BuildMaterial, n.BuildMaterial; Engineer, n.Engineers ]
                        |> List.filter (fun (_, q) -> q > 0)
                        |> List.map (fun (mat, q) -> $"{Construction.materialLabel mat} {q}")
                        |> String.concat "・"

                    $"{Construction.targetLabel a}: {parts}、工期 {d} 季"
                | None -> ""

            let table =
                [ BuildWell Farm
                  Invest(ObservationWell Farm)
                  Invest Reservoir
                  Invest ReservoirExpansion
                  Invest IrrigationEfficiency
                  Invest IrrigationUpgrade
                  Invest RainwaterHarvest
                  Invest EffluentReuse
                  Invest EffluentUpgrade ]
                |> List.map needs
                |> String.concat " / "

            [ "資材と建設 (入門を除く)", "工事 (井戸・観測井戸・設備投資) は、発注した季節には完成しません。必要な資材がそろうと着工し、標準工期のあとに完成します。資材は 3 区分で、機械設備 (ポンプ・主要機械)・建設資材 (鉄管・セメントなど)・専門技術者 (工事のあいだ、その工事につきっきりになり、完成すると戻ります) です。費用は、発注のときに払います。資金があっても、資材がなければ着工しません。複数の工事を発注した場合は、発注の早い順に、必要な資材がそろったものから着工します。着工した季節を標準工期の 1 季節目と数え、完成した季節の次の季節から効果が出ます。調査と節水キャンペーンは、資材を使わず、すぐに効きます。"
              "工事に必要な資材と工期", $"{table}。"
              "資材の正規配給", "開始時の在庫と、これからの配給の予定は、設備ページの「資材と工事」で見ます。機械設備と建設資材は定期的に、専門技術者は数季節に 1 人ずつ増えます。配給は、日程どおりに届き、乱数は使いません。"
              "資材の融通", $"他地域から、資材を 1 単位、すぐに借りられます (設備ページの押しボタン。1ターンに1つの主要行動です)。手数料は {wu cfg.TransferFee}。借りた資材は、{cfg.TransferRepayAfter} 季節あとに、正規配給から差し引かれます。同時に借りられるのは {cfg.MaxLoans} 単位までです。資材の足りない季節の工事を急ぐための手段ですが、将来の在庫を先に使うことになります。"
              "建設期限の指令", "指令の中に、設備の完成を求める建設期限の指令が届くことがあります。期限までに完成していれば達成、着工していて完成が間に合わなければ一部達成、期限までに着工できていなければ未達です。資材の配給の日程を見て、早めに発注してください。" ]

    [ "運転の流れ", "①ヘッダーの気象カードと、運転ページの予報メーターを見る → ②バルブ・ダイヤル・スイッチ・ポンプ・レバーで方針を決める。必要なら設備ページの押しボタンで主要行動を選ぶ → ③右下のナイフスイッチ「運転開始」を押す → ④運転日報で、計画と実績、なぜそうなったかを確認する。"
      "操作のしかた", "バルブ・ダイヤル・スイッチ・レバーは、盤面の好きな位置をクリックするとその位置になります (ドラッグでも動かせます)。値は、計器の下の桁カウンタで読みます。押しボタンは、点灯が「選択中」で、もう一度押すと取り消せます。"
      "予報のメーターと表示灯", "降水予報 (雨量計の水位)・取水点の流量・下流の流量の3つ。青い印 (かっこや弧) が、予報の降水の下限〜上限から見込んだ範囲で、針や水位が中央の見込みです。取水点の流量メーターの黄の針は取水目標の合計、下流の流量メーターの赤の針は環境流水の最低基準です。見込みは、見えている土壌水分・地下水の推定から計算した目安で、実績とはずれることがあります。「河川水の不足」の表示灯は、降水が少ない側の予報で、取水の目標に河川水だけでは足りない量です (揚水ポンプで地下水から補える分は引いていません)。"
      "取水バルブ", "河川から取水して供給したい量を決めます。文字盤の数字は、今季の需要に対する開度 (%) で、右端 (100) が需要どおりです。開度を下げると、その分の水が川に残り、下流の流量や、後ろの順位の用途に回ります。河川水で足りない分は、井戸があれば、揚水ポンプで地下水から補います。設定は、需要に対する割合として、次の季節に引き継がれます (季節ごとに需要が変わるので、引き継がれた開度のカウンタの値は、前の季節と違うことがあります)。"
      "環境流水の設定器", "取水したあとに下流の管理地点に残したい流量の目標です。左端が最低基準で、それより下げることはできません。最低基準を下回ると、河川環境が傷みます。上げても、実際の流量は、取水と優先順位、降水で決まります。"
      "優先順位切替器", "水が足りないときに、どの用途から先に河川水を確保するかを決めます (各段の ▲▼ の押し釦で、その項目を1つ上げ下げします。第1位が最も優先)。環境流水は、取水したあとに下流へ残す水で、順位が高いほど先に確保され、他の用途が使える水が減ります。切り替えられるのは1年に1回、春だけで、その年の4季節で続きます。"
      "揚水ポンプ", "井戸で1季節に汲む量の上限を決めます。揚水は、河川水で足りない農業・生活産業の用水にだけ使われ、優先順位の順に届けられます。文字盤の色帯は、推定の持続的揚水量に対して、緑=推定の下限以下 (確実に安全。下限は、推定値から誤差の幅を引いたもの)、黄=推定の下限〜上限 (不確かな領域)、赤=上限より上 (確実に超過) です。調査や観測井戸で、黄の帯が狭まります。ランプは、揚水コストが上がっている (赤)、季節末の貯留量が閾値を下回りそう (黄) を表します。持続的揚水量を超える揚水が積み重なる、または貯留量が閾値を下回ると、その区域の揚水コストが恒久的に上がり、井戸の能力も半分になります。"
      "貯水池ゲートと水位計", "ゲートのレバーを上へ動かすと河川水を貯め (取水点と下流に回る水が減る)、下へ動かすと貯めた水を放流します (取水点の水が増える)。黄の印が中立です。水位計の黄の線は、今のレバーのままなら季節末にどの水位になるかの見込みです。貯めた水は、季節ごとに蒸発で目減りします (夏が大きい)。雨が多く水が余る季節に貯め、足りない季節に放流するのが基本です。"
      "区域の計器 (地図)", "各区域に、土壌水分計・地下水の水位計・持続的揚水量のメーターがあります。土壌水分計は、保水容量に対する割合で、DRY (乾燥・赤)・MOIST (適湿・緑)・WET (湿潤・青) の帯を指します (実際の観測では、探針式の水分計やテンシオメータが使われます)。降水はまず土壌にしみ込み、土壌がいっぱいになるとあふれた分が地下水を涵養します。土壌が乾いているほど、雨は土壌に吸われ、地下水の涵養は少なくなります。土壌水分は、用水の供給元ではありません。数字の単位は WU (持続的揚水量は WU/季節) です。地下水の水位計の青いかっこは推定の幅で、実際の値はこの範囲のどこかにあります。赤い線は閾値です。調査や観測井戸で幅が狭まります。持続的揚水量のメーターの黄の針は 350、緑の針は 800 で、概査の評価の境目です (800 以上=有望、350 以上=中程度、それ未満=乏しい)。ランプは、概査・試掘の実施、井戸、観測井戸、揚水コスト上昇の警報を表します。"
      "評価のメーター", $"終了まで続けた場合の最終評価の目安です。総合点は、重み付き平均 (0.6) と最も低い指標 (0.4) の混合で、どれかを犠牲にすると高ランクになれません。地下水は見えている推定の値で、財政は今のペースが続いた場合の最終残高で見込んでいます (最終残高がこのシナリオの基準 {wu sc.Eval.TargetBalance} 以上なら満点、基準未満は比例して下がり、一度でもマイナスになると 0.2 引かれます。投資や建設で残高が減るので、途中では 0%% に近くなることもあります)。重みは、各メーターの名前の横の ×(数字) で、シナリオごとに異なります。農業・生活産業の充足度は、全ターンの供給量の合計を、需要量の合計で割ったもので、需要の大きい季節 (農業は夏) ほど、結果に響きます。"
      "指令 (入門を除く)", "中央から、最初のターンから、2〜3ターンおきに指令が届きます (期限は、次の指令が届く直前です)。種類は、生産ノルマ (綿花の増産: 農業用水の充足率) と、工業用水の供給 (生活・産業用水の充足率) です。目標は、期間の需要量に対する供給量 (河川水と地下水の合計) の割合で、期限のターンの結果で判定します。内容と進み具合は、指令ページで見ます。届いた指令と、来季の予告は、地図の下の表示灯にも出ます。指令を達成するために水を多く供給すると、環境流水や地下水が犠牲になることがあります。"
      "指令の判定・上司の信頼・交付金", $"目標以上=達成: 補助金 (年度の交付金の {pct (match sc.Directives with Some c -> c.RewardFraction | None -> 0.0)}) と、上司の信頼 +{(match sc.Directives with Some c -> c.TrustAchieved | None -> 0.0):F0}。目標の9割以上で届かない=一部達成: 変化なし。9割未満=未達: 上司の信頼 −{(match sc.Directives with Some c -> c.TrustFailed | None -> 0.0):F0}。信頼は 0〜100 で、始まりは 50 です (ヘッダーのメーター)。年度の交付金は、信頼 50 で標準の額、信頼 0 で 20%% 減り、信頼 100 で 20%% 増えます。"
      "上司の信頼・監査リスクのメーター", "ヘッダーのメーターです。上司の信頼は、0〜30 が赤、30〜70 が黄、70 以上が緑。監査リスクは、0〜30 が緑、30〜60 が黄、60 以上が赤で、白い印が 60 (監査の予兆が出る値) です。"
      "年次報告 (入門を除く)", "冬の季節 (年の最後) に、その年に期限が来た指令の結果を、中央へ報告します。方針は、指令ページの3つの押しボタンで選びます。「正確に報告」は、実際の判定のとおりに報告し、整合性リスクが下がります。「前向きに報告」は、判定を一段よく見せます (未達→一部達成、一部達成→達成)。「重大な問題を隠す」は、未達の指令を報告から外します。前向きな報告・隠す報告は、報告上は、信頼と補助金が、実際より増えますが、整合性リスクが上がります (前向き +15、隠す +30、1件あたり)。報告する指令の、実際の判定と、報告する判定は、指令ページの表で、並べて確認できます。"
      "整合性リスクと監査", "整合性リスク (ヘッダーの監査リスク) は、虚偽の報告で上がり、虚偽の報告が残っているあいだは、毎季節、1件につき +3 ずつ上がり続けます (記録や観測との矛盾が積み上がるためです)。残っていなければ、毎季節 −3、正確な報告で −10 です。60 に達すると、監査の予兆が出て、次の季節の終わりに監査が入ります。監査は、乱数を使わず、この規則だけで決まります (運には左右されません)。虚偽の報告が残っていれば露見し、報告で得た信頼と補助金が取り消され、さらに信頼が 15 下がり、予算が交付金の半分削減されます。残っていなければ、問題なしで、信頼が少し上がります。ゲームの終了時にも、虚偽の報告が残っていて、整合性リスクが 60 以上なら、監査が入ります。60 に届かなければ、虚偽は露見しないまま終わります (最終報告書に、露見しなかった虚偽の報告として出ます)。"
      "三つの評価", "計画経済版では、流域の実態 (5つの指標の総合ランク)、計画達成度 (判定済みの指令の達成率の平均。充足率÷目標で、上限 100%)、政治的評価 (上司の信頼) を、混ぜずに別々に評価します。どれを優先するかは、プレイヤーが決めます。"
      "資金と収入", $"ヘッダーの「資金」は、残高に、春は交付金 (今年度の分) を加えたもので、その季節に自由に使える額です (運転日報の間は、その季節の期末残高)。収入の単価は、農業 {sc.Finance.AgRevenuePerWU:F2}・生活産業 {sc.Finance.UrbanRevenuePerWU:F2} (供給 1 WU あたり)。費用は、揚水 {costs.PumpCostPerWU:F2}/WU (持続量の超過・コスト上昇後は割増)、井戸の維持 {sc.Finance.WellMaintenance:F0}/季節です。"
      "主要行動 (1ターンに1つ)",
      (if sc.Construction.IsSome then
           "調査・井戸建設・投資・政策・資材の融通は、1ターンに1つまで。区域をクリックすると、設備ページでその区域の押しボタンを選べます。井戸と投資は、発注すると工事になり、資材がそろって着工し、工期が終わったあとの「次の季節」から効きます (下の「資材と建設」)。"
       else
           "調査・井戸建設・投資・政策は、1ターンに1つまで。区域をクリックすると、設備ページでその区域の押しボタンを選べます。井戸と投資は、実施した季節の終わりに完成し、「次の季節」から効きます。")
      $"概査 (費用 {wu costs.ReconCost})", $"地下水の推定の幅を ±{pct costs.ReconRelErr} に狭めます。井戸の建設には概査が必要です。"
      $"試掘・揚水試験 (費用 {wu costs.TestDrillCost})", $"推定の幅を ±{pct costs.TestDrillRelErr} まで狭めます。"
      $"井戸を建設 (費用 {wu costs.WellCost})", $"完成した次の季節から揚水できます。能力は区域ごとに決まっています。維持費は 1 本あたり {wu sc.Finance.WellMaintenance}/季節です。"
      $"灌漑効率の改善 (農地、費用 {wu inv.IrrigationCost}) と拡張 (費用 {wu inv.IrrigationUpgradeCost})", $"改善 (水路の改修) で、農業の正味需要が {pct inv.IrrigationDemandReduction} 減ります (同じ生産に必要な水が減る)。改善のあとで、1度だけ拡張 (高効率の灌漑方式) でき、減る割合は合計 {pct inv.IrrigationUpgradeReduction} になります。"
      $"観測井戸 (費用 {wu inv.ObservationCost})", $"完成した次の季節から、季節ごとに、その区域の推定の幅が {pct inv.ObservationShrink} ずつ狭まります (下限 ±{pct inv.ObservationFloor})。観測のある区域は、水位を見ながら効率よく運転できるので、揚水費が {pct inv.ObservationPumpSaving} 下がります。"
      $"雨水利用施設 (市街地、費用 {wu inv.RainwaterCost})", $"生活・産業用水の需要が {pct inv.RainwaterDemandReduction} 減ります。"
      $"再生水の放流 (市街地、費用 {wu inv.EffluentCost}) と拡張 (費用 {wu inv.EffluentUpgradeCost})", $"河川水で供給した生活・産業用水の {pct inv.EffluentReturnRate} が下流へ戻り、下流の流量を支えます (環境流水の予約には数えません)。放流のあとで、1度だけ拡張 (処理能力の増強) でき、戻る割合が {pct inv.EffluentUpgradeReturnRate} になります。"
      $"節水キャンペーン (費用 {wu inv.WaterSavingCost})", $"農業・生活産業の需要が、どちらも {pct inv.WaterSavingDemandReduction} 減ります。"
      (match sc.Reservoir with
       | Some r -> $"貯水池 (上流山地・森林、建設 {wu r.Cost} / 拡張 {wu r.ExpansionCost})", $"取水点の上流に、容量 {wu r.CapacityWU} WU の貯水池をつくります。建設のあとで、1度だけ拡張でき、容量が {wu r.ExpandedCapacityWU} WU になります。操作は、運転ページの貯水池ゲートです。"
       | None -> "貯水池", "このシナリオでは建てられません。")
    ]
    @ constructionEntries
    @ [ "WU", "水量の単位。1 WU = 1,000 m³ (面積 1 km² に 1 mm の雨が降った量)。"
        "水収支", "降水 = 蒸発散 + 用水供給 + 下流端からの流出 + 貯留の増減 (土壌・地下水・貯水池)。毎季節、差がほぼ 0 になることを確認しています。"
        "蒸発散", "地面や植物から空気中へ出ていく水。気象カードの「蒸発散需要」が高いほど、多くが失われます。"
        "流出係数", "降水のうち、地面にしみ込まず、地表を流れて川に出る割合。舗装が多いほど高くなります。"
        "土壌保水容量", "土壌がためておける水の深さ (mm)。いっぱいになると、あふれた分が地下水へ向かいます。"
        "浸透・深部排水(涵養)", "地面にしみ込んだ水のうち、土壌から地下水へ落ちる分。この季節の涵養は、季節の終わりに地下水に加わります。"
        "湧出", "地下水から川へしみ出す水。貯留量の一定割合で、次の季節の川の流れを支えます。"
        "地下水貯留量(ストック)", "涵養と揚水と湧出の差が積み重なった量。使いすぎると、すぐには戻りません。"
        "持続的揚水量", "地下水を減らさずに、毎季節汲み続けられる量の目安。真の値は隠れていて、調査で推定が絞られます。"
        "閾値・揚水コスト上昇", "地下水がこの量を下回ると、その区域の揚水費が恒久的に上がります (回復しても戻らない)。井戸のある区域で判定されます。"
        "河川から地下水への浸透", "地下水位が低い区域では、川の水が地下水に吸われます (農地・市街地)。吸われた水は地下水に加わるので、地下水は少し回復しますが、取水点に届く川の水は減ります。地下水を汲みすぎると、川の流れも減るということです。"
        "取水点(配分管理地点)", "農業・生活産業の取水と、環境流水の配分を管理する地点。市街地の下流端です。"
        "環境流水", "取水したあとに、下流の管理地点を流れる量。最低基準を下回ると、河川環境が傷みます。"
        "充足率", "需要に対して供給できた割合 (河川水 + 地下水)。"
        "できごと", "季節のはじめに予告される出来事 (交付金の減額、停電など)。その季節だけ効きます。予告と今季のできごとは、地図の下の表示灯に出ます。"
        "総合ランク", "5つの指標の重み付き平均 (0.6) と、最も低い指標 (0.4) の混合です。どれかを犠牲にすると高ランクになれません。" ]

/// 項目を、ページの高さの目安に収まるように分ける
let private paginate (limit: float) (heightOf: 'a -> float) (items: 'a list) : 'a list list =
    let rec go pages current used =
        function
        | [] -> List.rev (if List.isEmpty current then pages else List.rev current :: pages)
        | x :: rest ->
            let h = heightOf x

            if not (List.isEmpty current) && used + h > limit then
                go (List.rev current :: pages) [ x ] h rest
            else
                go pages (x :: current) (used + h) rest

    go [] [] 0.0 items

let private manualPage (m: Model) dispatch : IView =
    let entryHeight (_: string, body: string) = 18.0 + float (body.Length / 74 + 1) * 15.0 + 8.0
    let pages = manualEntries m |> paginate 540.0 entryHeight
    let n = pages.Length
    let p = max 0 (min (n - 1) m.ManualPage)

    let entry (title: string, body: string) =
        StackPanel.create [ StackPanel.spacing 2.0; StackPanel.children [ onDark title 13.0 "#F3F0E6"; caption body ] ] :> IView

    let nav =
        hbox 10.0
            [ pushButtonW 60.0 "◀ 前へ" false (p > 0) (fun () -> dispatch (SetManualPage(p - 1)))
              vcenter (plate $"{p + 1} / {n}")
              pushButtonW 60.0 "次へ ▶" false (p < n - 1) (fun () -> dispatch (SetManualPage(p + 1))) ]

    StackPanel.create
        [ StackPanel.children
              [ Border.create [ Border.height 540.0; Border.child (deskPanel $"マニュアル ({p + 1}/{n})" [ for e in pages[p] -> entry e ]) ]
                nav ] ]
    :> IView

// ---------- 運転盤の下: 表示灯 (できごと) とヒント ----------

let private notices (m: Model) : IView =
    let finished = Turn.isFinished m.Scenario m.Game

    let hint =
        match m.Scenario.Hints |> Map.tryFind m.Game.Turn with
        | Some h when not finished ->
            [ Border.create
                  [ Border.background (brush "#FFF8E1")
                    Border.borderBrush (brush "#FFCA28")
                    Border.borderThickness 1.0
                    Border.cornerRadius 6.0
                    Border.padding (Thickness(8.0, 6.0))
                    Border.child (styled h 11.5 FontWeight.Normal "#10202D") ]
              :> IView ]
        | _ -> []

    let lamps =
        [ if not finished then
              match m.Game.Events |> Map.tryFind m.Game.Turn with
              | Some ev -> yield warningLampW 540.0 warnLamp $"今季のできごと: {ev.Name} — {ev.Description}"
              | None -> ()

              match m.Game.Events |> Map.tryFind (m.Game.Turn + 1) with
              | Some ev when m.Game.Turn + 1 < m.Scenario.Turns -> yield warningLampW 540.0 okLamp $"来季の予告: {ev.Name} — {ev.Description}"
              | _ -> ()

              // 監査の予兆 (整合性リスクが閾値に達した) と、年次報告の季節
              if m.Scenario.Directives.IsSome && m.Game.Organization.AuditDue then
                  yield warningLampW 540.0 badLamp $"監査の予兆: 整合性リスクが閾値 ({Organization.auditThreshold:F0}) に達しました。今季の終わりに監査が入ります"

              if m.Scenario.Directives.IsSome && Season.ofTurn m.Game.Turn = Winter && m.RightTab <> 2 then
                  yield warningLampW 540.0 warnLamp $"年次報告の季節です。指令ページで報告方針を選んでください (今の選択: {Organization.stanceLabel m.Report})"

              // 計画指令: 今季はじめに届いた指令と、来季の予告 (詳細は指令ページ)
              if m.Scenario.Directives.IsSome then
                  for d in Organization.active m.Game.Organization m.Game.Turn do
                      if d.IssuedTurn = m.Game.Turn && m.RightTab <> 2 then
                          yield warningLampW 540.0 warnLamp $"新しい指令が届きました: 第{d.Id + 1}号 {Organization.kindLabel d.Kind} ({goalShort d}。指令ページ)"

                  for d in Organization.active m.Game.Organization m.Game.Turn do
                      if d.DeadlineTurn = m.Game.Turn && d.IssuedTurn <> m.Game.Turn && m.RightTab <> 2 then
                          yield warningLampW 540.0 warnLamp $"今季が期限の指令: 第{d.Id + 1}号 {Organization.kindLabel d.Kind} ({goalShort d})"

                  for d in Organization.upcoming m.Game.Organization m.Game.Turn do
                      yield warningLampW 540.0 okLamp $"来季の指令の予告: 第{d.Id + 1}号 {Organization.kindLabel d.Kind} ({goalShort d})" ]

    let lampPanel =
        if lamps.IsEmpty then
            []
        else
            [ Border.create
                  [ Border.background (brush panelColor)
                    Border.borderBrush (brush panelEdge)
                    Border.borderThickness 1.0
                    Border.cornerRadius 5.0
                    Border.padding (Thickness(8.0, 6.0))
                    Border.child (StackPanel.create [ StackPanel.spacing 4.0; StackPanel.children lamps ]) ]
              :> IView ]

    StackPanel.create [ StackPanel.spacing 6.0; StackPanel.children (lampPanel @ hint) ] :> IView

// ---------- 机の全体: タブ・ページ・ナイフスイッチ ----------

let private desk (m: Model) dispatch : IView =
    // 入門には、指令のタブがない
    let tabs =
        [ yield 0, "運転"
          yield 1, "設備"
          if m.Scenario.Directives.IsSome then
              yield 2, "指令"
          yield 3, "評価・財政"
          yield 4, "マニュアル" ]

    let tabW = 864.0 / float tabs.Length

    let tab (k: int) (i: int, label: string) =
        Border.create
            [ Canvas.left (8.0 + float k * tabW)
              Canvas.top 6.0
              Border.width (tabW - 4.0)
              Border.child (pushButtonW (tabW - 56.0) label (m.RightTab = i) true (fun () -> dispatch (SelectTab i))) ]
        :> IView

    // 左の列: 各ページの本体と、その下の表示灯・ヒント / 右の列: 各ページの付属の区画
    let body =
        if m.RightTab = 4 then
            Border.create [ Border.width 864.0; Border.child (manualPage m dispatch) ] :> IView
        else
            let left, right =
                match m.RightTab with
                | 0 -> operationPage m dispatch
                | 1 -> equipmentPage m dispatch
                | 2 when m.Scenario.Directives.IsSome -> directivePage m dispatch
                | _ -> evaluationPage m dispatch

            StackPanel.create
                [ StackPanel.orientation Orientation.Horizontal
                  StackPanel.spacing 8.0
                  StackPanel.children
                      [ Border.create
                            [ Border.width 590.0
                              Border.child (StackPanel.create [ StackPanel.children [ left; notices m ] ]) ]
                        :> IView
                        StackPanel.create [ StackPanel.width 266.0; StackPanel.children [ right ] ] :> IView ] ]
            :> IView

    let content =
        Border.create
            [ Canvas.left 8.0
              Canvas.top 46.0
              Border.width 864.0
              Border.height 628.0
              Border.clipToBounds true
              Border.child (
                  StackPanel.create [ StackPanel.children [ body ] ]
                  |> View.withKey $"page-{m.RightTab}-{m.Game.Turn}-{m.ManualPage}"
              ) ]
        :> IView

    let knife =
        Border.create
            [ Canvas.left 190.0
              Canvas.top 678.0
              Border.width 500.0
              Border.child (
                  if Turn.isFinished m.Scenario m.Game then
                      knifeSwitch "最終報告書を開く" true (fun () -> dispatch ReopenFinal)
                  else
                      knifeSwitch "運転開始: この方針で季節を進める" true (fun () -> dispatch (EndTurn DateTime.UtcNow))
              ) ]
        :> IView

    Canvas.create
        [ Canvas.left deskX
          Canvas.top headerH
          Canvas.width deskW
          Canvas.height (boardH - headerH)
          Canvas.background (brush "#1F2428")
          Canvas.children
              [ yield! tabs |> List.mapi tab
                yield content
                yield knife ] ]
    :> IView

// ---------- 重ねて表示するカード (遊び方・運転日報・最終報告書) ----------

let private estText (value: float) (relErr: float) = $"約 {wu value} WU (±{pct relErr})"

let private rowWith (weight: FontWeight) (cells: (string * float) list) =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.children
              [ for (s, w) in cells ->
                    TextBlock.create
                        [ TextBlock.text s
                          TextBlock.width w
                          TextBlock.fontSize 12.0
                          TextBlock.fontWeight weight
                          TextBlock.foreground (brush "#20262B") ]
                    :> IView ] ]
    :> IView

let private headRow (cells: (string * float) list) = rowWith FontWeight.Bold cells

let private noteBox (color: string) (children: IView list) : IView =
    Border.create
        [ Border.background (brush color)
          Border.cornerRadius 6.0
          Border.padding 8.0
          Border.child (StackPanel.create [ StackPanel.spacing 3.0; StackPanel.children children ]) ]
    :> IView

let private surveyReportView (rep: SurveyReport) =
    noteBox
        "#FFF8E1"
        [ styled $"調査結果: {ZoneId.label rep.Zone} の{SurveyKind.label rep.Kind}" 13.0 FontWeight.Bold "#10202D"
          text $"貯留量の推定 {estText rep.GwEstWU rep.RelErr} / 持続的揚水量の推定 {estText rep.YieldEstWU rep.RelErr} (評価: {Groundwater.rating rep.YieldEstWU})"
          muted $"推定の幅が ±{pct rep.PrevRelErr} から ±{pct rep.RelErr} に狭まりました" ]

/// 運転日報 (3ページ): 1 計器・計画と実績・説明 / 2 用途別・財政 / 3 水の流れ・区域別の収支
let private resultPages (r: TurnResult) (ongoing: (Directive * float option) list) : IView list list =
    let a = r.Allocation
    let b = r.Balance
    let lo, hi = r.Weather.Card.PrecipMm
    let gaugeBands = [ (0.0, 0.5, "#C94A3F"); (0.5, 0.9, "#E0A526"); (0.9, 1.0, "#4F9A57") ]
    let pctMarks = [ (0.0, "0"); (0.25, "25"); (0.5, "50"); (0.75, "75"); (1.0, "100") ]

    let fillMeter (title: string) (v: float) =
        unitBox [ meter 124.0 0.0 1.0 v pctMarks gaugeBands None []; counter 3 (v * 100.0) "%"; plate title ]

    let pumpPlan = r.Policy.PumpLimitWU |> Map.toList |> List.sumBy snd

    let resOp =
        match r.ReservoirFlow with
        | Some f -> f.StoredWU - f.ReleasedWU
        | None -> 0.0

    let pvRow (item: string) (planned: string) (actual: string) = row [ item, 300.0; planned, 110.0; actual, 110.0 ]

    let title =
        [ styled "運 転 日 報" 20.0 FontWeight.Bold "#1B2A38"
          styled $"{r.Turn / 4 + 1}年目 {Season.label r.Season}  (第 {r.Turn + 1} 季)" 14.0 FontWeight.SemiBold "#3A2F1B" ]

    // ハイライト: 今季の水の動きの理由と、指令の判定 (計画経済版)
    let planLines =
        [ for d in r.Plan.Resolved ->
              let dd = d.Directive
              let trustText = if d.TrustChange = 0.0 then "上司の信頼は変わりません" else $"上司の信頼 {signed d.TrustChange}"
              let rewardText = if d.Reward > 0.0 then $"、補助金 +{wu d.Reward}" else ""

              styled
                  (if dd.Kind = BuildOrder then
                       $"指令第{dd.Id + 1}号 ({directiveGoal dd}): {Organization.outcomeLabel d.Outcome}。期限の時点の状況は「{measuredText dd d.Measured}」。{trustText}{rewardText}。"
                   else
                       $"指令第{dd.Id + 1}号 ({directiveGoal dd}): {Organization.outcomeLabel d.Outcome}。期間の充足率は {pct d.Measured}。{trustText}{rewardText}。")
                  12.0
                  FontWeight.Bold
                  (outcomeColor d.Outcome) ]

    // 資材と建設 (計画経済版): 今季の発注・着工・完成・融通・返礼
    let workLines =
        let w = r.Works
        let projectText (p: Project) = Construction.targetLabel p.Target
        // 今季に発注した工事の着工・完成は、上の説明文に書いてある
        let orderedNow (p: Project) = w.Ordered |> Option.exists (fun o -> o.Id = p.Id)

        [ for p in w.Started |> List.filter (orderedNow >> not) ->
              styled $"着工: {projectText p} (標準工期 {p.Duration} 季。着工した季節を 1 と数えます)" 12.0 FontWeight.Bold "#1F5E99"
          for p in w.Completed |> List.filter (orderedNow >> not) -> styled $"完成: {projectText p}。効果は来季から現れます。" 12.0 FontWeight.Bold okColor
          match w.Transferred with
          | Some mat -> yield styled $"融通: {Construction.materialLabel mat}が 1 単位、届きました。後日、正規配給から返します。" 12.0 FontWeight.Bold "#1F5E99"
          | None -> ()
          for mat in w.Repaid -> styled $"返礼: 借りていた{Construction.materialLabel mat}を、正規配給から差し引きました。" 12.0 FontWeight.Bold "#E65100"
          if not w.ArrivingNext.IsEmpty then
              let arr = w.ArrivingNext |> List.map (fun (mat, q) -> $"{Construction.materialLabel mat} +{q}") |> String.concat "・"
              yield muted $"来季のはじめの正規配給: {arr}" ]

    // 年次報告・監査 (計画経済版)。実値 (実際の判定) と報告値を並べる
    let reportLines =
        [ match r.Plan.Report with
          | Some rep ->
              let lies = rep.Items |> List.filter Organization.isMisreport
              let detail =
                  if lies.IsEmpty then
                      "実際の結果を、そのまま報告しました。"
                  else
                      let parts =
                          [ for i in lies ->
                                let reported = match i.Reported with Some o -> Organization.outcomeLabel o | None -> "報告から外す"
                                $"第{i.DirectiveId + 1}号 (実際 {Organization.outcomeLabel i.Actual} → 報告 {reported})" ]

                      "虚偽の報告: " + String.concat "、" parts + "。"

              let riskText =
                  if rep.RiskDelta < 0.0 && r.Plan.RiskBefore = 0.0 then "整合性リスクは、すでに最低 (0) です。"
                  else $"整合性リスク {signed rep.RiskDelta}。"

              yield styled $"年次報告 ({Organization.stanceLabel rep.Stance}): {detail}{riskText}" 12.0 FontWeight.Bold (if lies.IsEmpty then okColor else "#E65100")
          | None -> ()

          match r.Plan.Audit with
          | Some a when a.Exposed.IsEmpty -> yield styled $"監査: 問題は見つかりませんでした。上司の信頼 {signed a.TrustChange}。" 12.0 FontWeight.Bold okColor
          | Some a -> yield styled $"監査: 虚偽の報告が露見しました。報告で得た信頼と補助金が取り消され、さらに罰則があります。上司の信頼 {signed a.TrustChange}、予算 −{wu a.Fine}。" 12.0 FontWeight.Bold badColor
          | None -> ()

          if r.Plan.AuditNotice then
              yield styled "監査の予兆: 次の季節に監査が入ります。" 12.0 FontWeight.Bold badColor ]

    // 期限がまだ先の指令の、ここまでの進み具合
    let ongoingLines =
        [ for (d, p) in ongoing ->
              let fill = match p with Some v -> measuredText d v | None -> "—"
              let label = if d.Kind = BuildOrder then "状況" else "ここまでの充足率"
              muted $"指令第{d.Id + 1}号 ({directiveGoal d}) 進行中: {label} {fill}。期限は第{d.DeadlineTurn + 1}季。" ]

    let highlight =
        noteBox
            "#E8F1FA"
            ([ subheading "ハイライト"
               styled $"学習目標{r.ExplanationGoal}: {r.Explanation}" 12.0 FontWeight.Normal "#20262B" ]
             @ planLines
             @ workLines
             @ reportLines
             @ ongoingLines)

    let page0 =
        title
        @ [ highlight ]
        @ [ muted $"実績: 降水 {mm r.Weather.PrecipMm} mm (予報 {mm lo}〜{mm hi} mm) / 基準蒸発散需要 {mm r.Weather.EtDemandMm} mm"
            hbox 10.0 [ fillMeter "農業用水 充足率" r.AgFillRate; fillMeter "生活・産業用水 充足率" r.UrbanFillRate; fillMeter "環境流水 達成度" r.EnvAchievement ]
            headRow [ "項目", 300.0; "計画", 110.0; "実績", 110.0 ]
            pvRow "農業用水 (河川から取水)" (wu r.Policy.AgTargetWU) (wu a.AgSupplyWU)
            pvRow "生活・産業用水 (河川から取水)" (wu r.Policy.UrbanTargetWU) (wu a.UrbanSupplyWU)
            pvRow "環境流水 (下流管理地点の流量)" (wu r.EnvTargetWU) (wu a.DownstreamFlowWU)
            pvRow "揚水 (上限の合計 / 実際の揚水量)" (wu pumpPlan) (wu r.GwSupplyWU)
            pvRow "貯水池 (+貯める / −放流)" (signed r.Policy.ReservoirOpWU) (signed resOp) ]
        @ (if r.ActionTaken <> NoAction then [ muted $"実行した主要行動: {actionText r.ActionTaken}" ] else [])
        @ (match r.Event with
           | Some ev -> [ styled $"今季のできごと: {ev.Name} ({ev.Description})" 12.0 FontWeight.Bold "#E65100" ]
           | None -> [])
        @ (r.SurveyReports |> List.map surveyReportView)
        @ [ for (z, before, after) in r.Observations ->
                muted $"観測井戸 ({ZoneId.label z}): 推定の幅が ±{pct before} から ±{pct after} に狭まりました" ]
        @ [ for w in r.Warnings -> styled $"⚠ {w}" 12.0 FontWeight.SemiBold badColor ]

    let page1 =
        title
        @ [ subheading "用途別の供給"
            headRow [ "用途", 250.0; "需要", 90.0; "地下水から", 90.0; "充足率", 90.0 ]
            row [ "農業用水", 250.0; wu r.AgDemandWU, 90.0; wu r.AgGwSupplyWU, 90.0; pct r.AgFillRate, 90.0 ]
            row [ "生活・産業用水", 250.0; wu r.UrbanDemandWU, 90.0; wu r.UrbanGwSupplyWU, 90.0; pct r.UrbanFillRate, 90.0 ]
            row [ "環境流水 (最低基準)", 250.0; wu r.EnvMinWU, 90.0; "—", 90.0; pct r.EnvAchievement, 90.0 ]
            (if r.EnvMinMet then yesNo true "環境流水は最低基準を満たしました" "" else yesNo false "" "環境流水は最低基準を下回りました")
            muted "地下水は、全区域の井戸からの揚水の合計です。井戸のない区域の用水にも、供給網を通じて届けます。"
            subheading "財政"
            row [ "期首残高", 280.0; wu r.BalanceStart, 100.0 ] ]
        @ (if r.Grant > 0.0 then [ row [ "＋ 管理交付金", 280.0; wu r.Grant, 100.0 ] ] else [])
        @ (if r.Plan.Reward > 0.0 then [ row [ "＋ 指令の達成報酬 (補助金)", 280.0; wu r.Plan.Reward, 100.0 ] ] else [])
        @ (if r.Plan.Fine > 0.0 then [ row [ "− 監査による予算削減", 280.0; wu r.Plan.Fine, 100.0 ] ] else [])
        @ [ row [ "＋ 用水の収入 (農業・生活産業)", 280.0; wu r.Revenue, 100.0 ]
            row [ "− 費用の合計", 280.0; wu r.Costs.Total, 100.0 ]
            row [ "＝ 期末残高", 280.0; wu r.BalanceEnd, 100.0 ]
            muted $"費用の内訳: {costText r.Costs}"
            subheading "河川水の流れ (WU)"
            row [ "取水点の利用可能量", 280.0; wu a.AvailableWU, 100.0 ]
            muted "　(取水点より上流の 森林・農地・市街地 の、地表流出 + 地下水の湧出 − 河川から地下水への浸透、に貯水池の操作を加えた量)" ]
        @ (let seep = r.Zones |> List.sumBy _.SeepageWU

           if seep > 0.5 then
               [ muted $"　河川から地下水へ浸透: {wu seep} WU (川の水が減る代わりに、その分が地下水に加わります)" ]
           else
               [])
        @ (match r.ReservoirFlow with
           | Some f -> [ muted $"　貯水池: 期首 {wu f.StartWU} → 蒸発 −{wu f.EvaporationWU} / 貯める {wu f.StoredWU} / 放流 +{wu f.ReleasedWU} → 期末 {wu f.EndWU} WU (容量 {wu f.CapacityWU})" ]
           | None -> [])
        @ [ row [ "− 農業へ取水", 280.0; wu a.AgSupplyWU, 100.0 ]
            row [ "− 生活・産業へ取水", 280.0; wu a.UrbanSupplyWU, 100.0 ]
            row [ "＋ 下流の湿地からの流入", 280.0; wu a.BelowContributionWU, 100.0 ] ]
        @ (if a.ReturnFlowWU > 0.5 then [ row [ "＋ 再生水の還元", 280.0; wu a.ReturnFlowWU, 100.0 ] ] else [])
        @ [ row [ "＝ 下流管理地点の流量", 280.0; wu a.DownstreamFlowWU, 100.0 ] ]

    let zoneRow (z: ZoneResult) =
        row
            [ ZoneId.label z.Zone, 150.0
              wu z.PrecipWU, 58.0
              wu z.RunoffWU, 58.0
              wu z.InfiltrationWU, 58.0
              wu z.ActualEtWU, 58.0
              wu z.DeepDrainageWU, 58.0
              wu z.SeepageWU, 58.0
              wu z.PumpedWU, 58.0
              signed (z.SoilEndWU - z.SoilStartWU), 58.0 ]

    let page2 =
        title
        @ [ subheading "降水の行き先 (WU)"
            row [ "降水入力", 300.0; wu b.PrecipInWU, 100.0 ]
            row [ "　→ 蒸発散 (貯水池の蒸発を含む)", 300.0; wu b.ActualEtWU, 100.0 ]
            row [ "　→ 用水供給 (河川水+地下水−再生水の還元)", 300.0; wu b.ExternalSupplyWU, 100.0 ]
            row [ "　→ 下流端から流域の外へ", 300.0; wu b.OutflowWU, 100.0 ]
            row [ "　→ 貯留の増減 (土壌+地下水)", 300.0; signed b.StorageChangeWU, 100.0 ]
            muted "　(＋は土壌・地下水にたまった量、−はたまっていた水を使った量。行き先の合計が降水入力と一致します)"
            row [ "　行き先の合計", 300.0; wu (b.ActualEtWU + b.ExternalSupplyWU + b.OutflowWU + b.StorageChangeWU), 100.0 ]
            yesNo
                b.IsWithinTolerance
                "降水入力と行き先の合計は一致しています (差 0)"
                $"降水入力と行き先の合計が一致していません (差 {b.ResidualWU:E1})"
            subheading "区域ごとの水収支 (WU)"
            headRow [ "区域", 150.0; "降水", 58.0; "地表流出", 58.0; "浸透", 58.0; "蒸発散", 58.0; "涵養", 58.0; "河川浸透", 58.0; "揚水", 58.0; "土壌変化", 58.0 ] ]
        @ (r.Zones |> List.map zoneRow)

    [ page0; page1; page2 ]

/// 推定値(青)と実際の値(赤)を重ねた折れ線。縦軸は 0 から両者の最大値まで
let private gwChart (trace: GwPoint list) =
    let w, h = 145.0, 100.0
    let maxV = trace |> List.collect (fun p -> [ p.Estimated; p.Actual ]) |> List.max |> max 1.0
    let n = trace.Length
    let x i = 8.0 + float i * (w - 16.0) / float (max 1 (n - 1))
    let y v = h - 8.0 - v / maxV * (h - 16.0)
    let pts (f: GwPoint -> float) = trace |> List.mapi (fun i p -> Point(x i, y (f p)))

    Canvas.create
        [ Canvas.width w
          Canvas.height h
          Canvas.background (brush "#F2F6FA")
          Canvas.children
              [ Polyline.create
                    [ Polyline.points (pts _.Actual)
                      Shape.stroke (brush "#C62828")
                      Shape.strokeThickness 3.0 ]
                Polyline.create
                    [ Polyline.points (pts _.Estimated)
                      Shape.stroke (brush "#1565C0")
                      Shape.strokeThickness 2.0 ] ] ]
    :> IView

/// 最終報告書 (4ページ): 1 評価 / 2 振り返り / 3 地下水の推定と実際・考察 / 4 推移・収支の確認
let private finalPages (m: Model) (s: Summary) : IView list list =
    let ranked = Evaluation.rankLabel s.Rank

    let timeLines =
        match m.Elapsed with
        | Some e ->
            let avg = if m.DecisionSeconds.IsEmpty then 0.0 else List.average m.DecisionSeconds
            [ muted $"プレイ時間 {e.TotalMinutes:F1} 分 (目安 15〜25 分) / 方針決定は平均 {avg:F0} 秒 (目安 45〜90 秒)" ]
        | None -> []

    // 計画経済版は、流域の実態・計画達成度・政治的評価を、混ぜずに別々に出す
    let headline =
        match s.Plan with
        | None -> [ styled $"ランク {ranked}  (総合 {pct s.Score})" 30.0 FontWeight.Bold "#1F3A52" ]
        | Some p ->
            let achievement = p.Achievement |> Option.defaultValue 1.0

            [ hbox 30.0
                  [ unitBox [ counter 3 (s.Score * 100.0) "%"; plate $"流域の実態 (ランク {ranked})" ]
                    unitBox [ counter 3 (achievement * 100.0) "%"; plate "計画達成度" ]
                    unitBox [ counter 3 p.Trust ""; plate "政治的評価 (上司の信頼)" ] ]
              muted "三つの評価は、混ぜずに別々に示します。流域の実態は、下の5つの指標の総合です。" ]

    let page0 =
        [ heading $"最終報告書 ({m.Scenario.Name})" ]
        @ headline
        @ timeLines
        @ [ subheading "指標ごとの評価"
            headRow [ "指標", 190.0; "評価", 70.0; "重み", 60.0 ] ]
        @ [ for i in s.Indicators do
                yield row [ i.Name, 190.0; pct i.Score, 70.0; $"×{i.Weight:F0}", 60.0 ]
                yield muted i.Detail ]
        @ [ muted "指標の重みはシナリオごとに異なります。総合は、重み付き平均 (0.6) と最も低い指標 (0.4) の混合です。"
            muted $"地下水の評価: 終了時の貯留量が開始時の {pct m.Scenario.Eval.GwReferenceRatio} 以上なら満点です (何もしなくても自然な湧出で貯留量は減るため、その分は許容します)。揚水コスト上昇が起きた区域ごとに減点します。"
            subheading "費用と収入"
            row [ "用水の収入(合計)", 220.0; wu s.TotalRevenue, 100.0 ]
            row [ "費用の合計", 220.0; wu s.TotalCosts.Total, 100.0 ]
            muted $"内訳: {costText s.TotalCosts}" ]

    // 計画指令の結果と、報告との食い違い (実値と報告値)
    let planTable =
        match s.Plan with
        | Some p when not p.Results.IsEmpty ->
            let reportedOf (id: int) =
                p.Reports |> List.tryPick (fun rep -> rep.Items |> List.tryFind (fun i -> i.DirectiveId = id))

            let reportText (id: int) =
                match reportedOf id with
                | Some i ->
                    let t = match i.Reported with Some o -> Organization.outcomeLabel o | None -> "報告から外した"
                    if Organization.isMisreport i then $"{t} (虚偽)" else t
                | None -> "—"

            [ subheading "計画指令の結果と、報告"
              headRow [ "指令", 50.0; "種類", 130.0; "期間", 90.0; "目標", 60.0; "実績", 60.0; "実際の判定", 90.0; "報告した判定", 120.0; "信頼", 40.0 ] ]
            @ [ for r in p.Results ->
                    let d = r.Directive

                    row
                        [ $"第{d.Id + 1}号", 50.0
                          Organization.targetLabel d.Kind, 130.0
                          $"{d.IssuedTurn + 1}〜{d.DeadlineTurn + 1}季", 90.0
                          (if d.Kind = BuildOrder then "完成" else pct d.TargetFill), 60.0
                          measuredText d r.Measured, 60.0
                          Organization.outcomeLabel r.Outcome, 90.0
                          reportText d.Id, 120.0
                          signed r.TrustChange, 40.0 ] ]
            @ [ for rep in p.Reports ->
                    muted $"第{rep.Turn + 1}季の年次報告: {Organization.stanceLabel rep.Stance}" ]
            @ [ for a in p.Audits ->
                    styled
                        (if a.Exposed.IsEmpty then $"第{a.Turn + 1}季の監査: 問題なし" else $"第{a.Turn + 1}季の監査: 虚偽の報告が露見 (信頼 {signed a.TrustChange}、予算 −{wu a.Fine})")
                        12.0
                        FontWeight.SemiBold
                        (if a.Exposed.IsEmpty then okColor else badColor) ]
            @ (match p.FinalAudit with
               | Some f ->
                   [ styled $"終了時の監査: 露見しなかった虚偽の報告が {f.Exposed.Length} 件、発覚しました (信頼 {signed f.TrustChange})。" 12.0 FontWeight.Bold badColor ]
               | None -> [])
            @ (if p.Undetected.IsEmpty then
                   []
               else
                   [ styled
                         $"露見しなかった虚偽の報告: {p.Undetected.Length} 件。整合性リスクが {Organization.auditThreshold:F0} に届かず、終了時の監査は入りませんでした。"
                         12.0
                         FontWeight.Bold
                         "#E65100" ])
        | _ -> []

    let page1 =
        subheading "学習目標の振り返り"
        :: [ for (n, title, body) in Evaluation.reflections m.Scenario m.Game s ->
                 noteBox "#E8F1FA" [ styled $"学習目標{n}: {title}" 12.0 FontWeight.Bold "#10202D"; text body ] ]

    let charts =
        StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 8.0
              StackPanel.children
                  [ for z in m.Scenario.Zones ->
                        StackPanel.create
                            [ StackPanel.spacing 2.0
                              StackPanel.children [ styled (ZoneId.label z.Id) 12.0 FontWeight.SemiBold "#10202D"; gwChart (Evaluation.gwTrace m.Scenario m.Game z.Id) ] ]
                        :> IView ] ]
        :> IView

    let page2 =
        [ subheading "推定と実際の地下水貯留量"
          muted "青: プレイヤーが知っていた推定値 / 赤: 実際の値 (調査すると、推定が実際に近づきます)。縦軸は 0 から、その区域の最大値までです。調査で推定が急に動くのは、誤差が小さくなって実際の値に近づくためです (概査 ±25%、試掘 ±8%)。"
          charts
          subheading "考察" ]
        @ [ for line in Evaluation.debrief m.Scenario m.Game -> text line ]

    let residualText = if s.MaxAbsResidualWU < 1e-6 then "0" else s.MaxAbsResidualWU.ToString("0.000000", inv)

    let page3 =
        [ subheading "ターンごとの推移"
          headRow [ "ターン", 70.0; "農業", 80.0; "生活産業", 90.0; "環境達成", 90.0; "最低基準", 80.0; "期末残高", 90.0 ] ]
        @ [ for r in m.Game.History ->
                row
                    [ $"{r.Turn + 1} {Season.label r.Season}", 70.0
                      pct r.AgFillRate, 80.0
                      pct r.UrbanFillRate, 90.0
                      pct r.EnvAchievement, 90.0
                      (if r.EnvMinMet then "○" else "×"), 80.0
                      wu r.BalanceEnd, 90.0 ] ]
        @ [ subheading "水収支の最終確認"
            yesNo s.ResidualOk $"全ターンで、降水入力と行き先の合計が一致しました (最大の差 {residualText} WU)" $"収支が一致しないターンがあります (最大の差 {residualText} WU)" ]

    [ page0 ] @ (if planTable.IsEmpty then [] else [ planTable ]) @ [ page1; page2; page3 ]

let private introPages (m: Model) dispatch : IView list list =
    let scenarioButtons =
        [ for sc in Scenarios.all ->
              pushButtonW 590.0 $"【{sc.Level}】{sc.Name}  ({sc.Turns}ターン)\n{sc.Summary}" (sc.Name = m.Scenario.Name) true (fun () -> dispatch (StartScenario(sc, DateTime.UtcNow))) ]

    [ [ heading "Red River"
        muted "小流域水資源管理シミュレータ"
        text "あなたは小さな流域の水資源の管理責任者です。限られた水を農業・生活産業・河川環境に配分し、地下水を調べ、費用の範囲で投資します。"
        subheading "1ターン(1季節)の流れ"
        text "① ヘッダーの気象カードと、運転ページの予報メーターを読む → ② バルブ・ダイヤル・スイッチ・レバーで方針を決める → ③ 右下のナイフスイッチ「運転開始」を入れる → ④ 運転日報で「なぜそうなったか」を確認する"
        subheading "シナリオを選んで始める (初めての方はチュートリアルから)" ]
      @ scenarioButtons
      [ subheading "盤の見方"
        text "左の地図: 各区域の土壌水分計・地下水の水位計 (推定)・持続的揚水量のメーター。区域をクリックすると、設備ページで調査・井戸・投資の押しボタンを選べます。地下水の本当の量は見えません。調査するほど推定の幅が狭まります。"
        text "右の盤: 運転ページ (読む計器と、直す計器)・設備ページ (主要行動と資金)・評価・財政ページ・マニュアル。計器には説明を付けていません。操作の効果や言葉の意味は、マニュアルを見てください。"
        subheading "方針で決めること"
        text "・河川から農業・生活産業へ供給する量 (バルブ)と、下流に残す環境流水の目標 (最低基準以上)"
        text "・区域ごとの揚水の上限 (井戸がある区域のポンプ)、貯水池のゲート"
        text "・主要行動を1つまで: 地下水の調査・井戸の建設・設備投資 (押しボタン)"
        text "・各年の春だけ、配分の優先順位を切り替えられます"
        text "・中央からの指令 (ノルマ) への対応。入門を除いて、2〜3ターンおきに届きます。冬の季節には、年次報告の方針も選びます (指令ページ)"
        subheading "この遊びで学ぶこと"
        text "1 水収支 / 2 地下水のストック性 / 3 配分のトレードオフ / 4 情報の価値 / 5 不可逆性"
        muted "目安の時間: 1ターン 45〜90 秒"
        muted "運の要素は、気象だけです。それ以外 (地下水の真の量、できごと、指令、監査) は、すべて決まった規則で動きます。" ] ]

/// 重ねて表示するカード。本文はページに分け、下に ページ送りの押しボタン と ナイフスイッチ を固定する
let private overlayCard (m: Model) dispatch (pages: IView list list) (knife: IView) (extra: IView list) : IView =
    let n = pages.Length
    let p = max 0 (min (n - 1) m.OverlayPage)

    let nav =
        hbox 8.0
            [ pushButtonW 50.0 "◀ 前へ" false (p > 0) (fun () -> dispatch (SetPage(p - 1)))
              vcenter (plate $"{p + 1} / {n}")
              pushButtonW 50.0 "次へ ▶" false (p < n - 1) (fun () -> dispatch (SetPage(p + 1))) ]

    let footer =
        StackPanel.create
            [ Grid.row 1
              StackPanel.margin (Thickness(0.0, 8.0, 0.0, 0.0))
              StackPanel.spacing 6.0
              StackPanel.children
                  [ StackPanel.create
                        [ StackPanel.orientation Orientation.Horizontal
                          StackPanel.spacing 16.0
                          StackPanel.horizontalAlignment HorizontalAlignment.Center
                          StackPanel.children [ vcenter nav; knife ] ]
                    :> IView
                    yield! extra ] ]

    // 本文は、はみ出しても切り取り、ページ送りの釦は、つねに下に見えるようにする
    let body =
        Border.create
            [ Grid.row 0
              Border.clipToBounds true
              Border.child (
                  StackPanel.create [ StackPanel.spacing 5.0; StackPanel.children pages[p] ]
                  |> View.withKey $"card-{p}"
              ) ]

    Border.create
        [ Canvas.left 0.0
          Canvas.top 0.0
          Border.width boardW
          Border.height boardH
          Border.background (brush "#B0000000")
          Border.child (
              Border.create
                  [ Border.horizontalAlignment HorizontalAlignment.Center
                    Border.verticalAlignment VerticalAlignment.Center
                    Border.width 680.0
                    Border.height 770.0
                    Border.background (brush paperColor)
                    Border.borderBrush (brush "#C9BE9C")
                    Border.borderThickness 2.0
                    Border.cornerRadius 6.0
                    Border.padding 18.0
                    Border.clipToBounds true
                    Border.child (Grid.create [ Grid.rowDefinitions "*,Auto"; Grid.children [ body; footer ] ]) ]
          ) ]
    :> IView

let private overlayView (m: Model) dispatch : IView option =
    match m.Overlay with
    | NoOverlay -> None
    | ShowIntro ->
        Some(overlayCard m dispatch (introPages m dispatch) (knifeSwitch $"「{m.Scenario.Name}」で始める" true (fun () -> dispatch (DismissIntro DateTime.UtcNow))) [])
    | ShowResult r ->
        let last = Turn.isFinished m.Scenario m.Game
        let ongoing =
            m.Game.Organization.Directives
            |> List.filter (fun d -> d.IssuedTurn <= r.Turn && r.Turn < d.DeadlineTurn)
            |> List.map (fun d -> d, Organization.progress d m.Game.History)

        Some(overlayCard m dispatch (resultPages r ongoing) (knifeSwitch (if last then "最終結果へ" else "次の季節へ") true (fun () -> dispatch (CloseOverlay DateTime.UtcNow))) [])
    | ShowFinal s ->
        let extra =
            [ hbox 8.0
                  [ pushButtonW 110.0 "同じシードで再挑戦" false true (fun () -> dispatch (RestartSameSeed DateTime.UtcNow))
                    pushButtonW 110.0 "新しいシード" false true (fun () -> dispatch (RestartNewSeed DateTime.UtcNow))
                    pushButtonW 110.0 "シナリオ選択へ" false true (fun () -> dispatch OpenMenu) ] ]

        Some(overlayCard m dispatch (finalPages m s) (knifeSwitch "閉じて地図を見る" true (fun () -> dispatch CloseFinal)) extra)

// ---------- ルート ----------

let view (m: Model) (dispatch: Msg -> unit) : IView =
    let board =
        Canvas.create
            [ Canvas.width boardW
              Canvas.height boardH
              Canvas.background (brush "#14181C")
              Canvas.children
                  [ yield mapView m dispatch
                    yield desk m dispatch
                    yield header m
                    match overlayView m dispatch with
                    | Some o -> yield o
                    | None -> () ] ]

    Border.create
        [ Border.background (brush "#0E1114")
          Border.child (Viewbox.create [ Viewbox.stretch Stretch.Uniform; Viewbox.child board ]) ]
    :> IView
