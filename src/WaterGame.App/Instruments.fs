/// 計器・操作部の部品。入力は機材を模した操作部 (ハンドル式バルブ・ダイヤル・レバー・ロータリースイッチ・押しボタン)、
/// 表示は計器 (メーター・水位計・桁カウンタ) で行う。
/// どの操作部も、盤面の好きな位置をクリックするとその値になる (ドラッグでも回せる)。
module WaterGame.App.Instruments

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

let private inv = CultureInfo.InvariantCulture
let private n (x: float) = x.ToString("0.###", inv)
let private brush (hex: string) : IBrush = SolidColorBrush(Color.Parse hex) :> IBrush

// ---------- 配色 (制御盤) ----------

/// 盤面 (暗い灰色)
let panelColor = "#2A3037"
let panelEdge = "#4B555F"
/// 計器の文字盤 (クリーム)
let faceColor = "#F2EAD3"
let inkColor = "#20262B"
let brassColor = "#B08D3C"
let steelColor = "#8E99A3"
let okLamp = "#3DDC84"
let warnLamp = "#FFB300"
let badLamp = "#FF4D4D"
let paperColor = "#F7F1E1"

// ---------- 幾何 ----------

/// 角度は、真上を 0 度、時計回りを正とする
let private pt (cx: float) (cy: float) (r: float) (deg: float) =
    let a = deg * Math.PI / 180.0
    cx + r * sin a, cy - r * cos a

let private arcData cx cy r a0 a1 =
    let x0, y0 = pt cx cy r a0
    let x1, y1 = pt cx cy r a1
    let large = if abs (a1 - a0) > 180.0 then 1 else 0
    $"M {n x0},{n y0} A {n r},{n r} 0 {large} 1 {n x1},{n y1}"

let private shape (data: string) (stroke: string) (width: float) (cap: PenLineCap) =
    Path.create
        [ Path.data (Geometry.Parse data)
          Shape.stroke (brush stroke)
          Shape.strokeThickness width
          Shape.strokeLineCap cap ]
    :> IView

let private line (x0: float) (y0: float) (x1: float) (y1: float) (color: string) (width: float) =
    shape $"M {n x0},{n y0} L {n x1},{n y1}" color width PenLineCap.Round

let private disc (cx: float) (cy: float) (r: float) (fill: string) (edge: string) (edgeWidth: float) =
    Ellipse.create
        [ Canvas.left (cx - r)
          Canvas.top (cy - r)
          Ellipse.width (2.0 * r)
          Ellipse.height (2.0 * r)
          Shape.fill (brush fill)
          Shape.stroke (brush edge)
          Shape.strokeThickness edgeWidth ]
    :> IView

let private label (x: float) (y: float) (w: float) (text: string) (size: float) (color: string) (weight: FontWeight) =
    TextBlock.create
        [ Canvas.left x
          Canvas.top y
          TextBlock.width w
          TextBlock.text text
          TextBlock.fontSize size
          TextBlock.fontWeight weight
          TextBlock.foreground (brush color)
          TextBlock.textAlignment TextAlignment.Center ]
    :> IView

let private sweep = 135.0

/// 値 → 角度 (-135〜+135 度)
let private angleOf (lo: float) (hi: float) (v: float) =
    if hi <= lo then -sweep else -sweep + (v - lo) / (hi - lo) * 2.0 * sweep

// ---------- 入力の受け付け ----------

/// 透明な操作面。クリックとドラッグの位置 (操作面の中の座標) を onPoint へ渡す。
/// 描画部品の上に重ねるので、イベントの発生元は常にこの操作面になる。
let private surface (w: float) (h: float) (onPoint: Point -> unit) =
    let at (e: PointerEventArgs) =
        match e.Source with
        | :? Visual as v -> Some(e.GetPosition v)
        | _ -> None

    Border.create
        [ Canvas.left 0.0
          Canvas.top 0.0
          Border.width w
          Border.height h
          Border.background (brush "#01000000")
          Border.cursor (Cursor(StandardCursorType.Hand))
          Border.onPointerPressed (
              (fun e ->
                  at e |> Option.iter onPoint
                  e.Pointer.Capture(e.Source :?> IInputElement) |> ignore
                  e.Handled <- true),
              SubPatchOptions.Always
          )
          Border.onPointerMoved (
              (fun e ->
                  let pressed =
                      match e.Source with
                      | :? Visual as v -> e.GetCurrentPoint(v).Properties.IsLeftButtonPressed
                      | _ -> false

                  if pressed then at e |> Option.iter onPoint),
              SubPatchOptions.Always
          )
          Border.onPointerReleased (
              (fun e -> e.Pointer.Capture(null)),
              SubPatchOptions.Always
          ) ]
    :> IView

let private snapTo (step: float) (lo: float) (hi: float) (v: float) =
    let s = if step > 0.0 then Math.Round(v / step) * step else v
    // 上限・下限の半刻み以内なら端そのものにする (需要が刻みの倍数でなくても全開にできる)
    if hi - v < step / 2.0 then hi
    elif v - lo < step / 2.0 then lo
    else max lo (min hi s)

// ---------- 操作部: ハンドル式バルブ・ダイヤル ----------

/// 色帯: (開始値, 終了値, 色)
type Band = float * float * string

/// 目盛りの印: (値, 文字)
type Mark = float * string

/// ハンドル式バルブ (回すと開度が変わる)。文字盤の目盛りは固定で、ハンドルと指針が回る。
/// size は一辺 (px)。目盛りの文字は marks で与える。
let valve
    (key: string)
    (size: float)
    (lo: float)
    (hi: float)
    (step: float)
    (value: float)
    (marks: Mark list)
    (bands: Band list)
    (onChange: float -> unit)
    : IView =
    let c = size / 2.0
    let rScale = size * 0.46
    let rWheel = size * 0.30
    let ang = angleOf lo hi value

    let onPoint (p: Point) =
        let dx, dy = p.X - c, p.Y - c

        if sqrt (dx * dx + dy * dy) > size * 0.08 then
            let deg = Math.Atan2(dx, -dy) * 180.0 / Math.PI
            // 下側の不感帯 (|角度| > 135) は、近いほうの端に寄せる
            let deg = if deg > sweep then sweep elif deg < -sweep then -sweep else deg
            // 端の近く (6 度以内) は、端そのものにする (全開・全閉にしやすいように)
            let deg = if deg > sweep - 6.0 then sweep elif deg < -sweep + 6.0 then -sweep else deg
            let v = lo + (deg + sweep) / (2.0 * sweep) * (hi - lo)
            onChange (snapTo step lo hi v)

    let ticks =
        marks
        |> List.collect (fun (v, text) ->
            let a = angleOf lo hi v
            let x0, y0 = pt c c (rScale - 4.0) a
            let x1, y1 = pt c c (rScale + 4.0) a
            let tx, ty = pt c c (rScale + 12.0) a

            [ line x0 y0 x1 y1 faceColor 2.0
              label (tx - 20.0) (ty - 7.0) 40.0 text 9.0 faceColor FontWeight.Normal ])

    let bandArcs =
        bands |> List.map (fun (a, b, color) -> shape (arcData c c rScale (angleOf lo hi a) (angleOf lo hi b)) color 4.0 PenLineCap.Flat)

    // ハンドル: 3本のスポーク (開度に応じて回る) と、リム、ハブ
    let spokes =
        [ for k in 0..2 do
              let a = ang + float k * 60.0
              let x0, y0 = pt c c rWheel a
              let x1, y1 = pt c c rWheel (a + 180.0)
              yield line x0 y0 x1 y1 steelColor 5.0 ]

    let grips =
        [ for k in 0..5 do
              let a = ang + float k * 60.0
              let x, y = pt c c rWheel a
              yield disc x y 4.5 "#C9D1D8" "#59636D" 1.0 ]

    let pointerEnd = pt c c (rWheel + 2.0) ang
    let pointerStart = pt c c (rWheel * 0.35) ang

    Canvas.create
        [ Canvas.width size
          Canvas.height size
          Canvas.children
              [ yield disc c c (size * 0.5 - 1.0) "#1B2026" panelEdge 1.5
                yield! bandArcs
                yield shape (arcData c c rScale (-sweep) sweep) "#586570" 1.0 PenLineCap.Flat
                yield! ticks
                yield disc c c (rWheel + 7.0) "#3A434C" "#79858F" 1.5
                yield! spokes
                yield! grips
                yield disc c c (rWheel * 0.32) brassColor "#6B5520" 2.0
                yield line (fst pointerStart) (snd pointerStart) (fst pointerEnd) (snd pointerEnd) "#FF5A4D" 3.0
                yield surface size size onPoint ] ]
    |> View.withKey key
    :> IView

// ---------- 表示部: 指針式メーター ----------

/// 指針式メーター (読み取り専用)。
/// 範囲 (span) は「予報の下限〜上限」のような幅を外周の弧で示し、印 (flags) は基準値などを短い針で示す。
let meter
    (size: float)
    (lo: float)
    (hi: float)
    (value: float)
    (marks: Mark list)
    (bands: Band list)
    (span: (float * float) option)
    (flags: (float * string) list)
    : IView =
    let c = size / 2.0
    let rScale = size * 0.40
    let ang = angleOf lo hi (max lo (min hi value))

    let ticks =
        marks
        |> List.collect (fun (v, text) ->
            let a = angleOf lo hi v
            let x0, y0 = pt c c (rScale - 5.0) a
            let x1, y1 = pt c c (rScale + 1.0) a
            let tx, ty = pt c c (rScale - 16.0) a

            [ line x0 y0 x1 y1 inkColor 1.5
              label (tx - 22.0) (ty - 7.0) 44.0 text 9.0 inkColor FontWeight.Normal ])

    let bandArcs =
        bands |> List.map (fun (a, b, color) -> shape (arcData c c (rScale + 4.0) (angleOf lo hi a) (angleOf lo hi b)) color 4.0 PenLineCap.Flat)

    let spanArc =
        match span with
        | Some(a, b) when b > a ->
            [ shape (arcData c c (rScale + 11.0) (angleOf lo hi (max lo a)) (angleOf lo hi (min hi b))) "#2E7DD1" 5.0 PenLineCap.Flat ]
        | _ -> []

    let flagMarks =
        flags
        |> List.collect (fun (v, color) ->
            let a = angleOf lo hi (max lo (min hi v))
            let x0, y0 = pt c c (rScale - 9.0) a
            let x1, y1 = pt c c (rScale + 16.0) a
            [ line x0 y0 x1 y1 color 3.0 ])

    let nx, ny = pt c c (rScale - 4.0) ang

    Canvas.create
        [ Canvas.width size
          Canvas.height size
          Canvas.children
              [ yield disc c c (size * 0.5 - 1.0) "#1B2026" panelEdge 1.5
                yield disc c c (size * 0.5 - 5.0) faceColor "#9A927C" 1.0
                yield! bandArcs
                yield! spanArc
                yield shape (arcData c c rScale (-sweep) sweep) inkColor 1.0 PenLineCap.Flat
                yield! flagMarks
                yield line c c nx ny "#B3261E" 2.5
                yield disc c c 5.0 "#20262B" "#20262B" 1.0
                // 目盛りの文字は、針に隠れないよう、最後に描く
                yield! ticks ] ]
    :> IView

// ---------- 表示部: 桁カウンタ ----------

/// 桁カウンタ (機械式の数字車)。負の値は先頭に − を出す。
let counter (digits: int) (value: float) (unit: string) : IView =
    let v = int (Math.Round(abs value))
    let s = (min v (pown 10 digits - 1)).ToString().PadLeft(digits, ' ')

    let box (c: char) =
        Border.create
            [ Border.background (brush "#0F1316")
              Border.borderBrush (brush "#59636D")
              Border.borderThickness 1.0
              Border.cornerRadius 2.0
              Border.width 17.0
              Border.height 24.0
              Border.child (
                  TextBlock.create
                      [ TextBlock.text (if c = ' ' then "" else string c)
                        TextBlock.fontSize 16.0
                        TextBlock.fontFamily (FontFamily("Consolas, DejaVu Sans Mono, monospace"))
                        TextBlock.foreground (brush "#F3F0E6")
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.verticalAlignment VerticalAlignment.Center ]
              ) ]
        :> IView

    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 2.0
          StackPanel.children
              [ if value < 0.0 then
                    yield
                        TextBlock.create
                            [ TextBlock.text "−"
                              TextBlock.fontSize 16.0
                              TextBlock.foreground (brush badLamp)
                              TextBlock.verticalAlignment VerticalAlignment.Center ]
                        :> IView

                yield! s |> Seq.map box |> List.ofSeq

                yield
                    TextBlock.create
                        [ TextBlock.text unit
                          TextBlock.fontSize 11.0
                          TextBlock.foreground (brush "#C5CDD4")
                          TextBlock.verticalAlignment VerticalAlignment.Bottom
                          TextBlock.margin (Thickness(3.0, 0.0, 0.0, 2.0)) ]
                    :> IView ] ]
    :> IView

// ---------- 操作部: レバー (中立つき) ----------

/// 縦のレバー。lo〜hi の間を上下に動かす。上が hi、下が lo。zero に中立の印を付ける。
/// 例: 貯水池の操作 (上=貯める / 下=放流、中立=0)
let lever
    (key: string)
    (width: float)
    (height: float)
    (lo: float)
    (hi: float)
    (step: float)
    (value: float)
    (zero: float)
    (topText: string)
    (bottomText: string)
    (onChange: float -> unit)
    : IView =
    let trackX = width * 0.5
    let top, bottom = 22.0, height - 22.0
    let yOf (v: float) = if hi <= lo then bottom else bottom - (v - lo) / (hi - lo) * (bottom - top)

    let onPoint (p: Point) =
        let t = (bottom - p.Y) / (bottom - top)
        let v = lo + (max 0.0 (min 1.0 t)) * (hi - lo)
        // 中立の近くは中立に吸い付く (デテント)
        let v = if abs (v - zero) < (hi - lo) * 0.04 then zero else v
        onChange (snapTo step lo hi v)

    let ky = yOf value

    Canvas.create
        [ Canvas.width width
          Canvas.height height
          Canvas.children
              [ label 0.0 0.0 width topText 10.0 "#C5CDD4" FontWeight.Normal
                label 0.0 (height - 14.0) width bottomText 10.0 "#C5CDD4" FontWeight.Normal
                // みぞ
                Border.create
                    [ Canvas.left (trackX - 4.0)
                      Canvas.top top
                      Border.width 8.0
                      Border.height (bottom - top)
                      Border.background (brush "#0F1316")
                      Border.borderBrush (brush "#59636D")
                      Border.borderThickness 1.0
                      Border.cornerRadius 4.0 ]
                :> IView
                // 中立の印
                line (trackX - 14.0) (yOf zero) (trackX + 14.0) (yOf zero) warnLamp 2.0
                // つまみ
                Border.create
                    [ Canvas.left (trackX - 16.0)
                      Canvas.top (ky - 9.0)
                      Border.width 32.0
                      Border.height 18.0
                      Border.cornerRadius 4.0
                      Border.background (brush "#C9D1D8")
                      Border.borderBrush (brush "#3A434C")
                      Border.borderThickness 2.0
                      Border.child (
                          Border.create
                              [ Border.height 3.0
                                Border.margin (Thickness(4.0, 0.0))
                                Border.background (brush "#B3261E")
                                Border.verticalAlignment VerticalAlignment.Center ]
                      ) ]
                :> IView
                surface width height onPoint ] ]
    |> View.withKey key
    :> IView

// ---------- 操作部: ロータリースイッチ ----------

/// 接点が円弧状に並んだロータリースイッチ。各接点の文字を、スイッチの外側に表示する。
/// positions は (接点の文字, 値) の並び。選ばれている接点は selected。
let rotarySwitch (key: string) (size: float) (positions: string list) (selected: int) (locked: bool) (onSelect: int -> unit) : IView =
    let c = size / 2.0
    let r = size * 0.28
    let k = positions.Length
    let angleAt (i: int) = if k <= 1 then 0.0 else -sweep + float i * (2.0 * sweep) / float (k - 1)

    let onPoint (p: Point) =
        if not locked then
            let dx, dy = p.X - c, p.Y - c
            let deg = Math.Atan2(dx, -dy) * 180.0 / Math.PI
            // いちばん近い接点を選ぶ
            let best =
                [ 0 .. k - 1 ]
                |> List.minBy (fun i ->
                    let d = abs (angleAt i - deg)
                    min d (360.0 - d))

            onSelect best

    let contacts =
        positions
        |> List.mapi (fun i text ->
            let a = angleAt i
            let x, y = pt c c (r + 10.0) a
            let tx, ty = pt c c (size * 0.43) a
            let on = (i = selected)
            // 接点の文字は、行を分けて書ける (優先順位を上から並べる)
            let lines = text.Split('\n').Length

            [ disc x y 4.0 (if on then warnLamp else "#59636D") "#1B2026" 1.0
              label (tx - 36.0) (ty - float lines * 6.5) 72.0 text 10.0 (if on then "#FFE9A8" else "#AAB4BD") (if on then FontWeight.Bold else FontWeight.Normal) ])
        |> List.concat

    let ang = angleAt selected
    let tipX, tipY = pt c c (r + 6.0) ang

    Canvas.create
        [ Canvas.width size
          Canvas.height size
          Canvas.children
              [ yield disc c c (size * 0.5 - 1.0) "#1B2026" panelEdge 1.5
                yield! contacts
                yield disc c c (r * 0.85) (if locked then "#4A4F55" else "#3A434C") "#79858F" 2.0
                yield line c c tipX tipY (if locked then "#9AA2A9" else "#FF5A4D") 4.0
                yield disc c c (r * 0.30) brassColor "#6B5520" 2.0
                yield surface size size onPoint ] ]
    |> View.withKey key
    :> IView

// ---------- 操作部: 押しボタン (表示灯つき) ----------

/// 表示灯つきの押しボタン。点灯は「選択中」を表す。押せないときは暗く、理由を銘板に出す。
let pushButtonW (textWidth: float) (text: string) (lit: bool) (enabled: bool) (onClick: unit -> unit) : IView =
    let lamp =
        Ellipse.create
            [ Ellipse.width 14.0
              Ellipse.height 14.0
              Shape.fill (brush (if lit then okLamp elif enabled then "#2A4A36" else "#2B2F33"))
              Shape.stroke (brush (if lit then "#B7F5D1" else "#101417"))
              Shape.strokeThickness 1.5
              Ellipse.verticalAlignment VerticalAlignment.Center ]

    Border.create
        [ Border.background (brush (if enabled then "#39424B" else "#2D3238"))
          Border.borderBrush (brush (if lit then okLamp elif enabled then "#79858F" else "#3A4046"))
          Border.borderThickness 1.5
          Border.cornerRadius 5.0
          Border.padding (Thickness(8.0, 6.0))
          Border.cursor (Cursor(if enabled then StandardCursorType.Hand else StandardCursorType.Arrow))
          Border.onPointerPressed ((fun e -> if enabled then onClick (); e.Handled <- true), SubPatchOptions.Always)
          Border.child (
              StackPanel.create
                  [ StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing 8.0
                    StackPanel.children
                        [ lamp
                          TextBlock.create
                              [ TextBlock.text text
                                TextBlock.fontSize 12.0
                                TextBlock.foreground (brush (if enabled then "#E8EDF1" else "#7C858D"))
                                TextBlock.textWrapping TextWrapping.Wrap
                                TextBlock.maxWidth textWidth
                                TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
          ) ]
    :> IView

let pushButton (text: string) (lit: bool) (enabled: bool) (onClick: unit -> unit) : IView =
    pushButtonW 330.0 text lit enabled onClick

// ---------- 表示灯 ----------

/// 警告灯と銘板 (灯の色 + 文字)
let warningLampW (textWidth: float) (color: string) (text: string) : IView =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 6.0
          StackPanel.children
              [ Ellipse.create
                    [ Ellipse.width 12.0
                      Ellipse.height 12.0
                      Shape.fill (brush color)
                      Shape.stroke (brush "#101417")
                      Shape.strokeThickness 1.0
                      Ellipse.verticalAlignment VerticalAlignment.Top
                      Ellipse.margin (Thickness(0.0, 3.0, 0.0, 0.0)) ]
                TextBlock.create
                    [ TextBlock.text text
                      TextBlock.fontSize 12.0
                      TextBlock.foreground (brush "#E8EDF1")
                      TextBlock.textWrapping TextWrapping.Wrap
                      TextBlock.maxWidth textWidth ] ] ]
    :> IView

let warningLamp (color: string) (text: string) : IView = warningLampW 360.0 color text

// ---------- 銘板・盤の区画 ----------

/// 銘板 (彫り込みの文字)
let plate (text: string) : IView =
    Border.create
        [ Border.background (brush "#14181C")
          Border.borderBrush (brush "#59636D")
          Border.borderThickness 1.0
          Border.cornerRadius 3.0
          Border.padding (Thickness(6.0, 2.0))
          Border.horizontalAlignment HorizontalAlignment.Center
          Border.child (
              TextBlock.create
                  [ TextBlock.text text
                    TextBlock.fontSize 11.0
                    TextBlock.fontWeight FontWeight.SemiBold
                    TextBlock.foreground (brush "#D8DEE3")
                    TextBlock.textAlignment TextAlignment.Center
                    TextBlock.textWrapping TextWrapping.Wrap ]
          ) ]
    :> IView

/// 計器1台 (部品 + 銘板 + 桁カウンタ) を縦に並べた枠
let unitBox (children: IView list) : IView =
    StackPanel.create
        [ StackPanel.spacing 4.0
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.children children ]
    :> IView

/// 盤の区画。見出しの帯と、その下の操作部・計器。
let section (title: string) (children: IView list) : IView =
    Border.create
        [ Border.background (brush panelColor)
          Border.borderBrush (brush panelEdge)
          Border.borderThickness 1.0
          Border.cornerRadius 6.0
          Border.margin (Thickness(0.0, 0.0, 0.0, 8.0))
          Border.child (
              StackPanel.create
                  [ StackPanel.children
                        [ Border.create
                              [ Border.background (brush "#3A434C")
                                Border.cornerRadius (CornerRadius(5.0, 5.0, 0.0, 0.0))
                                Border.padding (Thickness(10.0, 4.0))
                                Border.child (
                                    TextBlock.create
                                        [ TextBlock.text title
                                          TextBlock.fontSize 12.0
                                          TextBlock.fontWeight FontWeight.Bold
                                          TextBlock.foreground (brush "#DDE3E8") ]
                                ) ]
                          :> IView
                          StackPanel.create
                              [ StackPanel.margin 10.0
                                StackPanel.spacing 8.0
                                StackPanel.children children ]
                          :> IView ] ]
          ) ]
    :> IView

/// 横に並べる (折り返さない)
let hbox (spacing: float) (children: IView list) : IView =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing spacing
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.children children ]
    :> IView

/// ナイフスイッチ (大きな主開閉器)。押すと閉じる (= 運転を開始する)。
let knifeSwitch (text: string) (enabled: bool) (onClick: unit -> unit) : IView =
    let drawing =
        Canvas.create
            [ Canvas.width 86.0
              Canvas.height 44.0
              Canvas.children
                  [ Border.create
                        [ Canvas.left 4.0
                          Canvas.top 34.0
                          Border.width 78.0
                          Border.height 6.0
                          Border.cornerRadius 2.0
                          Border.background (brush "#59636D") ]
                    :> IView
                    // 受け口
                    line 66.0 34.0 66.0 14.0 brassColor 4.0
                    line 78.0 34.0 78.0 14.0 brassColor 4.0
                    // 刃 (閉じる前は、すこし持ち上がっている)
                    line 14.0 34.0 70.0 8.0 "#D6DCE1" 5.0
                    // 取っ手
                    line 62.0 12.0 74.0 6.0 "#C0392B" 8.0
                    disc 14.0 34.0 5.0 brassColor "#6B5520" 2.0 ] ]

    Border.create
        [ Border.background (brush (if enabled then "#2F3840" else "#2A2F34"))
          Border.borderBrush (brush (if enabled then "#C0392B" else "#4A4F55"))
          Border.borderThickness 2.0
          Border.cornerRadius 6.0
          Border.padding (Thickness(12.0, 6.0))
          Border.cursor (Cursor(if enabled then StandardCursorType.Hand else StandardCursorType.Arrow))
          Border.onPointerPressed ((fun e -> if enabled then onClick (); e.Handled <- true), SubPatchOptions.Always)
          Border.child (
              StackPanel.create
                  [ StackPanel.orientation Orientation.Horizontal
                    StackPanel.spacing 14.0
                    StackPanel.horizontalAlignment HorizontalAlignment.Center
                    StackPanel.children
                        [ drawing
                          TextBlock.create
                              [ TextBlock.text text
                                TextBlock.fontSize 15.0
                                TextBlock.fontWeight FontWeight.Bold
                                TextBlock.foreground (brush "#F3F0E6")
                                TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
          ) ]
    :> IView

// ---------- 観測計器 (実際の観測で使われる形) ----------

/// 小さな桁カウンタ (狭い場所用)
let miniCounter (digits: int) (value: float) : IView =
    let v = int (Math.Round(abs value))
    let s = (min v (pown 10 digits - 1)).ToString().PadLeft(digits, ' ')

    let box (c: char) =
        Border.create
            [ Border.background (brush "#0F1316")
              Border.borderBrush (brush "#59636D")
              Border.borderThickness 1.0
              Border.cornerRadius 2.0
              Border.width 12.0
              Border.height 17.0
              Border.child (
                  TextBlock.create
                      [ TextBlock.text (if c = ' ' then "" else string c)
                        TextBlock.fontSize 11.0
                        TextBlock.fontFamily (FontFamily("Consolas, DejaVu Sans Mono, monospace"))
                        TextBlock.foreground (brush "#F3F0E6")
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.verticalAlignment VerticalAlignment.Center ]
              ) ]
        :> IView

    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 1.0
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.children (s |> Seq.map box |> List.ofSeq) ]
    :> IView

/// 土壌水分計 (探針式のダイヤル。DRY・MOIST・WET の帯がある。市販の水分計の表記に合わせた)。value は 0〜1 (保水容量に対する割合)。
let soilMeter (size: float) (value: float) : IView =
    let c = size / 2.0
    let rScale = size * 0.38
    let dry, wet = 0.3, 0.7
    let a t = angleOf 0.0 1.0 t
    let ang = a (max 0.0 (min 1.0 value))
    let nx, ny = pt c c (rScale - 3.0) ang
    let lx t = pt c c (rScale - 14.0) (a t)
    let dx, dy = lx (dry / 2.0)
    let mx, my = lx ((dry + wet) / 2.0)
    let wx, wy = lx ((wet + 1.0) / 2.0)

    Canvas.create
        [ Canvas.width size
          Canvas.height size
          Canvas.children
              [ disc c c (size * 0.5 - 1.0) "#1B2026" panelEdge 1.5
                disc c c (size * 0.5 - 4.0) faceColor "#9A927C" 1.0
                shape (arcData c c (rScale + 3.0) (a 0.0) (a dry)) "#C94A3F" 5.0 PenLineCap.Flat
                shape (arcData c c (rScale + 3.0) (a dry) (a wet)) "#4F9A57" 5.0 PenLineCap.Flat
                shape (arcData c c (rScale + 3.0) (a wet) (a 1.0)) "#2E7DD1" 5.0 PenLineCap.Flat
                line c c nx ny "#B3261E" 2.0
                disc c c 4.0 "#20262B" "#20262B" 1.0
                label (dx - 14.0) (dy - 5.0) 28.0 "DRY" 7.0 inkColor FontWeight.Bold
                label (mx - 14.0) (my - 5.0) 28.0 "MOIST" 7.0 inkColor FontWeight.Bold
                label (wx - 14.0) (wy - 5.0) 28.0 "WET" 7.0 inkColor FontWeight.Bold ] ]
    :> IView

/// 縦の水位計 (透明な管に目盛りと浮き。観測井の水位計・貯水池の水位計・貯留型雨量計に使う)。
/// value は水位 (lo〜hi の目盛り)。span は「この範囲のどこか」(推定の幅や予報の幅) を右側のかっこで示す。flags は基準の高さに引く線。
let levelGauge
    (width: float)
    (height: float)
    (lo: float)
    (hi: float)
    (value: float)
    (marks: Mark list)
    (span: (float * float) option)
    (flags: (float * string) list)
    (water: string)
    : IView =
    let tubeX, tubeW = 24.0, 18.0
    let top, bottom = 8.0, height - 8.0
    let yOf (v: float) = bottom - (max lo (min hi v) - lo) / (hi - lo) * (bottom - top)
    let vy = yOf value

    let ticks =
        marks
        |> List.collect (fun (v, text) ->
            let y = yOf v
            [ line (tubeX - 5.0) y tubeX y "#C5CDD4" 1.0
              label -4.0 (y - 6.0) 22.0 text 9.0 "#C5CDD4" FontWeight.Normal ])

    let spanBracket =
        match span with
        | Some(a, b) when b > a ->
            let y0, y1 = yOf b, yOf a
            let x = tubeX + tubeW + 5.0
            [ line x y0 x y1 "#4DA3FF" 3.0
              line (x - 3.0) y0 x y0 "#4DA3FF" 2.0
              line (x - 3.0) y1 x y1 "#4DA3FF" 2.0 ]
        | _ -> []

    let flagLines =
        flags
        |> List.map (fun (v, color) -> line (tubeX - 6.0) (yOf v) (tubeX + tubeW + 10.0) (yOf v) color 2.0)

    Canvas.create
        [ Canvas.width width
          Canvas.height height
          Canvas.children
              [ yield
                    Border.create
                        [ Canvas.left tubeX
                          Canvas.top top
                          Border.width tubeW
                          Border.height (bottom - top)
                          Border.background (brush "#0F1316")
                          Border.borderBrush (brush "#8E99A3")
                          Border.borderThickness 1.5
                          Border.cornerRadius 3.0 ]
                    :> IView
                // 水
                yield
                    Border.create
                        [ Canvas.left (tubeX + 1.5)
                          Canvas.top vy
                          Border.width (tubeW - 3.0)
                          Border.height (max 0.0 (bottom - vy - 1.5))
                          Border.background (brush water) ]
                    :> IView
                yield! ticks
                yield! spanBracket
                yield! flagLines
                // 浮き
                yield
                    shape $"M {n (tubeX - 1.0)},{n vy} L {n (tubeX - 7.0)},{n (vy - 4.0)} L {n (tubeX - 7.0)},{n (vy + 4.0)} Z" "#FFE9A8" 1.5 PenLineCap.Round ] ]
    :> IView

/// 点灯ランプ + 短い文字 (調査段階・井戸などの状態の表示)
let lampDot (lit: bool) (color: string) (text: string) : IView =
    StackPanel.create
        [ StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 2.0
          StackPanel.children
              [ Ellipse.create
                    [ Ellipse.width 9.0
                      Ellipse.height 9.0
                      Shape.fill (brush (if lit then color else "#2B2F33"))
                      Shape.stroke (brush (if lit then "#FFFFFF" else "#101417"))
                      Shape.strokeThickness 1.0
                      Ellipse.verticalAlignment VerticalAlignment.Center ]
                TextBlock.create
                    [ TextBlock.text text
                      TextBlock.fontSize 10.0
                      TextBlock.foreground (brush (if lit then "#E8EDF1" else "#7C858D"))
                      TextBlock.verticalAlignment VerticalAlignment.Center ] ] ]
    :> IView

/// 計器の下の小さな銘 (幅を指定した中央そろえの文字)
let gaugeLabel (width: float) (text: string) : IView =
    Canvas.create
        [ Canvas.width width
          Canvas.height 12.0
          Canvas.children [ label 0.0 0.0 width text 9.0 "#C5CDD4" FontWeight.Normal ] ]
    :> IView

/// 順位セレクタ (サムホイール式スイッチを縦に並べたもの)。各段に、順位・項目名の窓・上げ下げの押し釦がある。
/// onMove は (段, 動かす量: -1 で上へ / +1 で下へ)。固定のときは釦が暗くなる。
let rankSelector (key: string) (names: string list) (locked: bool) (onMove: int -> int -> unit) : IView =
    let count = names.Length

    let btn (text: string) (enabled: bool) (onClick: unit -> unit) =
        Border.create
            [ Border.width 24.0
              Border.height 24.0
              Border.cornerRadius 4.0
              Border.background (brush (if enabled then "#39424B" else "#2D3238"))
              Border.borderBrush (brush (if enabled then "#79858F" else "#3A4046"))
              Border.borderThickness 1.5
              Border.cursor (Cursor(if enabled then StandardCursorType.Hand else StandardCursorType.Arrow))
              Border.onPointerPressed ((fun e -> if enabled then onClick (); e.Handled <- true), SubPatchOptions.Always)
              Border.child (
                  TextBlock.create
                      [ TextBlock.text text
                        TextBlock.fontSize 11.0
                        TextBlock.foreground (brush (if enabled then "#E8EDF1" else "#6A737B"))
                        TextBlock.horizontalAlignment HorizontalAlignment.Center
                        TextBlock.verticalAlignment VerticalAlignment.Center ]
              ) ]
        :> IView

    let rowOf (i: int) (name: string) =
        StackPanel.create
            [ StackPanel.orientation Orientation.Horizontal
              StackPanel.spacing 4.0
              StackPanel.children
                  [ TextBlock.create
                        [ TextBlock.text $"第{i + 1}位"
                          TextBlock.width 34.0
                          TextBlock.fontSize 11.0
                          TextBlock.foreground (brush "#C5CDD4")
                          TextBlock.verticalAlignment VerticalAlignment.Center ]
                    :> IView
                    Border.create
                        [ Border.width 100.0
                          Border.height 24.0
                          Border.background (brush "#0F1316")
                          Border.borderBrush (brush "#59636D")
                          Border.borderThickness 1.0
                          Border.cornerRadius 3.0
                          Border.child (
                              TextBlock.create
                                  [ TextBlock.text name
                                    TextBlock.fontSize 12.0
                                    TextBlock.fontWeight FontWeight.Bold
                                    TextBlock.foreground (brush (if locked then "#B9AE8A" else "#FFE9A8"))
                                    TextBlock.horizontalAlignment HorizontalAlignment.Center
                                    TextBlock.verticalAlignment VerticalAlignment.Center ]
                          ) ]
                    :> IView
                    btn "▲" (not locked && i > 0) (fun () -> onMove i -1)
                    btn "▼" (not locked && i < count - 1) (fun () -> onMove i 1) ] ]
        :> IView

    StackPanel.create [ StackPanel.spacing 4.0; StackPanel.children (names |> List.mapi rowOf) ]
    |> View.withKey key
    :> IView
