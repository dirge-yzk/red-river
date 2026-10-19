module WaterGame.App.Program

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.FuncUI.Elmish
open Elmish

type MainWindow() as this =
    inherit HostWindow()

    do
        base.Title <- "Red River"
        base.Width <- 1180.0
        base.Height <- 800.0
        base.MinWidth <- 1100.0
        base.MinHeight <- 720.0

        // WATERGAME_TRACE=1 のとき、処理したメッセージを標準エラーに出す (画面が固まるなどの不具合の調査用)
        let traced =
            if System.Environment.GetEnvironmentVariable "WATERGAME_TRACE" = "1" then
                Program.withTrace (fun msg _ _ -> eprintfn "[msg] %A" msg)
            else
                id

        Program.mkProgram Model.init Model.update View.view
        |> traced
        |> Program.withHost this
        |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Light

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktop -> desktop.MainWindow <- MainWindow()
        | _ -> ()

        base.OnFrameworkInitializationCompleted()

[<EntryPoint>]
let main args =
    Model.launchOptions <- Model.parseArgs args

    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime(args)
