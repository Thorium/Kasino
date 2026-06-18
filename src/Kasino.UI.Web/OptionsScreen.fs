namespace Kasino.UI.Web

open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Options screen: toggle the optional settings. Opened as an overlay
// from the main menu and returns to it. Mirrors the desktop screen.
// ─────────────────────────────────────────────────────────────

module OptionsScreen =

    type OptionsState =
        { Settings: Settings.GameSettings
          BackClicked: bool }

    let create (settings: Settings.GameSettings) =
        { Settings = settings; BackClicked = false }

    let private toggleColor (on: bool) =
        if on then Color.rgb 40 120 60 else Color.rgb 90 90 90

    let private toggleButtons (screenW: int) (s: Settings.GameSettings) =
        let onOff b = if b then "ON" else "OFF"
        let y0 = 180
        let dy = 64
        [ Button.createCentered (sprintf "Random card backs:  %s" (onOff s.RandomCardBacks)) screenW y0 420 52 (toggleColor s.RandomCardBacks) Color.White
          Button.createCentered (sprintf "Table layout: %s" (if s.DefaultScatter then "Scatter" else "Grid")) screenW (y0 + dy) 420 52 (Color.rgb 60 70 110) Color.White
          Button.createCentered (sprintf "AI table-talk (chat):  %s" (onOff s.ChatEnabled)) screenW (y0 + 2 * dy) 420 52 (toggleColor s.ChatEnabled) Color.White
          Button.createCentered (sprintf "AI personalities:  %s" (onOff s.AiPersonalities)) screenW (y0 + 3 * dy) 420 52 (toggleColor s.AiPersonalities) Color.White ]

    let private backButton (screenH: int) =
        Button.create "Back" 20 (screenH - 70) 140 52 (Color.rgb 120 40 40) Color.White

    let update (input: Input.InputState) (screenW: int) (screenH: int) (state: OptionsState) =
        if Button.isClicked input (backButton screenH) || input.Keyboard.IsEscapePressed then
            { state with BackClicked = true }
        else
            let s = state.Settings
            match Button.findClicked input (toggleButtons screenW s) with
            | Some 0 -> { state with Settings = { s with RandomCardBacks = not s.RandomCardBacks } }
            | Some 1 -> { state with Settings = { s with DefaultScatter = not s.DefaultScatter } }
            | Some 2 -> { state with Settings = { s with ChatEnabled = not s.ChatEnabled } }
            | Some 3 -> { state with Settings = { s with AiPersonalities = not s.AiPersonalities } }
            | _ -> state

    let draw (g: Gfx) (input: Input.InputState) (state: OptionsState) (screenW: int) (screenH: int) =
        let cx = float (screenW / 2)
        let drawCentered (text: string) (y: int) (color: Color) =
            let size = Gfx.measure g text
            Gfx.fillText g text (cx - size.X / 2.0) (float y) color

        drawCentered "Options" 50 Color.Gold
        drawCentered "Tap a row to change it. These are all optional." 110 Color.LightGray

        Button.drawAll g input (toggleButtons screenW state.Settings)
        Button.draw g input (backButton screenH)
        drawCentered "Esc: back" (screenH - 20) Color.DarkGray
