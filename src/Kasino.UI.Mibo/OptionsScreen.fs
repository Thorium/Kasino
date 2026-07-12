namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Options overlay: toggle the optional settings. Update logic is identical to
// the MonoGame build; drawing emits Draw.* into the render buffer.
// ─────────────────────────────────────────────────────────────

module OptionsScreen =

    type OptionsState =
        { Settings: Settings.GameSettings
          BackClicked: bool }

    let create (settings: Settings.GameSettings) =
        { Settings = settings; BackClicked = false }

    let private toggleColor (on: bool) =
        if on then Color(40, 120, 60) else Color(90, 90, 90)

    let private toggleButtons (screenW: int) (s: Settings.GameSettings) =
        let onOff b = if b then "ON" else "OFF"
        let y0 = 180
        let dy = 64
        [ Button.createCentered (sprintf "Random card backs:  %s" (onOff s.RandomCardBacks)) screenW y0 420 52 (toggleColor s.RandomCardBacks) Color.White
          Button.createCentered (sprintf "Table layout: %s" (if s.DefaultScatter then "Scatter" else "Grid")) screenW (y0 + dy) 420 52 (Color(60, 70, 110)) Color.White
          Button.createCentered (sprintf "AI table-talk (chat):  %s" (onOff s.ChatEnabled)) screenW (y0 + 2 * dy) 420 52 (toggleColor s.ChatEnabled) Color.White
          Button.createCentered (sprintf "AI personalities:  %s" (onOff s.AiPersonalities)) screenW (y0 + 3 * dy) 420 52 (toggleColor s.AiPersonalities) Color.White ]

    let private backButton (screenH: int) =
        Button.create "Back" 20 (screenH - 70) 140 52 (Color(120, 40, 40)) Color.White

    // ── Update ──
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

    // ── Draw ──
    let draw buffer (font: SpriteFont) (input: Input.InputState) (state: OptionsState) (screenW: int) (screenH: int) =
        let cx = float32 screenW / 2.0f
        let drawCentered (text: string) (y: int) (color: Color) =
            Render.textCentered buffer Render.LLabel font text cx (float32 y) color

        drawCentered "Options" 50 Color.Gold
        drawCentered "Tap a row to change it. These are all optional." 110 Color.LightGray

        Button.drawAll buffer font input (toggleButtons screenW state.Settings)
        Button.draw buffer font input (backButton screenH)
        drawCentered "Esc: back" (screenH - 20) Color.DarkGray
