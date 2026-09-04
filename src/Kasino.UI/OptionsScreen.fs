namespace Kasino.UI

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open FontStashSharp
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Options screen: toggle the optional settings. Opened as an overlay
// from the main menu and returns to it. Each setting is a single
// toggle button (green = ON, gray = OFF).
// ─────────────────────────────────────────────────────────────

module OptionsScreen =

    type OptionsState =
        { Settings: Settings.GameSettings
          BackClicked: bool }

    let create (settings: Settings.GameSettings) =
        { Settings = settings; BackClicked = false }

    // ── Button layout ───────────────────────────────────
    let private toggleColor (on: bool) =
        if on then Color(40, 120, 60) else Color(90, 90, 90)

    /// The toggle buttons, in display order, with the text reflecting state.
    let private toggleButtons (screenW: int) (s: Settings.GameSettings) =
        let onOff b = if b then "ON" else "OFF"
        let y0 = 180
        let dy = 64
        [ Button.createCentered (sprintf "Random card backs:  %s" (onOff s.RandomCardBacks)) screenW y0 420 52 (toggleColor s.RandomCardBacks) Color.White
          Button.createCentered (sprintf "Table layout: %s" (if s.DefaultScatter then "Scatter" else "Grid")) screenW (y0 + dy) 420 52 (Color(60, 70, 110)) Color.White
          Button.createCentered (sprintf "AI table-talk (chat):  %s" (onOff s.ChatEnabled)) screenW (y0 + 2 * dy) 420 52 (toggleColor s.ChatEnabled) Color.White
          Button.createCentered (sprintf "AI personalities:  %s" (onOff s.AiPersonalities)) screenW (y0 + 3 * dy) 420 52 (toggleColor s.AiPersonalities) Color.White
          Button.createCentered (sprintf "Strict rules (no capture cancel):  %s" (onOff s.StrictRules)) screenW (y0 + 4 * dy) 420 52 (toggleColor s.StrictRules) Color.White
          Button.createCentered (sprintf "Card deck: %s" (Settings.CardStyle.label s.CardStyle)) screenW (y0 + 5 * dy) 420 52 (Color(60, 70, 110)) Color.White ]

    let private backButton (screenH: int) =
        Button.create "Back" 20 (screenH - 70) 140 52 (Color(120, 40, 40)) Color.White

    // ── Update ──────────────────────────────────────────
    let update (input: InputHandler.InputState) (screenW: int) (screenH: int) (state: OptionsState) =
        if Button.isClicked input (backButton screenH) || input.Keyboard.IsEscapePressed then
            { state with BackClicked = true }
        else
            let s = state.Settings
            match Button.findClicked input (toggleButtons screenW s) with
            | Some 0 -> { state with Settings = { s with RandomCardBacks = not s.RandomCardBacks } }
            | Some 1 -> { state with Settings = { s with DefaultScatter = not s.DefaultScatter } }
            | Some 2 -> { state with Settings = { s with ChatEnabled = not s.ChatEnabled } }
            | Some 3 -> { state with Settings = { s with AiPersonalities = not s.AiPersonalities } }
            | Some 4 -> { state with Settings = { s with StrictRules = not s.StrictRules } }
            | Some 5 -> { state with Settings = { s with CardStyle = Settings.CardStyle.next s.CardStyle } }
            | _ -> state

    // ── Draw ────────────────────────────────────────────
    let draw (sb: SpriteBatch) (font: SpriteFontBase) (decks: Settings.CardStyle -> CardRenderer.CardTextures option) (input: InputHandler.InputState) (state: OptionsState) (screenW: int) (screenH: int) =
        let cx = float32 screenW / 2.0f
        let drawCentered (text: string) (y: int) (color: Color) =
            let size = font.MeasureString text
            sb.DrawString(font, text, Vector2(cx - size.X / 2.0f, float32 y), color) |> ignore

        drawCentered "Options" 50 Color.Gold
        drawCentered "Tap a row to change it. These are all optional." 110 Color.LightGray

        Button.drawAll sb font input (toggleButtons screenW state.Settings)

        // Live preview of the selected deck (10 of diamonds and 2 of spades)
        // to the right of the option rows, vertically centred on them.
        match decks state.Settings.CardStyle with
        | Some tex ->
            let w, h = 120, 152
            let x0 = screenW / 2 + 240
            let y0 = 180 + (6 * 64 - 12 - h) / 2
            let size = font.MeasureString "Preview"
            sb.DrawString(font, "Preview", Vector2(float32 (x0 + w + 6) - size.X / 2.0f, float32 (y0 - 34)), Color.LightGray) |> ignore
            for i, card in List.indexed [ { Suit = Diamonds; Rank = Ten }; { Suit = Spades; Rank = Two } ] do
                let dest = Rectangle(x0 + i * (w + 12), y0, w, h)
                sb.Draw(CardRenderer.getTexture tex card, dest, Color.White)
        | None -> ()
        Button.draw sb font input (backButton screenH)
        drawCentered "Esc: back" (screenH - 20) Color.DarkGray
