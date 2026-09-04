namespace Kasino.UI.Web

open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Options screen: toggle the optional settings. Opened as an overlay
// from the main menu and returns to it. Mirrors the desktop screen.
// In mobile mode the rows, labels, preview and Back button scale up
// with CardRenderer.UiScale (capped so six rows still fit the height).
// ─────────────────────────────────────────────────────────────

module OptionsScreen =

    type OptionsState =
        { Settings: Settings.GameSettings
          BackClicked: bool }

    let create (settings: Settings.GameSettings) =
        { Settings = settings; BackClicked = false }

    let private toggleColor (on: bool) =
        if on then Color.rgb 40 120 60 else Color.rgb 90 90 90

    [<Literal>]
    let private RowsTop = 180
    [<Literal>]
    let private RowStep = 64
    [<Literal>]
    let private RowH = 52
    [<Literal>]
    let private RowW = 420

    /// Scale for this screen: the menu's UI scale, reduced if six rows plus
    /// the Back button would not fit the (landscape) height.
    let private layoutScale (screenH: int) =
        let fitH = float (screenH - RowsTop - 110) / float (5 * RowStep + RowH)
        max 1.0 (min CardRenderer.UiScale fitH)

    let private toggleButtons (screenW: int) (scale: float) (s: Settings.GameSettings) =
        let S (v: int) = int (float v * scale)
        let onOff b = if b then "ON" else "OFF"
        let row i = RowsTop + i * S RowStep
        [ Button.createCentered (sprintf "Random card backs:  %s" (onOff s.RandomCardBacks)) screenW (row 0) (S RowW) (S RowH) (toggleColor s.RandomCardBacks) Color.White
          Button.createCentered (sprintf "Table layout: %s" (if s.DefaultScatter then "Scatter" else "Grid")) screenW (row 1) (S RowW) (S RowH) (Color.rgb 60 70 110) Color.White
          Button.createCentered (sprintf "AI table-talk (chat):  %s" (onOff s.ChatEnabled)) screenW (row 2) (S RowW) (S RowH) (toggleColor s.ChatEnabled) Color.White
          Button.createCentered (sprintf "AI personalities:  %s" (onOff s.AiPersonalities)) screenW (row 3) (S RowW) (S RowH) (toggleColor s.AiPersonalities) Color.White
          Button.createCentered (sprintf "Strict rules (no capture cancel):  %s" (onOff s.StrictRules)) screenW (row 4) (S RowW) (S RowH) (toggleColor s.StrictRules) Color.White
          Button.createCentered (sprintf "Card deck: %s" (Settings.CardStyle.label s.CardStyle)) screenW (row 5) (S RowW) (S RowH) (Color.rgb 60 70 110) Color.White ]

    let private backButton (screenH: int) (scale: float) =
        let S (v: int) = int (float v * scale)
        Button.create "Back" 20 (screenH - S 70) (S 140) (S 52) (Color.rgb 120 40 40) Color.White

    let update (input: Input.InputState) (screenW: int) (screenH: int) (state: OptionsState) =
        let scale = layoutScale screenH
        if Button.isClicked input (backButton screenH scale) || input.Keyboard.IsEscapePressed then
            { state with BackClicked = true }
        else
            let s = state.Settings
            match Button.findClicked input (toggleButtons screenW scale s) with
            | Some 0 -> { state with Settings = { s with RandomCardBacks = not s.RandomCardBacks } }
            | Some 1 -> { state with Settings = { s with DefaultScatter = not s.DefaultScatter } }
            | Some 2 -> { state with Settings = { s with ChatEnabled = not s.ChatEnabled } }
            | Some 3 -> { state with Settings = { s with AiPersonalities = not s.AiPersonalities } }
            | Some 4 -> { state with Settings = { s with StrictRules = not s.StrictRules } }
            | Some 5 -> { state with Settings = { s with CardStyle = Settings.CardStyle.next s.CardStyle } }
            | _ -> state

    let draw (g: Gfx) (decks: Settings.CardStyle -> CardRenderer.CardTextures option) (input: Input.InputState) (state: OptionsState) (screenW: int) (screenH: int) =
        let scale = layoutScale screenH
        let S (v: int) = int (float v * scale)
        let cx = float (screenW / 2)
        let drawCentered (text: string) (y: int) (color: Color) =
            let size = Gfx.measure g text
            Gfx.fillText g text (cx - size.X / 2.0) (float y) color

        drawCentered "Options" 50 Color.Gold
        drawCentered "Tap a row to change it. These are all optional." 110 Color.LightGray

        // Rows and Back get the scaled font; the font shrinks just enough for
        // the widest label to stay inside its row.
        let rows = toggleButtons screenW scale state.Settings
        let baseFont = g.FontSize
        g.FontSize <- int (float baseFont * scale)
        let widest = rows |> List.map (fun b -> (Gfx.measure g b.Text).X) |> List.max
        let roomW = float (S RowW - 24)
        if widest > roomW then g.FontSize <- int (float g.FontSize * roomW / widest)
        Button.drawAll g input rows
        Button.draw g input (backButton screenH scale)
        g.FontSize <- baseFont

        // Live preview of the selected deck (10 of diamonds and 2 of spades):
        // beside the rows when there is room, otherwise centred below them.
        match decks state.Settings.CardStyle with
        | Some tex ->
            let w, h = S 120, S 152
            let rowsBottom = RowsTop + 5 * S RowStep + S RowH
            let rowsRight = screenW / 2 + S RowW / 2
            let x0, y0 =
                if rowsRight + 30 + 2 * w + 12 <= screenW - 10 then
                    rowsRight + 30, RowsTop + (rowsBottom - RowsTop - h) / 2
                else
                    (screenW - (2 * w + 12)) / 2, rowsBottom + 46
            let size = Gfx.measure g "Preview"
            Gfx.fillText g "Preview" (float (x0 + w + 6) - size.X / 2.0) (float (y0 - 34)) Color.LightGray
            for i, card in List.indexed [ { Suit = Diamonds; Rank = Ten }; { Suit = Spades; Rank = Two } ] do
                Gfx.drawImage g (CardRenderer.getTexture tex card) (x0 + i * (w + 12)) y0 w h
        | None -> ()
        drawCentered "Esc: back" (screenH - 20) Color.DarkGray
