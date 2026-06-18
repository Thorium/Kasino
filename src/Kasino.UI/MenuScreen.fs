namespace Kasino.UI

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open FontStashSharp
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Menu screen: variant selection, player count, human count.
// Supports both tappable buttons (touch) and keyboard shortcuts.
// ─────────────────────────────────────────────────────────────

module MenuScreen =

    type MenuChoice =
        | VariantSelect
        | PlayerCountSelect
        | HumanCountSelect
        | Ready
        | ShowRules
        | ShowOptions

    type MenuState =
        { Step: MenuChoice
          Variant: GameVariant
          PlayerCount: int
          HumanCount: int }

    let initial =
        { Step = VariantSelect
          Variant = StandardKasino
          PlayerCount = 2
          HumanCount = 1 }

    // ── Button definitions (recomputed each frame) ──────────

    let private variantButtons (screenW: int) =
        [ Button.createCentered "Standard Kasino (maximize)" screenW 200 360 52 (Color(40, 100, 40)) Color.White
          Button.createCentered "Laistokasino (minimize)"    screenW 264 360 52 (Color(140, 60, 40)) Color.White ]

    let private playerCountButtons (screenW: int) =
        let bw = 180
        let gap = 20
        let totalW = 3 * bw + 2 * gap
        let baseX = (screenW - totalW) / 2
        [ Button.create "2 Players" baseX              220 bw 52 (Color(40, 80, 120)) Color.White
          Button.create "3 Players" (baseX + bw + gap) 220 bw 52 (Color(40, 80, 120)) Color.White
          Button.create "4 Players" (baseX + 2 * (bw + gap)) 220 bw 52 (Color(40, 80, 120)) Color.White ]

    let private humanCountButtons (screenW: int) =
        [ Button.createCentered "Watch AI Only"  screenW 220 320 52 (Color(80, 60, 120)) Color.White
          Button.createCentered "Play Yourself"  screenW 284 320 52 (Color(40, 120, 80)) Color.White ]

    /// "How to Play" button — visible on all menu steps, positioned near bottom
    let private howToPlayButton (screenW: int) (screenH: int) =
        Button.createCentered "How to Play" screenW (screenH - 80) 220 52 (Color(80, 80, 40)) Color.White

    /// "Options" button — visible on all menu steps, just above "How to Play"
    let private optionsButton (screenW: int) (screenH: int) =
        Button.createCentered "Options" screenW (screenH - 142) 220 52 (Color(60, 60, 100)) Color.White

    /// Advance menu based on input (touch buttons + keyboard fallback)
    let update (input: InputHandler.InputState) (screenW: int) (screenH: int) (state: MenuState) =
        // Check "How to Play" / "Options" buttons first — available on all steps
        let helpBtn = howToPlayButton screenW screenH
        let optBtn = optionsButton screenW screenH
        if Button.isClicked input helpBtn then
            { state with Step = ShowRules }
        elif Button.isClicked input optBtn then
            { state with Step = ShowOptions }
        else
        match state.Step with
        | VariantSelect ->
            let buttons = variantButtons screenW
            match Button.findClicked input buttons with
            | Some 0 -> { state with Variant = StandardKasino; Step = PlayerCountSelect }
            | Some 1 -> { state with Variant = LaistoKasino;   Step = PlayerCountSelect }
            | _ ->
                // Keyboard fallback
                match input.Keyboard.NumberPressed with
                | Some 1 -> { state with Variant = StandardKasino; Step = PlayerCountSelect }
                | Some 2 -> { state with Variant = LaistoKasino;   Step = PlayerCountSelect }
                | _ -> state
        | PlayerCountSelect ->
            let buttons = playerCountButtons screenW
            match Button.findClicked input buttons with
            | Some 0 -> { state with PlayerCount = 2; Step = HumanCountSelect }
            | Some 1 -> { state with PlayerCount = 3; Step = HumanCountSelect }
            | Some 2 -> { state with PlayerCount = 4; Step = HumanCountSelect }
            | _ ->
                match input.Keyboard.NumberPressed with
                | Some n when n >= 2 && n <= 4 ->
                    { state with PlayerCount = n; Step = HumanCountSelect }
                | _ -> state
        | HumanCountSelect ->
            let buttons = humanCountButtons screenW
            match Button.findClicked input buttons with
            | Some 0 -> { state with HumanCount = 0; Step = Ready }
            | Some 1 -> { state with HumanCount = 1; Step = Ready }
            | _ ->
                match input.Keyboard.NumberPressed with
                | Some 0 -> { state with HumanCount = 0; Step = Ready }
                | Some 1 -> { state with HumanCount = 1; Step = Ready }
                | _ -> state
        | Ready -> state
        | ShowRules -> state
        | ShowOptions -> state

    /// Draw menu screen with tappable buttons
    let draw (sb: SpriteBatch) (font: SpriteFontBase) (input: InputHandler.InputState) (state: MenuState) (screenW: int) (screenH: int) =
        let cx = screenW / 2
        let drawCentered (text: string) (y: int) (color: Color) =
            let size = font.MeasureString(text)
            sb.DrawString(font, text, Vector2(float32 cx - size.X / 2.0f, float32 y), color) |> ignore

        drawCentered "KASINO" 40 Color.Gold
        drawCentered "Finnish Card Game" 80 Color.White

        match state.Step with
        | VariantSelect ->
            drawCentered "Choose game variant:" 160 Color.LightGray
            Button.drawAll sb font input (variantButtons screenW)

        | PlayerCountSelect ->
            let vName = match state.Variant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
            drawCentered ($"Variant: {vName}") 140 Color.Gold
            drawCentered "Number of players:" 180 Color.LightGray
            Button.drawAll sb font input (playerCountButtons screenW)

        | HumanCountSelect ->
            let vName = match state.Variant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
            drawCentered ($"Variant: {vName}  |  Players: {state.PlayerCount}") 140 Color.Gold
            drawCentered "How many human players?" 180 Color.LightGray
            Button.drawAll sb font input (humanCountButtons screenW)

        | Ready ->
            ()

        | ShowRules | ShowOptions ->
            ()   // handled by KasinoGame — should not draw menu in these states

        // "Options" / "How to Play" buttons visible on all non-transitional steps
        match state.Step with
        | Ready | ShowRules | ShowOptions -> ()
        | _ ->
            Button.draw sb font input (optionsButton screenW screenH)
            Button.draw sb font input (howToPlayButton screenW screenH)
