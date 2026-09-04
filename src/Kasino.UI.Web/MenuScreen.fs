namespace Kasino.UI.Web

open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Menu screen: variant selection, player count, human count.
// Supports tappable buttons and keyboard shortcuts.
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
    // Everything scales with CardRenderer.UiScale (about 1.8 on touch devices).
    let private S (v: int) = int (float v * CardRenderer.UiScale)

    let private variantButtons (screenW: int) =
        [ Button.createCentered "Standard Kasino (maximize)" screenW 200 (S 360) (S 52) (Color.rgb 40 100 40) Color.White
          Button.createCentered "Laistokasino (minimize)" screenW (200 + S 64) (S 360) (S 52) (Color.rgb 140 60 40) Color.White ]

    let private playerCountButtons (screenW: int) =
        let gap = S 20
        let bw = min (S 180) ((screenW - 2 * gap - 40) / 3)   // three across must fit the width
        let totalW = 3 * bw + 2 * gap
        let baseX = (screenW - totalW) / 2
        [ Button.create "2 Players" baseX 220 bw (S 52) (Color.rgb 40 80 120) Color.White
          Button.create "3 Players" (baseX + bw + gap) 220 bw (S 52) (Color.rgb 40 80 120) Color.White
          Button.create "4 Players" (baseX + 2 * (bw + gap)) 220 bw (S 52) (Color.rgb 40 80 120) Color.White ]

    let private humanCountButtons (screenW: int) =
        [ Button.createCentered "Watch AI Only" screenW 220 (S 320) (S 52) (Color.rgb 80 60 120) Color.White
          Button.createCentered "Play Yourself" screenW (220 + S 64) (S 320) (S 52) (Color.rgb 40 120 80) Color.White ]

    /// "How to Play" button — visible on all menu steps, near the bottom.
    let private howToPlayButton (screenW: int) (screenH: int) =
        Button.createCentered "How to Play" screenW (screenH - S 80) (S 220) (S 52) (Color.rgb 80 80 40) Color.White

    /// "Options" button — visible on all menu steps, just above "How to Play".
    let private optionsButton (screenW: int) (screenH: int) =
        Button.createCentered "Options" screenW (screenH - S 142) (S 220) (S 52) (Color.rgb 60 60 100) Color.White

    /// Advance menu based on input (touch buttons + keyboard fallback).
    let update (input: Input.InputState) (screenW: int) (screenH: int) (state: MenuState) =
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
            | Some 1 -> { state with Variant = LaistoKasino; Step = PlayerCountSelect }
            | _ ->
                match input.Keyboard.NumberPressed with
                | Some 1 -> { state with Variant = StandardKasino; Step = PlayerCountSelect }
                | Some 2 -> { state with Variant = LaistoKasino; Step = PlayerCountSelect }
                | _ -> state
        | PlayerCountSelect ->
            let buttons = playerCountButtons screenW
            match Button.findClicked input buttons with
            | Some 0 -> { state with PlayerCount = 2; Step = HumanCountSelect }
            | Some 1 -> { state with PlayerCount = 3; Step = HumanCountSelect }
            | Some 2 -> { state with PlayerCount = 4; Step = HumanCountSelect }
            | _ ->
                match input.Keyboard.NumberPressed with
                | Some n when n >= 2 && n <= 4 -> { state with PlayerCount = n; Step = HumanCountSelect }
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
        | Ready
        | ShowRules
        | ShowOptions -> state

    /// Draw menu screen with tappable buttons.
    let draw (g: Gfx) (texOpt: CardRenderer.CardTextures option) (input: Input.InputState) (state: MenuState) (screenW: int) (screenH: int) =
        let cx = float (screenW / 2)
        let drawCentered (text: string) (y: int) (color: Color) =
            let size = Gfx.measure g text
            Gfx.fillText g text (cx - size.X / 2.0) (float y) color

        drawCentered "KASINO" 40 Color.Gold
        drawCentered "Finnish Card Game" 80 Color.White

        // Decorative fan of the four aces — held-in-hand shape — in the empty
        // band between the selection buttons and the bottom Options button.
        // Scaled with the menu, but never taller than the band it sits in.
        match texOpt with
        | Some tex ->
            // x,y is the card centre (drawImageRotated rotates about it).
            let drawAce (card: Card) (x: float) (y: float) (w: int) (h: int) (rot: float) =
                Gfx.drawImageRotated g (CardRenderer.getTexture tex card) x y w h rot
            let bandTop = 220 + S 64 + S 52                       // below the tallest button set
            let bandBottom = screenH - S 142                      // top of the Options button
            let fanH = min (S 76) (int (float (bandBottom - bandTop) * 0.85))
            let fanW = fanH * 60 / 76
            let fanY = float ((bandTop + bandBottom) / 2)         // midpoint of the empty band
            let acesFan = [ Spades, -0.30; Hearts, -0.10; Diamonds, 0.10; Clubs, 0.30 ]
            for i, (suit, rot) in List.indexed acesFan do
                let off = float i - 1.5                          // -1.5, -0.5, 0.5, 1.5
                drawAce { Suit = suit; Rank = Ace } (cx + off * 0.9 * float fanW) (fanY + abs off * 0.15 * float fanW) fanW fanH rot
        | None -> ()

        // Bigger buttons get bigger labels: draw them with the font scaled by
        // UiScale, keeping the prompt texts at their normal size.
        let drawButtons (buttons: Button.ButtonDef list) =
            let baseFont = g.FontSize
            g.FontSize <- int (float baseFont * CardRenderer.UiScale)
            Button.drawAll g input buttons
            g.FontSize <- baseFont

        match state.Step with
        | VariantSelect ->
            drawCentered "Choose game variant:" 160 Color.LightGray
            drawButtons (variantButtons screenW)
        | PlayerCountSelect ->
            let vName = match state.Variant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
            drawCentered ($"Variant: %s{vName}") 140 Color.Gold
            drawCentered "Number of players:" 180 Color.LightGray
            drawButtons (playerCountButtons screenW)
        | HumanCountSelect ->
            let vName = match state.Variant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
            drawCentered ($"Variant: %s{vName}  |  Players: %d{state.PlayerCount}") 140 Color.Gold
            drawCentered "How many human players?" 180 Color.LightGray
            drawButtons (humanCountButtons screenW)
        | Ready
        | ShowRules | ShowOptions -> ()

        match state.Step with
        | Ready | ShowRules | ShowOptions -> ()
        | _ ->
            drawButtons [ optionsButton screenW screenH; howToPlayButton screenW screenH ]
