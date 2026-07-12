namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Menu screen: variant / player-count / human-count selection. Update logic
// is identical to the MonoGame build; drawing emits Draw.* into the buffer.
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

    let private howToPlayButton (screenW: int) (screenH: int) =
        Button.createCentered "How to Play" screenW (screenH - 80) 220 52 (Color(80, 80, 40)) Color.White

    let private optionsButton (screenW: int) (screenH: int) =
        Button.createCentered "Options" screenW (screenH - 142) 220 52 (Color(60, 60, 100)) Color.White

    // ── Update ──
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
            | Some 1 -> { state with Variant = LaistoKasino;   Step = PlayerCountSelect }
            | _ ->
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

    // ── Draw ──
    let draw buffer (font: SpriteFont) (texOpt: CardRenderer.CardTextures option) (input: Input.InputState) (state: MenuState) (screenW: int) (screenH: int) =
        let cx = float32 (screenW / 2)
        let drawCentered (text: string) (y: int) (color: Color) =
            Render.textCentered buffer Render.LLabel font text cx (float32 y) color

        drawCentered "KASINO" 40 Color.Gold
        drawCentered "Finnish Card Game" 80 Color.White

        // Decorative fan of the four aces in the empty band.
        match texOpt with
        | Some tex ->
            let drawAce (card: Card) (x: float32) (y: float32) (w: int) (h: int) (rot: float32) =
                // x,y is the card centre (the fan positions cards by their centre).
                Render.spriteCentered buffer Render.LTableCard (CardRenderer.getTexture tex card) (int x) (int y) w h rot
            let fanY = float32 ((336 + (screenH - 142)) / 2)
            let acesFan = [ Spades, -0.30f; Hearts, -0.10f; Diamonds, 0.10f; Clubs, 0.30f ]
            for i, (suit, rot) in List.indexed acesFan do
                let off = float32 i - 1.5f
                drawAce { Suit = suit; Rank = Ace } (cx + off * 54.0f) (fanY + abs off * 9.0f) 60 76 rot
        | None -> ()

        match state.Step with
        | VariantSelect ->
            drawCentered "Choose game variant:" 160 Color.LightGray
            Button.drawAll buffer font input (variantButtons screenW)
        | PlayerCountSelect ->
            let vName = match state.Variant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
            drawCentered $"Variant: {vName}" 140 Color.Gold
            drawCentered "Number of players:" 180 Color.LightGray
            Button.drawAll buffer font input (playerCountButtons screenW)
        | HumanCountSelect ->
            let vName = match state.Variant with StandardKasino -> "Standard" | LaistoKasino -> "Laisto"
            drawCentered $"Variant: {vName}  |  Players: {state.PlayerCount}" 140 Color.Gold
            drawCentered "How many human players?" 180 Color.LightGray
            Button.drawAll buffer font input (humanCountButtons screenW)
        | Ready -> ()
        | ShowRules | ShowOptions -> ()

        match state.Step with
        | Ready | ShowRules | ShowOptions -> ()
        | _ ->
            Button.draw buffer font input (optionsButton screenW screenH)
            Button.draw buffer font input (howToPlayButton screenW screenH)
