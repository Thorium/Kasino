namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Rules / Help screen. Page data and navigation logic are identical to the
// MonoGame build; drawing emits Draw.* into the render buffer.
// ─────────────────────────────────────────────────────────────

module RulesScreen =

    type RulesState =
        { CurrentPage: int
          BackClicked: bool }

    let create () =
        { CurrentPage = 0
          BackClicked = false }

    type private Page =
        | TextPage of title: string * lines: string list
        | VisualPage of title: string * id: int

    let private pages : Page[] =
        [| TextPage("Game Overview",
            [ "Kasino is a classic Finnish card game for 2-4 players."
              "The goal is to capture cards from the table by matching"
              "values from your hand."
              ""
              "Each round, players are dealt cards in waves of 4."
              "On your turn you MUST play one card from your hand:"
              "  - If it can capture table cards, you may take them"
              "    (optional in Standard, forced in Laisto)."
              "  - Otherwise your card is placed on the table."
              ""
              "After all cards are played, scores are tallied."
              "The first player to reach 16 cumulative points wins!"
              ""
              "The game uses a standard 52-card deck (no jokers)."
              "2 players: 6 deal waves.  3 players: 4.  4 players: 3." ])
           TextPage("Card Values",
            [ "Cards have TWO different value systems:"
              ""
              "TABLE VALUE (for summing on the table):"
              "  Ace = 1,  2-10 = face value,  J = 11,  Q = 12,  K = 13"
              ""
              "HAND VALUE (capture power when played from hand):"
              "  Most cards use their table value, but three are special:"
              ""
              "  Ace  = 14   (captures any combo summing to 14)"
              "  2 of Spades  = 15   (captures combos summing to 15)"
              "  10 of Diamonds = 16   (captures combos summing to 16)"
              ""
              "Example: Playing an Ace from hand can capture"
              "  a King + Ace on the table (13 + 1 = 14),"
              "  or a 9 + 5 (= 14), or even 8 + 5 + 1 (= 14)."
              ""
              "A lone King is captured only by another King, but a King on"
              "the table can also be swept up inside a bigger combo"
              "(e.g. King + Ace = 14, captured by an Ace)." ])
           VisualPage("Card Values at a Glance", 1)
           TextPage("Capturing Cards",
            [ "When you play a card, ALL non-overlapping subsets of"
              "table cards that sum to your hand card's value must"
              "be captured simultaneously."
              ""
              "Example: You play a 7 (hand value 7)."
              "  Table has: 3, 4, 2, 5, 7"
              "  Subsets summing to 7: {7}, {3,4}, {2,5}"
              "  {7} and {3,4} and {2,5} don't overlap => take ALL."
              "  You capture 5 cards at once!"
              ""
              "If subsets OVERLAP, you must choose which to take."
              "The game shows these as tappable buttons."
              ""
              "CAPTURE PREVIEW (green/yellow highlights):"
              "  When you select a hand card, table cards light up:"
              "  Green = definitely captured (in all options)"
              "  Yellow = captured in some options (choice needed)" ])
           VisualPage("Capturing with a 9", 2)
           VisualPage("Take or Leave", 3)
           TextPage("Sweeps & Round End",
            [ "SWEEP: If your capture takes ALL remaining table cards,"
              "that's a Sweep! Sweeps earn bonus points."
              ""
              "ROUND END: After all deal waves are exhausted and all"
              "hands are empty, the round ends."
              "  - The last player who captured cards takes any"
              "    cards remaining on the table (NOT a sweep)."
              "  - Scores are calculated for the round."
              "  - Each player's round score adds to their cumulative."
              ""
              "DEALING STRUCTURE:"
              "  The deck has 52 cards. 4 go to the table at the start."
              "  Remaining 48 cards dealt in waves of 4 per player:"
              "    2 players: 6 waves  (6 x 4 x 2 = 48)"
              "    3 players: 4 waves  (4 x 4 x 3 = 48)"
              "    4 players: 3 waves  (3 x 4 x 4 = 48)" ])
           TextPage("Scoring",
            [ "SCORING (per round):"
              ""
              "  Most cards captured .... 1 point"
              "  Most spades captured ... 2 points"
              "  Each Ace captured ...... 1 point  (max 4)"
              "  10 of Diamonds ......... 2 points"
              "  2 of Spades ............ 1 point"
              "  Each Sweep ............. 1 point"
              ""
              "TIE RULES: If two or more players tie for most cards"
              "or most spades, NOBODY scores that category."
              ""
              "SWEEP ADJUSTMENT: The minimum sweep count among all"
              "players is subtracted from everyone's sweep total."
              "(So if everyone got 1 sweep, nobody scores for sweeps.)"
              ""
              "TARGET: First player to reach 16 cumulative points wins."
              "If multiple players reach 16 in the same round,"
              "the highest score wins." ])
           VisualPage("Scoring Cards", 4)
           TextPage("Laistokasino",
            [ "LAISTOKASINO (also called Misa-Kasino):"
              ""
              "The rules are identical, but the goal is REVERSED:"
              "you want to MINIMIZE your point total!"
              ""
              "The first player to reach 16 points LOSES."
              "The winner is the player with the FEWEST points."
              ""
              "STRATEGY TIPS:"
              "  - Avoid capturing Aces and special cards."
              "  - Track which specials (Aces, 2♠, 10♦) are gone or still to come."
              "  - Keep any card on the table, so an opponent can't simply"
              "    park a special card on an empty table."
              "  - Don't accumulate too many cards or spades."
              "  - Sweeps hurt you! Try to leave cards on the table."
              "  - Sometimes placing a card is better than capturing."
              "  - Force opponents to sweep by leaving few table cards."
              "  - Usually play your bigger non-capturing cards first."
              "  - Beware feeding a special: with a 10 on the table, adding a 6"
              "    makes 16, so an opponent's 10♦ is forced to grab it." ]) |]

    let private totalPages = pages.Length

    let private pageTitle page =
        match pages.[page] with
        | TextPage(t, _) -> t
        | VisualPage(t, _) -> t

    // ── Button helpers ──
    let private backButton (screenW: int) (screenH: int) =
        Button.create "Back" 20 (screenH - 70) 140 52 (Color(120, 40, 40)) Color.White

    let private prevButton (screenW: int) (screenH: int) =
        let cx = screenW / 2
        Button.create "Previous" (cx - 250) (screenH - 70) 160 52 (Color(60, 60, 100)) Color.White

    let private nextButton (screenW: int) (screenH: int) =
        let cx = screenW / 2
        Button.create "Next" (cx + 90) (screenH - 70) 160 52 (Color(40, 100, 40)) Color.White

    // ── Visual-page drawing helpers ──
    let private cw = 60
    let private ch = 76
    let private card s r : Card = { Suit = s; Rank = r }

    let private centerLine buffer (font: SpriteFont) (cx: float32) (text: string) (y: int) (col: Color) =
        Render.textCentered buffer Render.LLabel font text cx (float32 y) col

    let private drawCardAt buffer (tex: CardRenderer.CardTextures) (c: Card) (x: int) (y: int) =
        Render.sprite buffer Render.LTableCard (CardRenderer.getTexture tex c) (Rectangle(x, y, cw, ch))

    /// A centered row of cards, each with a caption beneath it.
    let private drawRow buffer (font: SpriteFont) tex (cx: float32) (items: (Card * string) list) (y: int) (col: Color) =
        let gap = 64
        let n = items.Length
        let totalW = n * cw + (max 0 (n - 1)) * gap
        let startX = int cx - totalW / 2
        items
        |> List.iteri (fun i (c, lbl) ->
            let x = startX + i * (cw + gap)
            drawCardAt buffer tex c x y
            let sz = Render.measure font lbl
            Render.text buffer Render.LLabel font lbl
                (Vector2(float32 x + float32 cw / 2.0f - sz.X / 2.0f, float32 (y + ch + 6))) col)

    /// A group of cards laid left-to-right at (x, y), with a label to the right.
    let private drawGroup buffer (font: SpriteFont) tex (cards: Card list) (x: int) (y: int) (label: string) (col: Color) =
        let gap = 10
        cards |> List.iteri (fun i c -> drawCardAt buffer tex c (x + i * (cw + gap)) y)
        let tx = x + cards.Length * (cw + gap) + 14
        Render.text buffer Render.LLabel font label (Vector2(float32 tx, float32 (y + ch / 2 - 12))) col

    let private drawVisual buffer (font: SpriteFont) tex (cx: float32) (vid: int) =
        match vid with
        | 1 ->
            centerLine buffer font cx "Every card has a TABLE value and a HAND value." 116 Color.White
            centerLine buffer font cx "Add values on the table; spend HAND value to capture." 142 Color.LightGray
            centerLine buffer font cx "Normal cards: each card's value = its face value" 184 Color.Gold
            drawRow buffer font tex cx [ card Clubs Four, "worth 4"; card Hearts Seven, "worth 7"; card Spades King, "worth 13" ] 206 Color.White
            centerLine buffer font cx "Three special cards have EXTRA capture power:" 330 Color.Gold
            drawRow buffer font tex cx
                [ card Diamonds Ace, "Ace = 14"
                  card Spades Two, "2♠ = 15"
                  card Diamonds Ten, "10♦ = 16" ] 352 Color.LightGreen
        | 2 ->
            centerLine buffer font cx "You play a 9 from your hand." 116 Color.White
            centerLine buffer font cx "It captures any group of table cards that ADDS UP to 9." 142 Color.LightGray
            let gx = int cx - 250
            drawGroup buffer font tex [ card Hearts Nine ] gx 178 "<-  the 9 you play from your hand" Color.Gold
            drawGroup buffer font tex [ card Clubs Nine ] gx 272 "a 9          = 9      captured" Color.LimeGreen
            drawGroup buffer font tex [ card Spades Three; card Diamonds Six ] gx 362 "3 + 6     = 9      captured" Color.LimeGreen
            drawGroup buffer font tex [ card Hearts Eight ] gx 452 "8 alone is not 9   ->   it stays" Color.LightSalmon
            centerLine buffer font cx "One 9 grabs BOTH matching groups at once (here: 3 cards)." 560 Color.White
        | 3 ->
            centerLine buffer font cx "Your 9 CAN capture the 9 on the table. Must you?" 116 Color.White
            let gx = int cx - 110
            drawGroup buffer font tex [ card Hearts Nine; card Clubs Nine ] gx 150 "hand 9  +  table 9" Color.Gold
            centerLine buffer font cx "STANDARD KASINO  -  capturing is OPTIONAL" 274 Color.LimeGreen
            centerLine buffer font cx "You may take the 9, or simply place a card on the table." 302 Color.LightGray
            centerLine buffer font cx "LAISTOKASINO  -  capturing is FORCED" 362 Color.LightSalmon
            centerLine buffer font cx "If a capture is possible, you MUST take it." 390 Color.LightGray
            centerLine buffer font cx "(In Laisto you try NOT to collect cards, so a forced take hurts.)" 424 Color.Gray
        | 4 ->
            centerLine buffer font cx "Most points come from special cards and majorities:" 116 Color.White
            let gx = int cx - 250
            drawGroup buffer font tex [ card Diamonds Ten ] gx 150 "10♦  =  2 points" Color.Gold
            drawGroup buffer font tex [ card Spades Two ] gx 240 "2♠   =  1 point" Color.Gold
            drawGroup buffer font tex
                [ card Spades Ace; card Hearts Ace; card Diamonds Ace; card Clubs Ace ]
                gx 330 "each Ace = 1 point  (4 total)" Color.White
            drawGroup buffer font tex [ card Spades Four; card Spades Seven; card Spades Nine ] gx 420 "most Spades = 2 points" Color.White
            centerLine buffer font cx "Most cards = 1 point          Each sweep = 1 point" 540 Color.LightGray
        | _ -> ()

    // ── Update ──
    let update (input: Input.InputState) (screenW: int) (screenH: int) (state: RulesState) =
        let back = backButton screenW screenH
        if Button.isClicked input back || input.Keyboard.IsEscapePressed then
            { state with BackClicked = true }
        else
            let page = state.CurrentPage
            let page =
                if page > 0 && Button.isClicked input (prevButton screenW screenH) then page - 1
                elif page < totalPages - 1 && Button.isClicked input (nextButton screenW screenH) then page + 1
                elif input.Keyboard.IsLeftPressed && page > 0 then page - 1
                elif input.Keyboard.IsRightPressed && page < totalPages - 1 then page + 1
                else page
            { state with CurrentPage = page }

    // ── Draw ──
    let draw buffer (font: SpriteFont) (texOpt: CardRenderer.CardTextures option) (input: Input.InputState) (state: RulesState) (screenW: int) (screenH: int) =
        let cx = float32 screenW / 2.0f
        let drawCentered (text: string) (y: int) (color: Color) =
            Render.textCentered buffer Render.LLabel font text cx (float32 y) color

        drawCentered "How to Play Kasino" 20 Color.Gold
        drawCentered (pageTitle state.CurrentPage) 55 Color.White
        drawCentered $"Page {state.CurrentPage + 1} / {totalPages}" 80 Color.Gray

        // Separator line
        Render.fill buffer Render.LLabel Color.DarkGray (Rectangle(40, 100, screenW - 80, 1))

        match pages.[state.CurrentPage] with
        | TextPage(_, lines) ->
            let lineH = 22
            let startY = 115
            for i in 0 .. lines.Length - 1 do
                let line = lines[i]
                let y = startY + i * lineH
                if line <> "" then
                    let color = if line.StartsWith("  ") then Color.LightGray else Color.White
                    Render.text buffer Render.LLabel font line (Vector2(50.0f, float32 y)) color
        | VisualPage(_, vid) ->
            match texOpt with
            | Some tex -> drawVisual buffer font tex cx vid
            | None -> drawCentered "Loading cards..." 300 Color.Gray

        Button.draw buffer font input (backButton screenW screenH)
        if state.CurrentPage > 0 then Button.draw buffer font input (prevButton screenW screenH)
        if state.CurrentPage < totalPages - 1 then Button.draw buffer font input (nextButton screenW screenH)

        drawCentered "Arrow keys: navigate  |  Esc: back" (screenH - 20) Color.DarkGray
