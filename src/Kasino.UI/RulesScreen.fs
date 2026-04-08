namespace Kasino.UI

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open FontStashSharp

// ─────────────────────────────────────────────────────────────
// Rules / Help screen: 6 pages explaining Finnish Kasino.
// Touch-friendly navigation with Previous/Next/Back buttons.
// Accessible from the main menu and in-game "?" button.
// ─────────────────────────────────────────────────────────────

module RulesScreen =

    let private totalPages = 6

    type RulesState =
        { CurrentPage: int
          BackClicked: bool }

    let create () =
        { CurrentPage = 0
          BackClicked = false }

    // ── Button helpers ──────────────────────────────────

    let private backButton (screenW: int) (screenH: int) =
        Button.create "Back" 20 (screenH - 70) 140 52 (Color(120, 40, 40)) Color.White

    let private prevButton (screenW: int) (screenH: int) =
        let cx = screenW / 2
        Button.create "Previous" (cx - 250) (screenH - 70) 160 52 (Color(60, 60, 100)) Color.White

    let private nextButton (screenW: int) (screenH: int) =
        let cx = screenW / 2
        Button.create "Next" (cx + 90) (screenH - 70) 160 52 (Color(40, 100, 40)) Color.White

    // ── Page content ────────────────────────────────────

    let private pageTitle = function
        | 0 -> "Game Overview"
        | 1 -> "Card Values"
        | 2 -> "Capturing Cards"
        | 3 -> "Sweeps & Round End"
        | 4 -> "Scoring"
        | 5 -> "Laistokasino"
        | _ -> ""

    let private pageLines = function
        | 0 ->
            [ "Kasino is a classic Finnish card game for 2-4 players."
              "The goal is to capture cards from the table by matching"
              "values from your hand."
              ""
              "Each round, players are dealt cards in waves of 4."
              "On your turn you MUST play one card from your hand:"
              "  - If it can capture table cards, you take them."
              "  - If not, your card is placed on the table."
              ""
              "After all cards are played, scores are tallied."
              "The first player to reach 16 cumulative points wins!"
              ""
              "The game uses a standard 52-card deck (no jokers)."
              "2 players: 6 deal waves.  3 players: 4.  4 players: 3." ]
        | 1 ->
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
              "Kings can only be captured by Kings (table value 13)." ]
        | 2 ->
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
              "  Yellow = captured in some options (choice needed)" ]
        | 3 ->
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
              "    4 players: 3 waves  (3 x 4 x 4 = 48)" ]
        | 4 ->
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
              "the highest score wins." ]
        | 5 ->
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
              "  - Don't accumulate too many cards or spades."
              "  - Sweeps hurt you! Try to leave cards on the table."
              "  - Sometimes placing a card is better than capturing."
              "  - Force opponents to sweep by leaving few table cards."
              ""
              "Laistokasino requires a very different mindset"
              "from Standard Kasino. Think defensively!" ]
        | _ -> []

    // ── Update ──────────────────────────────────────────

    let update (input: InputHandler.InputState) (screenW: int) (screenH: int) (state: RulesState) =
        // Check Back button or Escape
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

    // ── Draw ────────────────────────────────────────────

    let draw (sb: SpriteBatch) (font: SpriteFontBase) (input: InputHandler.InputState) (state: RulesState) (screenW: int) (screenH: int) =
        let cx = float32 screenW / 2.0f

        let drawCentered (text: string) (y: int) (color: Color) =
            let size = font.MeasureString(text)
            sb.DrawString(font, text, Vector2(cx - size.X / 2.0f, float32 y), color) |> ignore

        // Header
        drawCentered "How to Play Kasino" 20 Color.Gold

        // Page title
        let title = pageTitle state.CurrentPage
        drawCentered title 55 Color.White

        // Page indicator
        let indicator = $"Page {state.CurrentPage + 1} / {totalPages}"
        drawCentered indicator 80 Color.Gray

        // Separator line
        let lineTex = CardRenderer.getCachedColorTexture (sb.GraphicsDevice) Color.DarkGray
        sb.Draw(lineTex, Rectangle(40, 100, screenW - 80, 1), Color.White)

        // Body text
        let lines = pageLines state.CurrentPage
        let lineH = 22
        let startY = 115
        for i in 0 .. lines.Length - 1 do
            let line = lines[i]
            let y = startY + i * lineH
            let color =
                if line.StartsWith("  ") then Color.LightGray
                elif line = "" then Color.Transparent
                else Color.White
            sb.DrawString(font, line, Vector2(50.0f, float32 y), color) |> ignore

        // Navigation buttons
        Button.draw sb font input (backButton screenW screenH)
        if state.CurrentPage > 0 then
            Button.draw sb font input (prevButton screenW screenH)
        if state.CurrentPage < totalPages - 1 then
            Button.draw sb font input (nextButton screenW screenH)

        // Keyboard hint
        drawCentered "Arrow keys: navigate  |  Esc: back" (screenH - 20) Color.DarkGray
