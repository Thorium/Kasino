namespace Kasino.UI.Web

open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Score screen: round breakdown and cumulative scores.
// Transitions to the next round or the game-over summary.
// ─────────────────────────────────────────────────────────────

module ScoreScreen =

    type ScorePhase =
        | RoundSummary
        | GameOver

    type ScoreState =
        { Scores: (Player * Scoring.ScoreBreakdown) list
          CumulativeScores: Map<string, int>
          /// Most-cards/most-spades pot left undistributed by ties this round.
          CarryOut: Scoring.CarryOver
          Phase: ScorePhase
          Variant: GameVariant
          RoundNumber: int
          TargetScore: int
          ContinueClicked: bool }

    let private actionButton (screenW: int) (screenH: int) (phase: ScorePhase) =
        let label = match phase with RoundSummary -> "Next Round" | GameOver -> "Back to Menu"
        let color = match phase with RoundSummary -> Color.rgb 40 80 140 | GameOver -> Color.rgb 140 80 40
        let s = CardRenderer.UiScale
        Button.createCentered label screenW (screenH - int (80.0 * s)) (int (220.0 * s)) (int (60.0 * s)) color Color.White

    /// Create score state from round results.
    let create
        (players: Player list)
        (prevCumulative: Map<string, int>)
        (variant: GameVariant)
        (roundNumber: int)
        (targetScore: int)
        (carry: Scoring.CarryOver)
        =
        let scores, carryOut = Scoring.calculateScoresCarry carry players
        let newCumulative =
            scores
            |> List.fold (fun acc (p, s) ->
                let prev = acc |> Map.tryFind p.Name |> Option.defaultValue 0
                acc |> Map.add p.Name (prev + s.Total))
                prevCumulative

        let gameOver = newCumulative |> Map.exists (fun _ score -> score >= targetScore)

        { Scores = scores
          CumulativeScores = newCumulative
          CarryOut = carryOut
          Phase = if gameOver then GameOver else RoundSummary
          Variant = variant
          RoundNumber = roundNumber
          TargetScore = targetScore
          ContinueClicked = false }

    let update (input: Input.InputState) (screenW: int) (screenH: int) (state: ScoreState) =
        let btn = actionButton screenW screenH state.Phase
        if Button.isClicked input btn || input.Keyboard.IsEnterPressed then
            { state with ContinueClicked = true }
        else
            state

    let draw (g: Gfx) (input: Input.InputState) (state: ScoreState) (screenW: int) (screenH: int) =
        let cx = float (screenW / 2)
        // Mobile mode: larger text, with the row pitch and column positions
        // following the font so the table stays legible.
        let baseFont = g.FontSize
        if CardRenderer.UiScale > 1.0 then
            // 1.4x, reduced so header + table + winner line still clear the button
            let fit = (float screenH - 320.0) / (14.0 * float baseFont)
            g.FontSize <- int (float baseFont * max 1.0 (min 1.4 fit))
        let fs = g.FontSize
        let rowH = fs + 4
        let drawCentered (text: string) (y: int) (color: Color) =
            let size = Gfx.measure g text
            Gfx.fillText g text (cx - size.X / 2.0) (float y) color
        let drawLeft (text: string) (x: int) (y: int) (color: Color) =
            Gfx.fillText g text (float x) (float y) color

        let title =
            match state.Phase with
            | RoundSummary -> $"Round %d{state.RoundNumber} Results"
            | GameOver -> "Game Over!"
        drawCentered title 30 Color.Gold


        let varName =
            match state.Variant with
            | StandardKasino -> "Standard Kasino"
            | LaistoKasino -> "Laistokasino"
        drawCentered varName (30 + fs + 6) Color.Gray

        let startY = 30 + 2 * fs + 24

        let categories =
            [ "Most cards (1pt)"; "Most spades (2pts)"; "Aces (1pt each)"
              "Diamond 10 (2pts)"; "Spade 2 (1pt)"; "Sweeps (1pt each)"
              "─────────────"; "Round total"; ""; "Cumulative" ]

        let catX = 20
        // player columns start after the widest category label
        let widestCat = categories |> List.map (fun c -> (Gfx.measure g c).X) |> List.max
        let colStart = catX + int widestCat + 40
        let blockH = rowH * (categories.Length + 1) + 24

        // 3-4 players: two blocks of two players (each with its own category
        // column) instead of cramped columns, whenever the columns would be
        // narrower than the widest name needs (always the case in mobile mode)
        // and both blocks fit above the button; otherwise one block.
        let blocks =
            let n = state.Scores.Length
            let twoBlocksFit = startY + 2 * blockH + 60 <= screenH - int (100.0 * CardRenderer.UiScale)
            let widestName = state.Scores |> List.map (fun (p, _) -> (Gfx.measure g p.Name).X) |> List.max
            let tooNarrow = float ((screenW - colStart - 20) / max 1 n) < widestName + 40.0
            if n > 2 && twoBlocksFit && (CardRenderer.UiScale > 1.0 || tooNarrow) then List.chunkBySize 2 state.Scores
            else [ state.Scores ]

        blocks
        |> List.iteri (fun bi block ->
            let top = startY + bi * blockH
            for i in 0 .. categories.Length - 1 do
                drawLeft categories[i] catX (top + rowH + i * rowH) Color.LightGray
            let colW = (screenW - colStart - 20) / max 1 block.Length
            block
            |> List.iteri (fun col (player, breakdown) ->
                let x = colStart + col * colW
                drawLeft player.Name x top Color.White

                let rows =
                    [ string breakdown.MostCards
                      string breakdown.MostSpades
                      string breakdown.Aces
                      string breakdown.DiamondTen
                      string breakdown.SpadeTwo
                      string breakdown.Sweeps
                      ""
                      string breakdown.Total
                      ""
                      string (state.CumulativeScores |> Map.tryFind player.Name |> Option.defaultValue 0) ]

                for i in 0 .. rows.Length - 1 do
                    let y = top + rowH + i * rowH
                    let color =
                        match i with
                        | 7 -> Color.Yellow
                        | 9 -> Color.Gold
                        | _ -> Color.White
                    drawLeft rows[i] x y color))

        let tableBottom = startY + (blocks.Length - 1) * blockH + rowH + categories.Length * rowH

        // Winner announcement. An exact tie for the deciding score names
        // every tied player rather than an arbitrary one.
        match state.Phase with
        | GameOver ->
            let scores = state.CumulativeScores |> Map.toList
            let bestScore =
                match state.Variant with
                | StandardKasino -> scores |> List.map snd |> List.max
                | LaistoKasino   -> scores |> List.map snd |> List.min
            let winners = scores |> List.filter (fun (_, s) -> s = bestScore) |> List.map fst
            let winnerY = tableBottom + 20
            let text =
                match winners with
                | [ w ] -> $"%s{w} wins with %d{bestScore} points!"
                | ws -> sprintf "%s tie with %d points!" (String.concat " & " ws) bestScore
            drawCentered text winnerY Color.Gold
        | RoundSummary -> ()

        Button.draw g input (actionButton screenW screenH state.Phase)
        g.FontSize <- baseFont
