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
        Button.createCentered label screenW (screenH - 80) 220 52 color Color.White

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
        let drawCentered (text: string) (y: int) (color: Color) =
            let size = Gfx.measure g text
            Gfx.fillText g text (cx - size.X / 2.0) (float y) color
        let drawLeft (text: string) (x: int) (y: int) (color: Color) =
            Gfx.fillText g text (float x) (float y) color

        let title =
            match state.Phase with
            | RoundSummary -> sprintf "Round %d Results" state.RoundNumber
            | GameOver -> "Game Over!"
        drawCentered title 30 Color.Gold

        let varName =
            match state.Variant with
            | StandardKasino -> "Standard Kasino"
            | LaistoKasino -> "Laistokasino"
        drawCentered varName 60 Color.Gray

        let startY = 100
        let colX = 60
        let colW = (screenW - 120) / max 1 state.Scores.Length

        let categories =
            [ "Most cards (1pt)"; "Most spades (2pts)"; "Aces (1pt each)"
              "Diamond 10 (2pts)"; "Spade 2 (1pt)"; "Sweeps (1pt each)"
              "─────────────"; "Round total"; ""; "Cumulative" ]

        let catX = 20
        for i in 0 .. categories.Length - 1 do
            let y = startY + 30 + i * 24
            drawLeft categories[i] catX y Color.LightGray

        state.Scores
        |> List.iteri (fun col (player, breakdown) ->
            let x = colX + 200 + col * colW
            drawLeft player.Name x startY Color.White

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
                let y = startY + 30 + i * 24
                let color =
                    if i = 7 then Color.Yellow
                    elif i = 9 then Color.Gold
                    else Color.White
                drawLeft rows[i] x y color)

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
            let winnerY = startY + 30 + categories.Length * 24 + 20
            let text =
                match winners with
                | [ w ] -> sprintf "%s wins with %d points!" w bestScore
                | ws -> sprintf "%s tie with %d points!" (String.concat " & " ws) bestScore
            drawCentered text winnerY Color.Gold
        | RoundSummary -> ()

        Button.draw g input (actionButton screenW screenH state.Phase)
