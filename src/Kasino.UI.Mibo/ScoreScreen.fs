namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Score screen: round breakdown + cumulative scores. Update/scoring logic is
// identical to the MonoGame build; drawing emits Draw.* into the buffer.
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
        let color = match phase with RoundSummary -> Color(40, 80, 140) | GameOver -> Color(140, 80, 40)
        Button.createCentered label screenW (screenH - 80) 220 52 color Color.White

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

        let gameOver =
            newCumulative |> Map.exists (fun _ score -> score >= targetScore)

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
        if Button.isClicked input btn || Input.has Input.Continue input then
            { state with ContinueClicked = true }
        else
            state

    let draw buffer (font: SpriteFont) (input: Input.InputState) (state: ScoreState) (screenW: int) (screenH: int) =
        let cx = float32 (screenW / 2)
        let drawCentered (text: string) (y: int) (color: Color) =
            Render.textCentered buffer Render.LLabel font text cx (float32 y) color
        let drawLeft (text: string) (x: int) (y: int) (color: Color) =
            Render.text buffer Render.LLabel font text (Vector2(float32 x, float32 y)) color

        let title =
            match state.Phase with
            | RoundSummary -> $"Round {state.RoundNumber} Results"
            | GameOver     -> "Game Over!"
        drawCentered title 30 Color.Gold

        let varName =
            match state.Variant with
            | StandardKasino -> "Standard Kasino"
            | LaistoKasino   -> "Laistokasino"
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

        state.Scores |> List.iteri (fun col (player, breakdown) ->
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
                | [ w ] -> $"{w} wins with {bestScore} points!"
                | ws -> String.concat " & " ws + $" tie with {bestScore} points!"
            drawCentered text winnerY Color.Gold
        | RoundSummary -> ()

        Button.draw buffer font input (actionButton screenW screenH state.Phase)
