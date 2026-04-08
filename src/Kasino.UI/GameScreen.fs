namespace Kasino.UI

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open FontStashSharp
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Main gameplay screen: table, hands, card selection, turns.
// Touch-friendly: tap card to select, tap "Play" to confirm.
// Capture options shown as tappable buttons with modal overlay.
// Keyboard shortcuts retained as fallback for desktop.
//
// Layout for 2 players:
//   Top:    Opponent hand (face-down)
//   Center: Table cards
//   Bottom: Player hand (face-up, tappable)
// For 3-4 players: sides show additional opponents.
//
// Supports drag & drop: drag a hand card to the table to play it.
// Table card layout can toggle between strict grid and random scatter.
// ─────────────────────────────────────────────────────────────

module GameScreen =

    /// Phase of the game screen
    type Phase =
        | Shuffling of elapsed: float          // shuffle animation before dealing
        | Dealing of step: int * elapsed: float * steps: DealStep list  // animated dealing
        | WaitingForHuman
        | ComputerThinking of elapsed: float
        | AnimatingPlay of elapsed: float * AI.PlayEvaluation * cardAnim: CardAnimation option * collectAnim: CollectAnimation option
        | ChoosingCaptureOption of cardIndex: int * Rules.CaptureOption list
        | RoundOver
        | GameOver

    /// A single deal step: 1-4 cards slide from deck to a target position
    and DealStep =
        { TargetLabel: string          // "table", "Player1", etc. (for debug)
          CardCount: int               // how many cards in this step (usually 2, or 4 for table)
          ToX: float32; ToY: float32   // screen-space destination center
          IsFaceUp: bool }             // whether cards are face-up at destination

    /// Card movement animation: card slides from source to destination
    and CardAnimation =
        { Card: Card
          FromX: float32; FromY: float32       // screen-space start position
          ToX: float32; ToY: float32           // screen-space end position
          Duration: float }                    // seconds for the slide

    /// Collect animation: captured table cards slide toward the player
    and CollectAnimation =
        { Cards: (Card * float32 * float32) list   // card, fromX, fromY
          ToX: float32; ToY: float32               // destination (player pile area)
          StartTime: float                         // when the collect begins (after card slide)
          Duration: float }                        // seconds for the slide

    /// Capture preview for selected hand card
    type CapturePreview =
        | NoCapture
        | SingleCapture of definite: Card list
        | MultipleCaptures of definite: Card list * possible: Card list

    /// Drag state for drag & drop
    type DragState =
        | NotDragging
        | Dragging of cardIndex: int * startPos: Point * currentPos: Point

    /// Table card layout mode
    type TableLayout =
        | StrictGrid
        | RandomScatter

    /// Full screen state
    type ScreenState =
        { GameState: GameEngine.GameState
          Config: GameEngine.GameConfig
          Phase: Phase
          SelectedCardIndex: int option
          HoveredCardIndex: int option
          LastPlayMessage: string
          RoundNumber: int
          CumulativeScores: Map<string, int>
          Rng: Random
          CardRects: (int * Rectangle) list   // (handIndex, rect) for hit-testing
          TableCardRects: Rectangle list
          CapturePreview: CapturePreview
          ContinueClicked: bool               // signal for KasinoGame transitions
          ShowRulesClicked: bool               // signal to open rules from game
          MenuClicked: bool                    // signal to return to main menu
          DragState: DragState                 // drag & drop state
          TableLayout: TableLayout             // strict grid or random scatter
          ScatteredPositions: Map<Card, (int * int * float32)> }  // card -> (x, y, rotation)

    let private computerDelay = 0.8
    let private animDelay = 1.4        // slightly longer to fit collect animation
    let private shuffleDuration = 0.6
    let private cardSlideDuration = 0.25
    let private collectSlideDuration = 0.35
    let private dealStepDuration = 0.18         // seconds per deal step (pair of cards slides)

    /// Drag threshold in pixels before starting a drag
    let private dragThreshold = 8

    /// Format a play result as a human-readable message
    let private formatPlayResult (playerName: string) (result: PlayResult) =
        match result with
        | Capture(hc, captured, sweep) ->
            let capturedStr = captured |> List.map Cards.display |> String.concat " "
            let sweepStr = if sweep then " SWEEP!" else ""
            $"{playerName} plays {Cards.display hc} -> captures {capturedStr}{sweepStr}"
        | Place hc ->
            $"{playerName} places {Cards.display hc} on table"

    /// Card spacing constants
    let private cardGap = 8
    let private tableCardGap = 6

    // ── Layout helpers ──────────────────────────────────────

    /// Calculate x-offset to center N cards horizontally
    let private centerCards (screenW: int) (count: int) =
        let totalW = count * (CardRenderer.scaledWidth() + cardGap) - cardGap
        (screenW - totalW) / 2

    /// Get the bounding rectangle for a hand card
    let private handCardRect (screenW: int) (screenH: int) (handSize: int) (idx: int) (isBottom: bool) =
        let x = centerCards screenW handSize + idx * (CardRenderer.scaledWidth() + cardGap)
        let y = if isBottom then screenH - CardRenderer.scaledHeight() - 20 else 20
        Rectangle(x, y, CardRenderer.scaledWidth(), CardRenderer.scaledHeight())

    /// Get the bounding rectangle for a table card (strict grid)
    let private tableCardRect (screenW: int) (screenH: int) (count: int) (idx: int) =
        let cols = min count 10
        let rows = (count + cols - 1) / cols
        let totalW = cols * (CardRenderer.scaledWidth() + tableCardGap) - tableCardGap
        let totalH = rows * (CardRenderer.scaledHeight() + tableCardGap) - tableCardGap
        let baseX = (screenW - totalW) / 2
        let baseY = (screenH - totalH) / 2
        let col = idx % cols
        let row = idx / cols
        Rectangle(
            baseX + col * (CardRenderer.scaledWidth() + tableCardGap),
            baseY + row * (CardRenderer.scaledHeight() + tableCardGap),
            CardRenderer.scaledWidth(), CardRenderer.scaledHeight())

    /// Get the table area rectangle.
    /// Horizontal margins leave room for potential side-opponent hands (3-4 player games).
    let private tableArea (screenW: int) (screenH: int) =
        let ch = CardRenderer.scaledHeight()
        let cw = CardRenderer.scaledWidth()
        let sideMargin = cw + 30  // room for one card column + gap on each side
        Rectangle(sideMargin, ch + 60, screenW - 2 * sideMargin, screenH - 2 * ch - 140)

    /// Compute scattered positions for table cards.
    /// Cards are placed center-outward: the first card lands near the middle
    /// of the table and each subsequent card tries progressively larger radii.
    /// Uses card identity as seed for deterministic but random-looking placement.
    let computeScatteredPositions (table: Card list) (screenW: int) (screenH: int) (existing: Map<Card, (int * int * float32)>) =
        let area = tableArea screenW screenH
        let cw = CardRenderer.scaledWidth()
        let ch = CardRenderer.scaledHeight()
        let centerX = area.X + area.Width / 2
        let centerY = area.Y + area.Height / 2
        let maxRadiusX = float32 (area.Width / 2 - cw / 2 - 10)
        let maxRadiusY = float32 (area.Height / 2 - ch / 2 - 10)

        let mutable result = existing

        // Remove cards no longer on table
        let tableSet = Set.ofList table
        result <- result |> Map.filter (fun card _ -> Set.contains card tableSet)

        // Add positions for new cards — center-outward placement
        let newCards = table |> List.filter (fun c -> not (Map.containsKey c result))
        let existingCount = Map.count result

        for cardIdx in 0 .. List.length newCards - 1 do
            let card = newCards[cardIdx]
            let seed = hash (card.Suit, card.Rank)
            let rng = Random(seed)
            // Placement order determines how far from center this card goes.
            // Cards already on table count toward the spread.
            let orderIdx = existingCount + cardIdx
            let spreadFraction = float32 orderIdx / float32 (max 1 (List.length table - 1))
            // Start radius at ~20% of max, grow toward 100% as table fills
            let radiusFrac = 0.15f + 0.85f * spreadFraction

            let mutable attempts = 0
            let mutable placed = false
            let mutable bestX = centerX
            let mutable bestY = centerY
            let mutable bestRot = 0.0f

            while not placed && attempts < 60 do
                // Random angle, radius proportional to fill-level with jitter
                let angle = float32 (rng.NextDouble()) * MathF.PI * 2.0f
                let jitter = 0.7f + float32 (rng.NextDouble()) * 0.6f  // 0.7..1.3
                let rFrac = radiusFrac * jitter |> min 1.0f
                let rx = int (float32 centerX + cos angle * maxRadiusX * rFrac)
                let ry = int (float32 centerY + sin angle * maxRadiusY * rFrac)
                // Clamp inside area
                let x = max (area.X + cw / 2 + 5) (min (area.X + area.Width - cw / 2 - 5) rx)
                let y = max (area.Y + ch / 2 + 5) (min (area.Y + area.Height - ch / 2 - 5) ry)
                let rot = (float32 (rng.NextDouble()) - 0.5f) * 0.35f

                let rect = Rectangle(x - cw / 2, y - ch / 2, cw, ch)
                let overlaps =
                    result |> Map.exists (fun _ (ox, oy, _) ->
                        let orect = Rectangle(ox - cw / 2, oy - ch / 2, cw, ch)
                        rect.Intersects(orect))

                if not overlaps then
                    bestX <- x; bestY <- y; bestRot <- rot; placed <- true
                else
                    bestX <- x; bestY <- y; bestRot <- rot

                attempts <- attempts + 1

            result <- result |> Map.add card (bestX, bestY, bestRot)

        result

    /// Build deal animation steps.
    /// First deal: 4 cards to table, then (2 per player) × 2 rounds.
    /// Subsequent deals: (2 per player) × 2 rounds.
    let private buildDealSteps (gs: GameEngine.GameState) (isFirstDeal: bool) (screenW: int) (screenH: int) =
        let tArea = tableArea screenW screenH
        let tableCenterX = float32 (tArea.X + tArea.Width / 2)
        let tableCenterY = float32 (tArea.Y + tArea.Height / 2)
        let playerCount = gs.Players.Length
        let bottomIdx = if gs.Players |> List.exists (fun p -> p.Type = Human) then 0 else gs.CurrentPlayerIndex

        // Compute destination for each player
        let playerDest (idx: int) =
            if idx = bottomIdx then
                // Bottom player — hand area center
                let handY = float32 (screenH - CardRenderer.scaledHeight() - 20 + CardRenderer.scaledHeight() / 2)
                (float32 (screenW / 2), handY)
            else
                // Top opponent
                let handY = float32 (20 + CardRenderer.scaledHeight() / 2)
                (float32 (screenW / 2), handY)

        let tableStep = { TargetLabel = "table"; CardCount = 4; ToX = tableCenterX; ToY = tableCenterY; IsFaceUp = false }

        // Build player steps: 2 cards per player, repeated twice
        let playerSteps =
            [for _ in 1 .. 2 do
                for pIdx in 0 .. playerCount - 1 do
                    let (px, py) = playerDest pIdx
                    { TargetLabel = gs.Players[pIdx].Name; CardCount = 2; ToX = px; ToY = py; IsFaceUp = (pIdx = bottomIdx) }]

        if isFirstDeal then tableStep :: playerSteps
        else playerSteps

    /// Build a collect animation from a play result.
    /// Captured cards slide from their table positions to the player's pile area.
    let private buildCollectAnimation
            (playResult: PlayResult)
            (isBottom: bool)
            (screenW: int) (screenH: int)
            (scatteredPos: Map<Card, (int * int * float32)>)
            (tableCards: Card list) (tableCount: int) =
        match playResult with
        | Capture(_, captured, _) when not (List.isEmpty captured) ->
            let destX, destY =
                if isBottom then float32 (screenW / 2), float32 (screenH - 10)   // slide off bottom
                else float32 (screenW / 2), 0.0f                                  // slide off top
            let cards =
                captured |> List.map (fun card ->
                    match Map.tryFind card scatteredPos with
                    | Some(x, y, _) -> (card, float32 x, float32 y)
                    | None ->
                        // Fallback: use grid position
                        let idx = tableCards |> List.tryFindIndex ((=) card) |> Option.defaultValue 0
                        let r = tableCardRect screenW screenH tableCount idx
                        (card, float32 (r.X + r.Width / 2), float32 (r.Y + r.Height / 2)))
            Some { Cards = cards; ToX = destX; ToY = destY
                   StartTime = cardSlideDuration; Duration = collectSlideDuration }
        | _ -> None

    // ── Button helpers ──────────────────────────────────────

    /// "Play Card" button — text and color change based on capture preview
    let private playButton (screenW: int) (screenH: int) (preview: CapturePreview) =
        let label, color =
            match preview with
            | NoCapture ->
                "Place on Table", Color(100, 100, 100)
            | SingleCapture cards ->
                $"Capture {cards.Length} Cards", Color(40, 140, 40)
            | MultipleCaptures _ ->
                "Play (Choose Capture)", Color(160, 160, 40)
        Button.createCentered label screenW (screenH - CardRenderer.scaledHeight() - 80) 240 52 color Color.White

    /// Small "?" help button — top-left area, available during gameplay
    /// Button.create enforces MinTouchWidth=120, MinTouchHeight=48
    let private helpButton (_screenW: int) =
        Button.create "?" 20 20 120 48 (Color(80, 80, 40)) Color.White

    /// Layout toggle button — switches between grid and scatter
    let private layoutToggleButton (_screenW: int) (layout: TableLayout) =
        let label = match layout with StrictGrid -> "Scatter" | RandomScatter -> "Grid"
        Button.create label 160 20 120 48 (Color(60, 80, 60)) Color.White

    /// "Menu" button — return to main menu from gameplay
    let private menuButton (_screenW: int) =
        Button.create "Menu" 300 20 120 48 (Color(120, 40, 40)) Color.White

    /// "Continue" button — shown when round is over
    let private continueButton (screenW: int) (screenH: int) =
        Button.createCentered "Continue" screenW (screenH / 2 + 60) 200 52 (Color(40, 80, 140)) Color.White

    /// Capture option buttons — one per option, stacked vertically in center
    let private captureOptionButtons (options: Rules.CaptureOption list) (screenW: int) (screenH: int) =
        let totalH = options.Length * 56 + 64
        let baseY = (screenH - totalH) / 2
        options |> List.mapi (fun i opt ->
            let label =
                let cards = opt.Captured |> List.map Cards.display |> String.concat " "
                $"{i + 1}: {cards} ({opt.Captured.Length} cards)"
            Button.createCentered label screenW (baseY + i * 56) 450 48 (Color(60, 80, 60)) Color.White)

    /// Cancel button — shown below capture options
    let private cancelButton (optionCount: int) (screenW: int) (screenH: int) =
        let totalH = optionCount * 56 + 64
        let baseY = (screenH - totalH) / 2
        Button.createCentered "Cancel" screenW (baseY + optionCount * 56 + 8) 180 48 (Color(120, 40, 40)) Color.White

    // ── Initialization ──────────────────────────────────────

    let create (config: GameEngine.GameConfig) (rng: Random) (players: Player list) (roundNumber: int) (scores: Map<string, int>) =
        let state = GameEngine.newRound config rng players roundNumber
        let state = GameEngine.dealRound state true
        { GameState = { state with DealRound = 1 }
          Config = config
          Phase = Shuffling 0.0
          SelectedCardIndex = None
          HoveredCardIndex = None
          LastPlayMessage = $"Round {roundNumber} - Deal 1"
          RoundNumber = roundNumber
          CumulativeScores = scores
          Rng = rng
          CardRects = []
          TableCardRects = []
          CapturePreview = NoCapture
          ContinueClicked = false
          ShowRulesClicked = false
          MenuClicked = false
          DragState = NotDragging
          TableLayout = RandomScatter
          ScatteredPositions = Map.empty }

    // ── Update logic ────────────────────────────────────────

    [<TailCall>]
    let rec private advanceTurn (screen: ScreenState) =
        let gs = screen.GameState
        // Check if all hands empty => need to deal or end round
        if GameEngine.allHandsEmpty gs then
            if gs.DealRound < gs.TotalDeals then
                let nextDeal = gs.DealRound + 1
                let newGs = GameEngine.dealRound gs false
                { screen with
                    GameState = { newGs with DealRound = nextDeal }
                    LastPlayMessage = $"Round {screen.RoundNumber} - Deal {nextDeal}"
                    Phase = Shuffling 0.0 }
            else
                // End of round
                let finalGs = GameEngine.endRound gs
                { screen with GameState = finalGs; Phase = RoundOver }
        else
            // Next player's turn
            let currentPlayer = gs.Players[gs.CurrentPlayerIndex]
            if List.isEmpty currentPlayer.Hand then
                // Skip player with empty hand — advance index and recurse
                let newGs = { gs with CurrentPlayerIndex = (gs.CurrentPlayerIndex + 1) % gs.Players.Length }
                advanceTurn { screen with GameState = newGs }
            else
                match currentPlayer.Type with
                | Human ->
                    { screen with Phase = WaitingForHuman; SelectedCardIndex = None; HoveredCardIndex = None }
                | Computer ->
                    { screen with Phase = ComputerThinking 0.0 }

    /// Play a human card (single option or no captures)
    let private processHumanPlay (screen: ScreenState) (cardIndex: int) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let player = gs.Players[gs.CurrentPlayerIndex]
        let card = player.Hand[cardIndex]
        let options = Rules.findCaptureOptions card gs.Table
        match options with
        | _ :: _ :: _ ->
            // Multiple capture options — show choice dialog
            { screen with Phase = ChoosingCaptureOption(cardIndex, options); SelectedCardIndex = None }
        | _ ->
            let fromRect = handCardRect screenW screenH player.Hand.Length cardIndex true
            let turnResult = GameEngine.playHumanTurn gs cardIndex None
            // Build collect animation from pre-play table positions
            let collectAnim =
                buildCollectAnimation turnResult.PlayResult true screenW screenH
                    screen.ScatteredPositions gs.Table (List.length gs.Table)
            // Compute animation target: scatter position for Place, table center for Capture
            let toX, toY, newScattered =
                match turnResult.PlayResult, screen.TableLayout with
                | Place _, RandomScatter ->
                    // Card was added to table — compute new scatter positions
                    let newPos = computeScatteredPositions turnResult.NewState.Table screenW screenH screen.ScatteredPositions
                    match Map.tryFind card newPos with
                    | Some(sx, sy, _) ->
                        (float32 (sx - CardRenderer.scaledWidth() / 2),
                         float32 (sy - CardRenderer.scaledHeight() / 2), newPos)
                    | None ->
                        let tArea = tableArea screenW screenH
                        (float32 (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth() / 2),
                         float32 (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight() / 2), newPos)
                | _ ->
                    let tArea = tableArea screenW screenH
                    (float32 (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth() / 2),
                     float32 (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight() / 2), screen.ScatteredPositions)
            let cardAnim =
                { Card = card
                  FromX = float32 fromRect.X
                  FromY = float32 fromRect.Y
                  ToX = toX; ToY = toY
                  Duration = cardSlideDuration }
            let msg = formatPlayResult player.Name turnResult.PlayResult
            { screen with
                GameState = turnResult.NewState
                LastPlayMessage = msg
                SelectedCardIndex = None
                ScatteredPositions = newScattered
                Phase = AnimatingPlay(0.0, turnResult.Evaluation, Some cardAnim, collectAnim) }

    /// Process a chosen capture option (from button tap or keyboard)
    let private processCapture (screen: ScreenState) (cardIdx: int) (chosen: Rules.CaptureOption) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let player = gs.Players[gs.CurrentPlayerIndex]
        let card = player.Hand[cardIdx]
        let fromRect = handCardRect screenW screenH player.Hand.Length cardIdx true
        let turnResult = GameEngine.playHumanTurn gs cardIdx (Some chosen)
        let collectAnim =
            buildCollectAnimation turnResult.PlayResult true screenW screenH
                screen.ScatteredPositions gs.Table (List.length gs.Table)
        // Captures always animate to table center (cards get collected away)
        let tArea = tableArea screenW screenH
        let cardAnim =
            { Card = card
              FromX = float32 fromRect.X
              FromY = float32 fromRect.Y
              ToX = float32 (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth() / 2)
              ToY = float32 (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight() / 2)
              Duration = cardSlideDuration }
        let msg = formatPlayResult player.Name turnResult.PlayResult
        { screen with
            GameState = turnResult.NewState
            LastPlayMessage = msg
            SelectedCardIndex = None
            Phase = AnimatingPlay(0.0, turnResult.Evaluation, Some cardAnim, collectAnim) }

    let update (input: InputHandler.InputState) (dt: float) (screenW: int) (screenH: int) (screen: ScreenState) =
        let gs = screen.GameState

        // Update scattered positions when table changes
        let screen =
            match screen.TableLayout with
            | RandomScatter ->
                let newPositions = computeScatteredPositions gs.Table screenW screenH screen.ScatteredPositions
                { screen with ScatteredPositions = newPositions }
            | StrictGrid -> screen

        match screen.Phase with
        | Shuffling elapsed ->
            let newElapsed = elapsed + dt
            if newElapsed >= shuffleDuration then
                // Shuffle done — start deal animation
                let isFirst = gs.DealRound = 1
                let steps = buildDealSteps gs isFirst screenW screenH
                { screen with Phase = Dealing(0, 0.0, steps) }
            else
                { screen with Phase = Shuffling newElapsed }

        | Dealing(step, elapsed, steps) ->
            if step >= List.length steps then
                // All deal steps done — advance to first turn
                advanceTurn screen
            else
                let newElapsed = elapsed + dt
                if newElapsed >= dealStepDuration then
                    { screen with Phase = Dealing(step + 1, 0.0, steps) }
                else
                    { screen with Phase = Dealing(step, newElapsed, steps) }

        | WaitingForHuman ->
            // Check "?" help button first (not during modal)
            let hBtn = helpButton screenW
            if Button.isClicked input hBtn then
                { screen with ShowRulesClicked = true }
            else

            // Check Menu button
            let mBtn = menuButton screenW
            if Button.isClicked input mBtn then
                { screen with MenuClicked = true }
            else

            // Check layout toggle button
            let ltBtn = layoutToggleButton screenW screen.TableLayout
            if Button.isClicked input ltBtn then
                let newLayout = match screen.TableLayout with StrictGrid -> RandomScatter | RandomScatter -> StrictGrid
                let newScatter =
                    match newLayout with
                    | RandomScatter -> computeScatteredPositions gs.Table screenW screenH Map.empty
                    | StrictGrid -> Map.empty
                { screen with TableLayout = newLayout; ScatteredPositions = newScatter }
            else

            // Build card rects for current player's hand
            let player = gs.Players[gs.CurrentPlayerIndex]
            let rects =
                player.Hand
                |> List.mapi (fun i _ ->
                    (i, handCardRect screenW screenH player.Hand.Length i true))

            // Check hover (only when not dragging)
            let hovered =
                match screen.DragState with
                | Dragging _ -> None
                | NotDragging ->
                    rects
                    |> List.tryFind (fun (_, r) -> InputHandler.hitTest r input.Mouse.Position)
                    |> Option.map fst

            // Compute capture preview for selected card
            let previewIdx =
                match screen.DragState with
                | Dragging(idx, _, _) -> Some idx
                | NotDragging -> screen.SelectedCardIndex
            let preview =
                match previewIdx with
                | Some idx when idx < player.Hand.Length ->
                    let card = player.Hand[idx]
                    let options = Rules.findCaptureOptions card gs.Table
                    match options with
                    | [] -> NoCapture
                    | [ single ] -> SingleCapture single.Captured
                    | multiple ->
                        let allSets = multiple |> List.map (fun o -> Set.ofList o.Captured)
                        let definite = allSets |> List.reduce Set.intersect |> Set.toList
                        let anyCapture = allSets |> List.reduce Set.union |> Set.toList
                        let possible = anyCapture |> List.filter (fun c -> not (List.contains c definite))
                        MultipleCaptures(definite, possible)
                | _ -> NoCapture

            let screen = { screen with CardRects = rects; HoveredCardIndex = hovered; CapturePreview = preview }

            // ── Drag & Drop handling ──
            match screen.DragState with
            | NotDragging ->
                // ── Keyboard shortcuts: number keys select, Enter confirms, Escape deselects/menu ──
                if input.Keyboard.IsEscapePressed then
                    if screen.SelectedCardIndex.IsSome then
                        { screen with SelectedCardIndex = None; CapturePreview = NoCapture }
                    else
                        { screen with MenuClicked = true }
                elif input.Keyboard.IsEnterPressed then
                    match screen.SelectedCardIndex with
                    | Some idx -> processHumanPlay screen idx screenW screenH
                    | None -> screen
                else
                match input.Keyboard.NumberPressed with
                | Some n when n >= 1 && n <= player.Hand.Length ->
                    let idx = n - 1
                    if screen.SelectedCardIndex = Some idx then
                        // Same key again -> confirm play
                        processHumanPlay screen idx screenW screenH
                    else
                        { screen with SelectedCardIndex = Some idx }
                | _ ->
                if input.Mouse.LeftJustClicked then
                    match InputHandler.findClickedCard rects input.Mouse.Position with
                    | Some idx when screen.SelectedCardIndex = Some idx ->
                        // Tap same card again -> confirm play
                        processHumanPlay screen idx screenW screenH
                    | Some idx ->
                        // Start potential drag and select card
                        { screen with
                            SelectedCardIndex = Some idx
                            DragState = Dragging(idx, input.Mouse.Position, input.Mouse.Position) }
                    | None ->
                        // Tapped outside cards — check Play button or deselect
                        match screen.SelectedCardIndex with
                        | Some selectedIdx ->
                            let btn = playButton screenW screenH preview
                            if Button.isClicked input btn then
                                processHumanPlay screen selectedIdx screenW screenH
                            else
                                { screen with SelectedCardIndex = None; CapturePreview = NoCapture }
                        | None -> screen
                else
                    screen

            | Dragging(idx, startPos, _) ->
                if input.Mouse.LeftPressed then
                    // Continue dragging — update position
                    { screen with DragState = Dragging(idx, startPos, input.Mouse.Position) }
                else
                    // Released — check if it was a drag (moved enough) or a click
                    let dx = abs(input.Mouse.Position.X - startPos.X)
                    let dy = abs(input.Mouse.Position.Y - startPos.Y)
                    if dx > dragThreshold || dy > dragThreshold then
                        // It was a real drag — check if dropped on table area
                        let tArea = tableArea screenW screenH
                        if tArea.Contains(input.Mouse.Position) then
                            // Play the card
                            let newScreen = { screen with DragState = NotDragging }
                            processHumanPlay newScreen idx screenW screenH
                        else
                            // Dropped outside table — cancel drag, keep selected
                            { screen with DragState = NotDragging; SelectedCardIndex = Some idx }
                    else
                        // It was just a click — select the card
                        { screen with DragState = NotDragging; SelectedCardIndex = Some idx }

        | ComputerThinking elapsed ->
            let newElapsed = elapsed + dt
            if newElapsed >= computerDelay then
                let player = gs.Players[gs.CurrentPlayerIndex]
                let turnResult = GameEngine.playComputerTurn gs
                let collectAnim =
                    buildCollectAnimation turnResult.PlayResult false screenW screenH
                        screen.ScatteredPositions gs.Table (List.length gs.Table)
                // Compute animation target: scatter position for Place, table center for Capture
                let toX, toY, newScattered =
                    match turnResult.PlayResult, screen.TableLayout with
                    | Place placedCard, RandomScatter ->
                        let newPos = computeScatteredPositions turnResult.NewState.Table screenW screenH screen.ScatteredPositions
                        match Map.tryFind placedCard newPos with
                        | Some(sx, sy, _) ->
                            (float32 (sx - CardRenderer.scaledWidth() / 2),
                             float32 (sy - CardRenderer.scaledHeight() / 2), newPos)
                        | None ->
                            let tArea = tableArea screenW screenH
                            (float32 (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth() / 2),
                             float32 (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight() / 2), newPos)
                    | _ ->
                        let tArea = tableArea screenW screenH
                        (float32 (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth() / 2),
                         float32 (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight() / 2), screen.ScatteredPositions)
                let cardAnim =
                    if not (List.isEmpty player.Hand) then
                        let oppHandSize = List.length player.Hand
                        let fromRect = handCardRect screenW screenH oppHandSize 0 false
                        Some { Card = player.Hand[0]
                               FromX = float32 fromRect.X
                               FromY = float32 fromRect.Y
                               ToX = toX; ToY = toY
                               Duration = cardSlideDuration }
                    else None
                let msg = formatPlayResult player.Name turnResult.PlayResult
                { screen with
                    GameState = turnResult.NewState
                    LastPlayMessage = msg
                    ScatteredPositions = newScattered
                    Phase = AnimatingPlay(0.0, turnResult.Evaluation, cardAnim, collectAnim) }
            else
                { screen with Phase = ComputerThinking newElapsed }

        | AnimatingPlay(elapsed, eval, cardAnim, collectAnim) ->
            let newElapsed = elapsed + dt
            if newElapsed >= animDelay then
                advanceTurn screen
            else
                { screen with Phase = AnimatingPlay(newElapsed, eval, cardAnim, collectAnim) }

        | ChoosingCaptureOption(cardIdx, options) ->
            // Touch: check capture option buttons
            let optButtons = captureOptionButtons options screenW screenH
            match Button.findClicked input optButtons with
            | Some n ->
                processCapture screen cardIdx options[n] screenW screenH
            | None ->
                // Check cancel button or Escape
                let cancel = cancelButton options.Length screenW screenH
                if Button.isClicked input cancel || input.Keyboard.IsEscapePressed then
                    { screen with Phase = WaitingForHuman; SelectedCardIndex = None }
                else
                    // Keyboard fallback for number keys
                    match input.Keyboard.NumberPressed with
                    | Some n when n >= 1 && n <= options.Length ->
                        processCapture screen cardIdx options[n - 1] screenW screenH
                    | _ -> screen

        | RoundOver ->
            // Check layout toggle even during round over
            let ltBtn = layoutToggleButton screenW screen.TableLayout
            if Button.isClicked input ltBtn then
                let newLayout = match screen.TableLayout with StrictGrid -> RandomScatter | RandomScatter -> StrictGrid
                { screen with TableLayout = newLayout; ScatteredPositions = Map.empty }
            else
            // Touch: Continue button. Keyboard: Enter key.
            let btn = continueButton screenW screenH
            if Button.isClicked input btn || input.Keyboard.IsEnterPressed then
                { screen with ContinueClicked = true }
            else
                screen

        | GameOver -> screen

    // ── Drawing ─────────────────────────────────────────────

    let private drawPlayerLabel (sb: SpriteBatch) (font: SpriteFontBase) (player: Player) (x: int) (y: int) (color: Color) =
        let text = $"{player.Name}  Cards:{List.length player.CapturedCards}  Sweeps:{player.Sweeps}"
        sb.DrawString(font, text, Vector2(float32 x, float32 y), color) |> ignore

    let draw (sb: SpriteBatch) (font: SpriteFontBase) (input: InputHandler.InputState) (textures: CardRenderer.CardTextures) (screen: ScreenState) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let cw = CardRenderer.scaledWidth()
        let ch = CardRenderer.scaledHeight()

        // ── Draw table background ───────────────────────
        let tArea = tableArea screenW screenH
        sb.Draw(textures.TableBg, tArea, Color.White)

        // ── Draw table cards (with capture preview overlays) ──
        let tableCount = List.length gs.Table
        // Premultiplied overlay colors for AlphaBlend
        let greenOverlay  = Color(0, 70, 0, 90)      // definite capture
        let yellowOverlay = Color(70, 70, 0, 90)      // possible capture (choice needed)
        let definiteSet, possibleSet =
            match screen.CapturePreview with
            | NoCapture -> Set.empty, Set.empty
            | SingleCapture cards -> Set.ofList cards, Set.empty
            | MultipleCaptures(definite, possible) -> Set.ofList definite, Set.ofList possible

        // During AnimatingPlay, hide the card being animated from the table to prevent flicker
        let animatingCard =
            match screen.Phase with
            | AnimatingPlay(elapsed, _, Some anim, _) when elapsed < anim.Duration -> Some anim.Card
            | _ -> None

        for i in 0 .. tableCount - 1 do
            let card = gs.Table[i]
            // Skip drawing this card if it's currently being animated (Place action flicker fix)
            if animatingCard = Some card then () else
            match screen.TableLayout with
            | StrictGrid ->
                let r = tableCardRect screenW screenH tableCount i
                if Set.contains card definiteSet then
                    CardRenderer.drawCardWithOverlay sb textures card r.X r.Y greenOverlay
                elif Set.contains card possibleSet then
                    CardRenderer.drawCardWithOverlay sb textures card r.X r.Y yellowOverlay
                else
                    CardRenderer.drawCard sb textures card r.X r.Y
            | RandomScatter ->
                match Map.tryFind card screen.ScatteredPositions with
                | Some (sx, sy, rot) ->
                    let drawX = sx - cw / 2
                    let drawY = sy - ch / 2
                    if Set.contains card definiteSet then
                        CardRenderer.drawCardWithOverlayRotated sb textures card drawX drawY greenOverlay rot
                    elif Set.contains card possibleSet then
                        CardRenderer.drawCardWithOverlayRotated sb textures card drawX drawY yellowOverlay rot
                    else
                        CardRenderer.drawCardRotated sb textures card drawX drawY rot
                | None ->
                    let r = tableCardRect screenW screenH tableCount i
                    CardRenderer.drawCard sb textures card r.X r.Y

        // ── Draw opponent hands (top, face-down) ────────
        let bottomIdx = if screen.Config.HumanCount > 0 then 0 else gs.CurrentPlayerIndex
        let opponents =
            gs.Players
            |> List.mapi (fun i p -> (i, p))
            |> List.filter (fun (i, _) -> i <> bottomIdx)

        // Top opponent
        match opponents with
        | (_, opp) :: _ ->
            let oppHandSize = List.length opp.Hand
            for i in 0 .. oppHandSize - 1 do
                let r = handCardRect screenW screenH oppHandSize i false
                if opp.Type = Computer then
                    CardRenderer.drawCardBack sb textures r.X r.Y
                else
                    CardRenderer.drawCard sb textures opp.Hand[i] r.X r.Y
            drawPlayerLabel sb font opp 20 (ch + 30) Color.LightSalmon
        | _ -> ()

        // Side opponents (3-4 player games)
        if opponents.Length >= 2 then
            let (_, opp2) = opponents[1]
            let sideY = screenH / 2 - (List.length opp2.Hand * (ch + 4)) / 2
            for i in 0 .. List.length opp2.Hand - 1 do
                CardRenderer.drawCardBack sb textures 10 (sideY + i * (ch / 3))
            sb.DrawString(font, opp2.Name, Vector2(10.0f, float32 (sideY - 20)), Color.LightBlue) |> ignore

        if opponents.Length >= 3 then
            let (_, opp3) = opponents[2]
            let sideY = screenH / 2 - (List.length opp3.Hand * (ch + 4)) / 2
            let sideX = screenW - cw - 10
            for i in 0 .. List.length opp3.Hand - 1 do
                CardRenderer.drawCardBack sb textures sideX (sideY + i * (ch / 3))
            sb.DrawString(font, opp3.Name, Vector2(float32 sideX, float32 (sideY - 20)), Color.Plum) |> ignore

        // ── Draw human hand (bottom, face-up) ───────────
        if screen.Config.HumanCount > 0 then
            let humanIdx = 0
            let human = gs.Players[humanIdx]
            let handSize = List.length human.Hand
            let isDraggingIdx = match screen.DragState with Dragging(idx, _, _) -> Some idx | _ -> None
            for i in 0 .. handSize - 1 do
                // Skip drawing card at its normal position if being dragged
                if isDraggingIdx = Some i then () else
                let r = handCardRect screenW screenH handSize i true
                let isSelected = screen.SelectedCardIndex = Some i
                let isHovered = screen.HoveredCardIndex = Some i
                let yOffset = if isSelected then -15 elif isHovered then -10 else 0
                if isSelected then
                    CardRenderer.drawCardHighlighted sb textures human.Hand[i] r.X (r.Y + yOffset) Color.LimeGreen
                elif isHovered then
                    CardRenderer.drawCardHighlighted sb textures human.Hand[i] r.X (r.Y + yOffset) Color.Yellow
                else
                    CardRenderer.drawCard sb textures human.Hand[i] r.X (r.Y + yOffset)

            // Draw dragged card at cursor position
            match screen.DragState with
            | Dragging(idx, startPos, curPos) when idx < handSize ->
                let dx = abs(curPos.X - startPos.X)
                let dy = abs(curPos.Y - startPos.Y)
                if dx > dragThreshold || dy > dragThreshold then
                    let drawX = curPos.X - cw / 2
                    let drawY = curPos.Y - ch / 2
                    CardRenderer.drawCardHighlighted sb textures human.Hand[idx] drawX drawY Color.LimeGreen
            | _ -> ()

            // Draw dynamic "Play Card" button when a card is selected (and not dragging)
            match screen.Phase, screen.SelectedCardIndex, screen.DragState with
            | WaitingForHuman, Some _, NotDragging ->
                Button.draw sb font input (playButton screenW screenH screen.CapturePreview)
            | _ -> ()
        else
            // AI-only: show current player's hand face-up
            let curIdx = gs.CurrentPlayerIndex
            let curPlayer = gs.Players[curIdx]
            let handSize = List.length curPlayer.Hand
            for i in 0 .. handSize - 1 do
                let r = handCardRect screenW screenH handSize i true
                CardRenderer.drawCard sb textures curPlayer.Hand[i] r.X r.Y

        // ── Draw player label for bottom player ─────────
        let bottomPlayer = gs.Players[bottomIdx]
        drawPlayerLabel sb font bottomPlayer 20 (screenH - 18) Color.LightGreen

        // ── Draw status bar ─────────────────────────────
        let statusY = screenH / 2 + tArea.Height / 2 + 10
        sb.DrawString(font, screen.LastPlayMessage, Vector2(20.0f, float32 statusY), Color.White) |> ignore

        // Current turn indicator
        let turnText =
            match screen.Phase with
            | WaitingForHuman when screen.SelectedCardIndex.IsSome ->
                match screen.DragState with
                | Dragging _ -> "Drag to table or release to cancel"
                | NotDragging ->
                    match screen.CapturePreview with
                    | NoCapture -> "Place on table. [Enter] play  [Esc] cancel"
                    | SingleCapture cards -> $"Capture {cards.Length} cards. [Enter] play  [Esc] cancel"
                    | MultipleCaptures _ -> "Multiple options. [Enter] choose  [Esc] cancel"
            | WaitingForHuman ->
                "Select a card ([1]-[4] or click)  [Esc] menu"
            | ComputerThinking _ ->
                $"{gs.Players[gs.CurrentPlayerIndex].Name} thinking..."
            | ChoosingCaptureOption _ -> ""
            | AnimatingPlay _ -> ""
            | Shuffling _ -> "Shuffling..."
            | Dealing _ -> "Dealing..."
            | RoundOver -> "Round over! [Enter] continue"
            | GameOver -> "Game over!"
        sb.DrawString(font, turnText, Vector2(20.0f, float32 (statusY + 20)), Color.Gold) |> ignore

        // ── Draw scoreboard (top-right, below opponent label) ──
        let scoreX = screenW - 200
        let scoreStartY = ch + 50
        sb.DrawString(font, "Scores:", Vector2(float32 scoreX, float32 scoreStartY), Color.Gold) |> ignore
        gs.Players |> List.iteri (fun i p ->
            let cumScore = screen.CumulativeScores |> Map.tryFind p.Name |> Option.defaultValue 0
            let text = $"{p.Name}: {cumScore}"
            sb.DrawString(font, text, Vector2(float32 scoreX, float32 (scoreStartY + 24 + i * 22)), Color.White) |> ignore)

        // Round / deal info with deck indicator
        let infoY = scoreStartY + 24 + gs.Players.Length * 22 + 8
        let deckCount = List.length gs.Deck
        let infoText = $"R{screen.RoundNumber} Deal {gs.DealRound}/{gs.TotalDeals}"
        sb.DrawString(font, infoText, Vector2(float32 scoreX, float32 infoY), Color.LightGray) |> ignore
        // Small card-back sprite as deck indicator
        let deckIconW = cw / 2
        let deckIconH = ch / 2
        let deckIconX = scoreX
        let deckIconY = infoY + 26
        sb.Draw(textures.Back, Rectangle(deckIconX, deckIconY, deckIconW, deckIconH), Color.White)
        let deckText = $"{deckCount}"
        sb.DrawString(font, deckText, Vector2(float32 (deckIconX + deckIconW + 6), float32 (deckIconY + 4)), Color.LightGray) |> ignore

        // ── "?" help button, layout toggle, and Menu button (shown when not in modal) ──
        match screen.Phase with
        | ChoosingCaptureOption _ -> ()  // modal hides them
        | _ ->
            Button.draw sb font input (helpButton screenW)
            Button.draw sb font input (layoutToggleButton screenW screen.TableLayout)
            Button.draw sb font input (menuButton screenW)

        // ── Shuffle animation (riffle shuffle, drawn on top of table) ──
        match screen.Phase with
        | Shuffling elapsed ->
            let t = float32 (elapsed / shuffleDuration)  // 0.0 to 1.0
            let cw = CardRenderer.scaledWidth()
            let ch = CardRenderer.scaledHeight()
            let centerX = screenW / 2
            let centerY = screenH / 2
            let numCards = 6
            let halfN = numCards / 2
            let tex = textures.Back
            let origin = Vector2(float32 tex.Width / 2.0f, float32 tex.Height / 2.0f)
            // Riffle: two halves start separated, slide together, interleave
            let separation = 100.0f  // max pixel distance between halves
            for i in 0 .. numCards - 1 do
                let isLeft = i % 2 = 0       // alternating: left, right, left, right...
                let stackIdx = i / 2          // position within its half
                // Phase 1 (0..0.4): halves separate outward
                // Phase 2 (0.4..1.0): halves slide back together, interleaving
                let sepAmount, interleaveY =
                    if t < 0.4f then
                        let p = t / 0.4f  // 0..1
                        let eased = 1.0f - (1.0f - p) * (1.0f - p)
                        (separation * eased, 0.0f)
                    else
                        let p = (t - 0.4f) / 0.6f  // 0..1
                        let eased = 1.0f - (1.0f - p) * (1.0f - p)
                        let sep = separation * (1.0f - eased)
                        // Interleave: cards from each half get slight vertical offset
                        let vertShift = float32 (stackIdx - halfN / 2) * 3.0f * eased
                        (sep, vertShift)
                let xOff = if isLeft then -sepAmount else sepAmount
                // Small vertical stacking offset within each half
                let yStack = float32 (stackIdx - halfN / 2) * 2.0f
                let dest = Rectangle(
                    centerX + int xOff,
                    centerY + int (yStack + interleaveY),
                    cw, ch)
                sb.Draw(tex, dest, System.Nullable(), Color.White, 0.0f, origin, SpriteEffects.None, 0.0f)
        | _ -> ()

        // ── Deal animation (cards slide from deck to destinations) ──
        match screen.Phase with
        | Dealing(step, elapsed, steps) when step < List.length steps ->
            let currentStep = steps[step]
            let cw = CardRenderer.scaledWidth()
            let ch = CardRenderer.scaledHeight()
            let tArea = tableArea screenW screenH
            let deckX = float32 (tArea.X + tArea.Width / 2)
            let deckY = float32 (tArea.Y + tArea.Height / 2)
            let t = float32 (elapsed / dealStepDuration)
            let eased = 1.0f - (1.0f - t) * (1.0f - t)  // ease-out
            let tex = textures.Back
            let origin = Vector2(float32 tex.Width / 2.0f, float32 tex.Height / 2.0f)
            for ci in 0 .. currentStep.CardCount - 1 do
                // Slight horizontal spread at destination
                let spread = float32 (ci - currentStep.CardCount / 2) * 12.0f
                let destX = currentStep.ToX + spread
                let destY = currentStep.ToY
                let x = deckX + (destX - deckX) * eased
                let y = deckY + (destY - deckY) * eased
                let dest = Rectangle(int x, int y, cw, ch)
                sb.Draw(tex, dest, System.Nullable(), Color.White, 0.0f, origin, SpriteEffects.None, 0.0f)
            // Also draw remaining deck cards as a stack at deck position
            let remainingSteps = List.length steps - step
            let stackCards = min 3 remainingSteps
            for si in 0 .. stackCards - 1 do
                let offset = float32 si * -2.0f
                let dest = Rectangle(int deckX, int (deckY + offset), cw, ch)
                sb.Draw(tex, dest, System.Nullable(), Color.White, 0.0f, origin, SpriteEffects.None, 0.0f)
        | _ -> ()

        // ── Card movement animation (drawn on top of everything else) ──
        match screen.Phase with
        | AnimatingPlay(elapsed, _, Some anim, _) when elapsed < anim.Duration ->
            let t = float32 (elapsed / anim.Duration)
            // Ease-out: smooth deceleration
            let eased = 1.0f - (1.0f - t) * (1.0f - t)
            let x = int (anim.FromX + (anim.ToX - anim.FromX) * eased)
            let y = int (anim.FromY + (anim.ToY - anim.FromY) * eased)
            CardRenderer.drawCard sb textures anim.Card x y
        | _ -> ()

        // ── Collect animation: captured cards slide to player ──
        match screen.Phase with
        | AnimatingPlay(elapsed, _, _, Some collect) when elapsed >= collect.StartTime && elapsed < collect.StartTime + collect.Duration ->
            let t = float32 ((elapsed - collect.StartTime) / collect.Duration)
            let eased = 1.0f - (1.0f - t) * (1.0f - t)  // ease-out
            for (card, fx, fy) in collect.Cards do
                let x = int (fx + (collect.ToX - fx) * eased)
                let y = int (fy + (collect.ToY - fy) * eased)
                CardRenderer.drawCard sb textures card (x - cw / 2) (y - ch / 2)
        | _ -> ()

        // ── Modal overlays (drawn on top of everything) ──
        match screen.Phase with
        | ChoosingCaptureOption(_, options) ->
            // Semi-transparent dark overlay
            let overlayTex = CardRenderer.getCachedColorTexture (sb.GraphicsDevice) (Color(0, 0, 0, 160))
            sb.Draw(overlayTex, Rectangle(0, 0, screenW, screenH), Color.White)

            // Header text
            let headerText = "Choose which cards to capture:"
            let headerSize = font.MeasureString(headerText)
            let optButtons = captureOptionButtons options screenW screenH
            let headerY =
                match optButtons with
                | btn :: _ -> float32 btn.Rect.Y - headerSize.Y - 12.0f
                | [] -> float32 (screenH / 2 - 80)
            sb.DrawString(font, headerText, Vector2(float32 screenW / 2.0f - headerSize.X / 2.0f, headerY), Color.Gold) |> ignore

            // Option buttons + cancel
            Button.drawAll sb font input optButtons
            Button.draw sb font input (cancelButton options.Length screenW screenH)

        | RoundOver ->
            // Continue button (no overlay needed)
            Button.draw sb font input (continueButton screenW screenH)

        | _ -> ()
