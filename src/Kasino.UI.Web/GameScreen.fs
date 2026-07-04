namespace Kasino.UI.Web

open System
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Main gameplay screen: table, hands, card selection, turns.
// Tap a card to select, tap "Play" (or the table, via drag) to
// confirm. Overlapping capture options are shown as a modal of
// tappable buttons. Keyboard shortcuts mirror the desktop build.
// Ported from the MonoGame GameScreen onto an HTML5 Canvas.
// ─────────────────────────────────────────────────────────────

module GameScreen =

    type Phase =
        | Shuffling of elapsed: float
        | Dealing of step: int * elapsed: float * steps: DealStep list
        | WaitingForHuman
        | ComputerThinking of elapsed: float
        | AnimatingPlay of elapsed: float * AI.PlayEvaluation * cardAnim: CardAnimation option * collectAnim: CollectAnimation option
        | ChoosingCaptureOption of cardIndex: int * Rules.CaptureOption list * page: int
        | RoundOver
        | GameOver

    and DealStep =
        { TargetLabel: string
          CardCount: int
          ToX: float; ToY: float
          IsFaceUp: bool }

    and CardAnimation =
        { Card: Card
          FromX: float; FromY: float
          ToX: float; ToY: float
          Duration: float
          Highlight: bool }   // human plays slide highlighted so they read as yours

    and CollectAnimation =
        { Cards: (Card * float * float) list
          ToX: float; ToY: float
          StartTime: float
          Duration: float }

    type CapturePreview =
        | NoCapture
        | SingleCapture of definite: Card list
        | MultipleCaptures of definite: Card list * possible: Card list

    type DragState =
        | NotDragging
        | Dragging of cardIndex: int * startPos: Point * currentPos: Point
        | DraggingTable of card: Card * grabOffsetX: int * grabOffsetY: int   // nudging a table card (scatter mode)

    type TableLayout =
        | StrictGrid
        | RandomScatter

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
          CardRects: (int * Rect) list
          TableCardRects: Rect list
          CapturePreview: CapturePreview
          ContinueClicked: bool
          ShowRulesClicked: bool
          MenuClicked: bool
          DragState: DragState
          TableLayout: TableLayout
          ScatteredPositions: Map<Card, (int * int * float)>
          Chat: (string * float) option }      // active table-talk line + seconds remaining

    let private computerDelay = 0.8
    let private animDelay = 1.4
    let private shuffleDuration = 0.6
    // Slow enough to register — at 0.25s a player's own play was over before
    // their eyes returned from the tap, and the computer's reply slide was
    // routinely mistaken for it.
    let private cardSlideDuration = 0.4
    let private collectSlideDuration = 0.35
    let private dealStepDuration = 0.18

    let private dragThreshold = 8

    let private formatPlayResult (playerName: string) (result: PlayResult) =
        match result with
        | Capture(hc, captured, sweep) ->
            let capturedStr = captured |> List.map Cards.display |> String.concat " "
            let sweepStr = if sweep then " SWEEP!" else ""
            sprintf "%s plays %s -> captures %s%s" playerName (Cards.display hc) capturedStr sweepStr
        | Place hc ->
            sprintf "%s places %s on table" playerName (Cards.display hc)

    let private cardGap = 8
    let private tableCardGap = 6

    // ── Layout helpers ──────────────────────────────────────
    let private centerCards (screenW: int) (count: int) =
        let totalW = count * (CardRenderer.scaledWidth () + cardGap) - cardGap
        (screenW - totalW) / 2

    let private handCardRect (screenW: int) (screenH: int) (handSize: int) (idx: int) (isBottom: bool) =
        let x = centerCards screenW handSize + idx * (CardRenderer.scaledWidth () + cardGap)
        let y = if isBottom then screenH - CardRenderer.scaledHeight () - 20 else 20
        { X = x; Y = y; Width = CardRenderer.scaledWidth (); Height = CardRenderer.scaledHeight () }

    let private tableCardRect (screenW: int) (screenH: int) (count: int) (idx: int) =
        let cols = min count 10
        let rows = (count + cols - 1) / cols
        let totalW = cols * (CardRenderer.scaledWidth () + tableCardGap) - tableCardGap
        let totalH = rows * (CardRenderer.scaledHeight () + tableCardGap) - tableCardGap
        let baseX = (screenW - totalW) / 2
        let baseY = (screenH - totalH) / 2
        let col = idx % cols
        let row = idx / cols
        { X = baseX + col * (CardRenderer.scaledWidth () + tableCardGap)
          Y = baseY + row * (CardRenderer.scaledHeight () + tableCardGap)
          Width = CardRenderer.scaledWidth ()
          Height = CardRenderer.scaledHeight () }

    let private tableArea (screenW: int) (screenH: int) =
        let ch = CardRenderer.scaledHeight ()
        let cw = CardRenderer.scaledWidth ()
        let sideMargin = cw + 30
        { X = sideMargin; Y = ch + 60; Width = screenW - 2 * sideMargin; Height = screenH - 2 * ch - 140 }

    /// Compute scattered positions for table cards, placed center-outward.
    /// Card identity seeds the RNG for deterministic but random-looking layout.
    let computeScatteredPositions (table: Card list) (screenW: int) (screenH: int) (existing: Map<Card, (int * int * float)>) =
        let area = tableArea screenW screenH
        let cw = CardRenderer.scaledWidth ()
        let ch = CardRenderer.scaledHeight ()
        let centerX = area.X + area.Width / 2
        let centerY = area.Y + area.Height / 2
        let maxRadiusX = float (area.Width / 2 - cw / 2 - 10)
        let maxRadiusY = float (area.Height / 2 - ch / 2 - 10)

        let mutable result = existing

        let tableSet = Set.ofList table
        result <- result |> Map.filter (fun card _ -> Set.contains card tableSet)

        let newCards = table |> List.filter (fun c -> not (Map.containsKey c result))
        let existingCount = Map.count result

        for cardIdx in 0 .. List.length newCards - 1 do
            let card = newCards[cardIdx]
            let seed = hash (card.Suit, card.Rank)
            let rng = Random(seed)
            let orderIdx = existingCount + cardIdx
            let spreadFraction = float orderIdx / float (max 1 (List.length table - 1))
            let radiusFrac = 0.15 + 0.85 * spreadFraction

            let mutable attempts = 0
            let mutable placed = false
            let mutable bestX = centerX
            let mutable bestY = centerY
            let mutable bestRot = 0.0

            while not placed && attempts < 60 do
                let angle = rng.NextDouble() * Math.PI * 2.0
                let jitter = 0.7 + rng.NextDouble() * 0.6
                let rFrac = min 1.0 (radiusFrac * jitter)
                let rx = int (float centerX + cos angle * maxRadiusX * rFrac)
                let ry = int (float centerY + sin angle * maxRadiusY * rFrac)
                let x = max (area.X + cw / 2 + 5) (min (area.X + area.Width - cw / 2 - 5) rx)
                let y = max (area.Y + ch / 2 + 5) (min (area.Y + area.Height - ch / 2 - 5) ry)
                let rot = (rng.NextDouble() - 0.5) * 0.35

                let rect = { X = x - cw / 2; Y = y - ch / 2; Width = cw; Height = ch }
                let overlaps =
                    result |> Map.exists (fun _ (ox, oy, _) ->
                        rect.Intersects { X = ox - cw / 2; Y = oy - ch / 2; Width = cw; Height = ch })

                if not overlaps then
                    bestX <- x; bestY <- y; bestRot <- rot; placed <- true
                else
                    bestX <- x; bestY <- y; bestRot <- rot

                attempts <- attempts + 1

            result <- result |> Map.add card (bestX, bestY, bestRot)

        result

    /// Topmost scattered table card whose rect contains `pos` (for drag-to-reposition).
    let private tableCardAtScatter (screen: ScreenState) (pos: Point) =
        let cw = CardRenderer.scaledWidth ()
        let ch = CardRenderer.scaledHeight ()
        screen.GameState.Table
        |> List.rev                                   // later in the list draws on top
        |> List.tryPick (fun card ->
            match Map.tryFind card screen.ScatteredPositions with
            | Some(sx, sy, _) ->
                if ({ X = sx - cw / 2; Y = sy - ch / 2; Width = cw; Height = ch }: Rect).Contains pos then Some card else None
            | None -> None)

    /// Clamp a scattered card's centre so the whole card stays within the table area.
    let private clampScatterCenter (screenW: int) (screenH: int) (cx: int) (cy: int) =
        let area = tableArea screenW screenH
        let cw = CardRenderer.scaledWidth ()
        let ch = CardRenderer.scaledHeight ()
        let x = max (area.X + cw / 2) (min (area.X + area.Width - cw / 2) cx)
        let y = max (area.Y + ch / 2) (min (area.Y + area.Height - ch / 2) cy)
        (x, y)

    /// Build deal animation steps. First deal: 4 to table, then 2/player x2.
    let private buildDealSteps (gs: GameEngine.GameState) (isFirstDeal: bool) (screenW: int) (screenH: int) =
        let tArea = tableArea screenW screenH
        let tableCenterX = float (tArea.X + tArea.Width / 2)
        let tableCenterY = float (tArea.Y + tArea.Height / 2)
        let playerCount = gs.Players.Length
        let bottomIdx = if gs.Players |> List.exists (fun p -> p.Type = Human) then 0 else gs.CurrentPlayerIndex

        let playerDest (idx: int) =
            if idx = bottomIdx then
                let handY = float (screenH - CardRenderer.scaledHeight () - 20 + CardRenderer.scaledHeight () / 2)
                (float (screenW / 2), handY)
            else
                let handY = float (20 + CardRenderer.scaledHeight () / 2)
                (float (screenW / 2), handY)

        let tableStep = { TargetLabel = "table"; CardCount = 4; ToX = tableCenterX; ToY = tableCenterY; IsFaceUp = false }

        let playerSteps =
            [ for _ in 1 .. 2 do
                for pIdx in 0 .. playerCount - 1 do
                    let (px, py) = playerDest pIdx
                    { TargetLabel = gs.Players[pIdx].Name; CardCount = 2; ToX = px; ToY = py; IsFaceUp = (pIdx = bottomIdx) } ]

        if isFirstDeal then tableStep :: playerSteps else playerSteps

    /// Build a collect animation: captured cards slide to the player's pile.
    let private buildCollectAnimation
        (playResult: PlayResult)
        (isBottom: bool)
        (screenW: int)
        (screenH: int)
        (scatteredPos: Map<Card, (int * int * float)>)
        (tableCards: Card list)
        (tableCount: int)
        =
        match playResult with
        | Capture(hc, captured, _) when not (List.isEmpty captured) ->
            let destX, destY =
                if isBottom then float (screenW / 2), float (screenH - 10)
                else float (screenW / 2), 0.0
            let cards =
                captured
                |> List.map (fun card ->
                    match Map.tryFind card scatteredPos with
                    | Some(x, y, _) -> (card, float x, float y)
                    | None ->
                        let idx = tableCards |> List.tryFindIndex ((=) card) |> Option.defaultValue 0
                        let r = tableCardRect screenW screenH tableCount idx
                        (card, float (r.X + r.Width / 2), float (r.Y + r.Height / 2)))
            // The played card is banked with the capture: after its slide
            // deposits it at the table centre, it visibly rides along to the
            // pile instead of vanishing there. Appended last so it draws on
            // top of the captured cards.
            let tArea = tableArea screenW screenH
            let playedFrom =
                (hc, float (tArea.X + tArea.Width / 2), float (tArea.Y + tArea.Height / 2))
            Some
                { Cards = cards @ [ playedFrom ]
                  ToX = destX
                  ToY = destY
                  StartTime = cardSlideDuration
                  Duration = collectSlideDuration }
        | _ -> None

    // ── Button helpers ──────────────────────────────────────
    let private playButton (screenW: int) (screenH: int) (preview: CapturePreview) =
        let label, color =
            match preview with
            | NoCapture -> "Place on Table", Color.rgb 100 100 100
            | SingleCapture cards -> sprintf "Capture %d Cards" cards.Length, Color.rgb 40 140 40
            | MultipleCaptures _ -> "Play (Choose Capture)", Color.rgb 160 160 40
        Button.createCentered label screenW (screenH - CardRenderer.scaledHeight () - 80) 240 52 color Color.White

    let private helpButton (_screenW: int) =
        Button.create "?" 20 20 120 48 (Color.rgb 80 80 40) Color.White

    let private layoutToggleButton (_screenW: int) (layout: TableLayout) =
        let label = match layout with StrictGrid -> "Scatter" | RandomScatter -> "Grid"
        Button.create label 160 20 120 48 (Color.rgb 60 80 60) Color.White

    let private menuButton (_screenW: int) =
        Button.create "Menu" 300 20 120 48 (Color.rgb 120 40 40) Color.White

    let private continueButton (screenW: int) (screenH: int) =
        Button.createCentered "Continue" screenW (screenH / 2 + 60) 200 52 (Color.rgb 40 80 140) Color.White

    /// Small "Place Instead" button — offered beside the Play button in
    /// Standard Kasino, where capturing is optional.
    let private placeInsteadButton (screenW: int) (screenH: int) =
        Button.create "Place Instead" (screenW / 2 + 130) (screenH - CardRenderer.scaledHeight () - 80) 170 52 (Color.rgb 100 100 100) Color.White

    /// The capture-choice modal, paginated so it always fits on screen
    /// (findCaptureOptions can return up to 64 options).
    type private CaptureModal =
        { OptionButtons: (int * Button.ButtonDef) list   // absolute option index * button
          MoreButton: Button.ButtonDef option            // cycles to the next page
          PlaceButton: Button.ButtonDef option           // Standard only: decline the capture
          CancelButton: Button.ButtonDef
          PageStart: int
          VisibleCount: int
          NextPage: int }

    let private captureModal (variant: GameVariant) (options: Rules.CaptureOption list) (page: int) (screenW: int) (screenH: int) : CaptureModal =
        let perPage = max 3 ((screenH - 240) / 56)
        let pageCount = (options.Length + perPage - 1) / perPage
        let page = ((page % pageCount) + pageCount) % pageCount
        let startIdx = page * perPage
        let visible = options |> List.skip startIdx |> List.truncate perPage
        let allowPlace = (variant = StandardKasino)
        let navRows = if pageCount > 1 then 1 else 0
        let placeRows = if allowPlace then 1 else 0
        let totalH = (visible.Length + navRows + placeRows) * 56 + 64
        let baseY = max 20 ((screenH - totalH) / 2)
        let optButtons =
            visible
            |> List.mapi (fun i opt ->
                let cards = opt.Captured |> List.map Cards.display |> String.concat " "
                let label = sprintf "%d: %s (%d cards)" (i + 1) cards opt.Captured.Length
                (startIdx + i, Button.createCentered label screenW (baseY + i * 56) 450 48 (Color.rgb 60 80 60) Color.White))
        let navY = baseY + visible.Length * 56
        let moreButton =
            if pageCount > 1 then
                Some (Button.createCentered (sprintf "More options (%d/%d)" (page + 1) pageCount) screenW navY 450 48 (Color.rgb 60 60 100) Color.White)
            else None
        let placeY = navY + navRows * 56
        let placeButton =
            if allowPlace then
                Some (Button.createCentered "Place on table instead" screenW placeY 450 48 (Color.rgb 100 100 100) Color.White)
            else None
        let cancelBtn =
            Button.createCentered "Cancel" screenW (placeY + placeRows * 56 + 8) 180 48 (Color.rgb 120 40 40) Color.White
        { OptionButtons = optButtons
          MoreButton = moreButton
          PlaceButton = placeButton
          CancelButton = cancelBtn
          PageStart = startIdx
          VisibleCount = visible.Length
          NextPage = (page + 1) % pageCount }

    // ── Initialization ──────────────────────────────────────
    let create (config: GameEngine.GameConfig) (rng: Random) (players: Player list) (roundNumber: int) (scores: Map<string, int>) =
        let state = GameEngine.newRound config rng players roundNumber
        let state = GameEngine.dealRound state true
        { GameState = { state with DealRound = 1 }
          Config = config
          Phase = Shuffling 0.0
          SelectedCardIndex = None
          HoveredCardIndex = None
          LastPlayMessage = sprintf "Round %d - Deal 1" roundNumber
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
          TableLayout = (if config.Settings.DefaultScatter then RandomScatter else StrictGrid)
          ScatteredPositions = Map.empty
          Chat = None }

    // ── Update logic ────────────────────────────────────────
    let rec private advanceTurn (screen: ScreenState) =
        let gs = screen.GameState
        if GameEngine.allHandsEmpty gs then
            if gs.DealRound < gs.TotalDeals then
                let nextDeal = gs.DealRound + 1
                let newGs = GameEngine.dealRound gs false
                { screen with
                    GameState = { newGs with DealRound = nextDeal }
                    LastPlayMessage = sprintf "Round %d - Deal %d" screen.RoundNumber nextDeal
                    Phase = Shuffling 0.0 }
            else
                let finalGs = GameEngine.endRound gs
                { screen with GameState = finalGs; Phase = RoundOver }
        else
            let currentPlayer = gs.Players[gs.CurrentPlayerIndex]
            if List.isEmpty currentPlayer.Hand then
                let newGs = { gs with CurrentPlayerIndex = (gs.CurrentPlayerIndex + 1) % gs.Players.Length }
                advanceTurn { screen with GameState = newGs }
            else
                match currentPlayer.Type with
                | Human -> { screen with Phase = WaitingForHuman; SelectedCardIndex = None; HoveredCardIndex = None }
                | Computer -> { screen with Phase = ComputerThinking 0.0 }

    /// Apply an already-resolved human turn to the screen: build the play and
    /// collect animations and advance to AnimatingPlay. Shared by the plain
    /// play, chosen-capture, and place-instead paths. fromPos (a pointer
    /// position) lets a drag-released card continue its slide from the drop
    /// point instead of snapping back to its hand slot.
    let private finishHumanPlay (screen: ScreenState) (cardIndex: int) (fromPos: (float * float) option) (turnResult: GameEngine.TurnResult) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let player = gs.Players[gs.CurrentPlayerIndex]
        let card = player.Hand[cardIndex]
        let fromRect =
            match fromPos with
            | Some(px, py) ->
                { X = int px - CardRenderer.scaledWidth () / 2
                  Y = int py - CardRenderer.scaledHeight () / 2
                  Width = CardRenderer.scaledWidth ()
                  Height = CardRenderer.scaledHeight () }
            | None -> handCardRect screenW screenH player.Hand.Length cardIndex true
        let collectAnim =
            buildCollectAnimation turnResult.PlayResult true screenW screenH screen.ScatteredPositions gs.Table (List.length gs.Table)
        let toX, toY, newScattered =
            match turnResult.PlayResult, screen.TableLayout with
            | Place _, RandomScatter ->
                let newPos = computeScatteredPositions turnResult.NewState.Table screenW screenH screen.ScatteredPositions
                match Map.tryFind card newPos with
                | Some(sx, sy, _) ->
                    (float (sx - CardRenderer.scaledWidth () / 2), float (sy - CardRenderer.scaledHeight () / 2), newPos)
                | None ->
                    let tArea = tableArea screenW screenH
                    (float (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth () / 2),
                     float (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight () / 2), newPos)
            | _ ->
                let tArea = tableArea screenW screenH
                (float (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth () / 2),
                 float (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight () / 2), screen.ScatteredPositions)
        let cardAnim =
            { Card = card
              FromX = float fromRect.X
              FromY = float fromRect.Y
              ToX = toX
              ToY = toY
              Duration = cardSlideDuration
              Highlight = true }
        let msg = formatPlayResult player.Name turnResult.PlayResult
        { screen with
            GameState = turnResult.NewState
            LastPlayMessage = msg
            SelectedCardIndex = None
            ScatteredPositions = newScattered
            Phase = AnimatingPlay(0.0, turnResult.Evaluation, Some cardAnim, collectAnim) }

    let private processHumanPlayFrom (screen: ScreenState) (cardIndex: int) (fromPos: (float * float) option) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let player = gs.Players[gs.CurrentPlayerIndex]
        let card = player.Hand[cardIndex]
        let options = Rules.findCaptureOptions card gs.Table
        match options with
        | _ :: _ :: _ ->
            { screen with Phase = ChoosingCaptureOption(cardIndex, options, 0); SelectedCardIndex = None }
        | _ ->
            finishHumanPlay screen cardIndex fromPos (GameEngine.playHumanTurn gs cardIndex None) screenW screenH

    let private processHumanPlay (screen: ScreenState) (cardIndex: int) (screenW: int) (screenH: int) =
        processHumanPlayFrom screen cardIndex None screenW screenH

    let private processCapture (screen: ScreenState) (cardIdx: int) (chosen: Rules.CaptureOption) (screenW: int) (screenH: int) =
        finishHumanPlay screen cardIdx None (GameEngine.playHumanTurn screen.GameState cardIdx (Some chosen)) screenW screenH

    /// Place a capture-capable card without capturing (Standard Kasino only,
    /// where capturing is optional).
    let private processHumanPlace (screen: ScreenState) (cardIndex: int) (screenW: int) (screenH: int) =
        finishHumanPlay screen cardIndex None (GameEngine.playHumanPlaceTurn screen.GameState cardIndex) screenW screenH

    let update (input: Input.InputState) (dt: float) (screenW: int) (screenH: int) (screen: ScreenState) =
        let gs = screen.GameState

        // Fade out any active table-talk line.
        let screen =
            match screen.Chat with
            | Some(text, t) ->
                let t2 = t - dt
                { screen with Chat = (if t2 <= 0.0 then None else Some(text, t2)) }
            | None -> screen

        let screen =
            match screen.TableLayout with
            | RandomScatter ->
                let newPositions = computeScatteredPositions gs.Table screenW screenH screen.ScatteredPositions
                { screen with ScatteredPositions = newPositions }
            | StrictGrid -> screen

        // ── Global controls ─────────────────────────────────
        // The "?", Menu, and layout buttons are drawn in every phase except
        // the capture modal, so they must also react in every such phase —
        // notably watch-AI-only games, which never reach WaitingForHuman.
        // Escape likewise returns to the menu from phases that have no
        // Escape handling of their own.
        let inCaptureModal =
            match screen.Phase with ChoosingCaptureOption _ -> true | _ -> false
        let escapeToMenu =
            match screen.Phase with
            | WaitingForHuman | ChoosingCaptureOption _ -> false  // handled per-phase
            | _ -> input.Keyboard.IsEscapePressed

        if not inCaptureModal && Button.isClicked input (helpButton screenW) then
            { screen with ShowRulesClicked = true }
        elif not inCaptureModal && Button.isClicked input (menuButton screenW) then
            { screen with MenuClicked = true }
        elif not inCaptureModal && Button.isClicked input (layoutToggleButton screenW screen.TableLayout) then
            let newLayout = match screen.TableLayout with StrictGrid -> RandomScatter | RandomScatter -> StrictGrid
            let newScatter =
                match newLayout with
                | RandomScatter -> computeScatteredPositions gs.Table screenW screenH Map.empty
                | StrictGrid -> Map.empty
            { screen with TableLayout = newLayout; ScatteredPositions = newScatter }
        elif escapeToMenu then
            { screen with MenuClicked = true }
        else

        match screen.Phase with
        | Shuffling elapsed ->
            let newElapsed = elapsed + dt
            if newElapsed >= shuffleDuration then
                let isFirst = gs.DealRound = 1
                let steps = buildDealSteps gs isFirst screenW screenH
                { screen with Phase = Dealing(0, 0.0, steps) }
            else
                { screen with Phase = Shuffling newElapsed }

        | Dealing(step, elapsed, steps) ->
            if step >= List.length steps then
                advanceTurn screen
            else
                let newElapsed = elapsed + dt
                if newElapsed >= dealStepDuration then
                    { screen with Phase = Dealing(step + 1, 0.0, steps) }
                else
                    { screen with Phase = Dealing(step, newElapsed, steps) }

        | WaitingForHuman ->
            // Build card rects for current player's hand. Selected/hovered
            // cards are drawn lifted by 15/10 px, so extend the hit rect
            // upward by the same amount — otherwise clicking the visible top
            // edge of a raised card misses it and deselects instead.
            let player = gs.Players[gs.CurrentPlayerIndex]
            let rects =
                player.Hand
                |> List.mapi (fun i _ ->
                    let r = handCardRect screenW screenH player.Hand.Length i true
                    let lift =
                        if screen.SelectedCardIndex = Some i then 15
                        elif screen.HoveredCardIndex = Some i then 10
                        else 0
                    (i, { r with Y = r.Y - lift; Height = r.Height + lift }))

            let hovered =
                match screen.DragState with
                | Dragging _ | DraggingTable _ -> None
                | NotDragging ->
                    rects
                    |> List.tryFind (fun (_, r) -> Input.hitTest r input.Mouse.Position)
                    |> Option.map fst

            let previewIdx =
                match screen.DragState with
                | Dragging(idx, _, _) -> Some idx
                | NotDragging | DraggingTable _ -> screen.SelectedCardIndex
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

            match screen.DragState with
            | NotDragging ->
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
                        processHumanPlay screen idx screenW screenH
                    else
                        { screen with SelectedCardIndex = Some idx }
                | _ ->
                if input.Mouse.LeftJustClicked then
                    match Input.findClickedCard rects input.Mouse.Position with
                    | Some idx when screen.SelectedCardIndex = Some idx ->
                        processHumanPlay screen idx screenW screenH
                    | Some idx ->
                        { screen with
                            SelectedCardIndex = Some idx
                            DragState = Dragging(idx, input.Mouse.Position, input.Mouse.Position) }
                    | None ->
                        // Tapped outside the hand. Priority: Play button, then Place
                        // Instead (Standard: capturing is optional), then nudge a
                        // table card (scatter mode only — the grid never overlaps),
                        // else deselect.
                        let btn = playButton screenW screenH preview
                        let canPlaceInstead =
                            gs.Variant = StandardKasino
                            && (match preview with NoCapture -> false | _ -> true)
                        if screen.SelectedCardIndex.IsSome && Button.isClicked input btn then
                            processHumanPlay screen screen.SelectedCardIndex.Value screenW screenH
                        elif screen.SelectedCardIndex.IsSome && canPlaceInstead
                             && Button.isClicked input (placeInsteadButton screenW screenH) then
                            processHumanPlace screen screen.SelectedCardIndex.Value screenW screenH
                        else
                            let tableCardOpt =
                                if screen.TableLayout = RandomScatter
                                then tableCardAtScatter screen input.Mouse.Position
                                else None
                            match tableCardOpt with
                            | Some card ->
                                match Map.tryFind card screen.ScatteredPositions with
                                | Some(sx, sy, _) ->
                                    { screen with DragState = DraggingTable(card, input.Mouse.Position.X - sx, input.Mouse.Position.Y - sy) }
                                | None -> screen
                            | None ->
                                match screen.SelectedCardIndex with
                                | Some _ -> { screen with SelectedCardIndex = None; CapturePreview = NoCapture }
                                | None -> screen
                else
                    screen

            | DraggingTable(card, gdx, gdy) ->
                if input.Mouse.LeftPressed then
                    // reposition the card under the cursor, kept inside the table area
                    let (cx, cy) = clampScatterCenter screenW screenH (input.Mouse.Position.X - gdx) (input.Mouse.Position.Y - gdy)
                    let rot = match Map.tryFind card screen.ScatteredPositions with Some(_, _, r) -> r | None -> 0.0
                    { screen with ScatteredPositions = Map.add card (cx, cy, rot) screen.ScatteredPositions }
                else
                    { screen with DragState = NotDragging }

            | Dragging(idx, startPos, _) ->
                if input.Mouse.LeftPressed then
                    { screen with DragState = Dragging(idx, startPos, input.Mouse.Position) }
                else
                    let dx = abs (input.Mouse.Position.X - startPos.X)
                    let dy = abs (input.Mouse.Position.Y - startPos.Y)
                    if dx > dragThreshold || dy > dragThreshold then
                        let tArea = tableArea screenW screenH
                        if tArea.Contains input.Mouse.Position then
                            // Continue the slide from the drop point rather than
                            // snapping the card back to its hand slot first.
                            let newScreen = { screen with DragState = NotDragging }
                            let dropPos = (float input.Mouse.Position.X, float input.Mouse.Position.Y)
                            processHumanPlayFrom newScreen idx (Some dropPos) screenW screenH
                        else
                            { screen with DragState = NotDragging; SelectedCardIndex = Some idx }
                    else
                        { screen with DragState = NotDragging; SelectedCardIndex = Some idx }

        | ComputerThinking elapsed ->
            let newElapsed = elapsed + dt
            if newElapsed >= computerDelay then
                let player = gs.Players[gs.CurrentPlayerIndex]
                let style = GameEngine.computerStyle screen.Config gs.CurrentPlayerIndex
                let turnResult = GameEngine.playComputerTurnStyled style gs
                let collectAnim =
                    buildCollectAnimation turnResult.PlayResult false screenW screenH screen.ScatteredPositions gs.Table (List.length gs.Table)
                let toX, toY, newScattered =
                    match turnResult.PlayResult, screen.TableLayout with
                    | Place placedCard, RandomScatter ->
                        let newPos = computeScatteredPositions turnResult.NewState.Table screenW screenH screen.ScatteredPositions
                        match Map.tryFind placedCard newPos with
                        | Some(sx, sy, _) ->
                            (float (sx - CardRenderer.scaledWidth () / 2), float (sy - CardRenderer.scaledHeight () / 2), newPos)
                        | None ->
                            let tArea = tableArea screenW screenH
                            (float (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth () / 2),
                             float (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight () / 2), newPos)
                    | _ ->
                        let tArea = tableArea screenW screenH
                        (float (tArea.X + tArea.Width / 2 - CardRenderer.scaledWidth () / 2),
                         float (tArea.Y + tArea.Height / 2 - CardRenderer.scaledHeight () / 2), screen.ScatteredPositions)
                // Animate the card the AI actually played (from its real slot
                // in the hand), not Hand[0] — anything else leaks a hidden
                // card face-up and breaks the in-flight table suppression.
                let playedCard =
                    match turnResult.PlayResult with
                    | Capture(hc, _, _) | Place hc -> hc
                let cardAnim =
                    if not (List.isEmpty player.Hand) then
                        let oppHandSize = List.length player.Hand
                        let cardIdx = player.Hand |> List.tryFindIndex ((=) playedCard) |> Option.defaultValue 0
                        let fromRect = handCardRect screenW screenH oppHandSize cardIdx false
                        Some
                            { Card = playedCard
                              FromX = float fromRect.X
                              FromY = float fromRect.Y
                              ToX = toX
                              ToY = toY
                              Duration = cardSlideDuration
                              Highlight = false }
                    else None
                let msg = formatPlayResult player.Name turnResult.PlayResult
                let newChat =
                    if screen.Config.Settings.ChatEnabled then
                        let mood =
                            if screen.Rng.Next 3 = 0 then Chat.Idle
                            else
                                match turnResult.PlayResult with
                                | Capture(_, _, true) -> Chat.Sweep
                                | Capture _ -> Chat.Capture
                                | Place _ -> Chat.Place
                        let seed = gs.DealRound * 7 + gs.CurrentPlayerIndex * 3 + List.length player.Hand
                        Some(sprintf "%s: %s" player.Name (Chat.pick seed mood), 3.5)
                    else None
                { screen with
                    GameState = turnResult.NewState
                    LastPlayMessage = msg
                    ScatteredPositions = newScattered
                    Chat = newChat
                    Phase = AnimatingPlay(0.0, turnResult.Evaluation, cardAnim, collectAnim) }
            else
                { screen with Phase = ComputerThinking newElapsed }

        | AnimatingPlay(elapsed, eval, cardAnim, collectAnim) ->
            let newElapsed = elapsed + dt
            if newElapsed >= animDelay then
                advanceTurn screen
            else
                { screen with Phase = AnimatingPlay(newElapsed, eval, cardAnim, collectAnim) }

        | ChoosingCaptureOption(cardIdx, options, page) ->
            let modal = captureModal gs.Variant options page screenW screenH
            // Touch: check the visible capture option buttons
            let clickedOption =
                modal.OptionButtons |> List.tryFind (fun (_, b) -> Button.isClicked input b)
            match clickedOption with
            | Some (optIdx, _) ->
                processCapture screen cardIdx options[optIdx] screenW screenH
            | None ->
                if (match modal.MoreButton with Some b -> Button.isClicked input b | None -> false) then
                    { screen with Phase = ChoosingCaptureOption(cardIdx, options, modal.NextPage) }
                elif (match modal.PlaceButton with Some b -> Button.isClicked input b | None -> false) then
                    processHumanPlace screen cardIdx screenW screenH
                elif Button.isClicked input modal.CancelButton || input.Keyboard.IsEscapePressed then
                    { screen with Phase = WaitingForHuman; SelectedCardIndex = None }
                else
                    // Keyboard fallback: number keys pick from the visible page
                    match input.Keyboard.NumberPressed with
                    | Some n when n >= 1 && n <= modal.VisibleCount ->
                        processCapture screen cardIdx options[modal.PageStart + n - 1] screenW screenH
                    | _ -> screen

        | RoundOver ->
            let btn = continueButton screenW screenH
            if Button.isClicked input btn || input.Keyboard.IsEnterPressed then
                { screen with ContinueClicked = true }
            else
                screen

        | GameOver -> screen

    // ── Drawing ─────────────────────────────────────────────
    let private drawPlayerLabel (g: Gfx) (player: Player) (x: int) (y: int) (color: Color) =
        let text = sprintf "%s  Cards:%d  Sweeps:%d" player.Name (List.length player.CapturedCards) player.Sweeps
        Gfx.fillText g text (float x) (float y) color

    let draw (g: Gfx) (input: Input.InputState) (textures: CardRenderer.CardTextures) (screen: ScreenState) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let cw = CardRenderer.scaledWidth ()
        let ch = CardRenderer.scaledHeight ()

        // ── Table background ───────────────────────
        let tArea = tableArea screenW screenH
        Gfx.drawImage g textures.TableBg tArea.X tArea.Y tArea.Width tArea.Height

        // ── Table cards (with capture preview overlays) ──
        let tableCount = List.length gs.Table
        let greenOverlay = Color.rgba 0 70 0 90
        let yellowOverlay = Color.rgba 70 70 0 90
        let definiteSet, possibleSet =
            match screen.CapturePreview with
            | NoCapture -> Set.empty, Set.empty
            | SingleCapture cards -> Set.ofList cards, Set.empty
            | MultipleCaptures(definite, possible) -> Set.ofList definite, Set.ofList possible

        let animatingCard =
            match screen.Phase with
            | AnimatingPlay(elapsed, _, Some anim, _) when elapsed < anim.Duration -> Some anim.Card
            | _ -> None

        for i in 0 .. tableCount - 1 do
            let card = gs.Table[i]
            if animatingCard = Some card then () else
            match screen.TableLayout with
            | StrictGrid ->
                let r = tableCardRect screenW screenH tableCount i
                if Set.contains card definiteSet then
                    CardRenderer.drawCardWithOverlay g textures card r.X r.Y greenOverlay
                elif Set.contains card possibleSet then
                    CardRenderer.drawCardWithOverlay g textures card r.X r.Y yellowOverlay
                else
                    CardRenderer.drawCard g textures card r.X r.Y
            | RandomScatter ->
                match Map.tryFind card screen.ScatteredPositions with
                | Some(sx, sy, rot) ->
                    let drawX = sx - cw / 2
                    let drawY = sy - ch / 2
                    if Set.contains card definiteSet then
                        CardRenderer.drawCardWithOverlayRotated g textures card drawX drawY greenOverlay rot
                    elif Set.contains card possibleSet then
                        CardRenderer.drawCardWithOverlayRotated g textures card drawX drawY yellowOverlay rot
                    else
                        CardRenderer.drawCardRotated g textures card drawX drawY rot
                | None ->
                    let r = tableCardRect screenW screenH tableCount i
                    CardRenderer.drawCard g textures card r.X r.Y

        // ── Opponent hands (top, face-down) ────────
        let bottomIdx = if screen.Config.HumanCount > 0 then 0 else gs.CurrentPlayerIndex
        let opponents =
            gs.Players
            |> List.mapi (fun i p -> (i, p))
            |> List.filter (fun (i, _) -> i <> bottomIdx)

        match opponents with
        | (_, opp) :: _ ->
            let oppHandSize = List.length opp.Hand
            for i in 0 .. oppHandSize - 1 do
                let r = handCardRect screenW screenH oppHandSize i false
                if opp.Type = Computer then CardRenderer.drawCardBack g textures r.X r.Y
                else CardRenderer.drawCard g textures opp.Hand[i] r.X r.Y
            drawPlayerLabel g opp 20 (ch + 30) Color.LightSalmon
        | _ -> ()

        if opponents.Length >= 2 then
            let (_, opp2) = opponents[1]
            let sideY = screenH / 2 - (List.length opp2.Hand * (ch + 4)) / 2
            for i in 0 .. List.length opp2.Hand - 1 do
                CardRenderer.drawCardBack g textures 10 (sideY + i * (ch / 3))
            Gfx.fillText g opp2.Name 10.0 (float (sideY - 20)) Color.LightBlue

        if opponents.Length >= 3 then
            let (_, opp3) = opponents[2]
            let sideY = screenH / 2 - (List.length opp3.Hand * (ch + 4)) / 2
            let sideX = screenW - cw - 10
            for i in 0 .. List.length opp3.Hand - 1 do
                CardRenderer.drawCardBack g textures sideX (sideY + i * (ch / 3))
            Gfx.fillText g opp3.Name (float sideX) (float (sideY - 20)) Color.Plum

        // ── Human hand (bottom, face-up) ───────────
        if screen.Config.HumanCount > 0 then
            let humanIdx = 0
            let human = gs.Players[humanIdx]
            let handSize = List.length human.Hand
            let isDraggingIdx = match screen.DragState with Dragging(idx, _, _) -> Some idx | _ -> None
            for i in 0 .. handSize - 1 do
                if isDraggingIdx = Some i then () else
                let r = handCardRect screenW screenH handSize i true
                let isSelected = screen.SelectedCardIndex = Some i
                let isHovered = screen.HoveredCardIndex = Some i
                let yOffset = if isSelected then -15 elif isHovered then -10 else 0
                if isSelected then
                    CardRenderer.drawCardHighlighted g textures human.Hand[i] r.X (r.Y + yOffset) Color.LimeGreen
                elif isHovered then
                    CardRenderer.drawCardHighlighted g textures human.Hand[i] r.X (r.Y + yOffset) Color.Yellow
                else
                    CardRenderer.drawCard g textures human.Hand[i] r.X (r.Y + yOffset)

            match screen.DragState with
            | Dragging(idx, startPos, curPos) when idx < handSize ->
                let dx = abs (curPos.X - startPos.X)
                let dy = abs (curPos.Y - startPos.Y)
                if dx > dragThreshold || dy > dragThreshold then
                    let drawX = curPos.X - cw / 2
                    let drawY = curPos.Y - ch / 2
                    CardRenderer.drawCardHighlighted g textures human.Hand[idx] drawX drawY Color.LimeGreen
            | _ -> ()

            // Play button when a card is selected (and not dragging), plus
            // "Place Instead" when declining the capture is legal (Standard
            // Kasino, card can capture).
            match screen.Phase, screen.SelectedCardIndex, screen.DragState with
            | WaitingForHuman, Some _, NotDragging ->
                Button.draw g input (playButton screenW screenH screen.CapturePreview)
                let canPlaceInstead =
                    gs.Variant = StandardKasino
                    && (match screen.CapturePreview with NoCapture -> false | _ -> true)
                if canPlaceInstead then
                    Button.draw g input (placeInsteadButton screenW screenH)
            | _ -> ()
        else
            let curIdx = gs.CurrentPlayerIndex
            let curPlayer = gs.Players[curIdx]
            let handSize = List.length curPlayer.Hand
            for i in 0 .. handSize - 1 do
                let r = handCardRect screenW screenH handSize i true
                CardRenderer.drawCard g textures curPlayer.Hand[i] r.X r.Y

        // ── Bottom player label ─────────
        let bottomPlayer = gs.Players[bottomIdx]
        drawPlayerLabel g bottomPlayer 20 (screenH - 18) Color.LightGreen

        // ── Status bar ─────────────────────────────
        let statusY = screenH / 2 + tArea.Height / 2 + 10
        Gfx.fillText g screen.LastPlayMessage 20.0 (float statusY) Color.White

        let turnText =
            match screen.Phase with
            | WaitingForHuman when screen.SelectedCardIndex.IsSome ->
                match screen.DragState with
                | Dragging _ -> "Drag to table or release to cancel"
                | DraggingTable _ -> "Repositioning card…"
                | NotDragging ->
                    match screen.CapturePreview with
                    | NoCapture -> "Place on table. [Enter] play  [Esc] cancel"
                    | SingleCapture cards -> sprintf "Capture %d cards. [Enter] play  [Esc] cancel" cards.Length
                    | MultipleCaptures _ -> "Multiple options. [Enter] choose  [Esc] cancel"
            | WaitingForHuman -> "Select a card ([1]-[4] or click)  [Esc] menu"
            | ComputerThinking _ -> sprintf "%s thinking..." gs.Players[gs.CurrentPlayerIndex].Name
            | ChoosingCaptureOption _ -> ""
            | AnimatingPlay _ -> ""
            | Shuffling _ -> "Shuffling..."
            | Dealing _ -> "Dealing..."
            | RoundOver -> "Round over! [Enter] continue"
            | GameOver -> "Game over!"
        Gfx.fillText g turnText 20.0 (float (statusY + 20)) Color.Gold

        // ── Table-talk bubble (optional AI chat) ────────
        match screen.Chat with
        | Some(text, _) ->
            let size = Gfx.measure g text
            let pad = 10
            let bw = int size.X + pad * 2
            let bh = int size.Y + pad
            let bx = (screenW - bw) / 2
            let by = statusY - bh - 12
            Gfx.fillRect g { X = bx; Y = by; Width = bw; Height = bh } (Color.rgba 20 20 30 220)
            Gfx.fillText g text (float (bx + pad)) (float (by + pad / 2)) Color.Yellow
        | None -> ()

        // ── Scoreboard (top-right) ──
        let scoreX = screenW - 200
        let scoreStartY = ch + 50
        Gfx.fillText g "Scores:" (float scoreX) (float scoreStartY) Color.Gold
        gs.Players
        |> List.iteri (fun i p ->
            let cumScore = screen.CumulativeScores |> Map.tryFind p.Name |> Option.defaultValue 0
            Gfx.fillText g (sprintf "%s: %d" p.Name cumScore) (float scoreX) (float (scoreStartY + 24 + i * 22)) Color.White)

        let infoY = scoreStartY + 24 + gs.Players.Length * 22 + 8
        let deckCount = List.length gs.Deck
        Gfx.fillText g (sprintf "R%d Deal %d/%d" screen.RoundNumber gs.DealRound gs.TotalDeals) (float scoreX) (float infoY) Color.LightGray
        let deckIconW = cw / 2
        let deckIconH = ch / 2
        let deckIconX = scoreX
        let deckIconY = infoY + 26
        Gfx.drawImage g textures.Back deckIconX deckIconY deckIconW deckIconH
        Gfx.fillText g (string deckCount) (float (deckIconX + deckIconW + 6)) (float (deckIconY + 4)) Color.LightGray

        // ── Help / layout / menu buttons (hidden during modal) ──
        match screen.Phase with
        | ChoosingCaptureOption _ -> ()
        | _ ->
            Button.draw g input (helpButton screenW)
            Button.draw g input (layoutToggleButton screenW screen.TableLayout)
            Button.draw g input (menuButton screenW)

        // ── Shuffle animation (riffle) ──
        match screen.Phase with
        | Shuffling elapsed ->
            let t = elapsed / shuffleDuration
            let centerX = screenW / 2
            let centerY = screenH / 2
            let numCards = 6
            let halfN = numCards / 2
            let tex = textures.Back
            let separation = 100.0
            for i in 0 .. numCards - 1 do
                let isLeft = i % 2 = 0
                let stackIdx = i / 2
                let sepAmount, interleaveY =
                    if t < 0.4 then
                        let p = t / 0.4
                        let eased = 1.0 - (1.0 - p) * (1.0 - p)
                        (separation * eased, 0.0)
                    else
                        let p = (t - 0.4) / 0.6
                        let eased = 1.0 - (1.0 - p) * (1.0 - p)
                        let sep = separation * (1.0 - eased)
                        let vertShift = float (stackIdx - halfN / 2) * 3.0 * eased
                        (sep, vertShift)
                let xOff = if isLeft then -sepAmount else sepAmount
                let yStack = float (stackIdx - halfN / 2) * 2.0
                Gfx.drawImageRotated g tex (float centerX + xOff) (float centerY + yStack + interleaveY) cw ch 0.0
        | _ -> ()

        // ── Deal animation ──
        match screen.Phase with
        | Dealing(step, elapsed, steps) when step < List.length steps ->
            let currentStep = steps[step]
            let deckX = float (tArea.X + tArea.Width / 2)
            let deckY = float (tArea.Y + tArea.Height / 2)
            let t = elapsed / dealStepDuration
            let eased = 1.0 - (1.0 - t) * (1.0 - t)
            let tex = textures.Back
            for ci in 0 .. currentStep.CardCount - 1 do
                let spread = float (ci - currentStep.CardCount / 2) * 12.0
                let destX = currentStep.ToX + spread
                let destY = currentStep.ToY
                let x = deckX + (destX - deckX) * eased
                let y = deckY + (destY - deckY) * eased
                Gfx.drawImageRotated g tex x y cw ch 0.0
            let remainingSteps = List.length steps - step
            let stackCards = min 3 remainingSteps
            for si in 0 .. stackCards - 1 do
                let offset = float si * -2.0
                Gfx.drawImageRotated g tex deckX (deckY + offset) cw ch 0.0
        | _ -> ()

        // ── Card movement animation ──
        match screen.Phase with
        | AnimatingPlay(elapsed, _, Some anim, _) when elapsed < anim.Duration ->
            let t = elapsed / anim.Duration
            let eased = 1.0 - (1.0 - t) * (1.0 - t)
            let x = int (anim.FromX + (anim.ToX - anim.FromX) * eased)
            let y = int (anim.FromY + (anim.ToY - anim.FromY) * eased)
            // Human plays slide highlighted so the player's own move is
            // unmistakable amid the computer's reply animation.
            if anim.Highlight then
                CardRenderer.drawCardHighlighted g textures anim.Card x y Color.LimeGreen
            else
                CardRenderer.drawCard g textures anim.Card x y
        | _ -> ()

        // ── Collect animation ──
        match screen.Phase with
        | AnimatingPlay(elapsed, _, _, Some collect) when elapsed >= collect.StartTime && elapsed < collect.StartTime + collect.Duration ->
            let t = (elapsed - collect.StartTime) / collect.Duration
            let eased = 1.0 - (1.0 - t) * (1.0 - t)
            for (card, fx, fy) in collect.Cards do
                let x = int (fx + (collect.ToX - fx) * eased)
                let y = int (fy + (collect.ToY - fy) * eased)
                CardRenderer.drawCard g textures card (x - cw / 2) (y - ch / 2)
        | _ -> ()

        // ── Modal overlays ──
        match screen.Phase with
        | ChoosingCaptureOption(_, options, page) ->
            Gfx.fillRect g { X = 0; Y = 0; Width = screenW; Height = screenH } (Color.rgba 0 0 0 160)

            let modal = captureModal gs.Variant options page screenW screenH

            let headerText = "Choose which cards to capture:"
            let headerSize = Gfx.measure g headerText
            let headerY =
                match modal.OptionButtons with
                | (_, btn) :: _ -> float btn.Rect.Y - headerSize.Y - 12.0
                | [] -> float (screenH / 2 - 80)
            Gfx.fillText g headerText (float screenW / 2.0 - headerSize.X / 2.0) headerY Color.Gold

            Button.drawAll g input (modal.OptionButtons |> List.map snd)
            modal.MoreButton |> Option.iter (Button.draw g input)
            modal.PlaceButton |> Option.iter (Button.draw g input)
            Button.draw g input modal.CancelButton

        | RoundOver -> Button.draw g input (continueButton screenW screenH)

        | _ -> ()
