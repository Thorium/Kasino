namespace Kasino.Mibo

open System
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish.Graphics2D
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Main gameplay screen: table, hands, card selection, turns, animations, and
// the capture-choice modal.
//
// All the *logic* (Phase machine, turn resolution, drag & drop, capture
// preview) is identical to the MonoGame build — it only depends on the domain,
// the Input.InputState record, and the layout helpers here. Only `draw` was
// rewritten to emit Draw.* commands into a render buffer with explicit layers.
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
          ToX: float32; ToY: float32
          IsFaceUp: bool }

    and CardAnimation =
        { Card: Card
          FromX: float32; FromY: float32
          ToX: float32; ToY: float32
          Duration: float
          Highlight: bool }

    and CollectAnimation =
        { Cards: (Card * float32 * float32) list
          ToX: float32; ToY: float32
          StartTime: float
          Duration: float }

    type CapturePreview =
        | NoCapture
        | SingleCapture of definite: Card list
        | MultipleCaptures of definite: Card list * possible: Card list

    type DragState =
        | NotDragging
        | Dragging of cardIndex: int * startPos: Point * currentPos: Point
        | DraggingTable of card: Card * grabOffsetX: int * grabOffsetY: int

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
          CardRects: (int * Rectangle) list
          TableCardRects: Rectangle list
          CapturePreview: CapturePreview
          ContinueClicked: bool
          ShowRulesClicked: bool
          MenuClicked: bool
          DragState: DragState
          TableLayout: TableLayout
          ScatteredPositions: Map<Card, (int * int * float32)>
          Chat: (string * float) option }

    let private computerDelay = 0.8
    let private animDelay = 1.4
    let private shuffleDuration = 0.6
    let private cardSlideDuration = 0.4
    let private collectSlideDuration = 0.35
    let private dealStepDuration = 0.18

    let private dragThreshold = 8

    let private formatPlayResult (playerName: string) (result: PlayResult) =
        match result with
        | Capture(hc, captured, sweep) ->
            let capturedStr = captured |> List.map Cards.display |> String.concat " "
            let sweepStr = if sweep then " SWEEP!" else ""
            $"{playerName} plays {Cards.display hc} -> captures {capturedStr}{sweepStr}"
        | Place hc ->
            $"{playerName} places {Cards.display hc} on table"

    let private cardGap = 8
    let private tableCardGap = 6

    // ── Layout helpers ──
    let private centerCards (screenW: int) (count: int) =
        let totalW = count * (CardRenderer.scaledWidth() + cardGap) - cardGap
        (screenW - totalW) / 2

    let private handCardRect (screenW: int) (screenH: int) (handSize: int) (idx: int) (isBottom: bool) =
        let x = centerCards screenW handSize + idx * (CardRenderer.scaledWidth() + cardGap)
        let y = if isBottom then screenH - CardRenderer.scaledHeight() - 20 else 20
        Rectangle(x, y, CardRenderer.scaledWidth(), CardRenderer.scaledHeight())

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

    let private tableArea (screenW: int) (screenH: int) =
        let ch = CardRenderer.scaledHeight()
        let cw = CardRenderer.scaledWidth()
        let sideMargin = cw + 30
        Rectangle(sideMargin, ch + 60, screenW - 2 * sideMargin, screenH - 2 * ch - 140)

    let computeScatteredPositions (table: Card list) (screenW: int) (screenH: int) (existing: Map<Card, (int * int * float32)>) =
        let area = tableArea screenW screenH
        let cw = CardRenderer.scaledWidth()
        let ch = CardRenderer.scaledHeight()
        let centerX = area.X + area.Width / 2
        let centerY = area.Y + area.Height / 2
        let maxRadiusX = float32 (area.Width / 2 - cw / 2 - 10)
        let maxRadiusY = float32 (area.Height / 2 - ch / 2 - 10)

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
            let spreadFraction = float32 orderIdx / float32 (max 1 (List.length table - 1))
            let radiusFrac = 0.15f + 0.85f * spreadFraction

            let mutable attempts = 0
            let mutable placed = false
            let mutable bestX = centerX
            let mutable bestY = centerY
            let mutable bestRot = 0.0f

            while not placed && attempts < 60 do
                let angle = float32 (rng.NextDouble()) * MathF.PI * 2.0f
                let jitter = 0.7f + float32 (rng.NextDouble()) * 0.6f
                let rFrac = radiusFrac * jitter |> min 1.0f
                let rx = int (float32 centerX + cos angle * maxRadiusX * rFrac)
                let ry = int (float32 centerY + sin angle * maxRadiusY * rFrac)
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

    let private tableCardAtScatter (screen: ScreenState) (pos: Point) =
        let cw = CardRenderer.scaledWidth()
        let ch = CardRenderer.scaledHeight()
        screen.GameState.Table
        |> List.rev
        |> List.tryPick (fun card ->
            match Map.tryFind card screen.ScatteredPositions with
            | Some (sx, sy, _) ->
                if Rectangle(sx - cw / 2, sy - ch / 2, cw, ch).Contains(pos) then Some card else None
            | None -> None)

    let private clampScatterCenter (screenW: int) (screenH: int) (cx: int) (cy: int) =
        let area = tableArea screenW screenH
        let cw = CardRenderer.scaledWidth()
        let ch = CardRenderer.scaledHeight()
        let x = max (area.X + cw / 2) (min (area.X + area.Width - cw / 2) cx)
        let y = max (area.Y + ch / 2) (min (area.Y + area.Height - ch / 2) cy)
        (x, y)

    let private buildDealSteps (gs: GameEngine.GameState) (isFirstDeal: bool) (screenW: int) (screenH: int) =
        let tArea = tableArea screenW screenH
        let tableCenterX = float32 (tArea.X + tArea.Width / 2)
        let tableCenterY = float32 (tArea.Y + tArea.Height / 2)
        let playerCount = gs.Players.Length
        let bottomIdx = if gs.Players |> List.exists (fun p -> p.Type = Human) then 0 else gs.CurrentPlayerIndex

        let playerDest (idx: int) =
            if idx = bottomIdx then
                let handY = float32 (screenH - CardRenderer.scaledHeight() - 20 + CardRenderer.scaledHeight() / 2)
                (float32 (screenW / 2), handY)
            else
                let handY = float32 (20 + CardRenderer.scaledHeight() / 2)
                (float32 (screenW / 2), handY)

        let tableStep = { TargetLabel = "table"; CardCount = 4; ToX = tableCenterX; ToY = tableCenterY; IsFaceUp = false }

        let playerSteps =
            [for _ in 1 .. 2 do
                for pIdx in 0 .. playerCount - 1 do
                    let (px, py) = playerDest pIdx
                    { TargetLabel = gs.Players[pIdx].Name; CardCount = 2; ToX = px; ToY = py; IsFaceUp = (pIdx = bottomIdx) }]

        if isFirstDeal then tableStep :: playerSteps
        else playerSteps

    let private buildCollectAnimation
            (playResult: PlayResult)
            (isBottom: bool)
            (screenW: int) (screenH: int)
            (scatteredPos: Map<Card, (int * int * float32)>)
            (tableCards: Card list) (tableCount: int) =
        match playResult with
        | Capture(hc, captured, _) when not (List.isEmpty captured) ->
            let destX, destY =
                if isBottom then float32 (screenW / 2), float32 (screenH - 10)
                else float32 (screenW / 2), 0.0f
            let cards =
                captured |> List.map (fun card ->
                    match Map.tryFind card scatteredPos with
                    | Some(x, y, _) -> (card, float32 x, float32 y)
                    | None ->
                        let idx = tableCards |> List.tryFindIndex ((=) card) |> Option.defaultValue 0
                        let r = tableCardRect screenW screenH tableCount idx
                        (card, float32 (r.X + r.Width / 2), float32 (r.Y + r.Height / 2)))
            let tArea = tableArea screenW screenH
            let playedFrom =
                (hc, float32 (tArea.X + tArea.Width / 2), float32 (tArea.Y + tArea.Height / 2))
            Some { Cards = cards @ [ playedFrom ]; ToX = destX; ToY = destY
                   StartTime = cardSlideDuration; Duration = collectSlideDuration }
        | _ -> None

    // ── Button helpers ──
    let private playButton (screenW: int) (screenH: int) (preview: CapturePreview) =
        let label, color =
            match preview with
            | NoCapture -> "Place on Table", Color(100, 100, 100)
            | SingleCapture cards -> $"Capture {cards.Length} Cards", Color(40, 140, 40)
            | MultipleCaptures _ -> "Play (Choose Capture)", Color(160, 160, 40)
        Button.createCentered label screenW (screenH - CardRenderer.scaledHeight() - 80) 240 52 color Color.White

    let private helpButton (_screenW: int) =
        Button.create "?" 20 20 120 48 (Color(80, 80, 40)) Color.White

    let private layoutToggleButton (_screenW: int) (layout: TableLayout) =
        let label = match layout with StrictGrid -> "Scatter" | RandomScatter -> "Grid"
        Button.create label 160 20 120 48 (Color(60, 80, 60)) Color.White

    let private menuButton (_screenW: int) =
        Button.create "Menu" 300 20 120 48 (Color(120, 40, 40)) Color.White

    let private continueButton (screenW: int) (screenH: int) =
        Button.createCentered "Continue" screenW (screenH / 2 + 60) 200 52 (Color(40, 80, 140)) Color.White

    let private placeInsteadButton (screenW: int) (screenH: int) =
        Button.create "Place Instead" (screenW / 2 + 130) (screenH - CardRenderer.scaledHeight() - 80) 170 52 (Color(100, 100, 100)) Color.White

    type private CaptureModal =
        { OptionButtons: (int * Button.ButtonDef) list
          MoreButton: Button.ButtonDef option
          PlaceButton: Button.ButtonDef option
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
            visible |> List.mapi (fun i opt ->
                let label =
                    let cards = opt.Captured |> List.map Cards.display |> String.concat " "
                    $"{i + 1}: {cards} ({opt.Captured.Length} cards)"
                (startIdx + i, Button.createCentered label screenW (baseY + i * 56) 450 48 (Color(60, 80, 60)) Color.White))
        let navY = baseY + visible.Length * 56
        let moreButton =
            if pageCount > 1 then
                Some (Button.createCentered $"More options ({page + 1}/{pageCount})" screenW navY 450 48 (Color(60, 60, 100)) Color.White)
            else None
        let placeY = navY + navRows * 56
        let placeButton =
            if allowPlace then
                Some (Button.createCentered "Place on table instead" screenW placeY 450 48 (Color(100, 100, 100)) Color.White)
            else None
        let cancelBtn =
            Button.createCentered "Cancel" screenW (placeY + placeRows * 56 + 8) 180 48 (Color(120, 40, 40)) Color.White
        { OptionButtons = optButtons
          MoreButton = moreButton
          PlaceButton = placeButton
          CancelButton = cancelBtn
          PageStart = startIdx
          VisibleCount = visible.Length
          NextPage = (page + 1) % pageCount }

    // ── Initialization ──
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
          TableLayout = (if config.Settings.DefaultScatter then RandomScatter else StrictGrid)
          ScatteredPositions = Map.empty
          Chat = None }

    // ── Update logic ──
    [<TailCall>]
    let rec private advanceTurn (screen: ScreenState) =
        let gs = screen.GameState
        if GameEngine.allHandsEmpty gs then
            if gs.DealRound < gs.TotalDeals then
                let nextDeal = gs.DealRound + 1
                let newGs = GameEngine.dealRound gs false
                { screen with
                    GameState = { newGs with DealRound = nextDeal }
                    LastPlayMessage = $"Round {screen.RoundNumber} - Deal {nextDeal}"
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
                | Human ->
                    { screen with Phase = WaitingForHuman; SelectedCardIndex = None; HoveredCardIndex = None }
                | Computer ->
                    { screen with Phase = ComputerThinking 0.0 }

    let private finishHumanPlay (screen: ScreenState) (cardIndex: int) (fromPos: (float32 * float32) option) (turnResult: GameEngine.TurnResult) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let player = gs.Players[gs.CurrentPlayerIndex]
        let card = player.Hand[cardIndex]
        let fromRect =
            match fromPos with
            | Some(px, py) ->
                Rectangle(int px - CardRenderer.scaledWidth() / 2,
                          int py - CardRenderer.scaledHeight() / 2,
                          CardRenderer.scaledWidth(), CardRenderer.scaledHeight())
            | None -> handCardRect screenW screenH player.Hand.Length cardIndex true
        let collectAnim =
            buildCollectAnimation turnResult.PlayResult true screenW screenH
                screen.ScatteredPositions gs.Table (List.length gs.Table)
        let toX, toY, newScattered =
            match turnResult.PlayResult, screen.TableLayout with
            | Place _, RandomScatter ->
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
              Duration = cardSlideDuration
              Highlight = true }
        let msg = formatPlayResult player.Name turnResult.PlayResult
        { screen with
            GameState = turnResult.NewState
            LastPlayMessage = msg
            SelectedCardIndex = None
            ScatteredPositions = newScattered
            Phase = AnimatingPlay(0.0, turnResult.Evaluation, Some cardAnim, collectAnim) }

    let private processHumanPlayFrom (screen: ScreenState) (cardIndex: int) (fromPos: (float32 * float32) option) (screenW: int) (screenH: int) =
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

    let private processHumanPlace (screen: ScreenState) (cardIndex: int) (screenW: int) (screenH: int) =
        finishHumanPlay screen cardIndex None (GameEngine.playHumanPlaceTurn screen.GameState cardIndex) screenW screenH

    let update (input: Input.InputState) (dt: float) (screenW: int) (screenH: int) (screen: ScreenState) =
        let gs = screen.GameState

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

        let inCaptureModal =
            match screen.Phase with ChoosingCaptureOption _ -> true | _ -> false
        let escapeToMenu =
            match screen.Phase with
            | WaitingForHuman | ChoosingCaptureOption _ -> false
            | _ -> Input.has Input.Back input

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
            let player = gs.Players[gs.CurrentPlayerIndex]
            let rects =
                player.Hand
                |> List.mapi (fun i _ ->
                    let r = handCardRect screenW screenH player.Hand.Length i true
                    let lift =
                        if screen.SelectedCardIndex = Some i then 15
                        elif screen.HoveredCardIndex = Some i then 10
                        else 0
                    (i, Rectangle(r.X, r.Y - lift, r.Width, r.Height + lift)))

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
                if Input.has Input.Back input then
                    if screen.SelectedCardIndex.IsSome then
                        { screen with SelectedCardIndex = None; CapturePreview = NoCapture }
                    else
                        { screen with MenuClicked = true }
                elif Input.has Input.Continue input then
                    match screen.SelectedCardIndex with
                    | Some idx -> processHumanPlay screen idx screenW screenH
                    | None -> screen
                else
                match Input.picked input with
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
                                | Some (sx, sy, _) ->
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
                    let (cx, cy) = clampScatterCenter screenW screenH (input.Mouse.Position.X - gdx) (input.Mouse.Position.Y - gdy)
                    let rot = match Map.tryFind card screen.ScatteredPositions with Some (_, _, r) -> r | None -> 0.0f
                    { screen with ScatteredPositions = Map.add card (cx, cy, rot) screen.ScatteredPositions }
                else
                    { screen with DragState = NotDragging }

            | Dragging(idx, startPos, _) ->
                if input.Mouse.LeftPressed then
                    { screen with DragState = Dragging(idx, startPos, input.Mouse.Position) }
                else
                    let dx = abs(input.Mouse.Position.X - startPos.X)
                    let dy = abs(input.Mouse.Position.Y - startPos.Y)
                    if dx > dragThreshold || dy > dragThreshold then
                        let tArea = tableArea screenW screenH
                        if tArea.Contains(input.Mouse.Position) then
                            let newScreen = { screen with DragState = NotDragging }
                            let dropPos = (float32 input.Mouse.Position.X, float32 input.Mouse.Position.Y)
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
                    buildCollectAnimation turnResult.PlayResult false screenW screenH
                        screen.ScatteredPositions gs.Table (List.length gs.Table)
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
                let playedCard =
                    match turnResult.PlayResult with
                    | Capture(hc, _, _) | Place hc -> hc
                let cardAnim =
                    if not (List.isEmpty player.Hand) then
                        let oppHandSize = List.length player.Hand
                        let cardIdx = player.Hand |> List.tryFindIndex ((=) playedCard) |> Option.defaultValue 0
                        let fromRect = handCardRect screenW screenH oppHandSize cardIdx false
                        Some { Card = playedCard
                               FromX = float32 fromRect.X
                               FromY = float32 fromRect.Y
                               ToX = toX; ToY = toY
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
                        Some($"{player.Name}: {Chat.pick seed mood}", 3.5)
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
                elif Button.isClicked input modal.CancelButton || Input.has Input.Back input then
                    { screen with Phase = WaitingForHuman; SelectedCardIndex = None }
                else
                    match Input.picked input with
                    | Some n when n >= 1 && n <= modal.VisibleCount ->
                        processCapture screen cardIdx options[modal.PageStart + n - 1] screenW screenH
                    | _ -> screen

        | RoundOver ->
            let btn = continueButton screenW screenH
            if Button.isClicked input btn || Input.has Input.Continue input then
                { screen with ContinueClicked = true }
            else
                screen

        | GameOver -> screen

    // ── Drawing ──
    let private drawPlayerLabel buffer (font: SpriteFont) (player: Player) (x: int) (y: int) (color: Color) =
        let text = $"{player.Name}  Cards:{List.length player.CapturedCards}  Sweeps:{player.Sweeps}"
        Render.text buffer Render.LLabel font text (Vector2(float32 x, float32 y)) color

    let draw buffer (font: SpriteFont) (input: Input.InputState) (textures: CardRenderer.CardTextures) (screen: ScreenState) (screenW: int) (screenH: int) =
        let gs = screen.GameState
        let cw = CardRenderer.scaledWidth()
        let ch = CardRenderer.scaledHeight()

        // Table background
        let tArea = tableArea screenW screenH
        Render.sprite buffer Render.LTableBg textures.TableBg tArea

        // Table cards (with capture-preview overlays)
        let tableCount = List.length gs.Table
        let greenOverlay  = Color(0, 70, 0, 90)
        let yellowOverlay = Color(70, 70, 0, 90)
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
                    CardRenderer.drawCardWithOverlay buffer Render.LTableCard textures card r.X r.Y greenOverlay
                elif Set.contains card possibleSet then
                    CardRenderer.drawCardWithOverlay buffer Render.LTableCard textures card r.X r.Y yellowOverlay
                else
                    CardRenderer.drawCard buffer Render.LTableCard textures card r.X r.Y
            | RandomScatter ->
                match Map.tryFind card screen.ScatteredPositions with
                | Some (sx, sy, rot) ->
                    let drawX = sx - cw / 2
                    let drawY = sy - ch / 2
                    if Set.contains card definiteSet then
                        CardRenderer.drawCardWithOverlayRotated buffer Render.LTableCard textures card drawX drawY greenOverlay rot
                    elif Set.contains card possibleSet then
                        CardRenderer.drawCardWithOverlayRotated buffer Render.LTableCard textures card drawX drawY yellowOverlay rot
                    else
                        CardRenderer.drawCardRotated buffer Render.LTableCard textures card drawX drawY rot
                | None ->
                    let r = tableCardRect screenW screenH tableCount i
                    CardRenderer.drawCard buffer Render.LTableCard textures card r.X r.Y

        // Opponent hands (top, face-down)
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
                if opp.Type = Computer then
                    CardRenderer.drawCardBack buffer Render.LHandBack textures r.X r.Y
                else
                    CardRenderer.drawCard buffer Render.LHand textures opp.Hand[i] r.X r.Y
            drawPlayerLabel buffer font opp 20 (ch + 30) Color.LightSalmon
        | _ -> ()

        if opponents.Length >= 2 then
            let (_, opp2) = opponents[1]
            let sideY = screenH / 2 - (List.length opp2.Hand * (ch + 4)) / 2
            for i in 0 .. List.length opp2.Hand - 1 do
                CardRenderer.drawCardBack buffer Render.LHandBack textures 10 (sideY + i * (ch / 3))
            Render.text buffer Render.LLabel font opp2.Name (Vector2(10.0f, float32 (sideY - 20))) Color.LightBlue

        if opponents.Length >= 3 then
            let (_, opp3) = opponents[2]
            let sideY = screenH / 2 - (List.length opp3.Hand * (ch + 4)) / 2
            let sideX = screenW - cw - 10
            for i in 0 .. List.length opp3.Hand - 1 do
                CardRenderer.drawCardBack buffer Render.LHandBack textures sideX (sideY + i * (ch / 3))
            Render.text buffer Render.LLabel font opp3.Name (Vector2(float32 sideX, float32 (sideY - 20))) Color.Plum

        // Human hand (bottom, face-up)
        if screen.Config.HumanCount > 0 then
            let humanIdx = 0
            let human = gs.Players[humanIdx]
            let handSize = List.length human.Hand
            // Only lift the card to the cursor once the drag passes the
            // threshold; below it, keep drawing it in its slot (otherwise the
            // card vanishes the instant you press it).
            let isDraggingIdx =
                match screen.DragState with
                | Dragging(idx, startPos, curPos)
                    when abs(curPos.X - startPos.X) > dragThreshold
                      || abs(curPos.Y - startPos.Y) > dragThreshold -> Some idx
                | _ -> None
            for i in 0 .. handSize - 1 do
                if isDraggingIdx = Some i then () else
                let r = handCardRect screenW screenH handSize i true
                let isSelected = screen.SelectedCardIndex = Some i
                let isHovered = screen.HoveredCardIndex = Some i
                let yOffset = if isSelected then -15 elif isHovered then -10 else 0
                if isSelected then
                    CardRenderer.drawCardHighlighted buffer Render.LHand textures human.Hand[i] r.X (r.Y + yOffset) Color.LimeGreen
                elif isHovered then
                    CardRenderer.drawCardHighlighted buffer Render.LHand textures human.Hand[i] r.X (r.Y + yOffset) Color.Yellow
                else
                    CardRenderer.drawCard buffer Render.LHand textures human.Hand[i] r.X (r.Y + yOffset)

            match screen.DragState with
            | Dragging(idx, startPos, curPos) when idx < handSize ->
                let dx = abs(curPos.X - startPos.X)
                let dy = abs(curPos.Y - startPos.Y)
                if dx > dragThreshold || dy > dragThreshold then
                    let drawX = curPos.X - cw / 2
                    let drawY = curPos.Y - ch / 2
                    CardRenderer.drawCardHighlighted buffer Render.LHandTop textures human.Hand[idx] drawX drawY Color.LimeGreen
            | _ -> ()

            match screen.Phase, screen.SelectedCardIndex, screen.DragState with
            | WaitingForHuman, Some _, NotDragging ->
                Button.draw buffer font input (playButton screenW screenH screen.CapturePreview)
                let canPlaceInstead =
                    gs.Variant = StandardKasino
                    && (match screen.CapturePreview with NoCapture -> false | _ -> true)
                if canPlaceInstead then
                    Button.draw buffer font input (placeInsteadButton screenW screenH)
            | _ -> ()
        else
            let curIdx = gs.CurrentPlayerIndex
            let curPlayer = gs.Players[curIdx]
            let handSize = List.length curPlayer.Hand
            for i in 0 .. handSize - 1 do
                let r = handCardRect screenW screenH handSize i true
                CardRenderer.drawCard buffer Render.LHand textures curPlayer.Hand[i] r.X r.Y

        // Bottom player label
        let bottomPlayer = gs.Players[bottomIdx]
        drawPlayerLabel buffer font bottomPlayer 20 (screenH - 18) Color.LightGreen

        // Status bar
        let statusY = screenH / 2 + tArea.Height / 2 + 10
        Render.text buffer Render.LLabel font screen.LastPlayMessage (Vector2(20.0f, float32 statusY)) Color.White

        let turnText =
            match screen.Phase with
            | WaitingForHuman when screen.SelectedCardIndex.IsSome ->
                match screen.DragState with
                | Dragging _ -> "Drag to table or release to cancel"
                | DraggingTable _ -> "Repositioning card…"
                | NotDragging ->
                    match screen.CapturePreview with
                    | NoCapture -> "Place on table. [Enter] play  [Esc] cancel"
                    | SingleCapture cards -> $"Capture {cards.Length} cards. [Enter] play  [Esc] cancel"
                    | MultipleCaptures _ -> "Multiple options. [Enter] choose  [Esc] cancel"
            | WaitingForHuman -> "Select a card ([1]-[4] or click)  [Esc] menu"
            | ComputerThinking _ -> $"{gs.Players[gs.CurrentPlayerIndex].Name} thinking..."
            | ChoosingCaptureOption _ -> ""
            | AnimatingPlay _ -> ""
            | Shuffling _ -> "Shuffling..."
            | Dealing _ -> "Dealing..."
            | RoundOver -> "Round over! [Enter] continue"
            | GameOver -> "Game over!"
        Render.text buffer Render.LLabel font turnText (Vector2(20.0f, float32 (statusY + 20))) Color.Gold

        // Table-talk bubble (optional AI chat)
        match screen.Chat with
        | Some(text, _) ->
            let size = Render.measure font text
            let pad = 10
            let bw = int size.X + pad * 2
            let bh = int size.Y + pad
            let bx = (screenW - bw) / 2
            let by = statusY - bh - 12
            Render.fill buffer Render.LLabel (Color(20, 20, 30, 220)) (Rectangle(bx, by, bw, bh))
            Render.text buffer (Render.LLabel + 2<RenderLayer>) font text (Vector2(float32 (bx + pad), float32 (by + pad / 2))) Color.LightYellow
        | None -> ()

        // Scoreboard (top-right)
        let scoreX = screenW - 200
        let scoreStartY = ch + 50
        Render.text buffer Render.LLabel font "Scores:" (Vector2(float32 scoreX, float32 scoreStartY)) Color.Gold
        gs.Players |> List.iteri (fun i p ->
            let cumScore = screen.CumulativeScores |> Map.tryFind p.Name |> Option.defaultValue 0
            let text = $"{p.Name}: {cumScore}"
            Render.text buffer Render.LLabel font text (Vector2(float32 scoreX, float32 (scoreStartY + 24 + i * 22))) Color.White)

        // Round / deal info with deck indicator
        let infoY = scoreStartY + 24 + gs.Players.Length * 22 + 8
        let deckCount = List.length gs.Deck
        let infoText = $"R{screen.RoundNumber} Deal {gs.DealRound}/{gs.TotalDeals}"
        Render.text buffer Render.LLabel font infoText (Vector2(float32 scoreX, float32 infoY)) Color.LightGray
        let deckIconW = cw / 2
        let deckIconH = ch / 2
        let deckIconX = scoreX
        let deckIconY = infoY + 26
        Render.sprite buffer Render.LHand textures.Back (Rectangle(deckIconX, deckIconY, deckIconW, deckIconH))
        Render.text buffer Render.LLabel font $"{deckCount}" (Vector2(float32 (deckIconX + deckIconW + 6), float32 (deckIconY + 4))) Color.LightGray

        // "?" help button, layout toggle, and Menu button (hidden in modal)
        match screen.Phase with
        | ChoosingCaptureOption _ -> ()
        | _ ->
            Button.draw buffer font input (helpButton screenW)
            Button.draw buffer font input (layoutToggleButton screenW screen.TableLayout)
            Button.draw buffer font input (menuButton screenW)

        // Shuffle animation (riffle shuffle)
        match screen.Phase with
        | Shuffling elapsed ->
            let t = float32 (elapsed / shuffleDuration)
            let centerX = screenW / 2
            let centerY = screenH / 2
            let numCards = 6
            let halfN = numCards / 2
            let tex = textures.Back
            let separation = 100.0f
            for i in 0 .. numCards - 1 do
                let isLeft = i % 2 = 0
                let stackIdx = i / 2
                let sepAmount, interleaveY =
                    if t < 0.4f then
                        let p = t / 0.4f
                        let eased = 1.0f - (1.0f - p) * (1.0f - p)
                        (separation * eased, 0.0f)
                    else
                        let p = (t - 0.4f) / 0.6f
                        let eased = 1.0f - (1.0f - p) * (1.0f - p)
                        let sep = separation * (1.0f - eased)
                        let vertShift = float32 (stackIdx - halfN / 2) * 3.0f * eased
                        (sep, vertShift)
                let xOff = if isLeft then -sepAmount else sepAmount
                let yStack = float32 (stackIdx - halfN / 2) * 2.0f
                Render.spriteCentered buffer Render.LAnim tex
                    (centerX + int xOff) (centerY + int (yStack + interleaveY)) cw ch 0.0f
        | _ -> ()

        // Deal animation
        match screen.Phase with
        | Dealing(step, elapsed, steps) when step < List.length steps ->
            let currentStep = steps[step]
            let tArea = tableArea screenW screenH
            let deckX = float32 (tArea.X + tArea.Width / 2)
            let deckY = float32 (tArea.Y + tArea.Height / 2)
            let t = float32 (elapsed / dealStepDuration)
            let eased = 1.0f - (1.0f - t) * (1.0f - t)
            let tex = textures.Back
            for ci in 0 .. currentStep.CardCount - 1 do
                let spread = float32 (ci - currentStep.CardCount / 2) * 12.0f
                let destX = currentStep.ToX + spread
                let destY = currentStep.ToY
                let x = deckX + (destX - deckX) * eased
                let y = deckY + (destY - deckY) * eased
                Render.spriteCentered buffer Render.LAnim tex (int x) (int y) cw ch 0.0f
            let remainingSteps = List.length steps - step
            let stackCards = min 3 remainingSteps
            for si in 0 .. stackCards - 1 do
                let offset = float32 si * -2.0f
                Render.spriteCentered buffer Render.LAnim tex (int deckX) (int (deckY + offset)) cw ch 0.0f
        | _ -> ()

        // Card movement animation
        match screen.Phase with
        | AnimatingPlay(elapsed, _, Some anim, _) when elapsed < anim.Duration ->
            let t = float32 (elapsed / anim.Duration)
            let eased = 1.0f - (1.0f - t) * (1.0f - t)
            let x = int (anim.FromX + (anim.ToX - anim.FromX) * eased)
            let y = int (anim.FromY + (anim.ToY - anim.FromY) * eased)
            if anim.Highlight then
                CardRenderer.drawCardHighlighted buffer Render.LAnimTop textures anim.Card x y Color.LimeGreen
            else
                CardRenderer.drawCard buffer Render.LAnimTop textures anim.Card x y
        | _ -> ()

        // Collect animation
        match screen.Phase with
        | AnimatingPlay(elapsed, _, _, Some collect) when elapsed >= collect.StartTime && elapsed < collect.StartTime + collect.Duration ->
            let t = float32 ((elapsed - collect.StartTime) / collect.Duration)
            let eased = 1.0f - (1.0f - t) * (1.0f - t)
            for (card, fx, fy) in collect.Cards do
                let x = int (fx + (collect.ToX - fx) * eased)
                let y = int (fy + (collect.ToY - fy) * eased)
                CardRenderer.drawCard buffer Render.LAnimTop textures card (x - cw / 2) (y - ch / 2)
        | _ -> ()

        // Modal overlays
        match screen.Phase with
        | ChoosingCaptureOption(_, options, page) ->
            Render.fill buffer Render.LOverlayBg (Color(0, 0, 0, 160)) (Rectangle(0, 0, screenW, screenH))

            let modal = captureModal gs.Variant options page screenW screenH

            let headerText = "Choose which cards to capture:"
            let headerSize = Render.measure font headerText
            let headerY =
                match modal.OptionButtons with
                | (_, btn) :: _ -> float32 btn.Rect.Y - headerSize.Y - 12.0f
                | [] -> float32 (screenH / 2 - 80)
            Render.text buffer Render.LModalText font headerText
                (Vector2(float32 screenW / 2.0f - headerSize.X / 2.0f, headerY)) Color.Gold

            Button.drawAllAt buffer Render.LModal font input (modal.OptionButtons |> List.map snd)
            modal.MoreButton |> Option.iter (Button.drawAt buffer Render.LModal font input)
            modal.PlaceButton |> Option.iter (Button.drawAt buffer Render.LModal font input)
            Button.drawAt buffer Render.LModal font input modal.CancelButton

        | RoundOver ->
            Button.draw buffer font input (continueButton screenW screenH)

        | _ -> ()
