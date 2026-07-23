module Kasino.Tests.HeadlessUITests

open System
open System.Numerics
open Xunit
open Mibo.Elmish
open Mibo.Input
open Kasino.Domain
open Kasino.Mibo

// ─────────────────────────────────────────────────────────────
// UI-level tests through Mibo's headless runtime: the real Elmish program
// (screens, transitions, AI turns) runs with virtual time and injected
// input messages — no window, graphics device, or SDL.
// ─────────────────────────────────────────────────────────────

let private frame = TimeSpan.FromMilliseconds 16.0

let private newRunner () =
    new HeadlessRunner<Game.Model, Game.Msg>(Game.createHeadless ())

/// Press-and-release a semantic action, then run a frame so Tick consumes it.
let private press (runner: HeadlessRunner<Game.Model, Game.Msg>) (action: Input.UiAction) =
    runner.Dispatch(Game.ActionsChanged { ActionState.empty with Started = Set.singleton action })
    runner.Step frame
    runner.Step frame

/// Left-click at a screen position (1024x768 layout), then run a frame.
let private click (runner: HeadlessRunner<Game.Model, Game.Msg>) (x: float32) (y: float32) =
    let delta : MouseDelta =
        { Position = Vector2(x, y)
          PositionDelta = Vector2.Zero
          Buttons = { Pressed = [| MouseButtonCode.Left |]; Released = [||] }
          ScrollDelta = 0.0f
          ScrollDeltaV = Vector2.Zero }
    runner.Dispatch(Game.MouseEvent delta)
    runner.Step frame
    runner.Step frame

/// Drive the menu to a running Standard 2-player AI-vs-AI game.
let private startAiGame (runner: HeadlessRunner<Game.Model, Game.Msg>) =
    press runner (Input.Pick 1)   // Standard Kasino
    press runner (Input.Pick 2)   // 2 players
    press runner (Input.Pick 0)   // watch AI only

/// Drive the menu to a running Standard 2-player game with one human seat.
let private startHumanGame (runner: HeadlessRunner<Game.Model, Game.Msg>) =
    press runner (Input.Pick 1)   // Standard Kasino
    press runner (Input.Pick 2)   // 2 players
    press runner (Input.Pick 1)   // play yourself

let private playing (runner: HeadlessRunner<Game.Model, Game.Msg>) =
    match runner.Model.Screen with
    | Game.Playing s -> s
    | other -> failwith $"expected Playing, got %A{other}"

let private atWaitingForHuman (m: Game.Model) =
    match m.Screen with
    | Game.Playing s -> match s.Phase with GameScreen.WaitingForHuman -> true | _ -> false
    | _ -> false

let private atRoundOver (m: Game.Model) =
    match m.Screen with
    | Game.Playing s -> match s.Phase with GameScreen.RoundOver -> true | _ -> false
    | _ -> false

[<Fact>]
let ``menu keyboard flow starts a Standard 2-player AI game`` () =
    use runner = newRunner ()
    startAiGame runner
    match runner.Model.Screen with
    | Game.Playing s ->
        Assert.Equal(StandardKasino, s.Config.Variant)
        Assert.Equal(2, s.GameState.Players.Length)
        Assert.Equal(0, s.Config.HumanCount)
    | other -> failwith $"expected Playing, got %A{other}"

[<Fact>]
let ``menu keyboard flow starts a Laisto 3-player game with one human`` () =
    use runner = newRunner ()
    press runner (Input.Pick 2)   // Laistokasino
    press runner (Input.Pick 3)   // 3 players
    press runner (Input.Pick 1)   // play yourself
    match runner.Model.Screen with
    | Game.Playing s ->
        Assert.Equal(LaistoKasino, s.Config.Variant)
        Assert.Equal(3, s.GameState.Players.Length)
        Assert.Equal(1, s.Config.HumanCount)
    | other -> failwith $"expected Playing, got %A{other}"

[<Fact>]
let ``escape on the menu quits`` () =
    use runner = newRunner ()
    runner.Step frame
    press runner Input.Back
    Assert.True(runner.ShouldQuit)

[<Fact>]
let ``how-to-play button opens the rules overlay and escape returns to the menu`` () =
    use runner = newRunner ()
    runner.Step frame
    // "How to Play" is centered at (512, 714) on the 1024x768 menu.
    click runner 512.0f 714.0f
    match runner.Model.Screen with
    | Game.Rules _ -> ()
    | other -> failwith $"expected Rules, got %A{other}"
    press runner Input.Back
    match runner.Model.Screen with
    | Game.Menu _ -> ()
    | other -> failwith $"expected Menu, got %A{other}"
    Assert.False(runner.ShouldQuit)   // the same Escape must not also quit

[<Fact>]
let ``rules pages navigate with arrow-key actions`` () =
    use runner = newRunner ()
    runner.Step frame
    click runner 512.0f 714.0f
    let page () =
        match runner.Model.Screen with
        | Game.Rules (r, _) -> r.CurrentPage
        | other -> failwith $"expected Rules, got %A{other}"
    Assert.Equal(0, page ())
    press runner Input.NextPage
    Assert.Equal(1, page ())
    press runner Input.PrevPage
    Assert.Equal(0, page ())
    press runner Input.PrevPage   // clamped at the first page
    Assert.Equal(0, page ())

[<Fact>]
let ``options button opens options and escape returns without quitting`` () =
    use runner = newRunner ()
    runner.Step frame
    // "Options" is centered at (512, 648) on the 1024x768 menu.
    click runner 512.0f 648.0f
    match runner.Model.Screen with
    | Game.Options _ -> ()
    | other -> failwith $"expected Options, got %A{other}"
    press runner Input.Back
    match runner.Model.Screen with
    | Game.Menu _ -> ()
    | other -> failwith $"expected Menu, got %A{other}"
    Assert.False(runner.ShouldQuit)

[<Fact>]
let ``an AI-only round plays to completion and continue shows the scores`` () =
    use runner = newRunner ()
    startAiGame runner
    // AI turns, deal waves, and animations are all dt-driven; a large virtual
    // step per frame keeps the frame count low without changing behavior.
    let step = TimeSpan.FromMilliseconds 50.0
    let reachedRoundOver =
        runner.StepUntil(
            (fun m ->
                match m.Screen with
                | Game.Playing s -> match s.Phase with GameScreen.RoundOver -> true | _ -> false
                | _ -> false),
            step,
            maxFrames = 20000)
    Assert.True(reachedRoundOver, "the AI round did not reach RoundOver")
    // All 52 cards must be accounted for at the end of the round.
    match runner.Model.Screen with
    | Game.Playing s ->
        let gs = s.GameState
        for p in gs.Players do Assert.Empty p.Hand
        Assert.Empty gs.Deck
        let captured = gs.Players |> List.sumBy (fun p -> p.CapturedCards.Length)
        Assert.Equal(52, captured + gs.Table.Length)
    | other -> failwith $"expected Playing, got %A{other}"
    press runner Input.Continue
    match runner.Model.Screen with
    | Game.Scores s -> Assert.Equal(2, s.Scores.Length)
    | other -> failwith $"expected Scores, got %A{other}"

[<Fact>]
let ``continue on the round summary starts the next round with carried scores`` () =
    use runner = newRunner ()
    startAiGame runner
    let step = TimeSpan.FromMilliseconds 50.0
    Assert.True(runner.StepUntil(atRoundOver, step, maxFrames = 20000), "round 1 did not finish")
    press runner Input.Continue   // RoundOver -> score summary
    press runner Input.Continue   // summary -> round 2 (unless the game already ended)
    match runner.Model.Screen with
    | Game.Playing s ->
        Assert.Equal(2, s.RoundNumber)
        Assert.Equal(2, s.CumulativeScores.Count)
    | Game.Menu _ -> ()   // a 16-point round-1 blowout legitimately ends the game
    | other -> failwith $"expected Playing or Menu, got %A{other}"

[<Fact>]
let ``a full AI game reaches game over and returns to the menu`` () =
    use runner = newRunner ()
    startAiGame runner
    let step = TimeSpan.FromMilliseconds 50.0
    let mutable gameOver = false
    let mutable rounds = 0
    while not gameOver && rounds < 25 do
        rounds <- rounds + 1
        Assert.True(runner.StepUntil(atRoundOver, step, maxFrames = 20000), $"round {rounds} did not finish")
        press runner Input.Continue   // RoundOver -> score screen
        match runner.Model.Screen with
        | Game.Scores s ->
            if s.Phase = ScoreScreen.GameOver then gameOver <- true
            else press runner Input.Continue   // next round
        | other -> failwith $"expected Scores, got %A{other}"
    Assert.True(gameOver, "no game over within 25 rounds")
    press runner Input.Continue   // "Back to Menu"
    match runner.Model.Screen with
    | Game.Menu _ -> ()
    | other -> failwith $"expected Menu, got %A{other}"

[<Fact>]
let ``human can select a hand card with a digit and play it with enter`` () =
    use runner = newRunner ()
    startHumanGame runner
    let step = TimeSpan.FromMilliseconds 50.0
    Assert.True(runner.StepUntil(atWaitingForHuman, step, maxFrames = 2000), "never reached the human's turn")
    let handBefore =
        (playing runner).GameState.Players
        |> List.find (fun p -> p.Type = Human)
        |> fun p -> p.Hand.Length
    press runner (Input.Pick 1)
    Assert.Equal(Some 0, (playing runner).SelectedCardIndex)
    press runner Input.Continue
    // The play is accepted: either it animates directly, or the capture modal
    // opened (multiple options) and picking option 1 resolves it.
    (match (playing runner).Phase with
     | GameScreen.AnimatingPlay _ -> ()
     | GameScreen.ChoosingCaptureOption _ ->
         press runner (Input.Pick 1)
         match (playing runner).Phase with
         | GameScreen.AnimatingPlay _ -> ()
         | other -> failwith $"expected AnimatingPlay after choosing, got %A{other}"
     | other -> failwith $"expected AnimatingPlay or capture modal, got %A{other}")
    let handAfter =
        (playing runner).GameState.Players
        |> List.find (fun p -> p.Type = Human)
        |> fun p -> p.Hand.Length
    Assert.Equal(handBefore - 1, handAfter)

[<Fact>]
let ``human card selection works by clicking its hit-rect`` () =
    use runner = newRunner ()
    startHumanGame runner
    let step = TimeSpan.FromMilliseconds 50.0
    Assert.True(runner.StepUntil(atWaitingForHuman, step, maxFrames = 2000), "never reached the human's turn")
    // Hit-rects are computed by the first update spent inside WaitingForHuman.
    runner.Step frame
    let idx, rect = (playing runner).CardRects |> List.head
    click runner (float32 rect.Center.X) (float32 rect.Center.Y)
    Assert.Equal(Some idx, (playing runner).SelectedCardIndex)

[<Fact>]
let ``menu button in game returns to the main menu`` () =
    use runner = newRunner ()
    startAiGame runner
    // "Menu" is the in-game top bar button at (300, 20) 120x48.
    click runner 360.0f 44.0f
    match runner.Model.Screen with
    | Game.Menu _ -> ()
    | other -> failwith $"expected Menu, got %A{other}"

[<Fact>]
let ``options toggle persists into a new game's settings`` () =
    use runner = newRunner ()
    runner.Step frame
    click runner 512.0f 648.0f    // "Options" on the menu
    click runner 512.0f 206.0f    // "Random card backs" row (topmost, y 180..232)
    press runner Input.Back
    Assert.False(runner.Model.Settings.RandomCardBacks)
    startAiGame runner
    Assert.False((playing runner).Config.Settings.RandomCardBacks)

// ── Capture modal (deterministic, crafted mid-round state) ──

let private card s r : Card = { Suit = s; Rank = r }

let private mkInput (actions: Input.UiAction list) : Input.InputState =
    { Mouse =
        { Position = Microsoft.Xna.Framework.Point(0, 0)
          LeftPressed = false
          LeftJustClicked = false
          RightJustClicked = false }
      Actions = Set.ofList actions }

let private mkClick (x: int) (y: int) : Input.InputState =
    { Mouse =
        { Position = Microsoft.Xna.Framework.Point(x, y)
          LeftPressed = true
          LeftJustClicked = true
          RightJustClicked = false }
      Actions = Set.empty }

/// A human about to play 10♠ onto a table of 10♦, 6♥, 4♦, 4♣. The combos are
/// [10♦], [6♥+4♦], and [6♥+4♣]; the two sums share the 6♥, so the maximal
/// captures differ ({10♦,6♥,4♦} vs {10♦,6♥,4♣}) and the option modal must
/// open. (Non-overlapping combos would merge into one take-everything option.)
let private mkMultiCaptureScreen () =
    let settings = { Settings.defaultSettings with RandomCardBacks = false; DefaultScatter = false }
    let config : GameEngine.GameConfig =
        { Variant = StandardKasino
          Seats = GameEngine.SeatCount.ofIntOrDefault 2
          HumanCount = 1
          Seed = None
          TargetScore = 16
          Settings = settings }
    let human = { Name = "You"; Type = Human; Hand = []; CapturedCards = []; Sweeps = 0 }
    let cpu = { Name = "CPU"; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 }
    let screen = GameScreen.create config (Random 1) [ human; cpu ] 1 (Map.ofList [ "You", 0; "CPU", 0 ]) Scoring.CarryOver.zero
    let gs =
        { screen.GameState with
            Players =
                [ { human with Hand = [ card Spades Ten ] }
                  { cpu with Hand = [ card Clubs Three ] } ]
            Table = [ card Diamonds Ten; card Hearts Six; card Diamonds Four; card Clubs Four ]
            Deck = []
            CurrentPlayerIndex = 0
            LastCapturer = None }
    { screen with
        GameState = gs
        Phase = GameScreen.WaitingForHuman
        TableLayout = GameScreen.StrictGrid }

[<Fact>]
let ``capture modal opens for a multi-capture play, cancels with escape, captures with a digit`` () =
    let step actions screen = GameScreen.update (mkInput actions) 0.016 1024 768 screen
    let s1 = mkMultiCaptureScreen () |> step [ Input.Pick 1 ]
    Assert.Equal(Some 0, s1.SelectedCardIndex)
    let s2 = s1 |> step [ Input.Continue ]
    (match s2.Phase with
     | GameScreen.ChoosingCaptureOption (0, options, 0) -> Assert.True(options.Length >= 2)
     | other -> failwith $"expected the capture modal, got %A{other}")
    // Escape cancels back to the waiting state without playing anything.
    let s3 = s2 |> step [ Input.Back ]
    (match s3.Phase with
     | GameScreen.WaitingForHuman -> ()
     | other -> failwith $"expected WaitingForHuman after cancel, got %A{other}")
    Assert.Equal(None, s3.SelectedCardIndex)
    Assert.Single(s3.GameState.Players.Head.Hand) |> ignore
    // Reopen and take the first option with a digit key.
    let s6 = s3 |> step [ Input.Pick 1 ] |> step [ Input.Continue ] |> step [ Input.Pick 1 ]
    (match s6.Phase with
     | GameScreen.AnimatingPlay _ -> ()
     | other -> failwith $"expected AnimatingPlay after capturing, got %A{other}")
    let human = s6.GameState.Players |> List.find (fun p -> p.Type = Human)
    Assert.Empty human.Hand
    // Each option is 10♠ + {10♦, 6♥, one of the fours} = 4 cards banked.
    Assert.Equal(4, human.CapturedCards.Length)

[<Fact>]
let ``standard kasino allows placing a capturable card with the place-instead button`` () =
    let step input screen = GameScreen.update input 0.016 1024 768 screen
    let s1 = mkMultiCaptureScreen () |> step (mkInput [ Input.Pick 1 ])
    Assert.Equal(Some 0, s1.SelectedCardIndex)
    // "Place Instead" sits right of the play button: (642, 593) 170x52.
    let s2 = s1 |> step (mkClick 727 619)
    (match s2.Phase with
     | GameScreen.AnimatingPlay _ -> ()
     | other -> failwith $"expected AnimatingPlay after place-instead, got %A{other}")
    let human = s2.GameState.Players |> List.find (fun p -> p.Type = Human)
    Assert.Empty human.Hand
    Assert.Empty human.CapturedCards
    Assert.Equal(5, s2.GameState.Table.Length)   // the four table cards + the placed 10♠
