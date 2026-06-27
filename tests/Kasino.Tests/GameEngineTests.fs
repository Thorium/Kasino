module Kasino.Tests.GameEngineTests

open System
open Xunit
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Tests for GameEngine module: state management, dealing, turns.
// ─────────────────────────────────────────────────────────────

let private makeConfig variant playerCount humanCount =
    { GameEngine.Variant = variant
      GameEngine.Seats = GameEngine.SeatCount.ofIntOrDefault playerCount
      GameEngine.HumanCount = humanCount
      GameEngine.Seed = Some 42
      GameEngine.TargetScore = 16
      GameEngine.Settings = Settings.defaultSettings }

[<Fact>]
let ``dealRounds should return 6 for 2 players`` () =
    Assert.Equal(6, GameEngine.SeatCount.dealRounds GameEngine.SeatCount.Two)

[<Fact>]
let ``dealRounds should return 4 for 3 players`` () =
    Assert.Equal(4, GameEngine.SeatCount.dealRounds GameEngine.SeatCount.Three)

[<Fact>]
let ``dealRounds should return 3 for 4 players`` () =
    Assert.Equal(3, GameEngine.SeatCount.dealRounds GameEngine.SeatCount.Four)

[<Fact>]
let ``createPlayers should create correct number of players`` () =
    let config = makeConfig StandardKasino 3 0
    let players = GameEngine.createPlayers config
    Assert.Equal(3, players.Length)
    Assert.True(players |> List.forall (fun p -> p.Type = Computer))

[<Fact>]
let ``createPlayers should set first player as Human when HumanCount > 0`` () =
    let config = makeConfig StandardKasino 2 1
    let players = GameEngine.createPlayers config
    Assert.Equal(Human, players[0].Type)
    Assert.Equal("Player", players[0].Name)
    Assert.Equal(Computer, players[1].Type)

[<Fact>]
let ``allHandsEmpty should return true when all hands empty`` () =
    let players =
        [ { Name = "P1"; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 }
          { Name = "P2"; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 } ]
    let state: GameEngine.GameState =
        { Players = players; Table = []; Deck = []
          CurrentPlayerIndex = 0; DealRound = 1; TotalDeals = 6
          LastCapturer = None; Variant = StandardKasino }
    Assert.True(GameEngine.allHandsEmpty state)

[<Fact>]
let ``allHandsEmpty should return false when any hand has cards`` () =
    let players =
        [ { Name = "P1"; Type = Computer
            Hand = [ { Suit = Spades; Rank = Ace } ]
            CapturedCards = []; Sweeps = 0 }
          { Name = "P2"; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 } ]
    let state: GameEngine.GameState =
        { Players = players; Table = []; Deck = []
          CurrentPlayerIndex = 0; DealRound = 1; TotalDeals = 6
          LastCapturer = None; Variant = StandardKasino }
    Assert.False(GameEngine.allHandsEmpty state)

[<Fact>]
let ``dealRound first deal should give 4 cards each and 4 to table`` () =
    let config = makeConfig StandardKasino 2 0
    let rng = Random(42)
    let players = GameEngine.createPlayers config
    let state = GameEngine.newRound config rng players 1
    let dealt = GameEngine.dealRound state true
    // Each player gets 4 cards
    Assert.Equal(4, dealt.Players[0].Hand.Length)
    Assert.Equal(4, dealt.Players[1].Hand.Length)
    // Table gets 4 cards
    Assert.Equal(4, dealt.Table.Length)
    // Deck: 52 - 4*2 - 4 = 40
    Assert.Equal(40, dealt.Deck.Length)

[<Fact>]
let ``dealRound subsequent deal should not add to table`` () =
    let config = makeConfig StandardKasino 2 0
    let rng = Random(42)
    let players = GameEngine.createPlayers config
    let state = GameEngine.newRound config rng players 1
    // First deal
    let state1 = GameEngine.dealRound state true
    // Clear hands (simulate round completion), keep table
    let clearHands = state1.Players |> List.map (fun p -> { p with Hand = [] })
    let state2 = { state1 with Players = clearHands }
    // Subsequent deal
    let dealt = GameEngine.dealRound state2 false
    Assert.Equal(4, dealt.Players[0].Hand.Length)
    Assert.Equal(4, dealt.Players[1].Hand.Length)
    // Table unchanged from state2 (4 cards from first deal)
    Assert.Equal(4, dealt.Table.Length)
    // Deck: 40 - 8 = 32
    Assert.Equal(32, dealt.Deck.Length)

[<Fact>]
let ``playComputerTurn should execute and advance`` () =
    let config = makeConfig StandardKasino 2 0
    let rng = Random(42)
    let players = GameEngine.createPlayers config
    let state = GameEngine.newRound config rng players 1
    let state = GameEngine.dealRound state true
    let state = { state with DealRound = 1 }
    let result = GameEngine.playComputerTurn state
    // Player should have 3 cards after playing one
    let currentPlayer = result.NewState.Players[state.CurrentPlayerIndex]
    Assert.Equal(3, currentPlayer.Hand.Length)
    // Current player index should advance
    Assert.NotEqual(state.CurrentPlayerIndex, result.NewState.CurrentPlayerIndex)

[<Fact>]
let ``endRound should give remaining table cards to last capturer`` () =
    let table = [ { Suit = Spades; Rank = Five }; { Suit = Hearts; Rank = Three } ]
    let players =
        [ { Name = "P1"; Type = Computer; Hand = []; CapturedCards = [ { Suit = Clubs; Rank = Ace } ]; Sweeps = 0 }
          { Name = "P2"; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 } ]
    let state: GameEngine.GameState =
        { Players = players; Table = table; Deck = []
          CurrentPlayerIndex = 0; DealRound = 6; TotalDeals = 6
          LastCapturer = Some 0; Variant = StandardKasino }
    let final = GameEngine.endRound state
    Assert.Equal(3, final.Players[0].CapturedCards.Length)
    Assert.Empty(final.Table)

[<Fact>]
let ``endRound with no last capturer should leave table unchanged`` () =
    let table = [ { Suit = Spades; Rank = Five } ]
    let players =
        [ { Name = "P1"; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 }
          { Name = "P2"; Type = Computer; Hand = []; CapturedCards = []; Sweeps = 0 } ]
    let state: GameEngine.GameState =
        { Players = players; Table = table; Deck = []
          CurrentPlayerIndex = 0; DealRound = 6; TotalDeals = 6
          LastCapturer = None; Variant = StandardKasino }
    let final = GameEngine.endRound state
    Assert.Equal(1, final.Table.Length)

[<Fact>]
let ``newRound should create fresh state`` () =
    let config = makeConfig StandardKasino 2 1
    let rng = Random(42)
    let players = GameEngine.createPlayers config
    let state = GameEngine.newRound config rng players 1
    Assert.Equal(52, state.Deck.Length)
    Assert.Empty(state.Table)
    Assert.True(state.Players |> List.forall (fun p -> List.isEmpty p.Hand))
    Assert.True(state.Players |> List.forall (fun p -> List.isEmpty p.CapturedCards))
    Assert.True(state.Players |> List.forall (fun p -> p.Sweeps = 0))

[<Fact>]
let ``createPlayers uses personality names when AiPersonalities is on`` () =
    let config = { makeConfig StandardKasino 2 0 with Settings = { Settings.defaultSettings with AiPersonalities = true } }
    let players = GameEngine.createPlayers config
    Assert.Equal(Personality.forCpu(0).Name, players[0].Name)
    Assert.Equal(Personality.forCpu(1).Name, players[1].Name)

[<Fact>]
let ``createPlayers uses plain CPU name when AiPersonalities is off`` () =
    let config = makeConfig StandardKasino 2 1
    let players = GameEngine.createPlayers config
    Assert.Equal("CPU", players[1].Name)

[<Fact>]
let ``computerStyle honours personalities for CPU seats`` () =
    let config = { makeConfig StandardKasino 2 1 with Settings = { Settings.defaultSettings with AiPersonalities = true } }
    Assert.Equal(AI.Balanced, GameEngine.computerStyle config 0)
    Assert.Equal((Personality.forCpu 0).Style, GameEngine.computerStyle config 1)
