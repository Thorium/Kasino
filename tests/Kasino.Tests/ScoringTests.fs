module Kasino.Tests.ScoringTests

open Xunit
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Tests for Scoring module: end-of-round score calculation.
// ─────────────────────────────────────────────────────────────

let private makePlayer name cards sweeps =
    { Name = name; Type = Computer; Hand = []; CapturedCards = cards; Sweeps = sweeps }

let private spades n =
    [ for i in 0 .. n - 1 do
        { Suit = Spades; Rank = Cards.allRanks[i % 13] } ]

let private nonSpades n =
    [ for i in 0 .. n - 1 do
        { Suit = Hearts; Rank = Cards.allRanks[i % 13] } ]

[<Fact>]
let ``should award Most Cards to unique maximum`` () =
    let p1 = makePlayer "P1" (nonSpades 30) 0
    let p2 = makePlayer "P2" (nonSpades 22) 0
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    let (_, s2) = scores[1]
    Assert.Equal(1, s1.MostCards)
    Assert.Equal(0, s2.MostCards)

[<Fact>]
let ``should not award Most Cards on tie`` () =
    let p1 = makePlayer "P1" (nonSpades 26) 0
    let p2 = makePlayer "P2" (nonSpades 26) 0
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    let (_, s2) = scores[1]
    Assert.Equal(0, s1.MostCards)
    Assert.Equal(0, s2.MostCards)

[<Fact>]
let ``should award Most Spades 2 points to unique maximum`` () =
    let p1 = makePlayer "P1" (spades 7) 0
    let p2 = makePlayer "P2" [] 0
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    let (_, s2) = scores[1]
    Assert.Equal(2, s1.MostSpades)
    Assert.Equal(0, s2.MostSpades)

[<Fact>]
let ``should count each Ace as 1 point`` () =
    let cards =
        [ { Suit = Spades; Rank = Ace }
          { Suit = Hearts; Rank = Ace }
          { Suit = Diamonds; Rank = Ace }
          { Suit = Clubs; Rank = Five } ]
    let p1 = makePlayer "P1" cards 0
    let p2 = makePlayer "P2" (nonSpades 30) 0  // give p2 more cards to avoid distortion
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    Assert.Equal(3, s1.Aces)

[<Fact>]
let ``should award Diamond Ten 2 points`` () =
    let cards = [ { Suit = Diamonds; Rank = Ten } ]
    let p1 = makePlayer "P1" cards 0
    let p2 = makePlayer "P2" (nonSpades 30) 0
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    Assert.Equal(2, s1.DiamondTen)

[<Fact>]
let ``should award Spade Two 1 point`` () =
    let cards = [ { Suit = Spades; Rank = Two } ]
    let p1 = makePlayer "P1" cards 0
    let p2 = makePlayer "P2" (nonSpades 30) 0
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    Assert.Equal(1, s1.SpadeTwo)

[<Fact>]
let ``should count sweeps with deduction`` () =
    let p1 = makePlayer "P1" [] 3
    let p2 = makePlayer "P2" [] 1
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    let (_, s2) = scores[1]
    // min = 1, so p1 gets 3-1=2, p2 gets 1-1=0
    Assert.Equal(2, s1.Sweeps)
    Assert.Equal(0, s2.Sweeps)

[<Fact>]
let ``should not deduct sweeps when some have zero`` () =
    let p1 = makePlayer "P1" [] 2
    let p2 = makePlayer "P2" [] 0
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    let (_, s2) = scores[1]
    Assert.Equal(2, s1.Sweeps)
    Assert.Equal(0, s2.Sweeps)

[<Fact>]
let ``should deduct universal sweeps from all players`` () =
    let p1 = makePlayer "P1" [] 4
    let p2 = makePlayer "P2" [] 2
    let p3 = makePlayer "P3" [] 3
    let scores = Scoring.calculateScores [ p1; p2; p3 ]
    let (_, s1) = scores[0]
    let (_, s2) = scores[1]
    let (_, s3) = scores[2]
    // min = 2
    Assert.Equal(2, s1.Sweeps)
    Assert.Equal(0, s2.Sweeps)
    Assert.Equal(1, s3.Sweeps)

[<Fact>]
let ``should compute correct total`` () =
    // P1: 27 cards (most), 7 spades (most), 2 aces, diamond 10, spade 2, 1 sweep
    let spadesCards = spades 7
    let extraCards = nonSpades 18
    let specialCards =
        [ { Suit = Diamonds; Rank = Ten }
          { Suit = Spades; Rank = Two } ]
    // 2 aces already in spades (A♠) and need 1 more
    let aceCard = [ { Suit = Hearts; Rank = Ace } ]
    let allCards = spadesCards @ extraCards @ specialCards @ aceCard
    let p1 = makePlayer "P1" allCards 1
    let p2 = makePlayer "P2" (nonSpades 20) 0
    let scores = Scoring.calculateScores [ p1; p2 ]
    let (_, s1) = scores[0]
    // Most cards = 1, Most spades = 2, Aces = 2 (♠A + ♥A), Diamond 10 = 2, Spade 2 = 1, Sweeps = 1
    Assert.Equal(1, s1.MostCards)
    Assert.Equal(2, s1.MostSpades)
    Assert.Equal(2, s1.DiamondTen)
    Assert.Equal(1, s1.SpadeTwo)
    Assert.Equal(1, s1.Sweeps)
    // Total should be 1+2+Aces+2+1+1
    let expectedTotal = 1 + 2 + s1.Aces + 2 + 1 + 1
    Assert.Equal(expectedTotal, s1.Total)

// ── Tied most-cards/most-spades carry-over ──

[<Fact>]
let ``tied categories bank their points into the carry-over pot`` () =
    let p1 = makePlayer "P1" (nonSpades 13 @ spades 13) 0
    let p2 = makePlayer "P2" (nonSpades 13 @ spades 13) 0
    let scores, carry = Scoring.calculateScoresCarry Scoring.CarryOver.zero [ p1; p2 ]
    for (_, s) in scores do
        Assert.Equal(0, s.MostCards)
        Assert.Equal(0, s.MostSpades)
    Assert.Equal(1, carry.CardsPool)
    Assert.Equal(2, carry.SpadesPool)

[<Fact>]
let ``carried pot accumulates across consecutive ties`` () =
    let p1 = makePlayer "P1" (nonSpades 13 @ spades 13) 0
    let p2 = makePlayer "P2" (nonSpades 13 @ spades 13) 0
    let _, carry1 = Scoring.calculateScoresCarry Scoring.CarryOver.zero [ p1; p2 ]
    let _, carry2 = Scoring.calculateScoresCarry carry1 [ p1; p2 ]
    Assert.Equal(2, carry2.CardsPool)
    Assert.Equal(4, carry2.SpadesPool)

[<Fact>]
let ``outright winner collects the current round plus the carried pot`` () =
    let carry : Scoring.CarryOver = { CardsPool = 2; SpadesPool = 4 }
    let p1 = makePlayer "P1" (nonSpades 20 @ spades 10) 0
    let p2 = makePlayer "P2" (nonSpades 15) 0
    let scores, carryOut = Scoring.calculateScoresCarry carry [ p1; p2 ]
    let (_, s1) = scores[0]
    Assert.Equal(3, s1.MostCards)    // 2 carried + 1 this round
    Assert.Equal(6, s1.MostSpades)   // 4 carried + 2 this round
    Assert.Equal(0, carryOut.CardsPool)
    Assert.Equal(0, carryOut.SpadesPool)

// ── 10-point sweep freeze ──

[<Fact>]
let ``a clearing capture scores no sweep once sweeps are frozen`` () =
    let human = { Name = "P1"; Type = Human; Hand = [ { Suit = Spades; Rank = Eight } ]; CapturedCards = []; Sweeps = 0 }
    let cpu = { Name = "P2"; Type = Computer; Hand = [ { Suit = Clubs; Rank = Three } ]; CapturedCards = []; Sweeps = 0 }
    let state : GameEngine.GameState =
        { Players = [ human; cpu ]; Table = [ { Suit = Diamonds; Rank = Eight } ]; Deck = []
          CurrentPlayerIndex = 0; DealRound = 6; TotalDeals = 6
          LastCapturer = None; Variant = StandardKasino; SweepsFrozen = true }
    let result = GameEngine.playHumanTurn state 0 None
    match result.PlayResult with
    | Capture(_, _, isSweep) -> Assert.False(isSweep, "frozen sweeps must not be flagged")
    | other -> failwith $"expected a capture, got %A{other}"
    Assert.Equal(0, result.NewState.Players[0].Sweeps)
    // The same play without the freeze is a sweep.
    let result2 = GameEngine.playHumanTurn { state with SweepsFrozen = false } 0 None
    Assert.Equal(1, result2.NewState.Players[0].Sweeps)
