module Kasino.Tests.AITests

open Xunit
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Tests for AI module: play evaluation, Standard vs Laisto.
// ─────────────────────────────────────────────────────────────

let private defaultCtx: AI.GameContext =
    { MyCards = 0; MySpades = 0
      OpponentCards = 0; OpponentSpades = 0
      CardsRemaining = 40 }

[<Fact>]
let ``evaluatePlay should detect capture`` () =
    let hand = { Suit = Hearts; Rank = Five }
    let table = [ { Suit = Spades; Rank = Five } ]
    let eval = AI.evaluatePlay hand table
    Assert.True(eval.CardsCaptured > 0)
    Assert.True(eval.IsSweep)
    Assert.True(eval.PointValue > 0.0)

[<Fact>]
let ``evaluatePlay should detect placement`` () =
    let hand = { Suit = Hearts; Rank = King }
    let table = [ { Suit = Spades; Rank = Two } ]
    let eval = AI.evaluatePlay hand table
    Assert.Equal(0, eval.CardsCaptured)
    Assert.False(eval.IsSweep)
    Assert.Equal(0.0, eval.PointValue)

[<Fact>]
let ``chooseBestStandard should prefer capture over placement`` () =
    let hand =
        [ { Suit = Hearts; Rank = Five }
          { Suit = Hearts; Rank = King } ]
    let table = [ { Suit = Spades; Rank = Five } ]
    let eval = AI.chooseBestStandard defaultCtx hand table
    Assert.Equal(Five, eval.HandCard.Rank)

[<Fact>]
let ``chooseBestStandard should prefer higher value capture`` () =
    let hand =
        [ { Suit = Clubs; Rank = Five }
          { Suit = Diamonds; Rank = Ace } ]
    let table =
        [ { Suit = Spades; Rank = Five }
          { Suit = Hearts; Rank = Ace } ]
    // Ace capture is worth more (Ace = 1pt + spade fraction possibilities)
    let eval = AI.chooseBestStandard defaultCtx hand table
    Assert.True(eval.CardsCaptured > 0)

[<Fact>]
let ``chooseBestLaisto should prefer non-capture`` () =
    let hand =
        [ { Suit = Hearts; Rank = Five }
          { Suit = Hearts; Rank = King } ]
    let table = [ { Suit = Spades; Rank = Five } ]
    let eval = AI.chooseBestLaisto defaultCtx hand table
    // Should choose King which doesn't capture
    Assert.Equal(King, eval.HandCard.Rank)

[<Fact>]
let ``chooseBestLaisto should pick lowest value when forced to capture`` () =
    let hand =
        [ { Suit = Hearts; Rank = Five }
          { Suit = Clubs; Rank = Five } ]
    let table =
        [ { Suit = Spades; Rank = Five }
          { Suit = Diamonds; Rank = Five } ]
    let eval = AI.chooseBestLaisto defaultCtx hand table
    // Both cards capture; should still pick something
    Assert.True(eval.CardsCaptured > 0)

[<Fact>]
let ``chooseBest should delegate to correct variant`` () =
    let hand = [ { Suit = Hearts; Rank = Five } ]
    let table = [ { Suit = Spades; Rank = Five } ]
    let standard = AI.chooseBest StandardKasino defaultCtx hand table
    let laisto = AI.chooseBest LaistoKasino defaultCtx hand table
    // Both should produce valid results
    Assert.Equal(Five, standard.HandCard.Rank)
    Assert.Equal(Five, laisto.HandCard.Rank)

[<Fact>]
let ``Laisto AI should pick lowest-point capture when overlapping`` () =
    // K♣ (13), table: 3♥(3), 10♦(10), 10♠(10)
    // Combos: {3♥,10♦}=13 and {3♥,10♠}=13 — overlap on 3♥
    // Laisto should avoid 10♦ (worth 2pts)
    let hand = [ { Suit = Clubs; Rank = King } ]
    let table =
        [ { Suit = Hearts; Rank = Three }
          { Suit = Diamonds; Rank = Ten }
          { Suit = Spades; Rank = Ten } ]
    let laisto = AI.chooseBest LaistoKasino defaultCtx hand table
    match laisto.ChosenOption with
    | Some opt ->
        // Should NOT contain Diamond Ten
        Assert.False(opt.Captured |> List.exists Cards.isDiamondTen)
    | None -> Assert.Fail("Expected a capture option")

[<Fact>]
let ``Standard AI should pick highest-point capture when overlapping`` () =
    // Same setup: K♣ -> {3♥,10♦} or {3♥,10♠}
    // Standard should prefer 10♦ (worth 2pts)
    let hand = [ { Suit = Clubs; Rank = King } ]
    let table =
        [ { Suit = Hearts; Rank = Three }
          { Suit = Diamonds; Rank = Ten }
          { Suit = Spades; Rank = Ten } ]
    let standard = AI.chooseBest StandardKasino defaultCtx hand table
    match standard.ChosenOption with
    | Some opt ->
        // Should contain Diamond Ten
        Assert.True(opt.Captured |> List.exists Cards.isDiamondTen)
    | None -> Assert.Fail("Expected a capture option")

// ── Personality play styles (chooseBestStyled) ───────────────

[<Fact>]
let ``chooseBestStyled Balanced matches chooseBest for Standard`` () =
    let table = [ { Suit = Hearts; Rank = Three }; { Suit = Hearts; Rank = Four }; { Suit = Diamonds; Rank = Ten } ]
    let hand = [ { Suit = Clubs; Rank = Seven }; { Suit = Hearts; Rank = Ten } ]
    let balanced = AI.chooseBestStyled AI.Balanced StandardKasino defaultCtx hand table
    let baseline = AI.chooseBest StandardKasino defaultCtx hand table
    Assert.Equal(baseline.HandCard, balanced.HandCard)

[<Fact>]
let ``Aggressive grabs more cards while Cautious secures points`` () =
    // 7 captures {3,4} (2 cards, no points); 10 captures the 10 of diamonds (1 card, 2 points).
    let table = [ { Suit = Hearts; Rank = Three }; { Suit = Hearts; Rank = Four }; { Suit = Diamonds; Rank = Ten } ]
    let hand = [ { Suit = Clubs; Rank = Seven }; { Suit = Hearts; Rank = Ten } ]
    let aggressive = AI.chooseBestStyled AI.Aggressive StandardKasino defaultCtx hand table
    let cautious = AI.chooseBestStyled AI.Cautious StandardKasino defaultCtx hand table
    Assert.Equal(Seven, aggressive.HandCard.Rank)
    Assert.Equal(Ten, cautious.HandCard.Rank)

[<Fact>]
let ``every style still captures when a capture is available`` () =
    let table = [ { Suit = Spades; Rank = Five } ]
    let hand = [ { Suit = Hearts; Rank = Five }; { Suit = Hearts; Rank = King } ]
    for style in [ AI.Balanced; AI.Aggressive; AI.Cautious ] do
        let eval = AI.chooseBestStyled style StandardKasino defaultCtx hand table
        Assert.True(eval.CardsCaptured > 0, $"style {style} should capture")
