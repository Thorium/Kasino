module Kasino.Tests.RulesTests

open Xunit
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Tests for Rules module: captures, options, sweeps, playCard.
// ─────────────────────────────────────────────────────────────

[<Fact>]
let ``findCaptures should find single card match`` () =
    let hand = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Spades; Rank = Seven } ]
    let combos = Rules.findCaptures hand table
    Assert.True(combos.Length >= 1)
    Assert.True(combos |> List.exists (fun c -> c |> List.contains { Suit = Spades; Rank = Seven }))

[<Fact>]
let ``findCaptures should find sum-based capture`` () =
    let hand = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Clubs; Rank = Three }; { Suit = Diamonds; Rank = Four } ]
    let combos = Rules.findCaptures hand table
    Assert.True(combos.Length >= 1)

[<Fact>]
let ``findCaptures should find multiple combinations`` () =
    let hand = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Spades; Rank = Seven }
                  { Suit = Clubs; Rank = Three }
                  { Suit = Diamonds; Rank = Four } ]
    let combos = Rules.findCaptures hand table
    Assert.True(combos.Length >= 2)

[<Fact>]
let ``findCaptures should return empty for no match`` () =
    let hand = { Suit = Hearts; Rank = King }
    let table = [ { Suit = Spades; Rank = Two }; { Suit = Clubs; Rank = Three } ]
    let combos = Rules.findCaptures hand table
    Assert.Empty(combos)

[<Fact>]
let ``findCaptureOptions should return single option for non-overlapping combos`` () =
    let hand = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Spades; Rank = Seven }
                  { Suit = Clubs; Rank = Three }
                  { Suit = Diamonds; Rank = Four } ]
    let options = Rules.findCaptureOptions hand table
    // [7♠] and [3♣, 4♦] don't overlap => merged into 1 option
    Assert.Equal(1, options.Length)
    Assert.Equal(3, options[0].Captured.Length)

[<Fact>]
let ``findCaptureOptions should return multiple options for overlapping combos`` () =
    // 8♦ played, table: A♦(1), 2♥(2), 5♠(5), 6♠(6)
    // Combos: [A,2,5]=8 and [2,6]=8 — overlap on 2♥
    let hand = { Suit = Diamonds; Rank = Eight }
    let table = [ { Suit = Diamonds; Rank = Ace }
                  { Suit = Hearts; Rank = Two }
                  { Suit = Spades; Rank = Five }
                  { Suit = Spades; Rank = Six } ]
    let options = Rules.findCaptureOptions hand table
    Assert.True(options.Length >= 2)
    // Each option's combos should not share cards
    for opt in options do
        let allCards = opt.Combos |> List.concat
        let distinct = allCards |> List.distinct
        Assert.Equal(allCards.Length, distinct.Length)

[<Fact>]
let ``findCaptureOptions should return empty for no captures`` () =
    let hand = { Suit = Hearts; Rank = King }
    let table = [ { Suit = Spades; Rank = Two } ]
    let options = Rules.findCaptureOptions hand table
    Assert.Empty(options)

[<Fact>]
let ``playCard should capture and remove cards from table`` () =
    let hand = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Spades; Rank = Seven } ]
    let result, newTable, _ = Rules.playCard hand table
    match result with
    | Capture(_, captured, isSweep) ->
        Assert.Equal(1, captured.Length)
        Assert.True(isSweep)
    | Place _ -> Assert.Fail("Expected capture")
    Assert.Empty(newTable)

[<Fact>]
let ``playCard should place card when no capture`` () =
    let hand = { Suit = Hearts; Rank = King }
    let table = [ { Suit = Spades; Rank = Two } ]
    let result, newTable, _ = Rules.playCard hand table
    match result with
    | Place hc -> Assert.Equal(King, hc.Rank)
    | Capture _ -> Assert.Fail("Expected place")
    Assert.Equal(2, newTable.Length)

[<Fact>]
let ``playCard should detect sweep when table is cleared`` () =
    let hand = { Suit = Hearts; Rank = Five }
    let table = [ { Suit = Spades; Rank = Five } ]
    let result, newTable, _ = Rules.playCard hand table
    match result with
    | Capture(_, _, isSweep) -> Assert.True(isSweep)
    | _ -> Assert.Fail("Expected capture")
    Assert.Empty(newTable)

[<Fact>]
let ``playCard should NOT be a sweep when table cards remain`` () =
    let hand = { Suit = Hearts; Rank = Five }
    let table = [ { Suit = Spades; Rank = Five }; { Suit = Clubs; Rank = King } ]
    let result, _, _ = Rules.playCard hand table
    match result with
    | Capture(_, _, isSweep) -> Assert.False(isSweep)
    | _ -> Assert.Fail("Expected capture")

[<Fact>]
let ``resolveCapture should compute correct result`` () =
    let hand = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Spades; Rank = Seven }; { Suit = Clubs; Rank = King } ]
    let option: Rules.CaptureOption =
        { Combos = [ [ { Suit = Spades; Rank = Seven } ] ]
          Captured = [ { Suit = Spades; Rank = Seven } ] }
    let result, newTable = Rules.resolveCapture hand option table
    match result with
    | Capture(_, captured, isSweep) ->
        Assert.Equal(1, captured.Length)
        Assert.False(isSweep)
    | _ -> Assert.Fail("Expected capture")
    Assert.Equal(1, newTable.Length)

[<Fact>]
let ``resolveCapture should detect sweep`` () =
    let hand = { Suit = Hearts; Rank = Five }
    let table = [ { Suit = Spades; Rank = Five } ]
    let option: Rules.CaptureOption =
        { Combos = [ [ { Suit = Spades; Rank = Five } ] ]
          Captured = [ { Suit = Spades; Rank = Five } ] }
    let result, newTable = Rules.resolveCapture hand option table
    match result with
    | Capture(_, _, isSweep) -> Assert.True(isSweep)
    | _ -> Assert.Fail("Expected capture")
    Assert.Empty(newTable)

[<Fact>]
let ``capturePointValue should sum scoring values`` () =
    let cards = [ { Suit = Diamonds; Rank = Ten }; { Suit = Hearts; Rank = Ace } ]
    let value = Rules.capturePointValue cards
    Assert.True(value > 3.0)

[<Fact>]
let ``getCapturedCards should return union of non-overlapping captures`` () =
    let hand = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Spades; Rank = Seven }
                  { Suit = Clubs; Rank = Three }
                  { Suit = Diamonds; Rank = Four } ]
    let captured = Rules.getCapturedCards hand table
    Assert.Equal(3, captured.Length)
