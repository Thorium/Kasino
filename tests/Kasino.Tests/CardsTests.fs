module Kasino.Tests.CardsTests

open Xunit
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Tests for Cards module: deck creation, values, predicates,
// display, dealing, shuffling, scoring values.
// ─────────────────────────────────────────────────────────────

[<Fact>]
let ``createDeck should produce 52 cards`` () =
    let deck = Cards.createDeck ()
    Assert.Equal(52, deck.Length)

[<Fact>]
let ``createDeck should have 13 cards per suit`` () =
    let deck = Cards.createDeck ()
    let spades = deck |> List.filter (fun c -> c.Suit = Spades)
    let hearts = deck |> List.filter (fun c -> c.Suit = Hearts)
    let diamonds = deck |> List.filter (fun c -> c.Suit = Diamonds)
    let clubs = deck |> List.filter (fun c -> c.Suit = Clubs)
    Assert.Equal(13, spades.Length)
    Assert.Equal(13, hearts.Length)
    Assert.Equal(13, diamonds.Length)
    Assert.Equal(13, clubs.Length)

[<Fact>]
let ``createDeck should have no duplicates`` () =
    let deck = Cards.createDeck ()
    let distinct = List.distinct deck
    Assert.Equal(deck.Length, distinct.Length)

[<Fact>]
let ``tableValue should return 1 for Ace and face values`` () =
    Assert.Equal(1, Cards.tableValue Ace)
    Assert.Equal(2, Cards.tableValue Two)
    Assert.Equal(10, Cards.tableValue Ten)
    Assert.Equal(11, Cards.tableValue Jack)
    Assert.Equal(12, Cards.tableValue Queen)
    Assert.Equal(13, Cards.tableValue King)

[<Fact>]
let ``handValue should return 14 for any Ace`` () =
    Assert.Equal(14, Cards.handValue { Suit = Spades; Rank = Ace })
    Assert.Equal(14, Cards.handValue { Suit = Hearts; Rank = Ace })

[<Fact>]
let ``handValue should return 15 for Spade Two`` () =
    Assert.Equal(15, Cards.handValue { Suit = Spades; Rank = Two })

[<Fact>]
let ``handValue should return 16 for Diamond Ten`` () =
    Assert.Equal(16, Cards.handValue { Suit = Diamonds; Rank = Ten })

[<Fact>]
let ``handValue should return table value for normal cards`` () =
    Assert.Equal(7, Cards.handValue { Suit = Hearts; Rank = Seven })
    Assert.Equal(13, Cards.handValue { Suit = Clubs; Rank = King })
    Assert.Equal(2, Cards.handValue { Suit = Hearts; Rank = Two })

[<Fact>]
let ``isSpade should detect spades`` () =
    Assert.True(Cards.isSpade { Suit = Spades; Rank = Five })
    Assert.False(Cards.isSpade { Suit = Hearts; Rank = Five })

[<Fact>]
let ``isSpadeTwo should detect only Spade Two`` () =
    Assert.True(Cards.isSpadeTwo { Suit = Spades; Rank = Two })
    Assert.False(Cards.isSpadeTwo { Suit = Hearts; Rank = Two })
    Assert.False(Cards.isSpadeTwo { Suit = Spades; Rank = Three })

[<Fact>]
let ``isDiamondTen should detect only Diamond Ten`` () =
    Assert.True(Cards.isDiamondTen { Suit = Diamonds; Rank = Ten })
    Assert.False(Cards.isDiamondTen { Suit = Spades; Rank = Ten })
    Assert.False(Cards.isDiamondTen { Suit = Diamonds; Rank = Nine })

[<Fact>]
let ``isAce should detect aces`` () =
    Assert.True(Cards.isAce { Suit = Spades; Rank = Ace })
    Assert.True(Cards.isAce { Suit = Hearts; Rank = Ace })
    Assert.False(Cards.isAce { Suit = Hearts; Rank = King })

[<Fact>]
let ``display should show rank before suit symbol`` () =
    let card = { Suit = Spades; Rank = Ace }
    Assert.Equal("A\u2660", Cards.display card)

[<Fact>]
let ``display should show 10 for Ten`` () =
    let card = { Suit = Diamonds; Rank = Ten }
    Assert.Equal("10\u2666", Cards.display card)

[<Fact>]
let ``deal should split deck correctly`` () =
    let deck = Cards.createDeck () |> List.take 10
    let dealt, remaining = Cards.deal 4 deck
    Assert.Equal(4, dealt.Length)
    Assert.Equal(6, remaining.Length)

[<Fact>]
let ``deal should not exceed deck size`` () =
    let deck = [ { Suit = Spades; Rank = Ace }; { Suit = Hearts; Rank = Two } ]
    let dealt, remaining = Cards.deal 5 deck
    Assert.Equal(2, dealt.Length)
    Assert.Equal(0, remaining.Length)

[<Fact>]
let ``shuffle should produce all original cards`` () =
    let deck = Cards.createDeck ()
    let shuffled = Cards.shuffle (System.Random(42)) deck
    Assert.Equal(52, shuffled.Length)
    let sortedOrig = deck |> List.sortBy (fun c -> (c.Suit, c.Rank))
    let sortedShuf = shuffled |> List.sortBy (fun c -> (c.Suit, c.Rank))
    Assert.Equal<Card list>(sortedOrig, sortedShuf)

[<Fact>]
let ``scoringValue should assign correct direct points`` () =
    let diamondTen = Cards.scoringValue { Suit = Diamonds; Rank = Ten }
    Assert.True(diamondTen > 2.0 && diamondTen < 2.2)

    let spadeTwo = Cards.scoringValue { Suit = Spades; Rank = Two }
    Assert.True(spadeTwo > 1.1 && spadeTwo < 1.3)

    let heartAce = Cards.scoringValue { Suit = Hearts; Rank = Ace }
    Assert.True(heartAce > 1.0 && heartAce < 1.1)

    let heartSeven = Cards.scoringValue { Suit = Hearts; Rank = Seven }
    Assert.True(heartSeven > 0.0 && heartSeven < 0.1)
