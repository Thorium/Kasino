module Kasino.Tests.CombinationsTests

open Xunit
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Tests for Combinations module: subset-sum solver.
// ─────────────────────────────────────────────────────────────

[<Fact>]
let ``findExactSubsets should find two combos for target 7`` () =
    // Items: 2, 5, 3, 4  => combos summing to 7: [2,5] and [3,4]
    let items = [ (0, 2); (1, 5); (2, 3); (3, 4) ]
    let results = Combinations.findExactSubsets items 7
    Assert.Equal(2, results.Length)
    Assert.Contains([0; 1], results)  // indices for 2+5
    Assert.Contains([2; 3], results)  // indices for 3+4

[<Fact>]
let ``findExactSubsets should find single item match`` () =
    let items = [ (0, 5); (1, 3) ]
    let results = Combinations.findExactSubsets items 5
    Assert.Equal(1, results.Length)
    Assert.Contains([0], results)

[<Fact>]
let ``findExactSubsets should return empty for no match`` () =
    let items = [ (0, 2); (1, 3) ]
    let results = Combinations.findExactSubsets items 10
    Assert.Empty(results)

[<Fact>]
let ``findExactSubsets should return empty for empty items`` () =
    let results = Combinations.findExactSubsets [] 5
    Assert.Empty(results)

[<Fact>]
let ``findExactSubsets should find multiple subsets`` () =
    // Items: 1, 2, 3, 4, 5 => target 5: [1,4], [2,3], [5]
    let items = [ (0, 1); (1, 2); (2, 3); (3, 4); (4, 5) ]
    let results = Combinations.findExactSubsets items 5
    Assert.Equal(3, results.Length)

[<Fact>]
let ``findCaptureCombinations should find card captures`` () =
    let handCard = { Suit = Hearts; Rank = Seven }
    let table = [ { Suit = Spades; Rank = Seven }
                  { Suit = Clubs; Rank = Three }
                  { Suit = Diamonds; Rank = Four } ]
    let combos = Combinations.findCaptureCombinations handCard table
    // Should find: [7♠] and [3♣, 4♦]
    Assert.Equal(2, combos.Length)

[<Fact>]
let ``findCaptureCombinations should return empty for no match`` () =
    let handCard = { Suit = Hearts; Rank = King }
    let table = [ { Suit = Spades; Rank = Two }; { Suit = Clubs; Rank = Three } ]
    let combos = Combinations.findCaptureCombinations handCard table
    Assert.Empty(combos)

[<Fact>]
let ``findCaptureCombinations should return empty for empty table`` () =
    let handCard = { Suit = Hearts; Rank = Five }
    let combos = Combinations.findCaptureCombinations handCard []
    Assert.Empty(combos)

[<Fact>]
let ``findCaptureCombinations should handle Ace with value 14`` () =
    let handCard = { Suit = Hearts; Rank = Ace }
    let table = [ { Suit = Spades; Rank = King }   // 13
                  { Suit = Clubs; Rank = Ace } ]     // 1
    // Ace hand value = 14; King(13) + Ace(1) = 14
    let combos = Combinations.findCaptureCombinations handCard table
    Assert.True(combos.Length >= 1)
    let combo = combos |> List.find (fun c -> c.Length = 2)
    Assert.Equal(2, combo.Length)
