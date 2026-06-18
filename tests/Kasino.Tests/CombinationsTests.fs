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

// ─────────────────────────────────────────────────────────────
// Ported from the original KasinoLibrary tests (KasinoLogicsTest.cs:
// "SaakoTesti" / "YksKakstesti"). These pin down the subset-sum
// existence and exact-result behaviour the original game relied on,
// expressed against the current findExactSubsets solver.
// ─────────────────────────────────────────────────────────────

/// Original `Saako k p`: does some non-empty subset of p sum to exactly k?
let private saako (target: int) (values: int list) =
    values
    |> List.mapi (fun i v -> (i, v))
    |> fun items -> Combinations.findExactSubsets items target
    |> List.isEmpty
    |> not

[<Fact>]
let ``Saako: subset-sum existence matches original behaviour`` () =
    // Six 1s can only ever reach 6, so 7 is unreachable...
    Assert.False(saako 7 [ 1; 1; 1; 1; 1; 1 ])
    // ...but swapping any one for a 2 makes 7 reachable (5x1 + 2).
    Assert.True(saako 7 [ 1; 1; 1; 1; 1; 2 ])
    Assert.True(saako 7 [ 1; 1; 1; 2; 1; 2 ])
    Assert.True(saako 7 [ 2; 1; 1; 1; 1; 1 ])

    // From {1,2,3,4,12,13}: 4 (=4 or 1+3) and 5 (=1+4 or 2+3) are reachable;
    // 11 is a gap (1+2+3+4 = 10, next jump is 12); 15 = 2+13 = 3+12.
    Assert.True(saako 4 [ 1; 2; 3; 4; 12; 13 ])
    Assert.True(saako 5 [ 1; 2; 3; 4; 12; 13 ])
    Assert.False(saako 11 [ 1; 2; 3; 4; 12; 13 ])
    Assert.True(saako 15 [ 1; 2; 3; 4; 12; 13 ])

[<Fact>]
let ``findExactSubsets returns exact picks over a [1;2] table (ported YksKaks)`` () =
    // Table holds an Ace (value 1) at index 0 and a Two (value 2) at index 1.
    let table = [ (0, 1); (1, 2) ]
    // Target 1 -> just the Ace.
    Assert.Equal<int list list>([ [ 0 ] ], Combinations.findExactSubsets table 1)
    // Target 2 -> just the Two.
    Assert.Equal<int list list>([ [ 1 ] ], Combinations.findExactSubsets table 2)
    // Target 3 -> both cards together (1 + 2).
    Assert.Equal<int list list>([ [ 0; 1 ] ], Combinations.findExactSubsets table 3)
