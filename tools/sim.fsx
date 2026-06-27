// ─────────────────────────────────────────────────────────────
// Headless self-play harness for Kasino — the AI plays full games
// against itself so you can sanity-check balance and, later, measure
// AI changes.
//
// Run:  dotnet build src/Kasino.Domain/Kasino.Domain.fsproj
//       dotnet fsi tools/sim.fsx
//
// The AI only ever sees what GameEngine.buildContext gives it — its own
// hand, the table, captured piles (cards that were on the table) and
// public card COUNTS. It never reads opponents' hands or the deck.
//
// To A/B two AI variants later: introduce a per-seat chooser (e.g. a
// GameEngine turn function that takes the strategy to use), then in
// `playRound` pick the seat's strategy by CurrentPlayerIndex. Play each
// deal in both seat orientations (same Random seed) and compare per-seat
// score to cancel deal luck — see git history for the earlier A/B version.
// ─────────────────────────────────────────────────────────────

#r "../src/Kasino.Domain/bin/Debug/net10.0/Kasino.Domain.dll"

open System
open Kasino.Domain

let settings = { Settings.defaultSettings with AiPersonalities = false }

let mkConfig variant playerCount : GameEngine.GameConfig =
    { Variant = variant
      Seats = GameEngine.SeatCount.ofIntOrDefault playerCount
      HumanCount = 0
      Seed = None
      TargetScore = 16
      Settings = settings }

/// Play one round to completion (all-CPU seats). Returns per-seat round Totals.
let playRound (config: GameEngine.GameConfig) (rng: Random) (players: Player list) (roundNumber: int) : int[] =
    let start = GameEngine.dealRound (GameEngine.newRound config rng players roundNumber) true
    let mutable state = { start with DealRound = 1 }
    let rec loop (s: GameEngine.GameState) =
        if GameEngine.allHandsEmpty s then
            if s.DealRound < s.TotalDeals then
                loop { GameEngine.dealRound s false with DealRound = s.DealRound + 1 }
            else
                GameEngine.endRound s
        else
            let idx = s.CurrentPlayerIndex
            if List.isEmpty s.Players[idx].Hand then
                loop { s with CurrentPlayerIndex = (idx + 1) % s.Players.Length }
            else
                loop (GameEngine.playComputerTurn s).NewState
    let final = loop state
    Scoring.calculateScores final.Players
    |> List.map (fun (_, b) -> b.Total)
    |> List.toArray

/// Play a full game to TargetScore. Returns (cumulative per seat, rounds played).
let playGame (config: GameEngine.GameConfig) (rng: Random) =
    let players = GameEngine.createPlayers config
    let cum = Array.zeroCreate (GameEngine.SeatCount.count config.Seats)
    let mutable round = 1
    let mutable over = false
    while not over do
        let totals = playRound config rng players round
        for i in 0 .. GameEngine.SeatCount.count config.Seats - 1 do
            cum.[i] <- cum.[i] + totals.[i]
        if Array.exists (fun s -> s >= config.TargetScore) cum then over <- true
        round <- round + 1
    cum, round - 1

/// Game winner by variant: Standard = highest score, Laisto = lowest.
/// None on a tie for the deciding score.
let winner (variant: GameVariant) (cum: int[]) =
    let pick cmp =
        let best = cum |> Array.reduce cmp
        match [ for i in 0 .. cum.Length - 1 do if cum.[i] = best then yield i ] with
        | [ single ] -> Some single
        | _ -> None
    match variant with
    | StandardKasino -> pick max
    | LaistoKasino   -> pick min

let run (variant: GameVariant) (playerCount: int) (nGames: int) =
    let wins = Array.zeroCreate playerCount
    let mutable draws = 0
    let mutable totalRounds = 0
    for i in 0 .. nGames - 1 do
        let cum, rounds = playGame (mkConfig variant playerCount) (Random(300000 + i))
        totalRounds <- totalRounds + rounds
        match winner variant cum with
        | Some s -> wins.[s] <- wins.[s] + 1
        | None   -> draws <- draws + 1
    let pct n = 100.0 * float n / float nGames
    printfn "── %A, %d players, %d games (self-play, all Balanced) ──" variant playerCount nGames
    printfn "   seat win%%:   %s"
        (wins |> Array.mapi (fun i w -> sprintf "P%d %.1f%%" i (pct w)) |> String.concat "   ")
    printfn "   draws %.1f%%   avg rounds/game %.2f" (pct draws) (float totalRounds / float nGames)
    printfn ""

let games = 3000
printfn "Kasino self-play harness  (games=%d)\n" games
run StandardKasino 2 games
run StandardKasino 4 games
run LaistoKasino 2 games
run LaistoKasino 4 games
