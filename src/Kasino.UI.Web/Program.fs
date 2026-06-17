module Kasino.UI.Web.Program

open System
open Fable.Core.JsInterop
open Browser.Dom
open Browser.Types
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Entry point. Owns the active screen, the game RNG, and the
// requestAnimationFrame loop — the web counterpart of the desktop
// KasinoGame (Update/Draw) class.
// ─────────────────────────────────────────────────────────────

type ActiveScreen =
    | Menu of MenuScreen.MenuState
    | Playing of GameScreen.ScreenState
    | Scores of ScoreScreen.ScoreState
    | Rules of RulesScreen.RulesState * ActiveScreen

// Logical drawing space. The height is fixed so card/font sizes stay
// constant; the width tracks the viewport's aspect ratio (set in `resize`)
// so a landscape device fills edge-to-edge instead of letterboxing a 4:3
// board. Every screen lays out relative to these, so a wider logical width
// just spreads the board out.
let mutable screenW = 1024
let mutable screenH = 768

let mutable private screen: ActiveScreen = Menu MenuScreen.initial
let mutable private rng = Random()
let mutable private textures: CardRenderer.CardTextures option = None
let mutable private lastTime = 0.0

let private updateScreen (input: Input.InputState) (dt: float) =
    match screen with
    | Menu menuState ->
        let newMenu = MenuScreen.update input screenW screenH menuState
        match newMenu.Step with
        | MenuScreen.Ready ->
            let config: GameEngine.GameConfig =
                { Variant = newMenu.Variant
                  PlayerCount = newMenu.PlayerCount
                  HumanCount = newMenu.HumanCount
                  Seed = None
                  TargetScore = 16 }
            rng <- Random()
            let players = GameEngine.createPlayers config
            let scores = players |> List.map (fun p -> p.Name, 0) |> Map.ofList
            let gameScreen = GameScreen.create config rng players 1 scores
            screen <- Playing gameScreen
        | MenuScreen.ShowRules ->
            let prevStep =
                match menuState.Step with
                | MenuScreen.VariantSelect -> MenuScreen.VariantSelect
                | MenuScreen.PlayerCountSelect -> MenuScreen.PlayerCountSelect
                | MenuScreen.HumanCountSelect -> MenuScreen.HumanCountSelect
                | _ -> MenuScreen.VariantSelect
            let returnMenu = { newMenu with Step = prevStep }
            screen <- Rules(RulesScreen.create (), Menu returnMenu)
        | _ -> screen <- Menu newMenu

    | Playing gameState ->
        let newGameState = GameScreen.update input dt screenW screenH gameState
        if newGameState.ShowRulesClicked then
            let cleanState = { newGameState with ShowRulesClicked = false }
            screen <- Rules(RulesScreen.create (), Playing cleanState)
        elif newGameState.MenuClicked then
            screen <- Menu MenuScreen.initial
        else
            match newGameState.Phase with
            | GameScreen.RoundOver when newGameState.ContinueClicked ->
                let finalState = newGameState.GameState
                let scoreScreen =
                    ScoreScreen.create
                        finalState.Players
                        newGameState.CumulativeScores
                        newGameState.Config.Variant
                        newGameState.RoundNumber
                        newGameState.Config.TargetScore
                screen <- Scores scoreScreen
            | _ -> screen <- Playing newGameState

    | Scores scoreState ->
        let newScoreState = ScoreScreen.update input screenW screenH scoreState
        if newScoreState.ContinueClicked then
            match newScoreState.Phase with
            | ScoreScreen.GameOver -> screen <- Menu MenuScreen.initial
            | ScoreScreen.RoundSummary ->
                let humanCount =
                    newScoreState.Scores
                    |> List.filter (fun (p, _) -> p.Type = Human)
                    |> List.length
                let config: GameEngine.GameConfig =
                    { Variant = newScoreState.Variant
                      PlayerCount = newScoreState.Scores.Length
                      HumanCount = humanCount
                      Seed = None
                      TargetScore = newScoreState.TargetScore }
                let players = newScoreState.Scores |> List.map fst
                let nextRound = newScoreState.RoundNumber + 1
                let gameScreen = GameScreen.create config rng players nextRound newScoreState.CumulativeScores
                screen <- Playing gameScreen
        else
            screen <- Scores newScoreState

    | Rules(rulesState, returnTo) ->
        let newRules = RulesScreen.update input screenW screenH rulesState
        if newRules.BackClicked then screen <- returnTo
        else screen <- Rules(newRules, returnTo)

let private drawAll (g: Gfx) (input: Input.InputState) =
    Gfx.clear g screenW screenH (Color.rgb 25 50 35)
    match screen, textures with
    | Menu m, _ -> MenuScreen.draw g input m screenW screenH
    | Playing p, Some tex -> GameScreen.draw g input tex p screenW screenH
    | Scores s, _ -> ScoreScreen.draw g input s screenW screenH
    | Rules(r, _), _ -> RulesScreen.draw g input r screenW screenH
    | Playing _, None -> Gfx.fillText g "Loading..." 40.0 40.0 Color.White

// ── Bootstrap ───────────────────────────────────────────────
let private canvas = document.getElementById "game" :?> HTMLCanvasElement

// Size the canvas backing store to the viewport's aspect ratio (fixed
// logical height, clamped logical width). The CSS sizes the element to fit
// while preserving this ratio, so landscape fills the screen with no
// letterbox; pointer coordinates are mapped back through the backing-store
// size in Input.fs.
let private logicalHeight = 768
let private minLogicalWidth = 1024
let private maxLogicalWidth = 2048

let private resize () =
    let vw = window.innerWidth
    let vh = window.innerHeight
    let aspect = if vh > 0.0 then vw / vh else 4.0 / 3.0
    let w =
        int (round (float logicalHeight * aspect))
        |> max minLogicalWidth
        |> min maxLogicalWidth
    screenW <- w
    screenH <- logicalHeight
    canvas?width <- w
    canvas?height <- logicalHeight

resize ()
window.addEventListener ("resize", fun _ -> resize ())

let private ctx: obj = canvas?getContext ("2d")
let private g = Gfx.create ctx

Input.init canvas
CardRenderer.Scale <- 1.0
CardRenderer.loadAll (fun tex -> textures <- Some tex)

let rec private loop (timestamp: float) =
    let dt = if lastTime <= 0.0 then 0.0 else min 0.1 ((timestamp - lastTime) / 1000.0)
    lastTime <- timestamp
    let input = Input.snapshot ()
    updateScreen input dt
    drawAll g input
    window.requestAnimationFrame loop |> ignore

window.requestAnimationFrame loop |> ignore
