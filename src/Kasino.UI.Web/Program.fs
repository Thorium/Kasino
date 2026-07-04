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
    | Options of OptionsScreen.OptionsState * ActiveScreen

// Logical drawing space. The height is fixed so card/font sizes stay
// constant; the width tracks the viewport's aspect ratio (set in `resize`)
// so a landscape device fills edge-to-edge instead of letterboxing a 4:3
// board. Every screen lays out relative to these, so a wider logical width
// just spreads the board out.
let mutable screenW = 1024
let mutable screenH = 768

let mutable private screen: ActiveScreen = Menu MenuScreen.initial
let mutable private settings = Settings.defaultSettings
let mutable private rng = Random()
let mutable private textures: CardRenderer.CardTextures option = None
let mutable private lastTime = 0.0

/// Choose the card back for a new game: random scenic back if enabled,
/// otherwise a single fixed back so it stays constant.
let private applyCardBack (config: GameEngine.GameConfig) (tex: CardRenderer.CardTextures) =
    if config.Settings.RandomCardBacks then CardRenderer.pickRandomBack rng tex
    elif tex.Backs.Length > 0 then tex.Back <- tex.Backs[0]

let private updateScreen (input: Input.InputState) (dt: float) =
    match screen with
    | Menu menuState ->
        let newMenu = MenuScreen.update input screenW screenH menuState
        match newMenu.Step with
        | MenuScreen.Ready ->
            let config: GameEngine.GameConfig =
                { Variant = newMenu.Variant
                  Seats = GameEngine.SeatCount.ofIntOrDefault newMenu.PlayerCount
                  HumanCount = newMenu.HumanCount
                  Seed = None
                  TargetScore = 16
                  Settings = settings }
            rng <- Random()
            let players = GameEngine.createPlayers config
            let scores = players |> List.map (fun p -> p.Name, 0) |> Map.ofList
            let gameScreen = GameScreen.create config rng players 1 scores
            textures |> Option.iter (applyCardBack config)
            screen <- Playing gameScreen
        | MenuScreen.ShowOptions ->
            // Return to a real menu step, not the transitional ShowOptions
            // (which would instantly re-open Options and trap the Back button).
            let prevStep =
                match menuState.Step with
                | MenuScreen.VariantSelect -> MenuScreen.VariantSelect
                | MenuScreen.PlayerCountSelect -> MenuScreen.PlayerCountSelect
                | MenuScreen.HumanCountSelect -> MenuScreen.HumanCountSelect
                | _ -> MenuScreen.VariantSelect
            let returnMenu = { newMenu with Step = prevStep }
            screen <- Options(OptionsScreen.create settings, Menu returnMenu)
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
                      Seats = GameEngine.SeatCount.ofIntOrDefault newScoreState.Scores.Length
                      HumanCount = humanCount
                      Seed = None
                      TargetScore = newScoreState.TargetScore
                      Settings = settings }
                let players = newScoreState.Scores |> List.map fst
                let nextRound = newScoreState.RoundNumber + 1
                let gameScreen = GameScreen.create config rng players nextRound newScoreState.CumulativeScores
                // Keep the same card back for every round of this game; the
                // back is randomized only when a new game starts (from menu).
                screen <- Playing gameScreen
        else
            screen <- Scores newScoreState

    | Rules(rulesState, returnTo) ->
        let newRules = RulesScreen.update input screenW screenH rulesState
        if newRules.BackClicked then screen <- returnTo
        else screen <- Rules(newRules, returnTo)

    | Options(optionsState, returnTo) ->
        let newOptions = OptionsScreen.update input screenW screenH optionsState
        if newOptions.BackClicked then
            settings <- newOptions.Settings
            screen <- returnTo
        else
            screen <- Options(newOptions, returnTo)

let private drawAll (g: Gfx) (input: Input.InputState) =
    Gfx.clear g screenW screenH (Color.rgb 25 50 35)
    match screen, textures with
    | Menu m, tex -> MenuScreen.draw g tex input m screenW screenH
    | Playing p, Some tex -> GameScreen.draw g input tex p screenW screenH
    | Scores s, _ -> ScoreScreen.draw g input s screenW screenH
    | Rules(r, _), tex -> RulesScreen.draw g tex input r screenW screenH
    | Options(o, _), _ -> OptionsScreen.draw g input o screenW screenH
    | Playing _, None -> Gfx.fillText g "Loading..." 40.0 40.0 Color.White

// ── Bootstrap ───────────────────────────────────────────────
let private canvas = document.getElementById "game" :?> HTMLCanvasElement
let private ctx: obj = canvas?getContext ("2d")
let private g = Gfx.create ctx

// The logical drawing space tracks the viewport's aspect ratio so the
// canvas can FILL the screen (scaling up as well as down — large screens
// used to show the board at its intrinsic 1024×768 size surrounded by
// margins). Wider than 4:3, the space stays 768 tall and grows sideways;
// squarer/taller than 4:3, it stays 1024 wide and grows downward instead,
// so half-tiled and portrait-ish windows fill too. Only beyond the clamps
// (ultra-wide, phone-portrait) does it letter/pillarbox rather than
// stretch. The backing store is allocated at device resolution with a
// uniform transform back to logical coordinates so the upscaled board and
// especially text stay crisp. Pointer positions are mapped into the
// logical space in Input.fs.
let private baseLogicalHeight = 768
let private baseLogicalWidth = 1024
let private maxLogicalWidth = 2048
let private maxLogicalHeight = 1280

let private resize () =
    let vw = window.innerWidth
    let vh = window.innerHeight
    // Some embedding contexts report a 0×0 viewport at script evaluation;
    // keep the current size and wait for the resize event that follows.
    if vw > 0.0 && vh > 0.0 then
        let aspect = vw / vh
        let w, h =
            if aspect >= float baseLogicalWidth / float baseLogicalHeight then
                // Wide: fixed height, width tracks the aspect (clamped)
                let w = int (round (float baseLogicalHeight * aspect)) |> min maxLogicalWidth
                (w, baseLogicalHeight)
            else
                // Tall/square: fixed width, height tracks the aspect (clamped)
                let h = int (round (float baseLogicalWidth / aspect)) |> min maxLogicalHeight
                (baseLogicalWidth, h)
        screenW <- w
        screenH <- h
        // CSS box: fill the viewport while preserving the logical aspect
        // ratio. Within the clamps the logical aspect equals the viewport's,
        // so the board runs edge-to-edge.
        let logicalAspect = float w / float h
        let cssW, cssH =
            if aspect > logicalAspect then (vh * logicalAspect, vh)
            else (vw, vw / logicalAspect)
        // Backing store at device resolution (capped at 2x) for crispness.
        let dpr = window.devicePixelRatio |> max 1.0 |> min 2.0
        let scale = cssH * dpr / float h
        canvas?width <- int (round (float w * scale))
        canvas?height <- int (round (float h * scale))
        canvas?style?width <- sprintf "%fpx" cssW
        canvas?style?height <- sprintf "%fpx" cssH
        // Assigning canvas.width resets the 2D context state, so reapply
        // the logical-coordinates transform afterwards.
        ctx?setTransform (scale, 0.0, 0.0, scale, 0.0, 0.0)
        Input.setLogicalSize w h

resize ()
window.addEventListener ("resize", fun _ -> resize ())

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
