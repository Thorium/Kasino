namespace Kasino.Mibo

open System
open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish
open Mibo.Elmish.Graphics
open Mibo.Elmish.Graphics2D
open Mibo.Input
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Top-level Elmish program: a functional Model-View-Update loop that owns the
// active screen and bridges the screens, mirroring the transitions the
// MonoGame build performed imperatively in KasinoGame.Update.
//
// The window is fixed 1024x768 (MonoGame windows aren't user-resizable by
// default), so screen dimensions are constants rather than threaded from the
// GameContext into every update.
// ─────────────────────────────────────────────────────────────

module Game =

    let WindowW = 1024
    let WindowH = 768

    /// The MonoGame GraphicsDeviceManager, captured at composition time so F11
    /// can toggle fullscreen. The backbuffer stays 1024x768 and is hardware-
    /// scaled to the display, so layout and mouse hit-testing are unchanged.
    let mutable private graphicsManager : GraphicsDeviceManager = Unchecked.defaultof<_>

    /// The currently active screen; Rules/Options are overlays that remember
    /// the screen to return to (same shape as the MonoGame build).
    type ActiveScreen =
        | Menu of MenuScreen.MenuState
        | Playing of GameScreen.ScreenState
        | Scores of ScoreScreen.ScoreState
        | Rules of RulesScreen.RulesState * ActiveScreen
        | Options of OptionsScreen.OptionsState * ActiveScreen

    type Model =
        { Screen: ActiveScreen
          Settings: Settings.GameSettings
          Rng: Random
          Textures: CardRenderer.CardTextures option
          Font: SpriteFont option
          Input: Input.RawInput }

    type Msg =
        | Tick of GameTime
        | MouseEvent of MouseDelta
        | ActionsChanged of ActionState<Input.UiAction>

    /// Choose the card back for a new game: random scenic back if enabled,
    /// otherwise a fixed back so it stays constant.
    let private applyCardBack (rng: Random) (config: GameEngine.GameConfig) (tex: CardRenderer.CardTextures) =
        if config.Settings.RandomCardBacks then CardRenderer.pickRandomBack rng tex
        elif tex.Backs.Length > 0 then tex.Back <- tex.Backs[0]

    let private findContentDir () =
        let d = Path.Combine(AppContext.BaseDirectory, "Content")
        if Directory.Exists d then d
        else
            let alt = Path.Combine(Directory.GetCurrentDirectory(), "Content")
            if Directory.Exists alt then alt
            else
                let src = Path.Combine(Directory.GetCurrentDirectory(), "src", "Kasino.UI.Mibo", "Content")
                if Directory.Exists src then src else d

    // ── Init ──

    /// Fresh starting model with no graphics resources attached. The update
    /// loop only touches Textures/Font through Option, so this model is also
    /// the complete headless starting state.
    let private freshModel () =
        { Screen = Menu MenuScreen.initial
          Settings = Settings.defaultSettings
          Rng = Random()
          Textures = None
          Font = None
          Input = Input.emptyRaw }

    let init (ctx: GameContext) : struct (Model * Cmd<Msg>) =
        let device = MonoGameGameContext.getGraphicsDevice ctx
        let textures = CardRenderer.loadAll device (findContentDir ())
        CardRenderer.Scale <- float32 WindowH / 768.0f
        let assets = GameContext.getService<IAssets> ctx
        let font = assets.Font("fonts/UI")
        struct ({ freshModel () with Textures = Some textures; Font = Some font }, Cmd.none)

    /// Init for the headless runtime: no window, no graphics services.
    let initHeadless (_ctx: GameContext) : struct (Model * Cmd<Msg>) =
        struct (freshModel (), Cmd.none)

    // ── Screen transitions (mirrors KasinoGame.Update) ──
    let private stepScreens (model: Model) (input: Input.InputState) (dt: float) : Model =
        let w, h = WindowW, WindowH
        match model.Screen with
        | Menu menuState ->
            let newMenu = MenuScreen.update input w h menuState
            match newMenu.Step with
            | MenuScreen.Ready ->
                let config : GameEngine.GameConfig =
                    { Variant = newMenu.Variant
                      Seats = GameEngine.SeatCount.ofIntOrDefault newMenu.PlayerCount
                      HumanCount = newMenu.HumanCount
                      Seed = None
                      TargetScore = 16
                      Settings = model.Settings }
                let rng = Random()
                let players = GameEngine.createPlayers config
                let scores = players |> List.map (fun p -> p.Name, 0) |> Map.ofList
                let gameScreen = GameScreen.create config rng players 1 scores Scoring.CarryOver.zero
                model.Textures |> Option.iter (applyCardBack rng config)
                { model with Screen = Playing gameScreen; Rng = rng }
            | MenuScreen.ShowOptions ->
                let prevStep =
                    match menuState.Step with
                    | MenuScreen.VariantSelect -> MenuScreen.VariantSelect
                    | MenuScreen.PlayerCountSelect -> MenuScreen.PlayerCountSelect
                    | MenuScreen.HumanCountSelect -> MenuScreen.HumanCountSelect
                    | _ -> MenuScreen.VariantSelect
                let returnMenu = { newMenu with Step = prevStep }
                { model with Screen = Options (OptionsScreen.create model.Settings, Menu returnMenu) }
            | MenuScreen.ShowRules ->
                let prevStep =
                    match menuState.Step with
                    | MenuScreen.VariantSelect -> MenuScreen.VariantSelect
                    | MenuScreen.PlayerCountSelect -> MenuScreen.PlayerCountSelect
                    | MenuScreen.HumanCountSelect -> MenuScreen.HumanCountSelect
                    | _ -> MenuScreen.VariantSelect
                let returnMenu = { newMenu with Step = prevStep }
                { model with Screen = Rules (RulesScreen.create (), Menu returnMenu) }
            | _ ->
                { model with Screen = Menu newMenu }

        | Playing gameState ->
            let newGameState = GameScreen.update input dt w h gameState
            if newGameState.ShowRulesClicked then
                let cleanState = { newGameState with ShowRulesClicked = false }
                { model with Screen = Rules (RulesScreen.create (), Playing cleanState) }
            elif newGameState.MenuClicked then
                { model with Screen = Menu MenuScreen.initial }
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
                            newGameState.Carry
                    { model with Screen = Scores scoreScreen }
                | _ ->
                    { model with Screen = Playing newGameState }

        | Scores scoreState ->
            let newScoreState = ScoreScreen.update input w h scoreState
            if newScoreState.ContinueClicked then
                match newScoreState.Phase with
                | ScoreScreen.GameOver ->
                    { model with Screen = Menu MenuScreen.initial }
                | ScoreScreen.RoundSummary ->
                    let humanCount =
                        newScoreState.Scores
                        |> List.filter (fun (p, _) -> p.Type = Human)
                        |> List.length
                    let config : GameEngine.GameConfig =
                        { Variant = newScoreState.Variant
                          Seats = GameEngine.SeatCount.ofIntOrDefault newScoreState.Scores.Length
                          HumanCount = humanCount
                          Seed = None
                          TargetScore = newScoreState.TargetScore
                          Settings = model.Settings }
                    let players = newScoreState.Scores |> List.map fst
                    let nextRound = newScoreState.RoundNumber + 1
                    let gameScreen = GameScreen.create config model.Rng players nextRound newScoreState.CumulativeScores newScoreState.CarryOut
                    { model with Screen = Playing gameScreen }
            else
                { model with Screen = Scores newScoreState }

        | Rules (rulesState, returnTo) ->
            let newRules = RulesScreen.update input w h rulesState
            if newRules.BackClicked then { model with Screen = returnTo }
            else { model with Screen = Rules (newRules, returnTo) }

        | Options (optionsState, returnTo) ->
            let newOptions = OptionsScreen.update input w h optionsState
            if newOptions.BackClicked then
                { model with Settings = newOptions.Settings; Screen = returnTo }
            else
                { model with Screen = Options (newOptions, returnTo) }

    // ── Update ──
    let update (msg: Msg) (model: Model) : struct (Model * Cmd<Msg>) =
        match msg with
        | MouseEvent d -> struct ({ model with Input = Input.applyMouse d model.Input }, Cmd.none)
        | ActionsChanged s -> struct ({ model with Input = Input.applyActions s model.Input }, Cmd.none)
        | Tick gt ->
            let dt = gt.ElapsedGameTime.TotalSeconds
            let input = Input.project model.Input
            let screenAtStart = model.Screen
            let model2 = stepScreens model input dt
            // F11 toggles fullscreen (safe to ApplyChanges during Update).
            if Input.has Input.ToggleFullscreen input && not (obj.ReferenceEquals(graphicsManager, null)) then
                graphicsManager.IsFullScreen <- not graphicsManager.IsFullScreen
                graphicsManager.ApplyChanges()
            // Back quits only when we began the frame already on the menu (so
            // an Escape that closed an overlay back to the menu doesn't also quit).
            let quit =
                match screenAtStart with
                | Menu _ when Input.has Input.Back input -> true
                | _ -> false
            let model3 = { model2 with Input = Input.clearEdges model2.Input }
            let cmd = if quit then Cmd.signalExit else Cmd.none
            struct (model3, cmd)

    // ── View ──
    let view (_ctx: GameContext) (model: Model) (buffer: RenderBuffer2D) =
        let w, h = WindowW, WindowH
        // Since Mibo 3.x sprites go through a SpriteBatch whose sampler the
        // renderer tracks per batch (default LinearClamp), so the 2.x-era
        // device-level PointClamp poke is both dead and unnecessary: the grey
        // fringe on rotated (scatter) cards came from the device default being
        // linear *wrap*, and clamp sampling never wraps.
        // The dark green background is the renderer's ClearColor (see create).

        match model.Font with
        | Some font ->
            let input = Input.project model.Input
            match model.Screen with
            | Menu m -> MenuScreen.draw buffer font model.Textures input m w h
            | Playing g ->
                match model.Textures with
                | Some tex -> GameScreen.draw buffer font input tex g w h
                | None -> ()
            | Scores s -> ScoreScreen.draw buffer font input s w h
            | Rules (r, _) -> RulesScreen.draw buffer font model.Textures input r w h
            | Options (o, _) -> OptionsScreen.draw buffer font input o w h
        | None -> ()

    // ── Subscriptions ──
    let subscribe (ctx: GameContext) (_model: Model) : Sub<Msg> =
        Sub.batch2(
            Mouse.listen MouseEvent ctx,
            InputMapper.subscribeStatic Input.uiMap ActionsChanged ctx)

    // ── Program composition ──
    let create () : MonoGameProgram<Model, Msg> =
        Program.mkProgram init update
        |> Program.withConfig (fun cfg ->
            { cfg with Width = WindowW; Height = WindowH; Title = "Kasino - Finnish Card Game"; TargetFPS = 60 })
        |> Program.withAssets
        |> Program.withInput
        |> Program.withSubscription subscribe
        |> Program.withTick Tick
        // The renderer's clear doubles as the dark-green backdrop behind the
        // felt table (the default config would clear to black and the view
        // would then have to paint the backdrop again itself).
        |> Program.withRenderer (fun () ->
            Renderer2D.createWith { ClearColor = ValueSome (Color(25, 50, 35)) } view)
        |> MonoGameProgram.ofProgram
        |> MonoGameProgram.withConfig (fun (game, deviceManager) ->
            game.Content.RootDirectory <- "Content"
            graphicsManager <- deviceManager)

    /// The same Elmish loop with no renderer, window, or input services — for
    /// tests and server-side simulation (Mibo headless mode). Input is injected
    /// by dispatching MouseEvent / ActionsChanged messages into the runner.
    let createHeadless () : HeadlessProgram<Model, Msg> =
        HeadlessProgram.mkHeadless initHeadless update
        |> HeadlessProgram.withTick Tick
