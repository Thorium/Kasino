namespace Kasino.Mibo

open System
open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish
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
        | KeyDown of KeyCode

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
    let init (ctx: GameContext) : struct (Model * Cmd<Msg>) =
        let device = MonoGameGameContext.getGraphicsDevice ctx
        let textures = CardRenderer.loadAll device (findContentDir ())
        CardRenderer.Scale <- float32 WindowH / 768.0f
        let assets = GameContext.getService<IAssets> ctx
        let font = assets.Font("fonts/UI")
        let model =
            { Screen = Menu MenuScreen.initial
              Settings = Settings.defaultSettings
              Rng = Random()
              Textures = Some textures
              Font = Some font
              Input = Input.emptyRaw }
        struct (model, Cmd.none)

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
                let gameScreen = GameScreen.create config rng players 1 scores
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
                    let gameScreen = GameScreen.create config model.Rng players nextRound newScoreState.CumulativeScores
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
        | KeyDown k -> struct ({ model with Input = Input.applyKeyDown k model.Input }, Cmd.none)
        | Tick gt ->
            let dt = gt.ElapsedGameTime.TotalSeconds
            let input = Input.project model.Input
            let screenAtStart = model.Screen
            let model2 = stepScreens model input dt
            // F11 toggles fullscreen (safe to ApplyChanges during Update).
            if input.Keyboard.IsF11Pressed && not (obj.ReferenceEquals(graphicsManager, null)) then
                graphicsManager.IsFullScreen <- not graphicsManager.IsFullScreen
                graphicsManager.ApplyChanges()
            // Escape quits only when we began the frame already on the menu (so
            // an Escape that closed an overlay back to the menu doesn't also quit).
            let quit =
                match screenAtStart with
                | Menu _ when input.Keyboard.IsEscapePressed -> true
                | _ -> false
            let model3 = { model2 with Input = Input.clearEdges model2.Input }
            let cmd = if quit then Cmd.signalExit else Cmd.none
            struct (model3, cmd)

    // ── View ──
    let view (ctx: GameContext) (model: Model) (buffer: RenderBuffer2D) =
        let w, h = WindowW, WindowH
        // Mibo draws textured sprites through a PrimitiveBatch that never sets a
        // sampler, so card art samples with the GraphicsDevice default (linear
        // wrap) — rotated (scatter) cards then bleed a grey fringe at their
        // edges. Force point sampling on the device for the sprite path; card
        // sprites (low layers) flush before the first text SpriteBatch, which
        // resets the sampler to linear for smooth downscaled text.
        let gd = MonoGameGameContext.getGraphicsDevice ctx
        gd.SamplerStates[0] <- SamplerState.PointClamp
        // Dark green background behind everything (the felt table draws its own).
        Render.fill buffer 0<RenderLayer> (Color(25, 50, 35)) (Rectangle(0, 0, w, h))

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
            Keyboard.onPressed KeyDown ctx)

    // ── Program composition ──
    let create () : MonoGameProgram<Model, Msg> =
        Program.mkProgram init update
        |> Program.withConfig (fun cfg ->
            { cfg with Width = WindowW; Height = WindowH; Title = "Kasino - Finnish Card Game"; TargetFPS = 60 })
        |> Program.withAssets
        |> Program.withInput
        |> Program.withSubscription subscribe
        |> Program.withTick Tick
        |> Program.withRenderer (fun () -> Renderer2D.create view)
        |> MonoGameProgram.ofProgram
        |> MonoGameProgram.withConfig (fun (game, deviceManager) ->
            game.Content.RootDirectory <- "Content"
            graphicsManager <- deviceManager)
