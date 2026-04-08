namespace Kasino.UI

open System
open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open FontStashSharp
open Kasino.Domain

// SDL2 P/Invoke for window icon (MonoGame DesktopGL ships SDL2)
module private SdlIcon =
    open System.Runtime.InteropServices

    [<DllImport("SDL2", CallingConvention = CallingConvention.Cdecl)>]
    extern nativeint SDL_CreateRGBSurfaceFrom(nativeint pixels, int width, int height, int depth, int pitch, uint32 Rmask, uint32 Gmask, uint32 Bmask, uint32 Amask)

    [<DllImport("SDL2", CallingConvention = CallingConvention.Cdecl)>]
    extern void SDL_SetWindowIcon(nativeint window, nativeint icon)

    [<DllImport("SDL2", CallingConvention = CallingConvention.Cdecl)>]
    extern void SDL_FreeSurface(nativeint surface)

    let trySetIcon (windowHandle: nativeint) (gd: Graphics.GraphicsDevice) (pngPath: string) =
        try
            use stream = File.OpenRead(pngPath)
            use tex = Graphics.Texture2D.FromStream(gd, stream)
            let w, h = tex.Width, tex.Height
            let pixels: Color array = Array.zeroCreate (w * h)
            tex.GetData(pixels)
            let bytes: byte array = Array.zeroCreate (w * h * 4)
            for i in 0 .. pixels.Length - 1 do
                let p = pixels[i]
                bytes[i * 4]     <- p.R
                bytes[i * 4 + 1] <- p.G
                bytes[i * 4 + 2] <- p.B
                bytes[i * 4 + 3] <- p.A
            let pinned = GCHandle.Alloc(bytes, GCHandleType.Pinned)
            try
                let ptr = pinned.AddrOfPinnedObject()
                let surface =
                    SDL_CreateRGBSurfaceFrom(
                        ptr, w, h, 32, w * 4,
                        0x000000FFu, 0x0000FF00u, 0x00FF0000u, 0xFF000000u)
                if surface <> IntPtr.Zero then
                    SDL_SetWindowIcon(windowHandle, surface)
                    SDL_FreeSurface(surface)
            finally
                pinned.Free()
        with _ -> ()

// ─────────────────────────────────────────────────────────────
// Main MonoGame Game class: manages screens, fonts, textures.
// Stores last input state for draw-time hover feedback.
// ─────────────────────────────────────────────────────────────

type ActiveScreen =
    | Menu of MenuScreen.MenuState
    | Playing of GameScreen.ScreenState
    | Scores of ScoreScreen.ScoreState
    | Rules of RulesScreen.RulesState * ActiveScreen   // rules overlay + screen to return to

type KasinoGame() as this =
    inherit Game()

    let graphics = new GraphicsDeviceManager(this)
    let mutable spriteBatch: SpriteBatch = Unchecked.defaultof<_>
    let mutable fontSystem: FontSystem = Unchecked.defaultof<_>
    let mutable font: SpriteFontBase = Unchecked.defaultof<_>
    let mutable fontSmall: SpriteFontBase = Unchecked.defaultof<_>
    let mutable textures: CardRenderer.CardTextures option = None
    let mutable screen: ActiveScreen = Menu MenuScreen.initial
    let mutable rng = Random()
    let mutable lastInput: InputHandler.InputState = InputHandler.defaultState

    let screenW () = graphics.PreferredBackBufferWidth
    let screenH () = graphics.PreferredBackBufferHeight

    do
        graphics.PreferredBackBufferWidth <- 1024
        graphics.PreferredBackBufferHeight <- 768
        this.IsMouseVisible <- true
        this.Window.Title <- "Kasino - Finnish Card Game"

    override _.Initialize() =
        base.Initialize()

    override _.LoadContent() =
        spriteBatch <- new SpriteBatch(this.GraphicsDevice)

        // Load font using FontStashSharp from a system TTF
        fontSystem <- new FontSystem()

        // Try common Windows font paths
        let fontPaths =
            [ @"C:\Windows\Fonts\segoeui.ttf"
              @"C:\Windows\Fonts\arial.ttf"
              @"C:\Windows\Fonts\consola.ttf"
              @"C:\Windows\Fonts\tahoma.ttf" ]

        let fontPath =
            fontPaths |> List.tryFind File.Exists

        match fontPath with
        | Some path ->
            let fontData = File.ReadAllBytes(path)
            fontSystem.AddFont(fontData)
        | None ->
            // No system font found — try all fonts in the Windows\Fonts directory
            let fontsDir = @"C:\Windows\Fonts"
            let fallbackFont =
                if Directory.Exists(fontsDir) then
                    Directory.GetFiles(fontsDir, "*.ttf") |> Array.tryHead
                else
                    None
            match fallbackFont with
            | Some path ->
                let fontData = File.ReadAllBytes(path)
                fontSystem.AddFont(fontData)
            | None ->
                // Absolute last resort: create a minimal font system that won't crash
                // FontStashSharp requires at least one font; without it GetFont throws.
                // Re-raise as a clear error instead of a cryptic NullReferenceException.
                failwith "No TTF font file found on this system. Please place a .ttf file in C:\\Windows\\Fonts or alongside the executable."

        font <- fontSystem.GetFont(24.0f)
        fontSmall <- fontSystem.GetFont(18.0f)

        // Load card textures
        let contentDir = Path.Combine(AppContext.BaseDirectory, "Content")
        // Also check relative to exe and current dir
        let contentDir =
            if Directory.Exists(contentDir) then contentDir
            else
                let altDir = Path.Combine(Directory.GetCurrentDirectory(), "Content")
                if Directory.Exists(altDir) then altDir
                else
                    let srcDir = Path.Combine(Directory.GetCurrentDirectory(), "src", "Kasino.UI", "Content")
                    if Directory.Exists(srcDir) then srcDir
                    else contentDir  // will fail gracefully in CardRenderer

        textures <- Some (CardRenderer.loadAll this.GraphicsDevice contentDir)

        // Set window icon to ace of hearts via SDL2
        let iconPath = Path.Combine(contentDir, "cards", "he1.png")
        if File.Exists(iconPath) then
            SdlIcon.trySetIcon this.Window.Handle this.GraphicsDevice iconPath

        // Set card scale based on screen size
        CardRenderer.Scale <- float32 (screenH()) / 768.0f * 1.0f

    override _.Update(gameTime) =
        let input = InputHandler.update()
        lastInput <- input
        let dt = gameTime.ElapsedGameTime.TotalSeconds

        match screen with
        | Menu menuState ->
            let newMenu = MenuScreen.update input (screenW()) (screenH()) menuState
            match newMenu.Step with
            | MenuScreen.Ready ->
                // Start the game
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
                // Open rules from menu; return to previous menu step
                let prevStep =
                    match menuState.Step with
                    | MenuScreen.VariantSelect -> MenuScreen.VariantSelect
                    | MenuScreen.PlayerCountSelect -> MenuScreen.PlayerCountSelect
                    | MenuScreen.HumanCountSelect -> MenuScreen.HumanCountSelect
                    | _ -> MenuScreen.VariantSelect
                let returnMenu = { newMenu with Step = prevStep }
                screen <- Rules (RulesScreen.create(), Menu returnMenu)
            | _ ->
                screen <- Menu newMenu

        | Playing gameState ->
            let newGameState = GameScreen.update input dt (screenW()) (screenH()) gameState

            if newGameState.ShowRulesClicked then
                // Open rules from game; return to game state (reset flag)
                let cleanState = { newGameState with ShowRulesClicked = false }
                screen <- Rules (RulesScreen.create(), Playing cleanState)
            elif newGameState.MenuClicked then
                // Return to main menu
                screen <- Menu MenuScreen.initial
            else

            match newGameState.Phase with
            | GameScreen.RoundOver when newGameState.ContinueClicked ->
                // Show score screen (triggered by button tap or Enter)
                let finalState = newGameState.GameState
                let scoreScreen =
                    ScoreScreen.create
                        finalState.Players
                        newGameState.CumulativeScores
                        newGameState.Config.Variant
                        newGameState.RoundNumber
                        newGameState.Config.TargetScore
                screen <- Scores scoreScreen

            | _ ->
                screen <- Playing newGameState

        | Scores scoreState ->
            let newScoreState = ScoreScreen.update input (screenW()) (screenH()) scoreState
            if newScoreState.ContinueClicked then
                match newScoreState.Phase with
                | ScoreScreen.GameOver ->
                    // Return to menu
                    screen <- Menu MenuScreen.initial
                | ScoreScreen.RoundSummary ->
                    // Start next round
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

        | Rules (rulesState, returnTo) ->
            let newRules = RulesScreen.update input (screenW()) (screenH()) rulesState
            if newRules.BackClicked then
                screen <- returnTo
            else
                screen <- Rules (newRules, returnTo)

        // Exit on Escape from menu
        match screen with
        | Menu _ when Keyboard.GetState().IsKeyDown(Keys.Escape) ->
            this.Exit()
        | _ -> ()

        // Toggle fullscreen on F11
        if input.Keyboard.IsF11Pressed then
            graphics.IsFullScreen <- not graphics.IsFullScreen
            graphics.ApplyChanges()
            CardRenderer.Scale <- float32 (screenH()) / 768.0f * 1.0f

        base.Update(gameTime)

    override _.Draw(gameTime) =
        this.GraphicsDevice.Clear(Color(25, 50, 35))

        spriteBatch.Begin(SpriteSortMode.Deferred, BlendState.AlphaBlend, SamplerState.PointClamp)

        match screen, textures with
        | Menu menuState, _ ->
            MenuScreen.draw spriteBatch font lastInput menuState (screenW()) (screenH())
        | Playing gameState, Some tex ->
            GameScreen.draw spriteBatch font lastInput tex gameState (screenW()) (screenH())
        | Scores scoreState, _ ->
            ScoreScreen.draw spriteBatch font lastInput scoreState (screenW()) (screenH())
        | Rules (rulesState, _), _ ->
            RulesScreen.draw spriteBatch font lastInput rulesState (screenW()) (screenH())
        | _ -> ()

        spriteBatch.End()

        base.Draw(gameTime)
