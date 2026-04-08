namespace Kasino.UI

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Input

// ─────────────────────────────────────────────────────────────
// Input handling: mouse clicks, keyboard, and hit-testing.
// ─────────────────────────────────────────────────────────────

module InputHandler =

    type MouseState =
        { Position: Point
          LeftPressed: bool
          LeftJustClicked: bool
          RightJustClicked: bool }

    type KeyboardState =
        { IsEscapePressed: bool
          IsEnterPressed: bool
          IsLeftPressed: bool
          IsRightPressed: bool
          IsF11Pressed: bool
          NumberPressed: int option }

    type InputState =
        { Mouse: MouseState
          Keyboard: KeyboardState }

    let mutable private prevMouseState = Microsoft.Xna.Framework.Input.MouseState()
    let mutable private prevKeyState   = Microsoft.Xna.Framework.Input.KeyboardState()

    /// Update and return current input state
    let update () =
        let ms = Mouse.GetState()
        let ks = Microsoft.Xna.Framework.Input.Keyboard.GetState()

        let leftJust = ms.LeftButton = ButtonState.Pressed && prevMouseState.LeftButton = ButtonState.Released
        let rightJust = ms.RightButton = ButtonState.Pressed && prevMouseState.RightButton = ButtonState.Released

        let numberPressed =
            [ (Keys.D0, 0); (Keys.NumPad0, 0)
              (Keys.D1, 1); (Keys.D2, 2); (Keys.D3, 3); (Keys.D4, 4)
              (Keys.NumPad1, 1); (Keys.NumPad2, 2); (Keys.NumPad3, 3); (Keys.NumPad4, 4) ]
            |> List.tryFind (fun (key, _) -> ks.IsKeyDown(key) && prevKeyState.IsKeyUp(key))
            |> Option.map snd

        let escapePressed = ks.IsKeyDown(Keys.Escape) && prevKeyState.IsKeyUp(Keys.Escape)
        let enterPressed  = ks.IsKeyDown(Keys.Enter)  && prevKeyState.IsKeyUp(Keys.Enter)
        let leftPressed   = ks.IsKeyDown(Keys.Left)   && prevKeyState.IsKeyUp(Keys.Left)
        let rightPressed  = ks.IsKeyDown(Keys.Right)   && prevKeyState.IsKeyUp(Keys.Right)
        let f11Pressed    = ks.IsKeyDown(Keys.F11)    && prevKeyState.IsKeyUp(Keys.F11)

        prevMouseState <- ms
        prevKeyState   <- ks

        { Mouse =
            { Position = Point(ms.X, ms.Y)
              LeftPressed = ms.LeftButton = ButtonState.Pressed
              LeftJustClicked = leftJust
              RightJustClicked = rightJust }
          Keyboard =
            { IsEscapePressed = escapePressed
              IsEnterPressed  = enterPressed
              IsLeftPressed   = leftPressed
              IsRightPressed  = rightPressed
              IsF11Pressed    = f11Pressed
              NumberPressed   = numberPressed } }

    /// Default input state (no keys pressed, mouse at origin)
    let defaultState: InputState =
        { Mouse =
            { Position = Point.Zero
              LeftPressed = false
              LeftJustClicked = false
              RightJustClicked = false }
          Keyboard =
            { IsEscapePressed = false
              IsEnterPressed = false
              IsLeftPressed = false
              IsRightPressed = false
              IsF11Pressed = false
              NumberPressed = None } }

    /// Check if a point is inside a rectangle
    let hitTest (rect: Rectangle) (point: Point) =
        rect.Contains(point)

    /// Find which card rectangle (if any) was clicked
    let findClickedCard (rects: (int * Rectangle) list) (point: Point) =
        rects |> List.tryFind (fun (_, r) -> hitTest r point) |> Option.map fst
