namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Mibo.Input

// ─────────────────────────────────────────────────────────────
// Input.
//
// The MonoGame build polled Mouse/Keyboard.GetState() every Update and
// edge-detected against the previous frame. Mibo is push-based: it delivers
// input as per-frame subscription messages. So we accumulate raw input into a
// RawInput value and, on each Tick, project the exact same InputState the
// screens expect and then clear the one-frame edges. This keeps every screen's
// `update` logic identical to the MonoGame version.
// ─────────────────────────────────────────────────────────────

module Input =

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

    /// Raw input accumulated between ticks.
    type RawInput =
        { MouseX: int
          MouseY: int
          LeftHeld: bool
          LeftClicked: bool          // a left-button press edge since last tick
          RightClicked: bool
          PressedKeys: KeyCode list } // key-down edges since last tick (newest first)

    let emptyRaw =
        { MouseX = 0; MouseY = 0
          LeftHeld = false; LeftClicked = false; RightClicked = false
          PressedKeys = [] }

    let defaultState : InputState =
        { Mouse = { Position = Point.Zero; LeftPressed = false; LeftJustClicked = false; RightJustClicked = false }
          Keyboard =
            { IsEscapePressed = false; IsEnterPressed = false; IsLeftPressed = false
              IsRightPressed = false; IsF11Pressed = false; NumberPressed = None } }

    /// Fold a mouse delta (position + button press/release edges) into RawInput.
    let applyMouse (d: MouseDelta) (r: RawInput) : RawInput =
        let mutable r2 = { r with MouseX = int d.Position.X; MouseY = int d.Position.Y }
        for b in d.Buttons.Pressed do
            if b = MouseButtonCode.Left then r2 <- { r2 with LeftHeld = true; LeftClicked = true }
            elif b = MouseButtonCode.Right then r2 <- { r2 with RightClicked = true }
        for b in d.Buttons.Released do
            if b = MouseButtonCode.Left then r2 <- { r2 with LeftHeld = false }
        r2

    /// Fold a key-down edge into RawInput.
    let applyKeyDown (k: KeyCode) (r: RawInput) : RawInput =
        { r with PressedKeys = k :: r.PressedKeys }

    let private numberOf (k: KeyCode) =
        if   k = KeyCode.D0 || k = KeyCode.Kp0 then Some 0
        elif k = KeyCode.D1 || k = KeyCode.Kp1 then Some 1
        elif k = KeyCode.D2 || k = KeyCode.Kp2 then Some 2
        elif k = KeyCode.D3 || k = KeyCode.Kp3 then Some 3
        elif k = KeyCode.D4 || k = KeyCode.Kp4 then Some 4
        elif k = KeyCode.D5 || k = KeyCode.Kp5 then Some 5
        elif k = KeyCode.D6 || k = KeyCode.Kp6 then Some 6
        elif k = KeyCode.D7 || k = KeyCode.Kp7 then Some 7
        elif k = KeyCode.D8 || k = KeyCode.Kp8 then Some 8
        elif k = KeyCode.D9 || k = KeyCode.Kp9 then Some 9
        else None

    /// Project accumulated raw input into the per-frame InputState screens consume.
    let project (r: RawInput) : InputState =
        let has k = List.contains k r.PressedKeys
        // Oldest-first so the first number key pressed this frame wins.
        let numberPressed = r.PressedKeys |> List.rev |> List.tryPick numberOf
        { Mouse =
            { Position = Point(r.MouseX, r.MouseY)
              LeftPressed = r.LeftHeld
              LeftJustClicked = r.LeftClicked
              RightJustClicked = r.RightClicked }
          Keyboard =
            { IsEscapePressed = has KeyCode.Escape
              IsEnterPressed = has KeyCode.Enter
              IsLeftPressed = has KeyCode.Left
              IsRightPressed = has KeyCode.Right
              IsF11Pressed = has KeyCode.F11
              NumberPressed = numberPressed } }

    /// Drop per-frame edges after a Tick consumed them (held state persists).
    let clearEdges (r: RawInput) : RawInput =
        { r with LeftClicked = false; RightClicked = false; PressedKeys = [] }

    // ── Hit testing (used by screens) ──
    let hitTest (rect: Rectangle) (point: Point) = rect.Contains(point)

    let findClickedCard (rects: (int * Rectangle) list) (point: Point) =
        rects |> List.tryFind (fun (_, r) -> hitTest r point) |> Option.map fst
