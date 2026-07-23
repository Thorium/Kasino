namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Mibo.Input

// ─────────────────────────────────────────────────────────────
// Input.
//
// Keyboard goes through Mibo's InputMap: physical keys are bound to semantic
// UI actions once, and InputMapper.subscribeStatic delivers ActionState
// messages whose Started set already carries the edge detection the old
// MonoGame build (and the first cut of this port) did by hand against the
// previous frame's keyboard state.
//
// The mouse stays on the Mouse.listen delta subscription because the screens
// hit-test against the pointer position; deltas accumulate into RawInput and
// each Tick projects the InputState the screens consume, then clears the
// one-frame edges.
// ─────────────────────────────────────────────────────────────

module Input =

    /// Semantic UI actions; every key is bound in [uiMap] below.
    type UiAction =
        | Continue          // confirm / advance (Enter)
        | Back              // back / cancel / quit from the menu (Escape)
        | PrevPage          // rules paging (Left)
        | NextPage          // rules paging (Right)
        | ToggleFullscreen  // F11
        | Pick of int       // menu / capture-modal option N (digit row + numpad)

    /// The one InputMap for the whole app. Screens are modal (menu, game,
    /// rules, ...) so a single static map with per-screen interpretation of
    /// the small action set is enough — no per-screen rebinding.
    let uiMap : InputMap<UiAction> =
        let digitKeys =
            [ 0, KeyCode.D0; 1, KeyCode.D1; 2, KeyCode.D2; 3, KeyCode.D3; 4, KeyCode.D4
              5, KeyCode.D5; 6, KeyCode.D6; 7, KeyCode.D7; 8, KeyCode.D8; 9, KeyCode.D9
              0, KeyCode.Kp0; 1, KeyCode.Kp1; 2, KeyCode.Kp2; 3, KeyCode.Kp3; 4, KeyCode.Kp4
              5, KeyCode.Kp5; 6, KeyCode.Kp6; 7, KeyCode.Kp7; 8, KeyCode.Kp8; 9, KeyCode.Kp9 ]
        let baseMap =
            InputMap.empty
            |> InputMap.key Continue KeyCode.Enter
            |> InputMap.key Back KeyCode.Escape
            |> InputMap.key PrevPage KeyCode.Left
            |> InputMap.key NextPage KeyCode.Right
            |> InputMap.key ToggleFullscreen KeyCode.F11
        (baseMap, digitKeys)
        ||> List.fold (fun m (n, k) -> m |> InputMap.key (Pick n) k)

    type MouseState =
        { Position: Point
          LeftPressed: bool
          LeftJustClicked: bool
          RightJustClicked: bool }

    type InputState =
        { Mouse: MouseState
          /// Actions whose key was pressed since the last Tick.
          Actions: Set<UiAction> }

    /// Did this action start this frame?
    let has (a: UiAction) (i: InputState) = i.Actions.Contains a

    /// The option number picked this frame, if any (0-9).
    let picked (i: InputState) : int option =
        i.Actions |> Seq.tryPick (function Pick n -> Some n | _ -> None)

    /// Raw input accumulated between ticks.
    type RawInput =
        { MouseX: int
          MouseY: int
          LeftHeld: bool
          LeftClicked: bool          // a left-button press edge since last tick
          RightClicked: bool
          Started: Set<UiAction> }   // action edges since last tick

    let emptyRaw =
        { MouseX = 0; MouseY = 0
          LeftHeld = false; LeftClicked = false; RightClicked = false
          Started = Set.empty }

    let defaultState : InputState =
        { Mouse = { Position = Point.Zero; LeftPressed = false; LeftJustClicked = false; RightJustClicked = false }
          Actions = Set.empty }

    /// Fold a mouse delta (position + button press/release edges) into RawInput.
    let applyMouse (d: MouseDelta) (r: RawInput) : RawInput =
        let mutable r2 = { r with MouseX = int d.Position.X; MouseY = int d.Position.Y }
        for b in d.Buttons.Pressed do
            if b = MouseButtonCode.Left then r2 <- { r2 with LeftHeld = true; LeftClicked = true }
            elif b = MouseButtonCode.Right then r2 <- { r2 with RightClicked = true }
        for b in d.Buttons.Released do
            if b = MouseButtonCode.Left then r2 <- { r2 with LeftHeld = false }
        r2

    /// Fold an InputMapper delta into RawInput. Only Started matters here:
    /// the UI acts on press edges, never on held keys.
    let applyActions (s: ActionState<UiAction>) (r: RawInput) : RawInput =
        { r with Started = Set.union r.Started s.Started }

    /// Project accumulated raw input into the per-frame InputState screens consume.
    let project (r: RawInput) : InputState =
        { Mouse =
            { Position = Point(r.MouseX, r.MouseY)
              LeftPressed = r.LeftHeld
              LeftJustClicked = r.LeftClicked
              RightJustClicked = r.RightClicked }
          Actions = r.Started }

    /// Drop per-frame edges after a Tick consumed them (held state persists).
    let clearEdges (r: RawInput) : RawInput =
        { r with LeftClicked = false; RightClicked = false; Started = Set.empty }

    // ── Hit testing (used by screens) ──
    let hitTest (rect: Rectangle) (point: Point) = rect.Contains(point)

    let findClickedCard (rects: (int * Rectangle) list) (point: Point) =
        rects |> List.tryFind (fun (_, r) -> hitTest r point) |> Option.map fst
