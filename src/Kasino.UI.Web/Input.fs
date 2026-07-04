namespace Kasino.UI.Web

open Fable.Core.JsInterop
open Browser.Dom
open Browser.Types

// ─────────────────────────────────────────────────────────────
// Browser input handling. Pointer events (mouse + touch) and the
// keyboard are accumulated by event listeners; `snapshot` produces
// a per-frame immutable InputState with edge-detected "just pressed"
// flags, mirroring the desktop InputHandler.update() semantics.
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
          NumberPressed: int option }

    type InputState =
        { Mouse: MouseState
          Keyboard: KeyboardState }

    // ── Accumulated state mutated by event listeners ────────
    let mutable private mouseX = 0
    let mutable private mouseY = 0
    let mutable private leftDown = false
    let mutable private leftJust = false
    let mutable private rightJust = false
    let mutable private downKeys: Set<string> = Set.empty
    let mutable private escJust = false
    let mutable private enterJust = false
    let mutable private leftKeyJust = false
    let mutable private rightKeyJust = false
    let mutable private numberJust: int option = None

    let mutable private canvasEl: HTMLCanvasElement = Unchecked.defaultof<_>

    // Logical drawing-space size, kept in sync by Program's resize handler.
    // The backing store is DPR-scaled for crisp rendering, so it can no
    // longer be read directly to map pointer positions.
    let mutable private logicalW = 1024.0
    let mutable private logicalH = 768.0

    /// Record the logical drawing-space size (called from Program.resize).
    let setLogicalSize (w: int) (h: int) =
        logicalW <- float w
        logicalH <- float h

    /// Map viewport client coordinates to the canvas's logical space. The
    /// logical size is set at runtime from the viewport aspect ratio, so
    /// read the synced fields rather than assuming 1024x768.
    let private updatePos (clientX: float) (clientY: float) =
        let rect = canvasEl.getBoundingClientRect ()
        if rect.width > 0.0 && rect.height > 0.0 then
            mouseX <- int ((clientX - rect.left) * (logicalW / rect.width))
            mouseY <- int ((clientY - rect.top) * (logicalH / rect.height))

    /// Parse a 0-4 shortcut from a KeyboardEvent. Uses `key` (top row / NumPad
    /// with NumLock on) and falls back to the physical `code` ("Digit3",
    /// "Numpad3") so the NumPad still works with NumLock off.
    let private parseDigit (key: string) (code: string) =
        match key with
        | "0" -> Some 0
        | "1" -> Some 1
        | "2" -> Some 2
        | "3" -> Some 3
        | "4" -> Some 4
        | _ ->
            match code with
            | "Digit0" | "Numpad0" -> Some 0
            | "Digit1" | "Numpad1" -> Some 1
            | "Digit2" | "Numpad2" -> Some 2
            | "Digit3" | "Numpad3" -> Some 3
            | "Digit4" | "Numpad4" -> Some 4
            | _ -> None

    /// Wire up pointer + keyboard listeners against the given canvas.
    let init (canvas: HTMLCanvasElement) =
        canvasEl <- canvas

        canvas.addEventListener ("pointermove", fun e ->
            let m = e :?> MouseEvent
            updatePos m.clientX m.clientY)

        canvas.addEventListener ("pointerdown", fun e ->
            let m = e :?> MouseEvent
            updatePos m.clientX m.clientY
            if m.button = 0 then
                leftDown <- true
                leftJust <- true
            elif m.button = 2 then
                rightJust <- true
            // Capture the pointer so a drag keeps receiving move/up events even
            // when the cursor leaves the canvas bounds (smooth mouse + touch drag).
            (try canvas?setPointerCapture (e?pointerId) |> ignore with _ -> ())
            e.preventDefault ())

        window.addEventListener ("pointerup", fun e ->
            let m = e :?> MouseEvent
            if m.button = 0 then leftDown <- false)

        // A cancelled pointer (touch interrupted, pointer lost, window blur)
        // must release the held button so a drag can't get stuck mid-gesture.
        // Held keys are cleared too: their keyup fires at the other window,
        // so they would otherwise stay latched and dead for the session.
        let release (_: Event) =
            leftDown <- false
            downKeys <- Set.empty
        window.addEventListener ("pointercancel", release)
        window.addEventListener ("blur", release)

        // Suppress the context menu so right-click can be used as input.
        canvas.addEventListener ("contextmenu", fun e -> e.preventDefault ())

        window.addEventListener ("keydown", fun e ->
            let k = e :?> KeyboardEvent
            // Edge-detect: only fire "just pressed" on the down transition,
            // ignoring auto-repeat while the key is held. Track the physical
            // key (code), not the logical key string: modifiers can change
            // the string between press and release (press "3", hold Shift,
            // release delivers "#"), which would latch the entry forever.
            let code = k.code
            if not (Set.contains code downKeys) then
                downKeys <- Set.add code downKeys
                match k.key with
                | "Escape" -> escJust <- true
                | "Enter" -> enterJust <- true
                | "ArrowLeft" -> leftKeyJust <- true
                | "ArrowRight" -> rightKeyJust <- true
                | key ->
                    match parseDigit key code with
                    | Some n -> numberJust <- Some n
                    | None -> ())

        window.addEventListener ("keyup", fun e ->
            let k = e :?> KeyboardEvent
            downKeys <- Set.remove k.code downKeys)

    /// Produce the current frame's input and reset per-frame edge flags.
    let snapshot () : InputState =
        let st =
            { Mouse =
                { Position = { X = mouseX; Y = mouseY }
                  LeftPressed = leftDown
                  LeftJustClicked = leftJust
                  RightJustClicked = rightJust }
              Keyboard =
                { IsEscapePressed = escJust
                  IsEnterPressed = enterJust
                  IsLeftPressed = leftKeyJust
                  IsRightPressed = rightKeyJust
                  NumberPressed = numberJust } }
        leftJust <- false
        rightJust <- false
        escJust <- false
        enterJust <- false
        leftKeyJust <- false
        rightKeyJust <- false
        numberJust <- None
        st

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
              NumberPressed = None } }

    /// Check if a point is inside a rectangle.
    let hitTest (rect: Rect) (point: Point) = rect.Contains point

    /// Find which card rectangle (if any) contains the point.
    let findClickedCard (rects: (int * Rect) list) (point: Point) =
        rects |> List.tryFind (fun (_, r) -> hitTest r point) |> Option.map fst
