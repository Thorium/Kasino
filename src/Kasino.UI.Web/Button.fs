namespace Kasino.UI.Web

// ─────────────────────────────────────────────────────────────
// Reusable touch-friendly button. Enforces minimum tap-target
// sizes (48x120) and provides hover feedback. Stateless — button
// definitions are recomputed each frame, as on the desktop.
// ─────────────────────────────────────────────────────────────

module Button =

    let MinTouchHeight = 48
    let MinTouchWidth = 120

    type ButtonDef =
        { Rect: Rect
          Text: string
          BgColor: Color
          HoverColor: Color
          TextColor: Color }

    /// Create a button at a position with enforced minimum size.
    let create (text: string) (x: int) (y: int) (w: int) (h: int) (bgColor: Color) (textColor: Color) =
        let w = max w MinTouchWidth
        let h = max h MinTouchHeight
        { Rect = { X = x; Y = y; Width = w; Height = h }
          Text = text
          BgColor = bgColor
          HoverColor = Color.lighten 40 bgColor
          TextColor = textColor }

    /// Create a button centered horizontally at a given y.
    let createCentered (text: string) (screenW: int) (y: int) (w: int) (h: int) (bgColor: Color) (textColor: Color) =
        let w = max w MinTouchWidth
        let x = (screenW - w) / 2
        create text x y w h bgColor textColor

    let isHovered (input: Input.InputState) (btn: ButtonDef) =
        btn.Rect.Contains input.Mouse.Position

    let isClicked (input: Input.InputState) (btn: ButtonDef) =
        btn.Rect.Contains input.Mouse.Position && input.Mouse.LeftJustClicked

    /// Draw a button with hover feedback and a 1px border.
    let draw (g: Gfx) (input: Input.InputState) (btn: ButtonDef) =
        let hover = isHovered input btn
        let bg = if hover then btn.HoverColor else btn.BgColor
        Gfx.fillRect g btn.Rect bg

        let borderColor = if hover then Color.White else Color.Gray
        let r = btn.Rect
        Gfx.fillRect g { X = r.X; Y = r.Y; Width = r.Width; Height = 1 } borderColor
        Gfx.fillRect g { X = r.X; Y = r.Bottom - 1; Width = r.Width; Height = 1 } borderColor
        Gfx.fillRect g { X = r.X; Y = r.Y; Width = 1; Height = r.Height } borderColor
        Gfx.fillRect g { X = r.Right - 1; Y = r.Y; Width = 1; Height = r.Height } borderColor

        let size = Gfx.measure g btn.Text
        let tx = float r.X + (float r.Width - size.X) / 2.0
        let ty = float r.Y + (float r.Height - size.Y) / 2.0
        Gfx.fillText g btn.Text tx ty btn.TextColor

    let drawAll (g: Gfx) (input: Input.InputState) (buttons: ButtonDef list) =
        buttons |> List.iter (draw g input)

    /// Index of the first clicked button in the list, if any.
    let findClicked (input: Input.InputState) (buttons: ButtonDef list) =
        buttons |> List.tryFindIndex (isClicked input)
