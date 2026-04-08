namespace Kasino.UI

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open FontStashSharp

// ─────────────────────────────────────────────────────────────
// Reusable touch-friendly button component.
// Enforces minimum tap-target sizes for comfortable touch input
// (48px height, 120px width). Supports hover feedback for mouse
// and keyboard-free interaction throughout all screens.
// ─────────────────────────────────────────────────────────────

module Button =

    /// Minimum touch-friendly dimensions
    let MinTouchHeight = 48
    let MinTouchWidth = 120

    /// A button definition — stateless, typically recomputed each frame
    type ButtonDef =
        { Rect: Rectangle
          Text: string
          BgColor: Color
          HoverColor: Color
          TextColor: Color }

    /// Create a button at a specific position with enforced minimum size
    let create (text: string) (x: int) (y: int) (w: int) (h: int) (bgColor: Color) (textColor: Color) =
        let w = max w MinTouchWidth
        let h = max h MinTouchHeight
        { Rect = Rectangle(x, y, w, h)
          Text = text
          BgColor = bgColor
          HoverColor =
              Color(
                  min 255 (int bgColor.R + 40),
                  min 255 (int bgColor.G + 40),
                  min 255 (int bgColor.B + 40),
                  int bgColor.A)
          TextColor = textColor }

    /// Create a button centered horizontally at a given y position
    let createCentered (text: string) (screenW: int) (y: int) (w: int) (h: int) (bgColor: Color) (textColor: Color) =
        let w = max w MinTouchWidth
        let x = (screenW - w) / 2
        create text x y w h bgColor textColor

    /// Check if the pointer is hovering over the button
    let isHovered (input: InputHandler.InputState) (btn: ButtonDef) =
        btn.Rect.Contains(input.Mouse.Position)

    /// Check if a button was tapped/clicked this frame
    let isClicked (input: InputHandler.InputState) (btn: ButtonDef) =
        btn.Rect.Contains(input.Mouse.Position) && input.Mouse.LeftJustClicked

    /// Draw a button with hover feedback and 1px border
    let draw (sb: SpriteBatch) (font: SpriteFontBase) (input: InputHandler.InputState) (btn: ButtonDef) =
        let hover = isHovered input btn
        let bg = if hover then btn.HoverColor else btn.BgColor
        let bgTex = CardRenderer.getCachedColorTexture (sb.GraphicsDevice) bg
        sb.Draw(bgTex, btn.Rect, Color.White)

        // 1px border (brighter on hover)
        let borderColor = if hover then Color.White else Color.Gray
        let borderTex = CardRenderer.getCachedColorTexture (sb.GraphicsDevice) borderColor
        let r = btn.Rect
        sb.Draw(borderTex, Rectangle(r.X, r.Y, r.Width, 1), Color.White)
        sb.Draw(borderTex, Rectangle(r.X, r.Bottom - 1, r.Width, 1), Color.White)
        sb.Draw(borderTex, Rectangle(r.X, r.Y, 1, r.Height), Color.White)
        sb.Draw(borderTex, Rectangle(r.Right - 1, r.Y, 1, r.Height), Color.White)

        // Center text in button
        let size = font.MeasureString(btn.Text)
        let tx = float32 r.X + (float32 r.Width - size.X) / 2.0f
        let ty = float32 r.Y + (float32 r.Height - size.Y) / 2.0f
        sb.DrawString(font, btn.Text, Vector2(tx, ty), btn.TextColor) |> ignore

    /// Draw a list of buttons
    let drawAll (sb: SpriteBatch) (font: SpriteFontBase) (input: InputHandler.InputState) (buttons: ButtonDef list) =
        buttons |> List.iter (draw sb font input)

    /// Find the first clicked button in a list (returns its index)
    let findClicked (input: InputHandler.InputState) (buttons: ButtonDef list) =
        buttons |> List.tryFindIndex (isClicked input)
