namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish.Graphics2D

// ─────────────────────────────────────────────────────────────
// Reusable touch-friendly button. Definitions and hit-testing are identical
// to the MonoGame build; only drawing changed (Draw.* into a render buffer).
// draw takes a base render-layer so modal buttons can sit above the dim
// overlay behind them.
// ─────────────────────────────────────────────────────────────

module Button =

    let MinTouchHeight = 48
    let MinTouchWidth = 120

    type ButtonDef =
        { Rect: Rectangle
          Text: string
          BgColor: Color
          HoverColor: Color
          TextColor: Color }

    let create (text: string) (x: int) (y: int) (w: int) (h: int) (bgColor: Color) (textColor: Color) =
        let w = max w MinTouchWidth
        let h = max h MinTouchHeight
        { Rect = Rectangle(x, y, w, h)
          Text = text
          BgColor = bgColor
          HoverColor =
              Color(min 255 (int bgColor.R + 40),
                    min 255 (int bgColor.G + 40),
                    min 255 (int bgColor.B + 40),
                    int bgColor.A)
          TextColor = textColor }

    let createCentered (text: string) (screenW: int) (y: int) (w: int) (h: int) (bgColor: Color) (textColor: Color) =
        let w = max w MinTouchWidth
        let x = (screenW - w) / 2
        create text x y w h bgColor textColor

    let isHovered (input: Input.InputState) (btn: ButtonDef) =
        btn.Rect.Contains(input.Mouse.Position)

    let isClicked (input: Input.InputState) (btn: ButtonDef) =
        btn.Rect.Contains(input.Mouse.Position) && input.Mouse.LeftJustClicked

    let findClicked (input: Input.InputState) (buttons: ButtonDef list) =
        buttons |> List.tryFindIndex (isClicked input)

    /// Draw a button starting at a base render-layer (bg / border / text stacked).
    let drawAt (buffer: RenderBuffer2D) (baseLayer: int<RenderLayer>) (font: SpriteFont) (input: Input.InputState) (btn: ButtonDef) =
        let hover = isHovered input btn
        let bg = if hover then btn.HoverColor else btn.BgColor
        Render.fill buffer baseLayer bg btn.Rect
        let borderColor = if hover then Color.White else Color.Gray
        Render.outline buffer (baseLayer + 1<RenderLayer>) borderColor 1.0f btn.Rect
        let size = Render.measure font btn.Text
        let tx = float32 btn.Rect.X + (float32 btn.Rect.Width - size.X) / 2.0f
        let ty = float32 btn.Rect.Y + (float32 btn.Rect.Height - size.Y) / 2.0f
        Render.text buffer (baseLayer + 2<RenderLayer>) font btn.Text (Vector2(tx, ty)) btn.TextColor

    let draw buffer font input btn = drawAt buffer Render.LButton font input btn
    let drawAll buffer font input (buttons: ButtonDef list) = buttons |> List.iter (draw buffer font input)
    let drawAllAt buffer baseLayer font input (buttons: ButtonDef list) =
        buttons |> List.iter (drawAt buffer baseLayer font input)
