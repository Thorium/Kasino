namespace Kasino.UI.Web

open Fable.Core.JsInterop
open Browser.Types

// ─────────────────────────────────────────────────────────────
// Thin drawing layer over a CanvasRenderingContext2D.
// Exposes the few primitives the screens need: filled rects,
// (rotated) images, and text. The 2D context is accessed via
// dynamic interop so we never depend on exact binding overloads.
// ─────────────────────────────────────────────────────────────

type Gfx =
    { Ctx: obj
      mutable FontSize: int }

module Gfx =

    let create (ctx: obj) : Gfx = { Ctx = ctx; FontSize = 24 }

    let private setFill (g: Gfx) (c: Color) =
        g.Ctx?fillStyle <- c.Css

    let private applyFont (g: Gfx) =
        g.Ctx?font <- sprintf "%dpx 'Segoe UI', Arial, sans-serif" g.FontSize
        g.Ctx?textBaseline <- "top"
        g.Ctx?textAlign <- "left"

    /// Clear the whole surface to a solid color.
    let clear (g: Gfx) (w: int) (h: int) (c: Color) =
        setFill g c
        g.Ctx?fillRect (0, 0, w, h)

    /// Fill an axis-aligned rectangle.
    let fillRect (g: Gfx) (r: Rect) (c: Color) =
        setFill g c
        g.Ctx?fillRect (r.X, r.Y, r.Width, r.Height)

    /// Fill a rectangle centered at (cx, cy), rotated by `rot` radians.
    let fillRectRotated (g: Gfx) (cx: float) (cy: float) (w: int) (h: int) (rot: float) (c: Color) =
        setFill g c
        g.Ctx?save ()
        g.Ctx?translate (cx, cy)
        g.Ctx?rotate (rot)
        g.Ctx?fillRect (-(float w) / 2.0, -(float h) / 2.0, w, h)
        g.Ctx?restore ()

    let private isReady (img: HTMLImageElement) =
        img.complete && img.naturalWidth > 0.0

    /// Draw an image into a destination rectangle (top-left x, y).
    let drawImage (g: Gfx) (img: HTMLImageElement) (x: int) (y: int) (w: int) (h: int) =
        if isReady img then
            g.Ctx?drawImage (img, x, y, w, h)

    /// Draw an image centered at (cx, cy), rotated by `rot` radians.
    let drawImageRotated (g: Gfx) (img: HTMLImageElement) (cx: float) (cy: float) (w: int) (h: int) (rot: float) =
        if isReady img then
            g.Ctx?save ()
            g.Ctx?translate (cx, cy)
            g.Ctx?rotate (rot)
            g.Ctx?drawImage (img, -(float w) / 2.0, -(float h) / 2.0, w, h)
            g.Ctx?restore ()

    /// Measure a string at the current font size (width X, line height Y).
    let measure (g: Gfx) (text: string) : TextSize =
        applyFont g
        let metrics = g.Ctx?measureText (text)
        let w: float = metrics?width
        { X = w; Y = float g.FontSize }

    /// Draw left-aligned text with its top-left at (x, y).
    let fillText (g: Gfx) (text: string) (x: float) (y: float) (c: Color) =
        applyFont g
        setFill g c
        g.Ctx?fillText (text, x, y)
