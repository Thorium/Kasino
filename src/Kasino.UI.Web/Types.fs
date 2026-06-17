namespace Kasino.UI.Web

// ─────────────────────────────────────────────────────────────
// Lightweight geometry / color primitives that mirror the small
// subset of the MonoGame (XNA) API the desktop UI relied on.
// This lets the screen modules be ported almost verbatim while
// rendering through an HTML5 Canvas instead of a SpriteBatch.
// ─────────────────────────────────────────────────────────────

/// RGBA color (0-255 channels). Mirrors Microsoft.Xna.Framework.Color.
type Color =
    { R: int; G: int; B: int; A: int }

    /// CSS rgba() string for use as a canvas fill style.
    member c.Css =
        let a = float (max 0 (min 255 c.A)) / 255.0
        sprintf "rgba(%d,%d,%d,%g)" c.R c.G c.B a

module Color =
    let rgba r g b a = { R = r; G = g; B = b; A = a }
    let rgb r g b = rgba r g b 255

    /// Lighten each channel (used for button hover feedback).
    let lighten (amount: int) (c: Color) =
        { c with
            R = min 255 (c.R + amount)
            G = min 255 (c.G + amount)
            B = min 255 (c.B + amount) }

    // Named colors used across the screens (XNA values).
    let White       = rgb 255 255 255
    let Black        = rgb 0 0 0
    let Gold         = rgb 255 215 0
    let Gray         = rgb 128 128 128
    let LightGray    = rgb 211 211 211
    let DarkGray     = rgb 169 169 169
    let Yellow       = rgb 255 255 0
    let LimeGreen    = rgb 50 205 50
    let LightGreen   = rgb 144 238 144
    let LightSalmon  = rgb 255 160 122
    let LightBlue    = rgb 173 216 230
    let Plum         = rgb 221 160 221
    let Transparent  = rgba 0 0 0 0

/// Integer 2D point. Mirrors Microsoft.Xna.Framework.Point.
type Point =
    { X: int; Y: int }
    static member Zero = { X = 0; Y = 0 }

/// Integer rectangle. Mirrors the handful of Rectangle members the UI uses.
type Rect =
    { X: int; Y: int; Width: int; Height: int }

    member r.Right = r.X + r.Width
    member r.Bottom = r.Y + r.Height
    member r.CenterX = r.X + r.Width / 2
    member r.CenterY = r.Y + r.Height / 2

    /// Point-in-rectangle test (left/top inclusive, right/bottom exclusive).
    member r.Contains (p: Point) =
        p.X >= r.X && p.X < r.Right && p.Y >= r.Y && p.Y < r.Bottom

    /// Axis-aligned overlap test.
    member a.Intersects (b: Rect) =
        not (a.Right <= b.X || b.Right <= a.X || a.Bottom <= b.Y || b.Bottom <= a.Y)

/// Result of measuring a string (width X, line height Y) — mirrors Vector2.
type TextSize = { X: float; Y: float }
