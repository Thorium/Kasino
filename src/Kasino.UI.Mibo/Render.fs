namespace Kasino.Mibo

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
// The extensions module only (not the whole Mibo namespace, whose Color type
// would shadow the XNA Color used throughout the screens).
open Mibo.MonoGameColorExtensions
open Mibo.Elmish.Graphics
open Mibo.Elmish.Graphics2D

// ─────────────────────────────────────────────────────────────
// Rendering helpers shared across every screen.
//
// Mibo's 2D renderer is a *deferred command buffer*: the view fills a
// RenderBuffer2D with draw commands (via the fluent DSL, Mibo 3.1+) and the
// renderer sorts them by an integer render-layer (higher = on top) before
// batching the GPU draws. So instead of the MonoGame build's implicit
// painter's order (call sequence), this port assigns explicit layers to each
// logical group.
//
// Text is a content-pipeline SpriteFont (built at 32px); the original UI drew
// everything at 24px, so we render at 0.75 scale for parity.
// ─────────────────────────────────────────────────────────────

module Render =

    /// SpriteFont atlas is 32px. The bundled DejaVu Sans renders heavier than
    /// the system font the MonoGame build used, so scale below the nominal
    /// 24/32 for a matching visual size.
    let uiScale = 0.5f

    /// 1x1 white pixel, assigned in CardRenderer.loadAll. fillRect is
    /// axis-aligned only, so rotated/tinted rectangles (scatter overlays,
    /// highlights on rotated cards) are drawn as a tinted white sprite.
    let mutable WhitePixel : Texture2D = Unchecked.defaultof<Texture2D>

    // ── Render layers (higher = drawn on top) ────────────────
    let LTableBg      =  5<RenderLayer>
    let LTableCard    = 10<RenderLayer>
    let LTableOverlay = 12<RenderLayer>
    let LHandBack     = 20<RenderLayer>
    let LHand         = 25<RenderLayer>
    let LHandTop      = 27<RenderLayer>   // dragged / lifted card above the rest
    let LLabel        = 30<RenderLayer>
    let LButton       = 40<RenderLayer>
    let LButtonBorder = 41<RenderLayer>
    let LButtonText   = 42<RenderLayer>
    let LAnim         = 50<RenderLayer>
    let LAnimTop      = 52<RenderLayer>
    let LOverlayBg    = 60<RenderLayer>
    let LModal        = 65<RenderLayer>
    let LModalBorder  = 66<RenderLayer>
    let LModalText    = 67<RenderLayer>

    /// Measure a string at the UI scale.
    let measure (font: SpriteFont) (s: string) : Vector2 =
        font.MeasureString(s) * uiScale

    /// Draw a filled, axis-aligned rectangle.
    let fill (buffer: RenderBuffer2D) (layer: int<RenderLayer>) (color: Color) (rect: Rectangle) =
        buffer.fillRect(float32 rect.X, float32 rect.Y, float32 rect.Width, float32 rect.Height,
                        color.ToMiboColor(), layer = layer) |> ignore

    /// Draw a rectangle outline as four filled edges.
    ///
    /// NOT rectOutline: at thickness <= 1 that emits a line strip and above it
    /// a triangle strip, and Mibo's PrimitiveBatch chains consecutive strips of
    /// the same kind into one group, so every button's outline ends up
    /// connected by a stray band ("drawn without lifting the pen").
    /// Filled edges are triangle lists, so they never chain.
    let outline (buffer: RenderBuffer2D) (layer: int<RenderLayer>) (color: Color) (thickness: float32) (rect: Rectangle) =
        let t = max 1 (int thickness)
        fill buffer layer color (Rectangle(rect.X, rect.Y, rect.Width, t))                 // top
        fill buffer layer color (Rectangle(rect.X, rect.Bottom - t, rect.Width, t))        // bottom
        fill buffer layer color (Rectangle(rect.X, rect.Y, t, rect.Height))                // left
        fill buffer layer color (Rectangle(rect.Right - t, rect.Y, t, rect.Height))        // right

    /// Draw text with its top-left at `pos`.
    let text (buffer: RenderBuffer2D) (layer: int<RenderLayer>) (font: SpriteFont) (s: string) (pos: Vector2) (color: Color) =
        let st = TextState.create(font, s, pos)
        buffer.text({ st with Color = color; Scale = uiScale; Layer = layer }) |> ignore

    /// Draw text horizontally centered on `cx`.
    let textCentered (buffer: RenderBuffer2D) (layer: int<RenderLayer>) (font: SpriteFont) (s: string) (cx: float32) (y: float32) (color: Color) =
        let size = measure font s
        text buffer layer font s (Vector2(cx - size.X / 2.0f, y)) color

    /// Draw a texture stretched to fill a destination rectangle.
    let sprite (buffer: RenderBuffer2D) (layer: int<RenderLayer>) (tex: Texture2D) (dest: Rectangle) =
        let st = SpriteState.create(tex, dest, Rectangle(0, 0, tex.Width, tex.Height))
        buffer.sprite({ st with Layer = layer; Color = Color.White }) |> ignore

    // NOTE on Mibo's sprite transform: its custom PrimitiveBatch places the
    // quad's top-left at Dest.X/Y and treats Origin purely as the *rotation
    // pivot* in Dest-local coordinates — Origin does NOT translate the sprite
    // (unlike SpriteBatch). So to draw a w×h sprite centered at (cx,cy) and
    // rotated about its centre, use Dest top-left = (cx-w/2, cy-h/2) and
    // Origin = (w/2, h/2).

    /// Draw a texture of size (w,h) centered at (cx,cy), rotated about the centre.
    let spriteCentered (buffer: RenderBuffer2D) (layer: int<RenderLayer>) (tex: Texture2D) (cx: int) (cy: int) (w: int) (h: int) (rotation: float32) =
        let dest = Rectangle(cx - w / 2, cy - h / 2, w, h)
        let origin = Vector2(float32 w / 2.0f, float32 h / 2.0f)
        let st = SpriteState.create(tex, dest, Rectangle(0, 0, tex.Width, tex.Height))
        buffer.sprite({ st with Layer = layer; Color = Color.White; Rotation = rotation; Origin = origin }) |> ignore

    /// A tinted rectangle of size (w,h) centered at (cx,cy), rotated about the
    /// centre (uses the white pixel, since fillRect is axis-aligned only).
    let tintedCentered (buffer: RenderBuffer2D) (layer: int<RenderLayer>) (color: Color) (cx: int) (cy: int) (w: int) (h: int) (rotation: float32) =
        let dest = Rectangle(cx - w / 2, cy - h / 2, w, h)
        let origin = Vector2(float32 w / 2.0f, float32 h / 2.0f)
        let st = SpriteState.create(WhitePixel, dest, Rectangle(0, 0, 1, 1))
        buffer.sprite({ st with Layer = layer; Color = color; Rotation = rotation; Origin = origin }) |> ignore
