namespace Kasino.UI

open System
open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Card texture loading and rendering utilities.
// Card images: 75x95 px PNGs, named by suit prefix + rank.
//   sp=Spades, he=Hearts, di=Diamonds, cl=Clubs
//   1=Ace, 2-10, j=Jack, q=Queen, k=King
// ─────────────────────────────────────────────────────────────

module CardRenderer =

    let CardWidth  = 75
    let CardHeight = 95

    /// Scale factor for rendering cards
    let mutable Scale = 1.0f

    /// Scaled card dimensions
    let scaledWidth  () = int (float32 CardWidth  * Scale)
    let scaledHeight () = int (float32 CardHeight * Scale)

    /// Map a domain Suit to its file prefix
    let private suitPrefix = function
        | Spades   -> "sp"
        | Hearts   -> "he"
        | Diamonds -> "di"
        | Clubs    -> "cl"

    /// Map a domain Rank to its file suffix
    let private rankSuffix = function
        | Ace   -> "1"   | Two   -> "2"   | Three -> "3"
        | Four  -> "4"   | Five  -> "5"   | Six   -> "6"
        | Seven -> "7"   | Eight -> "8"   | Nine  -> "9"
        | Ten   -> "10"  | Jack  -> "j"   | Queen -> "q"
        | King  -> "k"

    /// Build filename for a card (e.g. "sp1.png" for Ace of Spades)
    let cardFilename (card: Card) =
        $"{suitPrefix card.Suit}{rankSuffix card.Rank}.png"

    /// Load a Texture2D from a PNG file using stream
    let private loadTexture (device: GraphicsDevice) (path: string) =
        use stream = File.OpenRead(path)
        Texture2D.FromStream(device, stream)

    /// All card textures keyed by (Suit, Rank)
    type CardTextures =
        { Cards: Map<Suit * Rank, Texture2D>
          Back: Texture2D
          Highlight: Texture2D     // yellow highlight border
          TableBg: Texture2D }     // green felt

    /// Create a 1x1 colored texture (utility)
    let createColorTexture (device: GraphicsDevice) (color: Color) =
        let tex = new Texture2D(device, 1, 1)
        tex.SetData([| color |])
        tex

    /// Generate a procedural card back texture with a Balatro-inspired diamond pattern
    let private generateCardBack (device: GraphicsDevice) =
        let w, h = 75, 95
        let pixels = Array.create (w * h) Color.Transparent

        // Fill with dark navy blue base
        let baseBg = Color(25, 25, 80)
        for i in 0 .. pixels.Length - 1 do
            pixels[i] <- baseBg

        // Draw border (2px gold/amber frame)
        let borderColor = Color(180, 140, 60)
        let innerBorder = Color(120, 90, 40)
        for x in 0 .. w - 1 do
            for y in 0 .. h - 1 do
                if x < 2 || x >= w - 2 || y < 2 || y >= h - 2 then
                    pixels[y * w + x] <- borderColor
                elif x < 4 || x >= w - 4 || y < 4 || y >= h - 4 then
                    pixels[y * w + x] <- innerBorder

        // Diamond lattice pattern inside the border area
        let patternColor1 = Color(60, 40, 140)      // purple-blue diamonds
        let patternColor2 = Color(140, 50, 50)       // dark red accents
        let dotColor = Color(200, 160, 60)            // gold dots at intersections

        for x in 5 .. w - 6 do
            for y in 5 .. h - 6 do
                // Diamond grid pattern with 8px spacing
                let dx = (x - 5) % 8
                let dy = (y - 5) % 8
                // Diamond shape: |dx - 4| + |dy - 4| == 3
                let dist = abs(dx - 4) + abs(dy - 4)
                if dist = 3 then
                    pixels[y * w + x] <- patternColor1
                elif dist = 2 then
                    pixels[y * w + x] <- Color(40, 30, 110)
                // Small dots at diamond centers
                if dx = 4 && dy = 4 then
                    pixels[y * w + x] <- dotColor
                // Red accent dots at corners of each diamond cell
                if dx = 0 && dy = 0 then
                    pixels[y * w + x] <- patternColor2

        // Center emblem: small diamond shape
        let cx, cy = w / 2, h / 2
        for x in cx - 8 .. cx + 8 do
            for y in cy - 8 .. cy + 8 do
                if x >= 5 && x < w - 5 && y >= 5 && y < h - 5 then
                    let dist = abs(x - cx) + abs(y - cy)
                    if dist <= 6 && dist >= 4 then
                        pixels[y * w + x] <- Color(220, 180, 70)  // gold diamond outline
                    elif dist < 4 then
                        pixels[y * w + x] <- Color(160, 40, 40)   // red center

        let tex = new Texture2D(device, w, h)
        tex.SetData(pixels)
        tex

    /// Generate a poker-green felt texture with woven fiber pattern
    let private generateFeltTexture (device: GraphicsDevice) =
        let w, h = 128, 128
        let pixels = Array.create (w * h) Color.Transparent
        let rng = Random(42)  // deterministic seed for consistency

        for x in 0 .. w - 1 do
            for y in 0 .. h - 1 do
                // Base poker green
                let baseR, baseG, baseB = 35, 100, 55

                // Fine noise for felt grain
                let fineNoise = rng.Next(-6, 7)

                // Horizontal fiber lines: subtle brightness variation every 2-3 pixels
                let hFiber =
                    let row = y % 4
                    if row = 0 then -3 elif row = 2 then 2 else 0

                // Vertical fiber lines: fainter cross-weave
                let vFiber =
                    let col = x % 6
                    if col = 0 then -2 elif col = 3 then 1 else 0

                // Diagonal weave pattern: very subtle, every 8px
                let weave =
                    let d = (x + y) % 8
                    if d = 0 then -2 elif d = 4 then 1 else 0

                // Slight large-scale color variation (simulates dye irregularity)
                let regionNoise =
                    let rx = float32 x / 32.0f |> sin |> (*) 2.0f |> int
                    let ry = float32 y / 24.0f |> cos |> (*) 2.0f |> int
                    rx + ry

                let total = fineNoise + hFiber + vFiber + weave + regionNoise
                let r = max 0 (min 255 (baseR + total / 2))
                let g = max 0 (min 255 (baseG + total))
                let b = max 0 (min 255 (baseB + total / 2))
                pixels[y * w + x] <- Color(r, g, b)

        let tex = new Texture2D(device, w, h)
        tex.SetData(pixels)
        tex

    /// Load all card textures from the Content/cards directory
    let loadAll (device: GraphicsDevice) (contentDir: string) : CardTextures =
        let cardsDir = Path.Combine(contentDir, "cards")

        let cardMap =
            [ for suit in Cards.allSuits do
                for rank in Cards.allRanks do
                    let filename = cardFilename { Suit = suit; Rank = rank }
                    let path = Path.Combine(cardsDir, filename)
                    if File.Exists(path) then
                        yield ((suit, rank), loadTexture device path) ]
            |> Map.ofList

        let backPath = Path.Combine(cardsDir, "back.png")
        let back =
            if File.Exists(backPath) then loadTexture device backPath
            else generateCardBack device

        let tableBgPath = Path.Combine(cardsDir, "table_bg.png")
        let tableBg =
            if File.Exists(tableBgPath) then loadTexture device tableBgPath
            else generateFeltTexture device

        let highlight = createColorTexture device (Color(255, 255, 0, 128))

        { Cards = cardMap; Back = back; Highlight = highlight; TableBg = tableBg }

    /// Get the texture for a specific card (fallback to back if missing)
    let getTexture (textures: CardTextures) (card: Card) =
        match Map.tryFind (card.Suit, card.Rank) textures.Cards with
        | Some tex -> tex
        | None     -> textures.Back

    /// Draw a card at a position
    let drawCard (sb: SpriteBatch) (textures: CardTextures) (card: Card) (x: int) (y: int) =
        let tex = getTexture textures card
        let dest = Rectangle(x, y, scaledWidth(), scaledHeight())
        sb.Draw(tex, dest, Color.White)

    /// Draw a face-down card
    let drawCardBack (sb: SpriteBatch) (textures: CardTextures) (x: int) (y: int) =
        let dest = Rectangle(x, y, scaledWidth(), scaledHeight())
        sb.Draw(textures.Back, dest, Color.White)

    /// Cache for per-frame color textures to avoid GPU memory leak
    let private colorTextureCache = System.Collections.Generic.Dictionary<uint32, Texture2D>()

    /// Get or create a cached 1x1 color texture (avoids allocating every frame)
    let getCachedColorTexture (device: GraphicsDevice) (color: Color) =
        let key = color.PackedValue
        match colorTextureCache.TryGetValue(key) with
        | true, tex -> tex
        | false, _ ->
            let tex = createColorTexture device color
            colorTextureCache[key] <- tex
            tex

    /// Draw a card with a highlight border (for selection)
    let drawCardHighlighted (sb: SpriteBatch) (textures: CardTextures) (card: Card) (x: int) (y: int) (borderColor: Color) =
        let bw = 3
        let borderRect = Rectangle(x - bw, y - bw, scaledWidth() + bw * 2, scaledHeight() + bw * 2)
        let borderTex = getCachedColorTexture (sb.GraphicsDevice) borderColor
        sb.Draw(borderTex, borderRect, Color.White)
        drawCard sb textures card x y

    /// Draw a card with a capture preview overlay.
    /// Uses a white pixel texture with overlayColor as tint to avoid
    /// premultiplied-alpha squaring (color in texture * color in tint).
    let drawCardWithOverlay (sb: SpriteBatch) (textures: CardTextures) (card: Card) (x: int) (y: int) (overlayColor: Color) =
        drawCard sb textures card x y
        let whiteTex = getCachedColorTexture (sb.GraphicsDevice) Color.White
        let dest = Rectangle(x, y, scaledWidth(), scaledHeight())
        sb.Draw(whiteTex, dest, overlayColor)

    /// Draw a card at a position with rotation (angle in radians)
    let drawCardRotated (sb: SpriteBatch) (textures: CardTextures) (card: Card) (x: int) (y: int) (rotation: float32) =
        let tex = getTexture textures card
        let w = scaledWidth()
        let h = scaledHeight()
        let origin = Vector2(float32 tex.Width / 2.0f, float32 tex.Height / 2.0f)
        let dest = Rectangle(x + w / 2, y + h / 2, w, h)
        sb.Draw(tex, dest, System.Nullable(), Color.White, rotation, origin, SpriteEffects.None, 0.0f)

    /// Draw a card with overlay and rotation
    let drawCardWithOverlayRotated (sb: SpriteBatch) (textures: CardTextures) (card: Card) (x: int) (y: int) (overlayColor: Color) (rotation: float32) =
        drawCardRotated sb textures card x y rotation
        let whiteTex = getCachedColorTexture (sb.GraphicsDevice) Color.White
        let w = scaledWidth()
        let h = scaledHeight()
        let origin = Vector2(0.5f, 0.5f)
        let dest = Rectangle(x + w / 2, y + h / 2, w, h)
        sb.Draw(whiteTex, dest, System.Nullable(), overlayColor, rotation, origin, SpriteEffects.None, 0.0f)
