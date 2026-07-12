namespace Kasino.Mibo

open System
open System.IO
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Mibo.Elmish.Graphics2D
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Card texture loading and card-drawing utilities.
//
// Texture *loading* is identical to the MonoGame build (PNGs via
// Texture2D.FromStream, plus procedural fallbacks). Card *drawing* emits
// Mibo Draw.* commands into the render buffer at explicit layers instead of
// issuing SpriteBatch calls.
//
// Card images: 75x95 px PNGs named by suit prefix + rank.
//   sp=Spades he=Hearts di=Diamonds cl=Clubs ; 1=Ace 2-10 j=Jack q=Queen k=King
// ─────────────────────────────────────────────────────────────

module CardRenderer =

    let CardWidth  = 75
    let CardHeight = 95

    /// Scale factor for rendering cards (grows with screen height).
    let mutable Scale = 1.0f
    let scaledWidth  () = int (float32 CardWidth  * Scale)
    let scaledHeight () = int (float32 CardHeight * Scale)

    let private suitPrefix = function
        | Spades   -> "sp"
        | Hearts   -> "he"
        | Diamonds -> "di"
        | Clubs    -> "cl"

    let private rankSuffix = function
        | Ace   -> "1"   | Two   -> "2"   | Three -> "3"
        | Four  -> "4"   | Five  -> "5"   | Six   -> "6"
        | Seven -> "7"   | Eight -> "8"   | Nine  -> "9"
        | Ten   -> "10"  | Jack  -> "j"   | Queen -> "q"
        | King  -> "k"

    let cardFilename (card: Card) =
        $"{suitPrefix card.Suit}{rankSuffix card.Rank}.png"

    let private loadTexture (device: GraphicsDevice) (path: string) =
        use stream = File.OpenRead(path)
        Texture2D.FromStream(device, stream)

    /// All card textures keyed by (Suit, Rank).
    type CardTextures =
        { Cards: Map<Suit * Rank, Texture2D>
          mutable Back: Texture2D   // currently active card back
          Backs: Texture2D[]        // available card-back designs
          TableBg: Texture2D }      // green felt

    let createColorTexture (device: GraphicsDevice) (color: Color) =
        let tex = new Texture2D(device, 1, 1)
        tex.SetData([| color |])
        tex

    /// Procedural card back (Balatro-inspired diamond lattice) — fallback when
    /// no back.png ships.
    let private generateCardBack (device: GraphicsDevice) =
        let w, h = 75, 95
        let pixels = Array.create (w * h) Color.Transparent
        let baseBg = Color(25, 25, 80)
        for i in 0 .. pixels.Length - 1 do
            pixels[i] <- baseBg
        let borderColor = Color(180, 140, 60)
        let innerBorder = Color(120, 90, 40)
        for x in 0 .. w - 1 do
            for y in 0 .. h - 1 do
                if x < 2 || x >= w - 2 || y < 2 || y >= h - 2 then
                    pixels[y * w + x] <- borderColor
                elif x < 4 || x >= w - 4 || y < 4 || y >= h - 4 then
                    pixels[y * w + x] <- innerBorder
        let patternColor1 = Color(60, 40, 140)
        let patternColor2 = Color(140, 50, 50)
        let dotColor = Color(200, 160, 60)
        for x in 5 .. w - 6 do
            for y in 5 .. h - 6 do
                let dx = (x - 5) % 8
                let dy = (y - 5) % 8
                let dist = abs(dx - 4) + abs(dy - 4)
                if dist = 3 then pixels[y * w + x] <- patternColor1
                elif dist = 2 then pixels[y * w + x] <- Color(40, 30, 110)
                if dx = 4 && dy = 4 then pixels[y * w + x] <- dotColor
                if dx = 0 && dy = 0 then pixels[y * w + x] <- patternColor2
        let cx, cy = w / 2, h / 2
        for x in cx - 8 .. cx + 8 do
            for y in cy - 8 .. cy + 8 do
                if x >= 5 && x < w - 5 && y >= 5 && y < h - 5 then
                    let dist = abs(x - cx) + abs(y - cy)
                    if dist <= 6 && dist >= 4 then pixels[y * w + x] <- Color(220, 180, 70)
                    elif dist < 4 then pixels[y * w + x] <- Color(160, 40, 40)
        let tex = new Texture2D(device, w, h)
        tex.SetData(pixels)
        tex

    /// Procedural poker-green felt — fallback when no table_bg.png ships.
    let private generateFeltTexture (device: GraphicsDevice) =
        let w, h = 128, 128
        let pixels = Array.create (w * h) Color.Transparent
        let rng = Random(42)
        for x in 0 .. w - 1 do
            for y in 0 .. h - 1 do
                let baseR, baseG, baseB = 35, 100, 55
                let fineNoise = rng.Next(-6, 7)
                let hFiber = let row = y % 4 in if row = 0 then -3 elif row = 2 then 2 else 0
                let vFiber = let col = x % 6 in if col = 0 then -2 elif col = 3 then 1 else 0
                let weave = let d = (x + y) % 8 in if d = 0 then -2 elif d = 4 then 1 else 0
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

    /// Load all card textures from a Content/cards directory. Also installs the
    /// shared 1x1 white pixel used by Render for tinted/rotated fills.
    let loadAll (device: GraphicsDevice) (contentDir: string) : CardTextures =
        Render.WhitePixel <- createColorTexture device Color.White

        let cardsDir = Path.Combine(contentDir, "cards")

        let cardMap =
            [ for suit in Cards.allSuits do
                for rank in Cards.allRanks do
                    let filename = cardFilename { Suit = suit; Rank = rank }
                    let path = Path.Combine(cardsDir, filename)
                    if File.Exists(path) then
                        yield ((suit, rank), loadTexture device path) ]
            |> Map.ofList

        let scenicBacks =
            [ 1 .. 9 ]
            |> List.choose (fun i ->
                let p = Path.Combine(cardsDir, sprintf "back%d.png" i)
                if File.Exists(p) then Some(loadTexture device p) else None)

        let defaultBack =
            let backPath = Path.Combine(cardsDir, "back.png")
            if File.Exists(backPath) then loadTexture device backPath
            else generateCardBack device

        let backs =
            match scenicBacks with
            | [] -> [| defaultBack |]
            | xs -> List.toArray xs

        let tableBgPath = Path.Combine(cardsDir, "table_bg.png")
        let tableBg =
            if File.Exists(tableBgPath) then loadTexture device tableBgPath
            else generateFeltTexture device

        { Cards = cardMap; Back = backs[0]; Backs = backs; TableBg = tableBg }

    /// Pick a random card back for the next game (mutates the active Back).
    let pickRandomBack (rng: Random) (textures: CardTextures) =
        if textures.Backs.Length > 0 then
            textures.Back <- textures.Backs[rng.Next textures.Backs.Length]

    let getTexture (textures: CardTextures) (card: Card) =
        match Map.tryFind (card.Suit, card.Rank) textures.Cards with
        | Some tex -> tex
        | None     -> textures.Back

    // ── Card drawing (into a render buffer at a given layer) ──

    let drawCard buffer (layer: int<RenderLayer>) textures card x y =
        Render.sprite buffer layer (getTexture textures card) (Rectangle(x, y, scaledWidth(), scaledHeight()))

    let drawCardBack buffer (layer: int<RenderLayer>) (textures: CardTextures) x y =
        Render.sprite buffer layer textures.Back (Rectangle(x, y, scaledWidth(), scaledHeight()))

    /// Card with a colored border behind it (selection / hover).
    let drawCardHighlighted buffer (layer: int<RenderLayer>) textures card x y (borderColor: Color) =
        let bw = 3
        Render.fill buffer layer borderColor
            (Rectangle(x - bw, y - bw, scaledWidth() + bw * 2, scaledHeight() + bw * 2))
        drawCard buffer (layer + 1<RenderLayer>) textures card x y

    /// Card with a translucent capture-preview overlay.
    let drawCardWithOverlay buffer (layer: int<RenderLayer>) textures card x y (overlayColor: Color) =
        drawCard buffer layer textures card x y
        Render.fill buffer (layer + 2<RenderLayer>) overlayColor
            (Rectangle(x, y, scaledWidth(), scaledHeight()))

    /// x,y is the card's top-left; it is drawn rotated about its own centre.
    let drawCardRotated buffer (layer: int<RenderLayer>) textures card x y (rotation: float32) =
        let w = scaledWidth()
        let h = scaledHeight()
        Render.spriteCentered buffer layer (getTexture textures card) (x + w / 2) (y + h / 2) w h rotation

    let drawCardWithOverlayRotated buffer (layer: int<RenderLayer>) textures card x y (overlayColor: Color) (rotation: float32) =
        drawCardRotated buffer layer textures card x y rotation
        let w = scaledWidth()
        let h = scaledHeight()
        Render.tintedCentered buffer (layer + 2<RenderLayer>) overlayColor (x + w / 2) (y + h / 2) w h rotation
