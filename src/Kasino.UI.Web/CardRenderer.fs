namespace Kasino.UI.Web

open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom
open Browser.Types
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Card image loading and rendering for the web front-end.
// Card images: 75x95 px PNGs served from <base>/cards/, named by
// suit prefix + rank (sp/he/di/cl, 1..10/j/q/k), plus back.png and
// table_bg.png. Mirrors the desktop CardRenderer drawing helpers.
// ─────────────────────────────────────────────────────────────

module CardRenderer =

    let CardWidth = 75
    let CardHeight = 95

    /// Scale factor for rendering cards (canvas is a fixed 768 tall => 1.0).
    let mutable Scale = 1.0

    let scaledWidth () = int (float CardWidth * Scale)
    let scaledHeight () = int (float CardHeight * Scale)

    let private suitPrefix =
        function
        | Spades -> "sp"
        | Hearts -> "he"
        | Diamonds -> "di"
        | Clubs -> "cl"

    let private rankSuffix =
        function
        | Ace -> "1" | Two -> "2" | Three -> "3"
        | Four -> "4" | Five -> "5" | Six -> "6"
        | Seven -> "7" | Eight -> "8" | Nine -> "9"
        | Ten -> "10" | Jack -> "j" | Queen -> "q"
        | King -> "k"

    /// Filename for a card (e.g. "sp1.png" for Ace of Spades).
    let cardFilename (card: Card) =
        sprintf "%s%s.png" (suitPrefix card.Suit) (rankSuffix card.Rank)

    type CardTextures =
        { Cards: Dictionary<string, HTMLImageElement>
          mutable Back: HTMLImageElement  // currently active card back (one of Backs)
          Backs: HTMLImageElement[]       // available card-back designs (scenic photos)
          TableBg: HTMLImageElement }

    /// Vite's configured base URL (always ends in "/").
    let private baseUrl: string = emitJsExpr () "import.meta.env.BASE_URL"

    let private newImage (file: string) : HTMLImageElement =
        let img = document.createElement "img" :?> HTMLImageElement
        img.src <- baseUrl + "cards/" + file
        img

    /// Load every card image and invoke `onReady` once all have settled
    /// (each image resolves via either onload or onerror so we never hang).
    let loadAll (onReady: CardTextures -> unit) =
        // Scenic card backs carried over from the original 2002 deck; one is
        // chosen at random per game (see pickRandomBack). back.png is the
        // procedural fallback used only if none of these load.
        let backFiles = [ "back1.png"; "back2.png"; "back3.png" ]
        let files =
            [ for suit in Cards.allSuits do
                for rank in Cards.allRanks do
                    yield cardFilename { Suit = suit; Rank = rank } ]
            @ [ "back.png"; "table_bg.png" ]
            @ backFiles

        let mutable remaining = files.Length
        let images = Dictionary<string, HTMLImageElement>()

        let settle () =
            remaining <- remaining - 1
            if remaining <= 0 then
                let cards = Dictionary<string, HTMLImageElement>()
                for suit in Cards.allSuits do
                    for rank in Cards.allRanks do
                        let f = cardFilename { Suit = suit; Rank = rank }
                        match images.TryGetValue f with
                        | true, img -> cards[f] <- img
                        | _ -> ()
                let loadedBacks =
                    backFiles
                    |> List.choose (fun f ->
                        match images.TryGetValue f with
                        | true, img when img.naturalWidth > 0 -> Some img
                        | _ -> None)
                let backs =
                    match loadedBacks with
                    | [] -> [| images["back.png"] |]
                    | xs -> List.toArray xs
                onReady
                    { Cards = cards
                      Back = backs[0]
                      Backs = backs
                      TableBg = images["table_bg.png"] }

        for file in files do
            let img = newImage file
            images[file] <- img
            img.onload <- fun _ -> settle ()
            img.onerror <- fun _ -> settle ()

    /// Pick a random card back for the next game/round (mutates the active Back).
    let pickRandomBack (rng: System.Random) (textures: CardTextures) =
        if textures.Backs.Length > 0 then
            textures.Back <- textures.Backs[rng.Next textures.Backs.Length]

    /// Image for a specific card (falls back to the card back if missing).
    let getTexture (textures: CardTextures) (card: Card) =
        match textures.Cards.TryGetValue(cardFilename card) with
        | true, img -> img
        | _ -> textures.Back

    /// Draw a card at a position.
    let drawCard (g: Gfx) (textures: CardTextures) (card: Card) (x: int) (y: int) =
        Gfx.drawImage g (getTexture textures card) x y (scaledWidth ()) (scaledHeight ())

    /// Draw a face-down card.
    let drawCardBack (g: Gfx) (textures: CardTextures) (x: int) (y: int) =
        Gfx.drawImage g textures.Back x y (scaledWidth ()) (scaledHeight ())

    /// Draw a card with a highlight border (for selection / hover).
    let drawCardHighlighted (g: Gfx) (textures: CardTextures) (card: Card) (x: int) (y: int) (borderColor: Color) =
        let bw = 3
        Gfx.fillRect g { X = x - bw; Y = y - bw; Width = scaledWidth () + bw * 2; Height = scaledHeight () + bw * 2 } borderColor
        drawCard g textures card x y

    /// Draw a card with a translucent capture-preview overlay.
    let drawCardWithOverlay (g: Gfx) (textures: CardTextures) (card: Card) (x: int) (y: int) (overlayColor: Color) =
        drawCard g textures card x y
        Gfx.fillRect g { X = x; Y = y; Width = scaledWidth (); Height = scaledHeight () } overlayColor

    /// Draw a card rotated about its center (angle in radians).
    let drawCardRotated (g: Gfx) (textures: CardTextures) (card: Card) (x: int) (y: int) (rotation: float) =
        let w = scaledWidth ()
        let h = scaledHeight ()
        Gfx.drawImageRotated g (getTexture textures card) (float x + float w / 2.0) (float y + float h / 2.0) w h rotation

    /// Draw a rotated card with a translucent overlay.
    let drawCardWithOverlayRotated (g: Gfx) (textures: CardTextures) (card: Card) (x: int) (y: int) (overlayColor: Color) (rotation: float) =
        drawCardRotated g textures card x y rotation
        let w = scaledWidth ()
        let h = scaledHeight ()
        Gfx.fillRectRotated g (float x + float w / 2.0) (float y + float h / 2.0) w h rotation overlayColor
