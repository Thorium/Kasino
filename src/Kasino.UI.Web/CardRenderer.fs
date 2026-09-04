namespace Kasino.UI.Web

open System.Collections.Generic
open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom
open Browser.Types
open Kasino.Domain

// ─────────────────────────────────────────────────────────────
// Card image loading and rendering for the web front-end.
// Card images: PNGs served from <base>/cards/<style>/ (one folder per
// deck style, see Settings.CardStyle), named by suit prefix + rank
// (sp/he/di/cl, 1..10/j/q/k) plus back.png; table_bg.png is shared and
// sits in <base>/cards/. Mirrors the desktop CardRenderer drawing helpers.
// ─────────────────────────────────────────────────────────────

module CardRenderer =

    [<Literal>]
    let CardWidth = 75
    [<Literal>]
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
          mutable Back: HTMLImageElement      // currently active deck image (one of Backs)
          mutable HandBack: HTMLImageElement  // single card back for face-down hand cards (matches Back)
          Backs: HTMLImageElement[]           // available deck designs (scenic, stacked edges)
          HandBacks: HTMLImageElement[]       // the single-card back of each design (parallel to Backs)
          TableBg: HTMLImageElement }

    /// Vite's configured base URL (always ends in "/").
    let private baseUrl: string = emitJsExpr () "import.meta.env.BASE_URL"

    let private newImage (path: string) : HTMLImageElement =
        let img = document.createElement "img" :?> HTMLImageElement
        img.src <- $"{baseUrl}{path}"
        img

    /// Load every card image of one deck style and invoke `onReady` once all
    /// have settled (each image resolves via either onload or onerror so we
    /// never hang).
    let loadAll (style: Settings.CardStyle) (onReady: CardTextures -> unit) =
        let dir = Settings.CardStyle.folder style
        let pathOf file = if file = "table_bg.png" then $"cards/{file}" else $"cards/{dir}/{file}"
        // Scenic deck designs carried over from the original 2002 deck, each
        // with its single-card hand back; one design is chosen per game (see
        // pickRandomBack). back.png is the fallback if none of these load.
        let backFiles = [ "back1.png"; "back2.png"; "back3.png" ]
        let handBackFiles = [ "handback1.png"; "handback2.png"; "handback3.png" ]
        let files =
            [ for suit in Cards.allSuits do
                for rank in Cards.allRanks do
                    yield cardFilename { Suit = suit; Rank = rank } ]
            @ [ "back.png"; "table_bg.png" ]
            @ backFiles
            @ handBackFiles

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
                let loaded f =
                    match images.TryGetValue f with
                    | true, img when img.naturalWidth > 0 -> Some img
                    | _ -> None
                let loadedBacks =
                    List.zip backFiles handBackFiles
                    |> List.choose (fun (pile, single) ->
                        loaded pile |> Option.map (fun p -> p, defaultArg (loaded single) images["back.png"]))
                let backs, handBacks =
                    match loadedBacks with
                    | [] -> [| images["back.png"] |], [| images["back.png"] |]
                    | xs -> List.map fst xs |> List.toArray, List.map snd xs |> List.toArray
                // The scenic backN images are deck-pile art (stacked edges
                // baked in); face-down hand cards use the matching plain back.
                onReady
                    { Cards = cards
                      Back = backs[0]
                      HandBack = handBacks[0]
                      Backs = backs
                      HandBacks = handBacks
                      TableBg = images["table_bg.png"] }

        for file in files do
            let img = newImage (pathOf file)
            images[file] <- img
            img.onload <- fun _ -> settle ()
            img.onerror <- fun _ -> settle ()

    /// Select deck design i for the game: deck pile and hand back together.
    let selectBack (i: int) (textures: CardTextures) =
        if textures.Backs.Length > 0 then
            let i = ((i % textures.Backs.Length) + textures.Backs.Length) % textures.Backs.Length
            textures.Back <- textures.Backs[i]
            textures.HandBack <- textures.HandBacks[i]

    /// Pick a random deck design for the next game (mutates Back and HandBack).
    let pickRandomBack (rng: System.Random) (textures: CardTextures) =
        if textures.Backs.Length > 0 then
            selectBack (rng.Next textures.Backs.Length) textures

    /// Image for a specific card (falls back to the card back if missing).
    let getTexture (textures: CardTextures) (card: Card) =
        match textures.Cards.TryGetValue(cardFilename card) with
        | true, img -> img
        | _ -> textures.HandBack

    /// Draw a card at a position.
    let drawCard (g: Gfx) (textures: CardTextures) (card: Card) (x: int) (y: int) =
        Gfx.drawImage g (getTexture textures card) x y (scaledWidth ()) (scaledHeight ())

    /// Draw a face-down card (single card back, not the deck image).
    let drawCardBack (g: Gfx) (textures: CardTextures) (x: int) (y: int) =
        Gfx.drawImage g textures.HandBack x y (scaledWidth ()) (scaledHeight ())

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
