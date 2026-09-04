# Kasino - Finnish Card Game

A digital version of the classic Finnish card game Kasino, built with MonoGame.
There is also [Nu game engine version](https://github.com/Thorium/Nu/tree/finnish-card-game/Projects/Kasino) available.

**▶ [Play online](https://thorium.github.io/Kasino/)** (browser version, no install needed)

## Game Modes

- **Standard Kasino** -- Capture cards to score points. First to 16 wins.
- **Laistokasino** -- Reverse rules. First to 16 *loses*.

Play with 2-4 players in any mix of human and AI opponents.

## How to Play

Capture cards from the table by playing a card from your hand whose value matches the sum of one or more table cards. Capture all table cards at once for a bonus sweep.

### Scoring (per round)

| Category | Points |
|---|---|
| Most cards | 1 |
| Most spades | 2 |
| Each Ace | 1 |
| 10 of Diamonds | 2 |
| 2 of Spades | 1 |
| Each Sweep | 1 |

Scoring fine print:

- **Tied categories carry over.** When players tie for most cards or most spades, nobody scores it that round — the points join a pot that rolls into the next round, collected by whoever then wins the category outright.
- **Sweeps cancel out.** The lowest sweep count at the table is subtracted from every player's sweeps; only the surplus scores.
- **Sweep freeze.** Once any player has reached 10 cumulative points, sweeps score nothing for the rest of the game.
- **Round end.** The last player to capture takes any cards left on the table — this does not count as a sweep.
- The first player to reach 16 cumulative points wins.

## Controls

| Input | Action |
|---|---|
| Click / Number keys | Select a card |
| Double-click / Enter | Play selected card |
| Drag & drop | Play a card directly |
| Escape | Cancel / Return to menu |
| F11 | Toggle fullscreen |

The table layout can be toggled between **Strict Grid** and **Random Scatter** via the in-game button.

Full rules are available in-game via the **How to Play** button.

Missed what the computer players just did? The in-game **?** button opens a small panel listing every other seat's play since your own (who played which card, and what it took or that it was placed). Any click closes it again; the **i** button opens the rules.

## Options

The start menu has an **Options** button. All settings are optional and persist for the session:

| Setting | Default | Effect |
|---|---|---|
| Random card backs | On | Pick a random scenic card back per game (vs. a fixed back) |
| Table layout | Scatter on desktop, Grid on phones/tablets | Start games in Random Scatter or Strict Grid (the web build picks the default from the device, like the card deck) |
| AI table-talk (chat) | Off | Computer players make short remarks as they play |
| AI personalities | Off | Named opponents (e.g. *Reno the Risk-taker*, *Cautious Cara*) with distinct play styles |
| Card deck | Realistic on desktop, Screen-optimized on phones/tablets | **Screen-optimized**: big index and one large centre pip, made for phones (`cards/screen/`); **Realistic**: the original scanned deck (`cards/realistic/`). The web build detects a touch device at startup and picks the default accordingly. The Options screen previews the selected deck with the 10 of diamonds and 2 of spades |

## Requirements

- [.NET 10.0 SDK](https://dotnet.microsoft.com/download)
- [Node.js](https://nodejs.org/) 20.19+ (only for the web build)

## Build & Run (desktop)

```bash
dotnet build
dotnet run --project src/Kasino.UI/Kasino.UI.fsproj
```

## Run Tests

```bash
dotnet test
```

## Mibo version (Elmish game framework)

`src/Kasino.UI.Mibo` is an alternative desktop front-end built on the
[Mibo](https://angelmunoz.github.io/Mibo/) game framework — an Elmish/MVU F#
framework over MonoGame. It targets the same DesktopGL (OpenGL) backend as
`Kasino.UI`, reuses `Kasino.Domain` unchanged, and re-expresses the whole UI as
a functional Model-View-Update program: a top-level screen model with per-screen
`update`/`view`, drawing through Mibo's fluent DSL on sorted render layers,
keyboard bound to semantic actions via `InputMap`, and mouse delivered as a
subscription.

The same Elmish program also runs under Mibo's headless runtime — no window,
graphics, or SDL — which `tests/Kasino.Tests/HeadlessUITests.fs` uses to drive
menu flows, overlays, options, human card play (selection, the capture-option
modal, place-instead), and complete AI games through to game over, all with
virtual time and injected input messages.

Fonts use the MonoGame content pipeline (a compiled `SpriteFont`, built from the
bundled DejaVu Sans in `Content/fonts/`), since Mibo's `Draw.text` renders
SpriteFonts. Card images are the same PNGs as the MonoGame build, linked from
`../Kasino.UI/Content/cards`.

```bash
dotnet tool restore                                   # installs dotnet-mgcb (font pipeline)
dotnet run --project src/Kasino.UI.Mibo/Kasino.UI.Mibo.fsproj
```

## Web version (Fable + Canvas)

`src/Kasino.UI.Web` is a [Fable](https://fable.io/) front-end that reuses
`Kasino.Domain` unchanged and re-implements the UI on an HTML5 Canvas, so the
game runs in the browser. The desktop project (`Kasino.UI`) uses MonoGame and is
not part of the web build.

```bash
dotnet tool restore                 # installs the Fable compiler (once)
cd src/Kasino.UI.Web                 # all npm commands run from HERE
npm install                          # required before dev/build — installs vite locally
npm run dev                          # dev server with hot reload
npm run build                        # production build into dist/
```

> If `npm run dev` fails with `Cannot find package 'vite'`, the dependencies
> aren't installed in `src/Kasino.UI.Web/node_modules`. Run `npm install` from
> inside `src/Kasino.UI.Web` (not the repo root). If you have `NODE_ENV` set to
> `production`, use `npm install --include=dev`.

It deploys automatically to GitHub Pages on every push to `main` via
`.github/workflows/deploy.yml`. Enable **Settings → Pages → Source: GitHub
Actions** in the repository to publish it.
