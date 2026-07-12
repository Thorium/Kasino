module Kasino.Mibo.Program

open Mibo.Elmish
open Kasino.Mibo

[<EntryPoint>]
let main _ =
    let program = Game.create ()
    use game = new MiboGame<Game.Model, Game.Msg>(program)
    game.Run()
    0
