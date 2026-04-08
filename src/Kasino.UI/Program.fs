namespace Kasino.UI

module Program =

    [<EntryPoint>]
    let main _ =
        use game = new KasinoGame()
        game.Run()
        0
