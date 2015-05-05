



open System

[<EntryPoint>]
let main argv = 
  
  use renderer = Render.createRenderer ()
  use game = Game.createGame renderer

  let rec main () =
    let key = Console.ReadKey ()
    match key.Key with
    | ConsoleKey.UpArrow    -> game.Post <| Game.KeyUp
    | ConsoleKey.DownArrow  -> game.Post <| Game.KeyDown
    | ConsoleKey.LeftArrow  -> game.Post <| Game.KeyLeft
    | ConsoleKey.RightArrow -> game.Post <| Game.KeyRight
    | _                     -> ()

    main ()

  main ()

  0
