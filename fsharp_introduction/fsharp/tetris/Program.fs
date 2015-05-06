
open System
open System.Threading

open Events

[<EntryPoint>]
let main argv = 
  
  use renderer  = Render.createRenderer ()
  use game      = Game.createGame renderer

  let callback  = TimerCallback (fun x -> game.Post Tick)
  use timer     = new Timer(callback, null, TimeSpan.Zero, TimeSpan.FromSeconds 0.5)

  renderer.Post <| Game game

  ignore <| Console.ReadKey ()

  0
