module Game

open Block
open Lines
open FallingBlock

type Score = Score of int

type Game =
  | GameOver of Score
  | Playing  of Score*Lines*FallingBlock*Block

type GameEvent =
  | Tick
  | KeyDown
  | KeyUp
  | KeyLeft
  | KeyRight

let createGame (renderer : MailboxProcessor<Render.RenderEvent>) = MailboxProcessor.Start <| fun inbox ->
  async {
      while true do
        let! msg = inbox.Receive ()
        printfn "GAME: %A" msg
        match msg with
        | KeyDown -> ()
        | _ -> ()
        ()
    }
