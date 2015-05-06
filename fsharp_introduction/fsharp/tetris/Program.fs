// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
// If you cannot locate the  Microsoft Public License, please send an email to
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

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
