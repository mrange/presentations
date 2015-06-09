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

module Game

open System

open SharpDX
open Canvas

open Block
open Lines
open FallingBlock

open Animation
open VisualTree
open Events
open Render

type Score = Score of int

type Game =
  | GameOver of Score
  | Playing  of Score*Lines*FallingBlock*Block

module Details =

  let createBlock (rnd : Random) = 
    match rnd.Next 7 with
    | 0 -> O
    | 1 -> I
    | 2 -> S
    | 3 -> Z
    | 4 -> L
    | 5 -> J
    | 6 -> T
    | _ -> O

  let createFallingBlock (Lines (bb, _)) bl = FallingBlock (bl, v (bb.Size.X / 2) 0, Orientation 0)

  let mergeBlock rnd (Score score) fallingBlock lines nextBlock = 
    let full, newLines    = removeFullLines <| mergeWithLines fallingBlock lines
    let newFallingBlock   = createFallingBlock lines nextBlock
    let newNextBlock      = createBlock rnd
          
    if isColliding newFallingBlock newLines then
      GameOver (Score score)
    else
      Playing (Score (score + full*full), newLines, newFallingBlock, newNextBlock)


  let nextGameState rnd (g : Game) (e : GameEvent) : Game =
    match g with
    | GameOver (Score score) -> g
    | Playing (score, lines, fallingBlock, nextBlock) ->
      let inline newBlock bl      = Playing (score, lines, bl, nextBlock)
      let inline moveBlock offset = newBlock (move fallingBlock lines offset)
      match e with
      | BlockLeft   -> moveBlock <| v -1 0
      | BlockRight  -> moveBlock <| v 1 0
      | BlockRotate -> newBlock  <| rotate fallingBlock lines
      | BlockDrop   ->
        let rec drop fb =
          let nfb = move fb lines <| v 0 1
          if nfb = fb then
            fb
          else
            drop nfb
        let moved = drop fallingBlock
        mergeBlock rnd score moved lines nextBlock
      | BlockDown
      | Tick      -> 
        let moved = move fallingBlock lines <| v 0 1
        if moved = fallingBlock then
          mergeBlock rnd score moved lines nextBlock
        else
          newBlock moved

      

  module Brushes =
    let inline solidBrush c = BrushDescriptor.SolidBrush c

    let white       = solidBrush Color.White
    let red         = solidBrush Color.Red
    let yellow      = solidBrush Color.Yellow
    let transparent = BrushDescriptor.Transparent

  let textDescriptor = TextFormatDescriptor.SimpleTextFormat ("Consolas", 24.0F)

  let blockSize = sizef 25.F 25.F

  let renderCell (x : int) (y : int) : VisualTree =
    rectangle 
      (constant Brushes.transparent) 
      (constant Brushes.red) 
      (constant (rectf (blockSize.Width * float32 x + 1.F) (blockSize.Height * float32 y + 1.F) (blockSize.Width - 2.F) (blockSize.Height - 2.F)))
      (constant 0.F)

  let renderBlock (block : Block) (orientation : Orientation) (offset : IntVector) : VisualTree =
    let vts = ResizeArray<_> 16
    foldBits block orientation () <| fun () isSet ip -> if isSet then vts.Add <| renderCell (ip.X + offset.X) (ip.Y + offset.Y)
    Group <| vts.ToArray ()

  let border rect : VisualTree =
    rectangle 
      (constant Brushes.white) 
      (constant Brushes.transparent) 
      (constant rect)
      (constant 1.F)

  let expandBorder = expand 4.F 4.F

  let rectOfLines (Lines (bb, _)) : RectangleF = 
    let sz = bb.Size
    expandBorder <| rectf 0.F 0.F (blockSize.Width * float32 sz.X) (blockSize.Height * float32 sz.Y)

  let borderOfLines lines : VisualTree =
    border <| rectOfLines lines

  let renderLines (Lines (bb, lines)) : VisualTree =
    let sz = bb.Size
    let cells = ResizeArray (sz.X*sz.Y)
    for y = 0 to lines.Length - 1 do
      let (Line line) = lines.[y]
      for x = 0 to line.Length - 1 do
        let cell = line.[x]
        match cell with
        | Set -> cells.Add <| renderCell x y
        | _ -> ()

    Group <| cells.ToArray ()

  let renderFallingBlock (FallingBlock (fallingBlock, position, orientation)) : VisualTree =
    renderBlock fallingBlock orientation position

  let renderGameBoard fallingBlock lines : VisualTree =
    Transform
      (
        constant (Matrix3x2.Translation (v2 100.F 100.F)),
        Group
          [|
            renderLines lines
            renderFallingBlock fallingBlock
            borderOfLines lines
          |]
      )

  let borderOfNextBlock = border <| (expandBorder <| rectf 0.F 0.F (4.F*blockSize.Width) (4.F*blockSize.Height))

  let renderNextBlock nextBlock : VisualTree =
    Transform
      (
        constant (Matrix3x2.Translation (v2 400.F 200.F)),
        Group
          [|
            renderBlock nextBlock (Orientation 0) (v 2 1)
            borderOfNextBlock
          |]
      )

  let renderScore score : VisualTree =
    Text (sprintf "Score: %d" score, textDescriptor, constant Brushes.white, constant (rectf 0.F 0.F 200.F 40.F))

  let createVisualTree (g : Game) : VisualTree =
    match g with 
    | GameOver (Score score) -> 
      renderScore score
    | Playing (Score score, lines, fallingBlock, nextBlock) ->
      let t (ctx : RenderContext) = Matrix3x2.Translation (v2 (100.F + 100.F * (cos ctx.GameTime)) (100.F + 100.F * (sin ctx.GameTime)))
      Transform 
        (
          t,
          Group
            [|
              renderGameBoard fallingBlock lines
              renderNextBlock nextBlock
              renderScore score
            |]
        )


open Details

let createGame (renderer : MailboxProcessor<RenderEvent>) = MailboxProcessor.Start <| fun inbox ->
  async {
      let cols  = 10
      let rows  = 20
      let rnd   = Random 19740531

      let lines                 = createLines cols rows      
      let currentBlock          = createFallingBlock lines <| createBlock rnd
      let nextBlock             = createBlock rnd

      let game = ref <| Playing (Score 0, lines, currentBlock, nextBlock)

      let sendVisual () =
        let vt = createVisualTree !game
        renderer.Post (NewVisualTree vt)

      sendVisual ()

      while true do
        let! msg = inbox.Receive ()
        printfn "GAME: %A" msg

        game := nextGameState rnd !game msg
        sendVisual ()
    }
