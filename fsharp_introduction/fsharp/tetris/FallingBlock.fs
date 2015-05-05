module FallingBlock

open Block
open Lines

type FallingBlock = FallingBlock of Block*IntVector*Orientation

module Details =

  let collisionTest (FallingBlock (block, pos, orientation)) (lines : Lines) : bool =
    let hitTest result bit bitPos =
      let p = bitPos + pos
      match result, getCell lines p with
      | true  , _     -> true
      | false , Unset -> false
      | false , _     -> true
    
    foldBits block orientation false hitTest

  let mutate (previousBlock : FallingBlock) (lines : Lines) (newFallingBlock : FallingBlock) : FallingBlock =
    if collisionTest newFallingBlock lines then
      previousBlock
    else
      newFallingBlock

open Details

let rotate (FallingBlock (block, pos, orientation) as previousBlock) (lines : Lines) : FallingBlock =
  let newFallingBlock = FallingBlock (block, pos, rotate block orientation)

  mutate previousBlock lines newFallingBlock

let move (FallingBlock (block, pos, orientation) as previousBlock) (lines : Lines) (translate : IntVector) : FallingBlock =
  let newFallingBlock = FallingBlock (block, pos + translate, orientation)

  mutate previousBlock lines newFallingBlock
