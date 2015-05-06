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

module FallingBlock

open Block
open Lines

type FallingBlock = FallingBlock of Block*IntVector*Orientation

module Details =

  let collisionTest (FallingBlock (block, pos, orientation)) (lines : Lines) : bool =
    let hitTest result bit bitPos =
      let p = bitPos + pos
      let c = getCell lines p
      match bit, result, c with
      | false , _     , _     -> result
      | true  , true  , _     -> true
      | true  , false , Unset -> false
      | true  , false , _     -> true

    foldBits block orientation false hitTest

  let mutate (previousBlock : FallingBlock) (lines : Lines) (newFallingBlock : FallingBlock) : FallingBlock =
    if collisionTest newFallingBlock lines then
      previousBlock
    else
      newFallingBlock

open Details

let isColliding (fallingBlock : FallingBlock) (lines : Lines) : bool =
  collisionTest fallingBlock lines

let rotate (FallingBlock (block, pos, orientation) as previousBlock) (lines : Lines) : FallingBlock =
  let newFallingBlock = FallingBlock (block, pos, rotate block orientation)

  mutate previousBlock lines newFallingBlock

let move (FallingBlock (block, pos, orientation) as previousBlock) (lines : Lines) (translate : IntVector) : FallingBlock =
  let newFallingBlock = FallingBlock (block, pos + translate, orientation)

  mutate previousBlock lines newFallingBlock

let mergeWithLines (FallingBlock (block, pos, orientation)) lines : Lines =
  let newLines = copyLines lines

  let merger () bit bitPos =
    let p = bitPos + pos
    match bit with
    | true -> mutates_SetCell newLines p Set
    | false -> ()
    ()

  foldBits block orientation () merger

  newLines