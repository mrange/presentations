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

module Block

//[<RequireQualifiedAccess>]
type Block =
  | O
  | I
  | S
  | Z
  | L
  | J
  | T

let getBitmap = function
  | O ->  0b0000uy,
          0b0110uy,
          0b0110uy,
          0b0000uy
  | I ->  0b0000uy,
          0b1111uy,
          0b0000uy,
          0b0000uy
  | S ->  0b0000uy,
          0b0011uy,
          0b0110uy,
          0b0000uy
  | Z ->  0b0000uy,
          0b0110uy,
          0b0011uy,
          0b0000uy
  | L ->  0b0000uy,
          0b0111uy,
          0b0100uy,
          0b0000uy
  | J ->  0b0000uy,
          0b0111uy,
          0b0001uy,
          0b0000uy
  | T ->  0b0000uy,
          0b0111uy,
          0b0010uy,
          0b0000uy

type Orientation = Orientation of int

let rotate (block : Block) (Orientation orientation) : Orientation =
  let orientations =
    match block with
    | O -> 1
    | I | S | Z -> 2
    | L | J | T -> 4
  Orientation <| (orientation + 1) % orientations

let shapeOffset = v -2 -1

let foldBits (block : Block) (Orientation orientation) (initial : 'TState) (visitor : 'TState -> bool -> IntVector -> 'TState) : 'TState =
  let l0,l1,l2,l3 = getBitmap block

  let inline rotate (pos : IntVector) =
    match orientation with
    | 0 -> v pos.X  pos.Y
    | 1 -> v pos.Y  -pos.X
    | 2 -> v -pos.X -pos.Y
    | 3 -> v -pos.Y pos.X
    | _ -> v pos.X  pos.Y

  let inline getLine y =
    match y with
    | 0 -> l0
    | 1 -> l1
    | 2 -> l2
    | 3 -> l3
    | _ -> l0

  let mutable state = initial

  for y in 0..3 do
    for x in 0..3 do
      let l = getLine y
      let p = ((l <<< x) &&& 0b1000uy) <> 0uy
      let v = rotate (v x y + shapeOffset)
      state <- visitor state p v

  state
