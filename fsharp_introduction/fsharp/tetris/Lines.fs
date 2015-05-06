module Lines

(*
let boundingBox = IntBoundingBox.New (v 0 0) (v 9 19)
let size        = boundingBox.Size
*)

type Cell =
  | Set
  | Unset
  | Outside

type Line = Line of Cell []

type Lines = Lines of IntBoundingBox*Line []

let createLines c r : Lines =
  let bb    = IntBoundingBox.New IntVector.Zero (v (c - 1) (r - 1))
  let lines = [| for y in 1..r -> Line (Array.create c Unset) |]
  Lines (bb, lines)

let getCell (Lines (bb,lines)) (pos : IntVector) : Cell =
  if bb.Contains pos then
    let p = pos - bb.TopLeft
    let (Line line) = lines.[p.Y]
    let cell = line.[p.X]
    cell
  else
    Outside

let mutates_SetCell (Lines (bb,lines)) (pos : IntVector) (cell : Cell) : unit =
  if bb.Contains pos then
    let p = pos - bb.TopLeft
    let (Line line) = lines.[p.Y]
    line.[p.X] <- cell
  else
    ()

let removeFullLines (Lines (bb,lines) as previousLines) : int*Lines =
  let size = bb.Size
  let keptLines = 
    lines
    |> Array.filter (function Line cells -> cells |> Array.exists (fun c -> c <> Set))

  let removed = lines.Length - keptLines.Length

  if removed = 0 then
    0, previousLines
  else
    let emptyLines = [| for x in 1..removed -> Line (Array.create size.X Unset) |]

    let newLines = Array.append emptyLines keptLines

    removed, Lines (bb, newLines)

let copyLines (Lines (bb,lines)) : Lines =
  let newLines = 
    [|
      for (Line line) in lines -> Line (line |> Array.copy)
    |]
  Lines (bb, newLines)