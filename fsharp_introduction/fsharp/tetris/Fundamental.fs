[<AutoOpen>]
module Fundamental

open System.Diagnostics

open SharpDX

let inline clamp (v : 'T) (b : 'T) (e : 'T) : 'T =
  if v < b then b
  elif v > e then e
  else v

let inline lerpf (t : float32) (b : float32) (e : float32) : float32 = b + (t * (e - b))

let inline lerpv (t : float32) (b : Vector2) (e : Vector2) : Vector2 = b + (t * (e - b))

let clock =
  let sw = Stopwatch ()
  sw.Start ()
  sw

let gameTime () = float32 clock.Elapsed.TotalSeconds

[<Struct>]
[<NoComparison>]
type IntVector(x : int, y : int) =

  member __.X = x
  member __.Y = y

  member __.Min (o : IntVector) = IntVector (min __.X o.X, min __.Y o.Y)
  member __.Max (o : IntVector) = IntVector (max __.X o.X, max __.Y o.Y)

  override __.ToString () = sprintf "(%d,%d)" x y

  static member inline ( + ) (l : IntVector, r : IntVector) = IntVector (l.X + r.X, l.Y + r.Y)
  static member inline ( - ) (l : IntVector, r : IntVector) = IntVector (l.X - r.X, l.Y - r.Y)

  static member inline Zero  = IntVector (0, 0)
  static member inline One   = IntVector (1, 1)

let inline v x y = IntVector(x,y)

[<NoComparison>]
type IntBoundingBox =
  {
    TopLeft     : IntVector
    BottomRight : IntVector
  }

  // + One as the edges are closed
  member x.Size = x.BottomRight - x.TopLeft + IntVector.One

  member x.Include (pos : IntVector) = 
    let min = x.TopLeft.Min pos
    let max = x.BottomRight.Max pos

    IntBoundingBox.UnsafeNew min max

  member x.Contains (pos : IntVector) = 
    let bb = x.Include pos

    x.TopLeft = bb.TopLeft && x.BottomRight = bb.BottomRight

  override x.ToString () = sprintf "{%s,%s}" (x.TopLeft.ToString ()) (x.BottomRight.ToString ())

  static member private UnsafeNew (min : IntVector) (max : IntVector) : IntBoundingBox = 
    {
      TopLeft     = min
      BottomRight = max
    }

  static member New (l : IntVector) (r : IntVector) : IntBoundingBox = IntBoundingBox.UnsafeNew (l.Min r) (l.Max r)

