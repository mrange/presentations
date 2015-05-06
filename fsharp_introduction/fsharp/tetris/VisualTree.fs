module VisualTree

open Canvas

open Animation

type VisualTree =
  | Empty
  | Generator of (RenderContext -> VisualTree)
  | Group     of VisualTree []
  | Transform of AnimatedTransform*VisualTree
  | Text      of string*TextFormatDescriptor*AnimatedBrush*AnimatedRectangleF
  | Rectangle of AnimatedBrush*AnimatedBrush*AnimatedRectangleF*AnimatedFloat32

let empty : VisualTree = Empty

let group children : VisualTree = Group children

let inline transform atransform vt : VisualTree =
  Transform (atransform, vt)

let inline text text tfd abrush alayout : VisualTree =
  Text (text, tfd, abrush, alayout)

let inline rectangle astrokeBrush afillBrush arect astrokeWidth : VisualTree =
  Rectangle (astrokeBrush, afillBrush, arect, astrokeWidth)
