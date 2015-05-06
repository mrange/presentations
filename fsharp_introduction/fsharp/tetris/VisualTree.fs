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
