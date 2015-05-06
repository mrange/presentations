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

namespace Canvas

open System
open SharpDX

[<AutoOpen>]
module CanvasAutoOpen =

    let inline sizef w h = Size2F (w,h)

    let inline rectf x y w h = RectangleF (x,y,w,h)

    let inline expand (w : float32) (h : float32) (rect : RectangleF) : RectangleF =
      let hw = w / 2.0F
      let hh = h / 2.0F
      RectangleF (rect.X - hw, rect.Y - hh, rect.Width + w, rect.Height + h)

    let inline v2 x y  = Vector2 (x,y)

    let bounds (v1 : Vector2) (v2 : Vector2) = 
        let min = Vector2.Min (v1, v2)
        let max = Vector2.Max (v1, v2)
        rectf min.X min.Y (max.X - min.X) (max.Y - min.Y)

    type Direct2D1.RenderTarget with
        member x.Clear (c : Color) =
            x.Clear (Nullable<_> (c.ToColor4 ()))
