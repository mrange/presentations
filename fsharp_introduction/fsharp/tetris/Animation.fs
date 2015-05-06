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

module Animation

open SharpDX

open Canvas

type RenderContext =
  {
    GameTime  : float32
    Size      : Size2F
  }

  static member New sz =
    {
      GameTime  = gameTime ()
      Size      = sz
    }

type AnimatedTransform  = (RenderContext -> Matrix3x2)
type AnimatedVector2    = (RenderContext -> Vector2)
type AnimatedRectangleF = (RenderContext -> RectangleF)
type AnimatedFloat32    = (RenderContext -> float32)
type AnimatedBrush      = (RenderContext -> BrushDescriptor)

let constant v (ctx : RenderContext) : 'T = v

let linear_float32 start stop b e (ctx : RenderContext) : float32 =
  let t = clamp ctx.GameTime start stop
  lerpf t b e

let linear_vector2 start stop b e (ctx : RenderContext) : Vector2 =
  let t = clamp ctx.GameTime start stop
  lerpv t b e

