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

