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

namespace Physical

open FarseerPhysics
open FarseerPhysics.Dynamics
open FarseerPhysics.Factories
open Microsoft.Xna.Framework

module Physics = 

    type Resources =
        {
            BlackBrush  : SharpDX.Direct2D1.Brush
            GreenBrush  : SharpDX.Direct2D1.Brush
            VioletBrush : SharpDX.Direct2D1.Brush
        }           

        static member New bb gb vb = { BlackBrush = bb; GreenBrush = gb; VioletBrush = vb }

    type BodyRenderer = Resources*SharpDX.Direct2D1.RenderTarget*Body -> unit

    let RenderRect w h : BodyRenderer =
        let hw = w / 2.F
        let hh = h / 2.F
        let rect = SharpDX.RectangleF (-hw, -hh, w, h)
        fun (res, rt, body) -> 

            let pos     = body.Position
            let rot     = body.Rotation

            let t = rt.Transform

            let transform = 
                t
                <*> SharpDX.Matrix3x2.Rotation rot
                <*> SharpDX.Matrix3x2.Translation (dv2 pos)
            rt.Transform <- transform

            rt.FillRectangle (rect, res.GreenBrush)
            rt.DrawRectangle (rect, res.BlackBrush)

            rt.Transform <- t

    let RenderCircle r : BodyRenderer =
        fun (res, rt, body) -> 

            let pos     = body.Position

            let ellipse = SharpDX.Direct2D1.Ellipse (dv2 pos, r, r) 

            rt.FillEllipse (ellipse, res.VioletBrush)
            rt.DrawEllipse (ellipse, res.BlackBrush)

    let pi      = float32 System.Math.PI
    let v2 x y  = Vector2(x,y)
    let world   = World (Vector2(0.F,10.F))

    let adorn bt x y rot (br : BodyRenderer) (body : Body) = 
        body.BodyType <- bt
        body.UserData <- box br
        body.SetTransform(v2 x y, rot)

    let rect bt x y rot w h = 
        adorn bt x y rot (RenderRect w h) <| BodyFactory.CreateRectangle(world, w, h, 1.F)

    let circle bt x y r = 
        adorn bt x y 0.F (RenderCircle r) <| BodyFactory.CreateCircle(world, r, 1.F)

    let srect   = rect BodyType.Static
    let drect   = rect BodyType.Dynamic
    let krect   = rect BodyType.Kinematic

    let scircle = circle BodyType.Static
    let dcircle = circle BodyType.Dynamic
    let kcircle = circle BodyType.Kinematic

