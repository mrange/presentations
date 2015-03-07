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

open Physical

open System

open FarseerPhysics
open FarseerPhysics.Dynamics
open FarseerPhysics.Factories
open Microsoft.Xna.Framework

[<EntryPoint>]
let main argv = 

    let pi      = float32 Math.PI
    let v2 x y  = Vector2(x,y)
    let world   = World (Vector2(0.F,10.F))

    let adorn bt x y rot (br : Physics.BodyRenderer) (body : Body) = 
        body.BodyType <- bt
        body.UserData <- box br
        body.SetTransform(v2 x y, rot)

    let rect bt x y rot w h= 
        adorn bt x y rot (Physics.RenderRect w h) <| BodyFactory.CreateRectangle(world, w, h, 1.F)

    let circle bt x y r = 
        adorn bt x y 0.F (Physics.RenderCircle r) <| BodyFactory.CreateCircle(world, r, 1.F)

    let srect   = rect BodyType.Static
    let drect   = rect BodyType.Dynamic
    let krect   = rect BodyType.Kinematic

    let scircle = circle BodyType.Static
    let dcircle = circle BodyType.Dynamic
    let kcircle = circle BodyType.Kinematic

    let random = Random 19740531

    //      x       y       rot     w       h
    srect   500.F   510.F   0.1F    800.F   20.F
    drect   700.F   475.F   0.F     50.F    50.F

    //      x       y       r
    dcircle 500.F   450.F   50.F

    for i in 0..100 do
        let dx = random.NextFloat32 200.F 800.F
        let dy = random.NextFloat32 000.F 400.F
        let rot= random.NextFloat32 000.F 2.0F*pi
        drect   dx  dy  rot 20.F    20.F

    PhysicsWindow.Show world
        
    0
