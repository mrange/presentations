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

    let v2 x y = Vector2(x,y)

    let world = World (Vector2(0.F,10.F))

    let rect bt x y w h = 
        let rect = BodyFactory.CreateRectangle(world, w, h, 1.F, v2 x y,box <| Physics.RenderRect w h)
        rect.BodyType <- bt
              
    let srect = rect BodyType.Static
    let drect = rect BodyType.Dynamic
    let krect = rect BodyType.Kinematic

    let random = Random 19740531

    //      x       y       w       h
    srect   500.F   500.F   800.F   10.F
    for i in 0..100 do
        let dx = random.NextFloat32 200.F 800.F
        let dy = random.NextFloat32 000.F 400.F
        drect   dx  dy  20.F    20.F

    PhysicsWindow.Show world
        
    0
