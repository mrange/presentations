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

open GravitySucks

open Gravity

open System

[<EntryPoint>]
let main argv = 

    let random  = Random (19740531)

    let pi      = float32 Math.PI

    let sun     = Particle.New 1000000.F (V2 0.F 0.F) (V2 0.F 0.F)
    let jupiter = CreateParticle sun 5000.F 600.F -pi 1.F
    let moon1   = CreateParticle jupiter 100.F 30.F 0.F 1.F
    let moon2   = CreateParticle jupiter 100.F 30.F pi 1.F

    let particles =
        Array.concat
            [|
                // Predefined particles
                [|
                    sun
                    jupiter
                    moon1
                    moon2
                |]

                // Predefined randomized particles
                [|
                    for i in 0..199 do
                        let m   = random.NextFloat32 10.F   30.F
                        let r   = random.NextFloat32 150.F  800.F
                        let a   = random.NextFloat32 0.F    (2.0F * pi)
    //                    let d   = if random.NextDouble () > 0.4 then 1.F else -1.F
                        let d   = 1.F
                        let f   = d * (random.NextFloat32 0.9F   1.2F)
                        let p   = CreateParticle sun m r a f
                        yield p
                |]
            |]    

    Window.Show particles
    0
