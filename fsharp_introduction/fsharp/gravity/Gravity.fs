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

namespace GravitySucks

open SharpDX

open System

module Gravity =
    
    let M = 10.F

    type Particle =
        {
            mutable Mass        : float32 
            mutable Radius      : float32
            mutable Current     : Vector2
            mutable Velocity    : Vector2
        }

        member x.UpdateRadius ()                = x.Radius <- sqr3 x.Mass

        member x.KineticEnergy                  = let v = x.Velocity
                                                  x.Mass * (v * v) / 2.0F

        member x.Momentum                       = x.Mass * x.Velocity


        member x.GravityForce (p : Particle)    = let diff        = x.Current - p.Current
                                                  let lsq         = diff.LengthSquared ()
                                                  let force       = M*x.Mass*p.Mass / lsq
                                                  force


        static member inline New m c v = 
            {
                Mass        = m 
                Radius      = sqr3 m
                Current     = c 
                Velocity    = v
            }

    let CreateParticle (center : Particle) (mass : float32) (radius : float32) (angle : float32) (velocityScale : float32) = 
        let x   = radius * sin angle
        let y   = radius * cos angle
        let p   = Particle.New (float32 mass) (center.Current + (V2 x y)) (V2 0.F 0.F)
        let gf  = center.GravityForce p
        let v   = velocityScale * sqrt (radius * gf / mass)
        let vx  = v * cos angle 
        let vy  = - v * sin angle 
        p.Velocity <- center.Velocity + (V2 vx vy)
        p

    [<Struct>]
    type RenderParticle (mass : float32, radius : float32, current : Vector2, velocity : Vector2) =

        member x.Mass       = mass
        member x.Radius     = radius
        member x.Current    = current
        member x.Velocity   = velocity

    let ApplyGravity (timeStep : float32) (particles : Particle []) =
        let last = particles.Length - 1
        for o in 0..last do
            let outer           = particles.[o]
            for i in o + 1..last do
                let inner       = particles.[i]
                let diff        = outer.Current - inner.Current
                diff.Normalize ()
                let force       = outer.GravityForce inner
                outer.Velocity  <-outer.Velocity - timeStep * force*diff / outer.Mass
                inner.Velocity  <-inner.Velocity + timeStep * force*diff / inner.Mass
        particles

    let ApplyInertia (timeStep : float32) (particles : Particle []) = 
        let last = particles.Length - 1
        for p in 0..last do
            let particle        = particles.[p]
            let newPos          = particle.Current + timeStep * particle.Velocity
            particle.Current    <- newPos
        particles

    let ApplyCollision (particles : Particle []) = 
        let last = particles.Length - 1
        for o in 0..last do
            let outer           = particles.[o]
            let mutable i       = o + 1
            while i <= last && outer.Mass > 0.F do
                let inner       = particles.[i]
                let diff        = outer.Current - inner.Current
                let length      = diff.Length ()
                let d           = (outer.Radius + inner.Radius) - length
                if d > 0.F then
                    let twposition  = outer.Mass * outer.Current + inner.Mass * inner.Current
                    let tmomentum   = outer.Momentum + inner.Momentum
                    let tmass       = outer.Mass + inner.Mass
                    let tposition   = twposition / tmass
                    let tvelocity   = tmomentum / tmass
                    inner.Mass      <- tmass
                    inner.Current   <- tposition
                    inner.Velocity  <- tvelocity
                    inner.UpdateRadius ()
                    outer.Mass      <- 0.F  // Will break the while loop
                i <- i + 1
        particles

    let RemoveMassless (particles : Particle []) =
        particles |> Array.filter (fun p -> p.Mass > 0.F)

    let ToRenderParticles (particles : Particle []) =
        [|
            for p in particles -> RenderParticle(p.Mass, p.Radius, p.Current, p.Velocity)
        |]

    let Cleanup (particles : Particle []) = 
        particles
        |> ApplyCollision  
        |> RemoveMassless

    let TimeStep (timeStep : float32) (particles : Particle []) =
        let particles = particles
                        |> ApplyGravity    timeStep 
                        |> ApplyInertia    timeStep 
                        |> ApplyCollision  
                        |> RemoveMassless
        particles, ToRenderParticles particles



