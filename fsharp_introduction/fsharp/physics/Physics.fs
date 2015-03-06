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

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic

open SharpDX

module Physics = 

    type Resources =
        {
            BlackBrush  : Direct2D1.Brush
        }           

        static member New bb = { BlackBrush = bb }

    type BodyRenderer = Resources*Direct2D1.RenderTarget*FarseerPhysics.Dynamics.Body -> unit

    let RenderRect w h : BodyRenderer =
        let hw = w / 2.F
        let hh = h / 2.F
        let rect = RectangleF (-hw, -hh, w, h)
        fun (res, rt, body) -> 

            let pos     = body.Position
            let rot     = body.Rotation

            let t = rt.Transform

            let transform = 
                t
                <*> Matrix3x2.Rotation rot
                <*> Matrix3x2.Translation (dv2 pos)
            rt.Transform <- transform

            rt.DrawRectangle (rect, res.BlackBrush)

            rt.Transform <- t
