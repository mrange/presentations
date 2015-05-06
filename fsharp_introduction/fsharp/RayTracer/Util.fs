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

namespace RayTracer

open System
open System.Windows.Threading

[<AutoOpen>]
module Util =

    let cutoff = 0.000001

    let inline sign d = if d < 0. then -1. else 1.
    let pi = Math.PI
    let pi2 = 2. * pi

    let inline degree2rad d = pi * d / 180.
    let inline rad2degree r = r * 180. / pi

    let inline clamp x min max =
        if x < min then min
        elif x > max then max
        else x

    let inline norm x = clamp x -1. 1.
    let inline unorm x = clamp x 0. 1.

    let dispatch (d : Dispatcher) (a : unit -> unit) =
        let a' = Action a
        ignore <| d.BeginInvoke (DispatcherPriority.ApplicationIdle, a')

    let inline asByte d = byte ((unorm d) * 255.)


