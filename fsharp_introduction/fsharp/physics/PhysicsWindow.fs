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

module PhysicsWindow = 

    let Show (world : FarseerPhysics.Dynamics.World) = 
        let sw = Stopwatch ()

        use form                = new Windows.RenderForm ("Physics")

        form.ClientSize         <- System.Drawing.Size (1600,1200)

        let device              = ref <| new Device (form)

        let disposeDevice ()    = TryRun (upcast !device : IDisposable).Dispose
        let recreateDevice ()   = disposeDevice ()
                                  device := new Device (form)

        use onExitDisposeDevice = OnExit disposeDevice

        use cts = new CancellationTokenSource ()
        let ct = cts.Token

        use onExitCancelTask    = OnExit cts.Cancel

        let resizer             = EventHandler (fun o e -> recreateDevice ())

        form.Resize.AddHandler  resizer

        use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

        sw.Start ()

        Windows.RenderLoop.Run (form, fun () -> 
            let elapsed = sw.ElapsedMilliseconds
            sw.Restart ()

            world.Step (float32 elapsed / 1000.F)

            let d = !device
                                
            let r = Physics.Resources.New d.BlackBrush d.LimeGreenBrush d.MediumVioletRedBrush

            d.Draw <| fun d2dRenderTarget -> 
                
                d2dRenderTarget.Clear (Nullable<_> (Color.White.ToColor4 ()))

                let bl = world.BodyList
                let bc = bl.Count
                for i in 0..(bc - 1) do
                    let b = bl.[i]
                    let br= b.UserData :?> Physics.BodyRenderer
                    
                    br (r, d2dRenderTarget, b) 

(*
                let transform = 
                    Matrix3x2.Identity 
                    <*> Matrix3x2.Rotation (Deg2Rad * 180.F)
                    <*> Matrix3x2.Translation (d.Width/2.F, d.Height - 20.F) 
                d2dRenderTarget.Transform <- transform
*)

                )






