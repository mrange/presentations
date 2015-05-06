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

namespace Canvas

open System
open System.Diagnostics
open System.Threading
open System.Collections.Generic

open SharpDX

module Window =

    let Show
      (title      : string                                  )
      (width      : int                                     )
      (height     : int                                     )
      (onKeyUp    : int -> unit                             )
      (onRender   : Device -> Direct2D1.RenderTarget -> unit) =
        use form                = new Windows.RenderForm (title)

        form.ClientSize         <- Drawing.Size (width,height)

        let device              = ref <| new Device (form)

        let disposeDevice ()    = Dispose !device
        let recreateDevice ()   = disposeDevice ()
                                  device := new Device (form)

        use onExitDisposeDevice = OnExit disposeDevice

        let resizer             = EventHandler (fun o e -> recreateDevice ())
        let keyUp               = Windows.Forms.KeyEventHandler (fun o e -> onKeyUp e.KeyValue)

        form.Resize.AddHandler resizer
        form.KeyUp.AddHandler keyUp

        use onExitRemoveHandler = OnExit <| fun () -> form.Resize.RemoveHandler resizer

        let render () =
            let d = !device

            d.Draw <| fun d2dRenderTarget -> onRender d d2dRenderTarget

        Windows.RenderLoop.Run (form, render)







