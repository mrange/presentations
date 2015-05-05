module Render

open SharpDX
open SharpDX.Direct2D1

open System
open System.Threading

open Canvas

open Animation
open VisualTree

type RenderEvent =
  | NewVisualTree of VisualTree

module Details =

  let switchToSTAThread : Async<unit> =
    let callback (s, e, c) = 
      let threadStart () = 
        try
          s ()
        with
        | :? OperationCanceledException as ex -> c ex
        | ex -> e ex
      let thread = Thread (ThreadStart threadStart)
      thread.SetApartmentState ApartmentState.STA
      thread.Start ()
    Async.FromContinuations callback

open Details

let createRenderer () =

  let renderer = MailboxProcessor.Start <| fun inbox ->
    let vt = ref Empty

    let x = rectangle (constant (BrushDescriptor.SolidBrush Color.BlueViolet)) (constant (BrushDescriptor.SolidBrush Color.Yellow)) (constant (rect 10.F 10.F 100.F 100.F)) (constant 4.F)

    vt := x

    let renderProc (d : Device) (rt : RenderTarget) = 
      let width   = d.Width
      let height  = d.Height

      let rec renderTree (ctx : RenderContext) = function
        | Empty       -> ()
        | Group group -> for child in group do renderTree ctx child
        | Transform (atransform, child) ->
          let t   = atransform ctx
          let ct  = rt.Transform
          let nt  = ct * t

          rt.Transform <- nt
          renderTree ctx child
          rt.Transform <- ct
        | Text (text, textDescriptor, abrush, alayout) ->
          let layout  = alayout ctx
          let sz      = layout.Size

          if sz.Width > 0.F && sz.Height > 0.F then
            let bd  = abrush ctx
            let b   = d.GetBrush bd
            if b <> null then
              let tf  = d.GetTextFormat textDescriptor
              rt.DrawText (text, tf, layout, b)
        | Rectangle (astrokeBrush, afillBrush, arect, astrokeWidth) ->
          let rect  = arect ctx
          let sz    = rect.Size

          if sz.Width > 0.F && sz.Height > 0.F then
            let fb  = afillBrush ctx
            let f   = d.GetBrush fb
            if f <> null then
              rt.FillRectangle (rect, f)

            let sb  = astrokeBrush ctx
            let s   = d.GetBrush sb
            let sw  = astrokeWidth ctx
            if s <> null && sw > 0.F then
              rt.DrawRectangle (rect, s, sw)

      let ctx = RenderContext.New ()

      renderTree ctx !vt

    let window = 
      async {
        do! switchToSTAThread
        Window.Show "Tet3is" 1600 1200 renderProc
      }

    async {
        let! _ = Async.StartChild window
        while true do
          let! msg = inbox.Receive ()
          match msg with
          | NewVisualTree nvt -> 
            vt := nvt
          | _ -> ()
          ()
      }

  renderer
