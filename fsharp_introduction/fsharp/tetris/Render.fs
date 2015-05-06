module Render

open SharpDX
open SharpDX.Direct2D1

open System
open System.Threading

open Canvas

open Animation
open VisualTree
open Events

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

let createRenderer () = MailboxProcessor.Start <| fun inbox ->
  // TODO: Thread-safety, might not be needed as AFAIK .NET uses implicit "volatile" for references
  let vt    = ref Empty
  let game  = ref (None : MailboxProcessor<GameEvent> option)

  let renderProc (d : Device) (rt : RenderTarget) = 
    let width   = d.Width
    let height  = d.Height

    let rec renderTree (ctx : RenderContext) = function
      | Empty       -> ()
      | Generator g -> renderTree ctx <| g ctx
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

    let ctx = RenderContext.New d.ClientSize

    rt.Clear Color.Black

    renderTree ctx !vt

  let window = 
    async {
      do! switchToSTAThread
      let onKeyUp i = 
        match !game, i with
        | Some g, 37 -> g.Post KeyLeft
        | Some g, 38 -> g.Post KeyUp
        | Some g, 39 -> g.Post KeyRight
        | Some g, 40 -> g.Post KeyDown
        | _     , _  -> ()

      Window.Show "Tet3is" 1600 1200 onKeyUp renderProc
    }

  async {
      let! _ = Async.StartChild window
      while true do
        let! msg = inbox.Receive ()
        match msg with
        | Game g -> game := Some g
        | NewVisualTree nvt -> vt := nvt
    }
