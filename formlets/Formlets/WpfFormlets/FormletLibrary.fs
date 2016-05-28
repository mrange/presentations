// A Formlet library for WPF - Inspired by Formlets in Websharper
//  The Formlet concept will work fine for Windows.Forms and Xamarin.Forms as well

// This Formlet library doesn't try to be complete. It's an illustration on how
//  a Formlet library can be built

module FormletLibrary

open System.Threading.Tasks

// IFormletElement is an UI element that represents Formlet
[<AllowNullLiteral>]
type IFormletElement =
  interface
  end

[<AllowNullLiteral>]
type IAdornerFormletElement =
  interface
    inherit IFormletElement
  end

[<AllowNullLiteral>]
type ILabelFormletElement =
  interface
    inherit IFormletElement
  end

[<RequireQualifiedAccess>]
type TaskResult<'T> =
| Result    of 'T
| Exception of exn
| Cancelled
| Delayed

// FormletContext is passed to Formlet operations
type FormletContext =
  // Rebuild triggers a build up of FormletElements (ie the UI)
  abstract Rebuild                          : (unit -> unit) with get
  // Submit triggers a submit request (will not be processed if there validation failures)
  abstract Submit                           : (unit -> unit) with get
  // Reset triggers a reset of the FormletElements (back to initial state)
  abstract Reset                            : (unit -> unit) with get
  // Gets the Task result if available, otherwise awaits it and returns the fallback value
  abstract GetTaskResult                    : Task<'T> -> TaskResult<'T>

// CollectFailureTree is an aggregate of all validation failures seen during Collect
//  Empty means no validation failures exists and submit can proceed
//  A validation failure is pair of a path and the message. The path informs the user where the validation
//  failed
[<RequireQualifiedAccess>]
type CollectFailureTree =
  | Empty
  | Leaf  of string list*string
  | Group of string list*string []
  | Fork  of CollectFailureTree*CollectFailureTree

  member x.Join r =
    match x, r with
    | Empty           , _               -> r
    | _               , Empty           -> x
    | Group (_, [||]) , _               -> r
    | _               , Group (_, [||]) -> x
    | _               , _               -> Fork (x, r)

  member x.Flatten () : (string list*string) [] =
    let ra = ResizeArray<string list*string> 16
    let rec loop = function
      | Empty          -> ()
      | Leaf (p, s)    -> ra.Add (p, s)
      | Group (_, [||])-> ()
      | Group (p, ss)  ->
        for s in ss do
          ra.Add (p, s)
      | Fork (l, r)    ->
        loop l
        loop r
    loop x

    ra
    |> Seq.distinct
    |> Seq.toArray

[<RequireQualifiedAccess>]
type FormletTree =
  | Empty
  | Leaf    of IFormletElement
  | Fork    of FormletTree*FormletTree
  | Many    of FormletTree []
  | Adorner of IAdornerFormletElement*FormletTree
  | Label   of ILabelFormletElement*FormletTree
  | Tag     of string*FormletTree

  // TODO: This is often used with Adorners but it's not rock-solid
  //  In order to match the expected behavior should introduce
  //  wrapping element in those cases we are adorning a Fork
  //  If this is scrapped AllowNullLiteral can be removed on IFormletElement
  member x.FormletElement : IFormletElement =
    match x with
    | Empty           -> null
    | Leaf fe         -> fe
    | Fork (lft, rft) ->
      let re = rft.FormletElement
      if re <> null then re
      else lft.FormletElement
    | Many fts        ->
      if fts.Length > 0 then fts.[fts.Length - 1].FormletElement
      else null
    | Adorner (fe, _) -> upcast fe
    | Label (_, ft)   -> ft.FormletElement
    | Tag (_, ft)     -> ft.FormletElement

  member inline x.Split () : FormletTree*FormletTree =
    match x with
    | Fork (l,r)  -> l    , r
    | _           -> Empty, Empty

  member inline x.Join y = Fork (x, y)

  member inline x.GetMany () : FormletTree [] =
    match x with
    | Many fts  -> fts
    | _         -> [||]

  member inline x.GetTagged (l : string) : FormletTree =
    match x with
    | Tag (tag, t) when tag = l   -> t
    | _                           -> Empty

  member inline x.GetLeaf (ctor : unit -> 'T) : 'T =
    match x with
    | Leaf (:? 'T as fe)  -> fe
    | _                   -> ctor ()

  member inline x.GetAdorner (ctor : unit -> 'T) : 'T*FormletTree =
    match x with
    | Adorner (:? 'T as fe, ft) -> fe       , ft
    | _                         -> ctor ()  , Empty

  member inline x.GetLabel (ctor : unit -> 'T) : 'T*FormletTree=
    match x with
    | Label (:? 'T as fe, ft) -> fe       , ft
    | _                       -> ctor ()  , Empty

[<Struct>]
type FormletResult<'T>(v : 'T, cft : CollectFailureTree, ft : FormletTree) =
  member x.Value              = v
  member x.CollectFailureTree = cft
  member x.FormletTree        = ft

  member inline x.KeepRight (o : FormletResult<'U>) : FormletResult<'U> =
    FormletResult<'U> (o.Value, x.CollectFailureTree.Join o.CollectFailureTree, x.FormletTree.Join o.FormletTree)

  member inline x.MergeCollectFailureTree (cft : CollectFailureTree) : FormletResult<'T> =
    FormletResult<'T> (x.Value, cft, x.FormletTree)

  member inline x.ChangeValue (v : 'U) : FormletResult<'U> =
    FormletResult<'U> (v, x.CollectFailureTree, x.FormletTree)

  member inline x.ChangeFormletTree (ft : FormletTree) : FormletResult<'T> =
    FormletResult<'T> (x.Value, x.CollectFailureTree, ft)

let inline result (v : 'T) cft ft: FormletResult<'T> =
  FormletResult<'T> (v, cft, ft)

let inline success (v : 'T) ft : FormletResult<'T> =
  result v CollectFailureTree.Empty ft

// Formlet is a function that given a
//    formlet context
//    the path to the FormletTree
//    the FormletTree which represents the current UI state
//  produces a FormletResult which is
//    a best effort value
//    a CollectFailureTree that indicates if the value is valid
//    a FormletTree which represent the new UI state
type Formlet<'T> =
  | Formlet of (FormletContext*string list*FormletTree -> FormletResult<'T>)

  member inline x.Run (fc, p, ft) =
    let (Formlet f) = x
    f (fc, p, ft)

// Core formlet module, has classics like bind/return/...
[<RequireQualifiedAccess>]
module Formlet =
  open System

  // The all important bind operation
  //  Note that BuildUp returns a best effort value of 'T that will be used
  //  to call uf with.
  let bind (t : Formlet<'T>) (uf : 'T -> Formlet<'U>) : Formlet<'U> =
    Formlet <| fun (fc, p, ft) ->
      let tft, uft  = ft.Split ()
      let tfr       = t.Run (fc, p, tft)
      let u         = uf tfr.Value
      let ufr       = u.Run (fc, p, uft)
      tfr.KeepRight ufr

  // The all important return operation
  let return_ v : Formlet<'T> =
    Formlet <| fun (fc, p, ft) ->
      success v FormletTree.Empty

  // -------------------------------------------------------------------------

  let apply (t : Formlet<'A -> 'B>) (u : Formlet<'A>) : Formlet<'B> =
    Formlet <| fun (fc, p, ft) ->
      let tft, uft  = ft.Split ()
      let tfr       = t.Run (fc, p, tft)
      let ufr       = u.Run (fc, p, uft)
      (tfr.KeepRight ufr).ChangeValue (tfr.Value ufr.Value)

  let bindTask (t : Task<'T>) (ud : Formlet<'U>) (uf : 'T -> Formlet<'U>) : Formlet<'U> =
    Formlet <| fun (fc, p, ft) ->
      match fc.GetTaskResult t with
      | TaskResult.Result tv ->
        let u = uf tv
        u.Run (fc, p, ft)
      | _ ->
        // TODO: Handle exception states
        ud.Run (fc, p, ft)

  let many (ts : Formlet<'T> []) : Formlet<'T []> =
    Formlet <| fun (fc, p, ft) ->
      let n = ts.Length
      if n < 1 then
        success [||] FormletTree.Empty
      else
        let fts   = ft.GetMany ()
        let nvs   = Array.zeroCreate<'T> n
        let nfts  = Array.zeroCreate<FormletTree> n
        let rec loop i (ncft : CollectFailureTree) =
          if i < n then
            let tft       =  if i < fts.Length then fts.[i] else FormletTree.Empty
            let t         =  ts.[i]
            let tfr       =  t.Run (fc, p, tft)
            nvs.[i]       <- tfr.Value
            nfts.[i]      <- tfr.FormletTree
            loop (i + 1) (ncft.Join tfr.CollectFailureTree)
          else
            ncft
        let ncft = loop 0 CollectFailureTree.Empty
        result nvs ncft (FormletTree.Many nfts)

  let map (m : 'T -> 'U) (t : Formlet<'T>) : Formlet<'U> =
    Formlet <| fun (fc, p, ft) ->
      let tfr = t.Run (fc, p, ft)
      tfr.ChangeValue (m tfr.Value)

  let pair (t : Formlet<'T>) (u : Formlet<'U>) : Formlet<'T*'U> =
    Formlet <| fun (fc, p, ft) ->
      let tft, uft  = ft.Split ()
      let tfr       = t.Run (fc, p, tft)
      let ufr       = u.Run (fc, p, uft)
      (tfr.KeepRight ufr).ChangeValue (tfr.Value, ufr.Value)

  let returnFrom t : Formlet<'T> = t

  let tag (l : string) (t : Formlet<'T>) : Formlet<'T> =
    Formlet <| fun (fc, p, ft) ->
      let tft = ft.GetTagged l
      let tfr = t.Run (fc, p, tft)
      tfr.ChangeFormletTree (FormletTree.Tag (l, tft))

  let taskResult (t : Task<'T>) : Formlet<'T option> =
    Formlet <| fun (fc, p, ft) ->
      match fc.GetTaskResult t with
      | TaskResult.Result tv ->
        success (Some tv) FormletTree.Empty
      | _ ->
        // TODO: Handle exception states
        success None FormletTree.Empty

  let yield_ (tf : unit -> Formlet<'T>) : Formlet<'T> =
    Formlet <| fun (fc, p, ft) ->
      let t = tf ()
      t.Run (fc, p, ft)

  let inline zero () : Formlet<'T> =
    Formlet <| fun (fc, p, ft) ->
      success LanguagePrimitives.GenericZero<'T> FormletTree.Empty

  // -------------------------------------------------------------------------

  let generate (n : int) (tf : int -> Formlet<'T>) : Formlet<'T []> =
    let ts = Array.init n tf
    many ts

  let join (t : Formlet<Formlet<'T>>) : Formlet<'T> = bind t id

  // FormletBuilder is the computation expression builder that
  //  will allow us to define Formlets in a convenient way
  type FormletBuilder() =
    member inline x.Bind       (t, uf)   = bind        t uf
    member inline x.Return     v         = return_     v
    member inline x.ReturnFrom t         = returnFrom  t
    member inline x.Yield      tf        = yield_ tf
    member inline x.Zero       ()        = zero ()

let formlet = Formlet.FormletBuilder ()

module Infixes =
  let inline (<&>) t u  = Formlet.apply t u
  let inline (>>=) t uf = Formlet.bind  t uf
  let inline (|>>) t m  = Formlet.map   m t
  let inline (<*>) t u  = Formlet.pair  t u

// Ensure module contains various Formlets used for validation
[<RequireQualifiedAccess>]
module Ensure =
  open System
  open System.Text.RegularExpressions

  // usingValidator validates the 'T value using the validator function. The validator returns
  //  an array of strings representing all validation failures found during validation
  //  An empty array indicates success
  let usingValidator (validator : 'T -> string []) (t : Formlet<'T>) : Formlet<'T> =
    Formlet <| fun (fc, p, ft) ->
      let tfr   = t.Run (fc, p, ft)
      let errs  = validator tfr.Value
      if errs.Length = 0 then
        tfr
      else
        tfr.MergeCollectFailureTree (CollectFailureTree.Group (p, errs))

  // usingRegEx validates the string value using checkWith as a regex. If no match is found
  //  regex fails with failWith
  let usingRegEx (checkWith : string) (failWith : string) (t : Formlet<string>) : Formlet<string> =
    let f v = if Regex.IsMatch (v, checkWith) then [||] else [|failWith|]
    usingValidator f t

  // notEmpty validates the string value to be non-empty
  let notEmpty (t : Formlet<string>) : Formlet<string> =
    let f v = if String.IsNullOrWhiteSpace v then [|"Input a value"|] else [||]
    usingValidator f t

  // failWith always fails with message
  let failWith (m : string) (t : Formlet<'T>) : Formlet<'T> =
    let a   = [|m|]
    let f v = a
    usingValidator f t


module Wpf =
  module internal Elements =
    open System
    open System.Collections.Concurrent
    open System.Text
    open System.Windows
    open System.Windows.Controls
    open System.Windows.Data
    open System.Windows.Input
    open System.Windows.Media
    open System.Windows.Media.Animation
    open System.Windows.Documents

    let elementMargin = Thickness 2.
    let labelColumn   = 0
    let inputColumn   = 1

    let inline notsame f s =
      Object.ReferenceEquals (f, s) |> not

    let inline clamp v b e =
      if   v < b then b
      elif e < v then e
      else            v

    let inline clearChildren (p : Panel) : unit =
      p.Children.Clear ()

    let inline addChild (p : Panel) (c : UIElement) : unit =
      p.Children.Add c |> ignore

    let auto            = GridLength.Auto
    let inline star v   = GridLength (v, GridUnitType.Star)
    let inline pixel v  = GridLength (v, GridUnitType.Pixel)
    let star1           = star 1.

    let addGridColumn (g : Grid) (w : GridLength) : unit =
      g.ColumnDefinitions.Add (ColumnDefinition (Width = w))

    let addGridRow (g : Grid) (h : GridLength) : unit =
      g.RowDefinitions.Add (RowDefinition (Height = h))

    let addGridChild (g : Grid) x y cx cy  (ui : UIElement) : unit =
      Grid.SetColumn      (ui, x)
      Grid.SetRow         (ui, y)
      Grid.SetColumnSpan  (ui, cx)
      Grid.SetRowSpan     (ui, cy)
      g.Children.Add ui |> ignore

    let doNothing _     = ()
    let defaultCtor ()  = new 'T ()

    let button c onclick =
      let b     =  Button ()
      b.Margin  <- Thickness 2.
      b.Padding <- Thickness (12., 2., 12., 2.)
      b.Content <- c
      b.Click.Add onclick
      b

    type TaskRepository (rebuild : unit -> unit) =
      let repo  = ConcurrentDictionary<Task, bool> ()
      let r t   =
        repo.TryRemove t |> ignore
        rebuild ()

      member x.GetTaskResult<'T> (t : Task<'T>) : TaskResult<'T> =
        // TODO: Handle exception states
        if t.IsCompleted then
          TaskResult.Result t.Result
        elif t.IsCanceled then
          TaskResult.Exception t.Exception
        elif t.IsFaulted then
          TaskResult.Cancelled
        else
          if repo.TryAdd (t, true) then
            t.ContinueWith (Action<Task> r) |> ignore
          TaskResult.Delayed

    type WpfFormletContext(rebuild : unit -> unit, submit : unit -> unit, reset : unit -> unit, repo : TaskRepository) =
      interface FormletContext with
        override x.Rebuild          = rebuild
        override x.Submit           = submit
        override x.Reset            = reset
        override x.GetTaskResult t  = repo.GetTaskResult t
      end

    [<AllowNullLiteral>]
    [<Sealed>]
    type ErrorAdorner (ui : UIElement) =
      inherit Adorner (ui)

      let errorPen = Pen (Brushes.Red, 1.0)

      override x.OnRender ctx =
        base.OnRender ctx
        let sz = x.RenderSize
        ctx.DrawRectangle (null, errorPen, Rect sz)

    type Properties () =
      static let register nm t (dv : obj) = DependencyProperty.RegisterAttached (nm, t, typeof<Properties>, PropertyMetadata dv)
      static let errorAdornerProperty           = register "ErrorAdorner"       typeof<ErrorAdorner>        null
      static let collectFailureTreeProperty     = register "CollectFailureTree" typeof<CollectFailureTree>  CollectFailureTree.Empty
      static let clockProperty                  = register "Clock"              typeof<float>               0.

      static member ErrorAdornerProperty        = errorAdornerProperty
      static member CollectFailureTreeProperty  = collectFailureTreeProperty
      static member ClockProperty               = clockProperty

      static member GetFormletErrorAdorner (d : DependencyObject) : ErrorAdorner =
        downcast d.GetValue Properties.ErrorAdornerProperty
      static member SetFormletErrorAdorner (d : DependencyObject, ea : ErrorAdorner) : unit =
        if notsame (Properties.GetFormletErrorAdorner d) ea then
          d.SetValue (Properties.ErrorAdornerProperty, ea)

      static member GetCollectFailureTree (d : DependencyObject) : CollectFailureTree =
        downcast d.GetValue Properties.CollectFailureTreeProperty
      static member SetCollectFailureTree (d : DependencyObject, cft : CollectFailureTree) : unit =
        if Properties.GetCollectFailureTree d <> cft then
          d.SetValue (Properties.CollectFailureTreeProperty, cft)

      static member GetClock (d : DependencyObject) : float =
        downcast d.GetValue Properties.ClockProperty
      static member SetClock (d : DependencyObject, c : float) : unit =
        if Properties.GetClock d <> c then
          d.SetValue (Properties.ClockProperty, c)

    let toolTipOpening (o : obj) e =
      match o with
      | :? UIElement as ui ->
        let cft = Properties.GetCollectFailureTree ui
        match cft with
        | CollectFailureTree.Empty -> ToolTipService.SetToolTip (ui, "Ok")
        | _ ->
          let fs                = cft.Flatten ()
          let sb                = StringBuilder "Validation failed:"
          for (_, f) in fs do
            sb.AppendFormat ("\n  {0}", f) |> ignore
          ToolTipService.SetToolTip (ui, sb.ToString ())
      | _ -> ()

    let addErrorAdorner (ui : UIElement) : unit =
      let l = AdornerLayer.GetAdornerLayer ui
      if l <> null then
        let ea = Properties.GetFormletErrorAdorner ui
        let ea =
          if ea = null then
            let ea = ErrorAdorner ui
            Properties.SetFormletErrorAdorner (ui, ea)
            ToolTipService.SetToolTip (ui, "Ok")
            ToolTipService.AddToolTipOpeningHandler (ui, ToolTipEventHandler toolTipOpening)
            ea
          else
            ea
        if ui.IsVisible && ui.IsEnabled then
          ea.Visibility <- Visibility.Visible
        else
          ea.Visibility <- Visibility.Collapsed
        l.Remove ea
        l.Add ea

    let removeErrorAdorner (ui : UIElement) : unit =
      let l = AdornerLayer.GetAdornerLayer ui
      if l <> null then
        let ea = Properties.GetFormletErrorAdorner ui
        if ea <> null then
          l.Remove ea

    let updateErrorAdorner (f : IFormletElement) : unit =
      match f with
      | :? UIElement as ui  ->
        match Properties.GetCollectFailureTree ui with
        | CollectFailureTree.Empty  ->
          removeErrorAdorner ui
        | _                         -> addErrorAdorner    ui
      | _                   -> ()

    let setCollectFailureTree (f: IFormletElement) (cft : CollectFailureTree) : unit =
      match f with
      | :? UIElement as ui  -> Properties.SetCollectFailureTree (ui, cft)
      | _                   ->  ()

    [<Sealed>]
    type AdornedGrid () as ae =
      inherit Grid ()

      do
        addGridColumn ae auto
        addGridColumn ae star1

      member x.SetAdorned (ft : FormletTree) : unit =
        let rds = x.RowDefinitions
        let cs  = x.Children
        rds.Clear ()
        cs.Clear ()

        let inline add c (fe : IFormletElement) r =
          match fe with
          | :? UIElement as ui ->
            rds.Add (RowDefinition (Height = auto))
            Grid.SetColumn (ui, c)
            Grid.SetRow (ui, r)
            cs.Add ui |> ignore
            r + 1
          | _ -> r

        let rec loop t r =
          match t with
          | FormletTree.Empty           -> r
          | FormletTree.Leaf fe         -> add inputColumn fe r
          | FormletTree.Fork (lft, rft) ->
            let r = loop lft r
            loop rft r
          | FormletTree.Many fts        ->
            let rec iloop ii rr =
              if ii < fts.Length then
                let rr = loop fts.[ii] rr
                iloop (ii + 1) rr
              else
                rr
            iloop 0 r
          | FormletTree.Adorner (fe, _) -> add inputColumn fe r
          | FormletTree.Label (fe, ft)  ->
            add labelColumn fe r |> ignore  // Preserve row
            loop ft r
          | FormletTree.Tag (_, ft)     -> loop ft r
        loop ft 0 |> ignore

    [<AbstractClass>]
    type StackElement (o : Orientation) =
      inherit StackPanel (Orientation = o)

      let adorned = AdornedGrid ()

      member x.Adorned          = adorned
      member x.ClearAdorned ()  = clearChildren adorned
      member x.SetAdorned ft    = adorned.SetAdorned ft

      interface IAdornerFormletElement

    [<AbstractClass>]
    type GridElement () =
      inherit Grid ()

      let adorned = AdornedGrid ()

      member x.Adorned          = adorned
      member x.ClearAdorned ()  = clearChildren adorned
      member x.SetAdorned ft    = adorned.SetAdorned ft

      interface IAdornerFormletElement

    [<AbstractClass>]
    type BorderElement () =
      inherit Border ()

      let adorned = AdornedGrid ()

      member x.Adorned          = adorned
      member x.ClearAdorned ()  = clearChildren adorned
      member x.SetAdorned ft    = adorned.SetAdorned ft

      interface IAdornerFormletElement

    [<Sealed>]
    type BuzyElement () as be =
      inherit FrameworkElement ()

      static let animation  =
        let a             = DoubleAnimation (1., 0., TimeSpan.FromSeconds 0.5 |> Duration)
        a.RepeatBehavior  <- RepeatBehavior.Forever
        a.Freeze ()
        a

      static let count      = 12
      static let color      = Colors.Black

      static let brush i =
        let ratio         =  float i / float count
        let brush         =  SolidColorBrush color
        brush.Opacity     <- 0.
        let transform cx cy o=
          let m = Matrix.Identity
          m.Translate (o, 0.)
          m.Rotate (ratio * 360.)
          m.Translate (cx, cy)
          MatrixTransform m
        let opacity o     =
          brush.Opacity   <- (o + ratio) % 1.
        brush, transform, opacity

      static let textBrush  =
        let b = SolidColorBrush (color, Opacity = 0.75)
        b.Freeze ()
        b

      static let typeFace   = Typeface (FontFamily "Calibri", FontStyles.Normal, FontWeights.Normal, FontStretches.Normal)

      let brushes           = Array.init count brush

      let mutable buzyText  = "Please wait..."
      let formattedText sz  = FormattedText (buzyText, System.Globalization.CultureInfo.InvariantCulture, FlowDirection.LeftToRight, typeFace, sz, textBrush)

      let loaded e          =
        be.BeginAnimation   (Properties.ClockProperty, animation)

      let unloaded e        =
        be.BeginAnimation   (Properties.ClockProperty, null)

      do
        be.Margin       <- Thickness 4.
        be.Loaded.Add   loaded
        be.Unloaded.Add unloaded

      member x.BuzyText
        with get () = buzyText
        and  set v  =
          if buzyText <> v then
            buzyText <- v
            x.InvalidateVisual ()

      override x.OnPropertyChanged e =
        base.OnPropertyChanged e
        if e.Property = Properties.ClockProperty then
          if x.IsLoaded && x.Visibility = Visibility.Visible then
            let v = e.NewValue :?> float
            for _, _, opacity in brushes do
              opacity v

      override x.OnRender ctx =
        let cx      = x.ActualWidth
        let cy      = x.ActualHeight
        let xy      = min cx cy
        if xy > 0. then
          let ft      = formattedText xy

          let off     = xy / 4.
          let w       = xy / 4.
          let bw      = 2.* (off + w)
          let sw      = 8.
          let tw      = ft.Width
          let fw      = min cx (bw + sw + tw)
          let bh      = bw
          let th      = ft.Height
          let fh      = min cy (max bh th)
          let ox      = (cx  - fw) / 2.
          let oy      = (cy - fh) / 2.

          let h2      = w   / 5. / 2.
          let r       = Rect (0., -h2, w, 2.*h2)
          for brush, transform, _ in brushes do
            ctx.PushTransform (transform (ox + bw / 2.) (oy + bw / 2.) off)
            ctx.DrawRectangle (brush, null, r)
            ctx.Pop ()

          ctx.DrawText (ft, Point (ox + bw + sw, (cy - th) / 2.))

      interface IFormletElement

    [<Sealed>]
    type CheckElement () as ce =
      inherit CheckBox ()

      do
        ce.Margin     <- elementMargin
        ce.IsChecked  <- Nullable<bool> false

      member val Rebuild  = doNothing with get, set

      member x.Label
        with  get () : string   = downcast x.Content
        and   set (v : string)  = x.Content <- v

      override x.OnChecked e =
        base.OnChecked e
        x.Rebuild ()

      override x.OnUnchecked e =
        base.OnUnchecked e
        x.Rebuild ()

      interface IFormletElement

    [<Sealed>]
    type ChoiceElement<'T> () as ce =
      inherit ComboBox ()

      let mutable choices : (string*'T) []  = [||]

      do
        ce.Margin <- elementMargin

      member val Rebuild  = doNothing with get, set

      member x.Choice =
        let i = clamp x.SelectedIndex 0 (choices.Length - 1)
        let _, choice = choices.[i]
        choice

      member x.SetChoices i cs =
        if notsame choices cs then
          choices <- cs
          x.Items.Clear ()
          for t, _ in cs do
            x.Items.Add t |> ignore

          x.SelectedIndex <- clamp i 0 (choices.Length - 1)

      override x.OnSelectionChanged e =
        base.OnSelectionChanged e
        x.Rebuild ()

      interface IFormletElement

    [<Sealed>]
    type FormElement<'T> (onSubmit : 'T -> unit, f : Formlet<'T>) as fe =
      inherit BorderElement ()

      let mutable ft = FormletTree.Empty

      // TODO: Make sure we don't double dispatch Rebuilds
      let post (a : unit -> unit) : unit =
        let a             = Action a
        let d : Delegate  = upcast a
        fe.Dispatcher.BeginInvoke (Threading.DispatcherPriority.ApplicationIdle, d) |> ignore

      let rec rebuildAndCollect () =
        fe.ClearAdorned ()
        let fc          =  WpfFormletContext (postRebuild, postSubmit, postReset, repo)
        let tfr         =  f.Run (fc, [], ft)
        ft              <- tfr.FormletTree
        fe.SetAdorned ft

        // We must wait to update error adorners till after we setup the visual tree
        let rec loop t =
          match t with
          | FormletTree.Empty           -> ()
          | FormletTree.Leaf fe         -> updateErrorAdorner fe
          | FormletTree.Fork (lft, rft) -> loop lft; loop rft
          | FormletTree.Many fts        ->
            let rec iloop ii =
              if ii < fts.Length then
                let rr = loop fts.[ii]
                iloop (ii + 1)
            iloop 0
          | FormletTree.Adorner (fe, ft)-> updateErrorAdorner fe; loop ft
          | FormletTree.Label (fe, ft)  -> updateErrorAdorner fe; loop ft
          | FormletTree.Tag (_, ft)     -> loop ft
        loop ft

        match tfr.CollectFailureTree with
        | CollectFailureTree.Empty  -> Some tfr.Value
        | _                         -> None

      and rebuild () =
        rebuildAndCollect () |> ignore

      and submit () =
        match rebuildAndCollect () with
        | Some v  -> onSubmit v
        | _       -> ()

      and reset () =
        ft <- FormletTree.Empty
        rebuild ()

      and postRebuild ()  = post rebuild
      and postSubmit ()   = post submit
      and postReset ()    = post reset
      and repo            = TaskRepository postRebuild

      let loaded v        = postRebuild ()

      do
        fe.Loaded.Add loaded
        fe.Child <- fe.Adorned

    [<Sealed>]
    type GroupElement () as ge =
      inherit GridElement ()

      let label     =
        let label                 =  TextBlock ()
        label.Background          <- Brushes.White
        label.HorizontalAlignment <- HorizontalAlignment.Left
        label.VerticalAlignment   <- VerticalAlignment.Top
        label.RenderTransform     <- TranslateTransform (6., -8.)
        label

      let border    =
        let border                =  Border ()
        border.BorderThickness    <- Thickness 2.
        border.Padding            <- Thickness 4.
        border.CornerRadius       <- CornerRadius 4.
        border.BorderBrush        <- Brushes.LightBlue
        border.Child              <- ge.Adorned
        border

      do
        ge.Margin <- Thickness (0., 8., 0., 2.)
        addChild ge border
        addChild ge label

      member x.Label
        with  get () = label.Text
        and   set v  = label.Text <- v


    [<Sealed>]
    type LabelElement () as le =
      inherit TextBlock ()

      do
        le.Margin <- Thickness (2., 2., 8., 0.)

      interface ILabelFormletElement

    [<Sealed>]
    type ListElementListBoxItem () as lbi =
      inherit ListBoxItem ()

      let adorned = AdornedGrid ()

      do
        lbi.Content                     <- adorned
        lbi.HorizontalContentAlignment  <- HorizontalAlignment.Stretch

      member x.Adorned          = adorned
      member x.ClearAdorned ()  = clearChildren adorned
      member x.SetAdorned ft    = adorned.SetAdorned ft

    [<Sealed>]
    type ListElementListBox () as lb =
      inherit ListBox ()

      do
        lb.SelectionMode <- SelectionMode.Single
        KeyboardNavigation.SetTabNavigation (lb, KeyboardNavigationMode.Local)

      override x.GetContainerForItemOverride () =
        upcast ListElementListBoxItem ()

    [<RequireQualifiedAccess>]
    type ListElementAction =
      | Append
      | Remove  of int

    [<Sealed>]
    type ListElement () as le =
      inherit StackPanel (Orientation = Orientation.Vertical)

      let items   = ListElementListBox ()

      let actions = ResizeArray<ListElementAction> 16

      let append  =
        let click _ = actions.Add ListElementAction.Append; le.Rebuild ()
        button "_Append" click

      let remove  =
        let click _ =
          let i = items.SelectedIndex
          if i > -1 then
            actions.Add (ListElementAction.Remove i)
            le.Rebuild ()
        button "_Remove" click

      do
        let buttons             = StackPanel (Orientation = Orientation.Horizontal)
        addChild buttons append
        addChild buttons remove

        addChild le buttons
        addChild le items

      member x.Actions    = actions

      member val Rebuild  = doNothing with get, set

      member x.SetAdornedHeight (h : float) =
        items.Height <- h

      member x.SetManyAdorned (fts : FormletTree []) =
        let items = items.Items
        let lbis  = Array.zeroCreate items.Count

        let rec iloop i =
          if i < lbis.Length then
            lbis.[i] <- items.[i] :?> ListElementListBoxItem
            iloop (i + 1)
        iloop 0

        let rec cloop i =
          if i < lbis.Length then
            let lbi = lbis.[i]
            lbi.ClearAdorned ()
            cloop (i + 1)
        cloop 0

        let l = min fts.Length lbis.Length
        let rec sloop i =
          if i < l then
            let ft  = fts.[i]
            let lbi = lbis.[i]
            lbi.SetAdorned ft
            sloop (i + 1)
        sloop 0

        if fts.Length < lbis.Length then
          let rec rloop () =
            if fts.Length < items.Count then
              items.RemoveAt (items.Count - 1)
              rloop ()
          rloop ()
        elif lbis.Length < fts.Length then
          let rec aloop i =
            if i < fts.Length then
              let ft  = fts.[i]
              let lbi = ListElementListBoxItem ()
              lbi.SetAdorned ft
              items.Add lbi |> ignore
              aloop (i + 1)
          aloop lbis.Length
        else
          ()

      interface IAdornerFormletElement

    [<Sealed>]
    type TextElement () as te =
      inherit TextBox ()

      let mutable v   = ""

      let rebuild ()  =
        let t = te.Text
        if v <> t then
          v <- t
          te.Rebuild ()

      do
        te.Margin <- elementMargin

      member val Rebuild  = doNothing with get, set

      override x.OnKeyUp e =
        base.OnKeyUp e
        match e.Key with
        | Input.Key.Enter
        | Input.Key.Return  -> rebuild ()
        | _                 -> ()

      override x.OnLostFocus e =
        base.OnLostFocus e
        rebuild ()

      interface IFormletElement

    [<Sealed>]
    type SubmitElement () as se =
      inherit StackElement (Orientation.Vertical)

      let button c onclick =
        let b     =  Button ()
        b.Margin  <- Thickness 2.
        b.Padding <- Thickness (12., 2., 12., 2.)
        b.Content <- c
        b.Click.Add onclick
        b

      let submit  =
        let b       =  button "_Submit"  (fun _ -> se.Submit ())
        b.IsDefault <- true
        b
      let reset   = button "_Reset"   (fun _ -> se.Reset  ())

      let errors  =
        let tb                          =  TextBox ()
        tb.VerticalScrollBarVisibility  <- ScrollBarVisibility.Visible
        tb.MaxLines                     <- 6
        tb.IsReadOnly                   <- true
        tb.BorderThickness              <- Thickness ()
        tb

      do
        let border              =  Border ()
        border.BorderThickness  <- Thickness 2.
        border.Padding          <- Thickness 2.
        border.CornerRadius     <- CornerRadius 4.
        border.BorderBrush      <- Brushes.Gray

        let grid                =  Grid ()

        addGridRow    grid auto

        addGridColumn grid auto
        addGridColumn grid star1

        let buttons             = StackPanel (Orientation = Orientation.Vertical)

        addChild buttons submit
        addChild buttons reset

        addGridChild grid 0 0 1 1 buttons
        addGridChild grid 1 0 1 1 errors

        border.Child            <- grid

        addChild se border
        addChild se se.Adorned

      member val Submit = doNothing with get, set
      member val Reset  = doNothing with get, set

      member x.FailureTree
        with set (cft : CollectFailureTree) : unit =
          match cft with
          | CollectFailureTree.Empty ->
            submit.IsEnabled  <- true
            errors.Text       <- "Ready to submit!"
          | _ ->
            submit.IsEnabled      <- false
            let fs                =  cft.Flatten ()
            let sb                =  Text.StringBuilder 16
            let app (s : string)  =  sb.Append s |> ignore
            app "Please correct before submitting: "
            for (p, f) in fs do
              app "\n§ "
              match p with
              | []     -> app f
              | _  ->
                let rec loop = function
                  | p::ps -> app p; app ":"; loop ps
                  | _     -> ()
                loop (p |> List.rev)

                app " "
                app f
            errors.Text <- sb.ToString ()

  module Input =
    open Elements

    let buzy (h : float) (t : string) (v : 'T) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let te      =  ft.GetLeaf<BuzyElement> defaultCtor
        te.BuzyText <- t
        te.Height   <- h
        success v (FormletTree.Leaf te)


    // choice creates a ComboBox Formlet that produces different values depending on the
    //  state of the ComboBox
    let choice (initial : int) (choices : (string*'T) []) : Formlet<'T> =
      if choices.Length = 0 then
        failwith "choice requires non-empty choices input"
      Formlet <| fun (fc, p, ft) ->
        let te      =  ft.GetLeaf<ChoiceElement<'T>> defaultCtor
        te.SetChoices initial choices
        te.Rebuild  <- fc.Rebuild
        let tv      =  te.Choice
        success tv (FormletTree.Leaf te)

    // check creates a CheckBox Formlet that produces 2 different values depending on the
    //  state of the CheckBox
    let check (l : string) (notCheckedValue : 'T) (checkedValue : 'T) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let te      =  ft.GetLeaf<CheckElement> defaultCtor
        te.Label    <- l
        te.Rebuild  <- fc.Rebuild
        let tv      =
          if te.IsChecked.HasValue && te.IsChecked.Value then
            checkedValue
          else
            notCheckedValue
        success tv (FormletTree.Leaf te)

    // text creates a simple textbox input
    let text (initial : string) : Formlet<string> =
      let ctor () =
        let te = TextElement ()
        te.Text <- initial
        te
      Formlet <| fun (fc, p, ft) ->
        let te      =  ft.GetLeaf<TextElement> ctor
        te.Rebuild  <- fc.Rebuild
        let tv      =  te.Text
        success tv (FormletTree.Leaf te)

    // integer creates a simple textbox input that accept integer input
    let integer (initial : int option) : Formlet<int option> =
      let ctor () =
        let te = TextElement ()
        te.Text <-
          match initial with
          | Some i  -> i.ToString ()
          | None    -> ""
        te
      Formlet <| fun (fc, p, ft) ->
        let te      =  ft.GetLeaf<TextElement> ctor
        te.Rebuild  <- fc.Rebuild
        let tv      =  te.Text
        let tb, tv  =  System.Int32.TryParse tv
        if tb then
          success (Some tv) (FormletTree.Leaf te)
        else
          success None (FormletTree.Leaf te)

  module EnhanceWith =
    open Elements

    // errorVisualizer adds an error adorner to the FormletElement if a validation
    //  error is detected during Collect
    let errorVisualizer (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let tfr = t.Run (fc, p, ft)
        setCollectFailureTree tfr.FormletTree.FormletElement tfr.CollectFailureTree
        tfr

    // form creates a Form around the input Formlet
    //  onSubmit will be called when the user clicks the submit button
    let form (onSubmit : 'T -> unit) (t : Formlet<'T>) : System.Windows.FrameworkElement =
      let f     =  FormElement (onSubmit, t)
      upcast f

    // group adds a label and a group border around the input Formlet
    //  The label will be added to the vailidation failure path
    let group (l : string) (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let te, tft   =  ft.GetAdorner<GroupElement> defaultCtor
        te.Label      <- l
        let tfr       =  t.Run (fc, l::p, tft)
        te.SetAdorned tfr.FormletTree
        tfr.ChangeFormletTree (FormletTree.Adorner (te, tfr.FormletTree))

    // label adds a label to the input Formlet
    //  The label will be added to the vailidation failure path
    let label (l : string) (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let te, tft         =  ft.GetLabel<LabelElement> defaultCtor
        te.Text             <- l
        let tfr             =  t.Run (fc, l::p, tft)
        tfr.ChangeFormletTree (FormletTree.Label (te, tfr.FormletTree))

    // list adds a ListBox around the input Formlet
    //  TODO: list isn't fully done yet
    //    Validator doesn't trigger on first append
    //    Validator path is same for all list items
    //    Seems to flicker when clicking around
    //    Scrolls a full item
    //    Hard to differentiate between different items (use int -> Formlet<'T>)
    let list (h : float) (t : Formlet<'T>) : Formlet<'T []> =
      Formlet <| fun (fc, p, ft) ->
        let te, tft         =  ft.GetAdorner<ListElement> defaultCtor
        te.Rebuild          <- fc.Rebuild
        te.SetAdornedHeight h
        let fts             =  tft.GetMany ()
        let rafts           =  ResizeArray<FormletTree> fts
        let actions         =  te.Actions
        let rec loop i =
          if i < actions.Count then
            let action = actions.[i]
            match action with
            | ListElementAction.Append    ->
              rafts.Add FormletTree.Empty
            | ListElementAction.Remove i  ->
              if i < rafts.Count then
                rafts.RemoveAt i
            loop (i + 1)
        loop 0
        actions.Clear ()

        let nvs             = Array.zeroCreate rafts.Count
        let nfts            = Array.zeroCreate rafts.Count

        let rec loop (ncft : CollectFailureTree) i =
          if i < rafts.Count then
            let ft  =  rafts.[i]
            let tfr =  t.Run (fc, p, ft)
            nvs.[i] <- tfr.Value
            nfts.[i]<- tfr.FormletTree
            loop (ncft.Join tfr.CollectFailureTree) (i + 1)
          else
            ncft

        let ncft = loop CollectFailureTree.Empty 0

        te.SetManyAdorned nfts

        result nvs ncft (FormletTree.Adorner (te, (FormletTree.Many nfts)))

    // submit creates a "complex" UI that has submit && reset buttons as well
    //  as showing the validation errors
    let submit (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let te, tft       =  ft.GetAdorner<SubmitElement> defaultCtor
        te.Submit         <- fc.Submit
        te.Reset          <- fc.Reset
        let tfr           =  t.Run (fc, p, tft)
        te.FailureTree    <- tfr.CollectFailureTree
        te.SetAdorned tfr.FormletTree
        tfr.ChangeFormletTree (FormletTree.Adorner (te, tfr.FormletTree))

  [<RequireQualifiedAccess>]
  module MakeElement =
    open System.Windows

    // disabled disables a FormletElement
    let disabled (disabledValue : 'T) (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let tfr = t.Run (fc, p, ft)
        match tfr.FormletTree.FormletElement with
        | :? UIElement as ui  -> ui.IsEnabled <- false
                                 ui.Opacity   <- 0.5
        | _                   -> ()
        success disabledValue tfr.FormletTree

    // enabled enables a FormletElement, useful together with disable that disables a FormletElement.
    //  As the UI state is preserved the disabling is also preserved. enable makes sure the element is enabled again
    let enabled (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let tfr = t.Run (fc, p, ft)
        match tfr.FormletTree.FormletElement with
        | :? UIElement as ui  -> ui.IsEnabled <- true
                                 ui.Opacity   <- 1.
        | _                   -> ()
        tfr

    // hidden hides a FormletElement, useful when one likes to preserve
    //  UI state but not showing the UI element
    let hidden (hiddenValue : 'T) (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let tfr = t.Run (fc, p, ft)
        match tfr.FormletTree.FormletElement with
        | :? UIElement as ui  -> ui.Visibility <- Visibility.Collapsed
        | _                   -> ()
        success hiddenValue tfr.FormletTree

    // visible displays a FormletElement, useful together with hide that hides a FormletElement.
    //  As the UI state is preserved the hiding is also preserved. show makes sure the element is visible
    let visible (t : Formlet<'T>) : Formlet<'T> =
      Formlet <| fun (fc, p, ft) ->
        let tfr = t.Run (fc, p, ft)
        match tfr.FormletTree.FormletElement with
        | :? UIElement as ui  -> ui.Visibility <- Visibility.Visible
        | _                   -> ()
        tfr

