module FormletWindow =
  open FormletLibrary
  open FormletLibrary.Wpf

  open System.Windows
  open System.Windows.Controls
  open System.Windows.Media

  let start (f : Formlet<'T>) =
    let w             =  Window ()

    let onSubmit v    =  MessageBox.Show (w, sprintf "%A" v, "Submitted") |> ignore
    let form          =  f |> EnhanceWith.form onSubmit

    let sw            =  ScrollViewer ()
    sw.LayoutTransform<- ScaleTransform (2., 2.)
    sw.Padding        <- Thickness 4.
    sw.Content        <- form

    w.MinWidth        <- 800.
    w.MinHeight       <- 600.
    w.Title           <- "Formlets are cool!"
    w.Content         <- sw

    w.ShowDialog ()   |> ignore

module FormletListDemo =
  open FormletLibrary
  open FormletLibrary.Infixes
  open FormletLibrary.Wpf

  let l i = Input.text "" |> Ensure.notEmpty |> EnhanceWith.errorVisualizer |> EnhanceWith.label i

  let f =
    Formlet.return_ (fun co s z c o y -> co, s, z, c, o , y)
    <&> l "C/O"
    <&> l "Street"
    <&> l "Zip"
    <&> l "City"
    <&> l "County"
    <&> l "Country"


  let demoFormlet =
    formlet {
      let! t0 = f |> EnhanceWith.list 400.
      return t0
    } |> EnhanceWith.submit

module FormletBuzy =
  open System.Threading.Tasks

  open FormletLibrary
  open FormletLibrary.Infixes
  open FormletLibrary.Wpf

  let demoFormlet () =
    let task    = (Task.Delay 2000).ContinueWith (fun _ -> 3)
    let buzy v  = Input.buzy 24. "Please wait for data..." v |> Ensure.failWith "Wait for data"
    formlet {
      let! i0 = Formlet.taskResult task
      let! t0 =
        match i0 with
        | Some i0 -> Formlet.generate i0 (fun i -> Input.integer None)
        | None    -> buzy [||]
      return i0, t0
    } |> EnhanceWith.submit

open System

[<STAThread>]
[<EntryPoint>]
let main argv =
  FormletWindow.start FormletSample.customerFormlet
//  FormletWindow.start FormletListDemo.demoFormlet
//  FormletWindow.start <| FormletBuzy.demoFormlet ()
//  FormletWindow.start FormletDemo.demoFormlet
  0
