namespace WebSharperFormlets

open WebSharper
open WebSharper.Formlets
open WebSharper.JavaScript
open WebSharper.Html.Client

[<JavaScript>]
module Client =
  open FormletSample

  [<JavaScript>]
  let Formlet () : Formlet<_> =
//    FormletSample.customerFormlet ()
    FormletDemo.demoFormlet ()
    |> Enhance.WithSubmitAndResetButtons
    |> Enhance.WithErrorSummary "Error Summary"
    |> Enhance.WithFormContainer

  let Main () =
    Div [Formlet ()]
