[<WebSharper.Pervasives.JavaScript>]
module WebSharperFormlets.FormletDemo
open WebSharper.Formlets

let formlet = WebSharper.Formlets.FormletBuilder ()

let demoFormlet () =
  formlet {
    let! t0 = Controls.Input ""
    return t0
  }
