module FormletDemo

open FormletLibrary
open FormletLibrary.Infixes
open FormletLibrary.Wpf

let demoFormlet =
  formlet {
    let! t0 = Input.text ""
    return t0
  } |> EnhanceWith.submit
