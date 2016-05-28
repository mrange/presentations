module FormletDemo

open FormletLibrary
open FormletLibrary.Infixes
open FormletLibrary.Wpf

let any       lbl = Input.text "" |> EnhanceWith.label lbl
let notEmpty  lbl = Input.text "" |> Ensure.notEmpty |> EnhanceWith.errorVisualizer |> EnhanceWith.label lbl

let address lbl =
  formlet {
    let! name   = notEmpty "Name"
    let! co     = any      "C/O"
    let! street = notEmpty "Street"
    let! zip    = notEmpty "Zip"
    let! city   = notEmpty "City"
    let! state  = notEmpty "State"
    return name, co, street, zip, city, state
  } |> EnhanceWith.group lbl

let demoFormlet =
  formlet {
    let! invoiceAddress   = address "Invoice Address"
    let! useSeparate      = Input.check "Use separate delivery address?" false true
    let! deliveryAddress  =
      if useSeparate then
        address "Delivery Address"
      else
        Formlet.return_ invoiceAddress
    return invoiceAddress, deliveryAddress
  } |> EnhanceWith.submit
