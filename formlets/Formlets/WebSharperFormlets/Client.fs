namespace WebSharperFormlets

open WebSharper
open WebSharper.Formlets
open WebSharper.JavaScript
open WebSharper.Html.Client

[<JavaScript>]
module FormletSample =
  let formlet = WebSharper.Formlets.FormletBuilder ()

  type StreetAddress =
    {
      CarryOver : string
      Street    : string
      Zip       : string
      City      : string
      County    : string
      Country   : string
    }
    static member Zero = { CarryOver = ""; Street = ""; Zip = ""; City = ""; County = ""; Country = "" }
    static member New carryOver street zip city county country =
      { StreetAddress.Zero with
          CarryOver = carryOver
          Street    = street
          Zip       = zip
          City      = city
          County    = county
          Country   = country
      }

  type Person =
    {
      FirstName : string
      LastName  : string
      SocialNo  : string
    }
    static member Zero = { FirstName = ""; LastName = ""; SocialNo = "" }
    static member New firstName lastName socialNo =
      { Person.Zero with
          FirstName = firstName
          LastName  = lastName
          SocialNo  = socialNo
      }

  type Company =
    {
      Name      : string
      CompanyNo : string
      TaxNo     : string
    }
    static member Zero = { Name = ""; CompanyNo = ""; TaxNo = "" }
    static member New name companyNo taxNo =
      { Company.Zero with
          Name      = name
          CompanyNo = companyNo
          TaxNo     = taxNo
      }

  type CustomerKind =
    | Person  of Person
    | Company of Company
    static member Zero = Person Person.Zero

  type Customer =
    {
      Kind              : CustomerKind
      InvoiceAddress    : StreetAddress
      DeliveryAddress   : StreetAddress option
    }
    static member Zero = { Kind = CustomerKind.Zero; InvoiceAddress = StreetAddress.Zero; DeliveryAddress = None }

  // labeled is a labeled textbox with some validation
  let labeled l v = Controls.Input "" |> v |> Enhance.WithValidationIcon |> Enhance.WithTextLabel l

  let customerFormlet () =

    let any l       = labeled l            <| id
    let notEmpty l  = labeled l            <| Validator.IsNotEmpty    (sprintf "%s must not be empty" l)
    let socialNo    = labeled "Social No"  <| Validator.IsRegexMatch  @"^\d{6}-\d{4}$"  "Social No must look like: YYMMDD-XXXX"
    let companyNo   = labeled "Company No" <| Validator.IsRegexMatch  @"^\d{6}$"        "Company No must look like: 123456"
    let taxNo       = labeled "Tax No"     <| Validator.IsRegexMatch  @"^MVA\d{6}$"     "Tax No must look like: MVA123456"

    // A "simple" formlet collect address info
    let streetAddress l =
      Formlet.Return StreetAddress.New
      <*> any       "C/O"
      <*> notEmpty  "Street"
      <*> notEmpty  "Zip"
      <*> notEmpty  "City"
      <*> any       "County"
      <*> notEmpty  "Country"
      |>  Enhance.WithLegend l

    let invoiceAddress  = streetAddress "Invoice Address"

    // A "simple" formlet collect person info
    let person =
      Formlet.Return Person.New
      <*> notEmpty "First name"
      <*> notEmpty "Last name"
      <*> socialNo
      |>  Formlet.Map CustomerKind.Person
      |>  Enhance.WithLegend "Person"

    // A "simple" formlet collect company info
    let company =
      Formlet.Return Company.New
      <*> notEmpty "Name"
      <*> companyNo
      <*> taxNo
      |>  Formlet.Map CustomerKind.Company
      |>  Enhance.WithLegend "Company"

    // A "cool" formlet that uses the mustSelect to select either the person/company
    // formlet and then collect it to produce the value we are actually interested in
    let kind =
      Controls.Select 0 ["Person", person; "Company", company]
      |> Formlet.Join

    // Delivery address is optional
    let deliveryAddress =
      formlet {
        let! useSeparate      = Controls.Checkbox false |> Enhance.WithTextLabel "Use separate delivery address"
        let! deliveryAddress  =
          if useSeparate then
            streetAddress "Delivery Address" |> Formlet.Map Some
          else
            Formlet.Return None
        return deliveryAddress
      }

    let customer =
      formlet {
        let!    kind            = kind
        let!    invoiceAddress  = invoiceAddress
        let!    deliveryAddress = deliveryAddress
        return
          { Customer.Zero with
              Kind            = kind
              InvoiceAddress  = invoiceAddress
              DeliveryAddress = deliveryAddress
          }
      }

    customer

[<JavaScript>]
module FormletDemo =
  let formlet = WebSharper.Formlets.FormletBuilder ()

  let demoFormlet () =
    formlet {
      let! t0 = Controls.Input ""
      return t0
    }
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
