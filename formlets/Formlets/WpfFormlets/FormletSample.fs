module FormletSample
open FormletLibrary
open FormletLibrary.Infixes
open FormletLibrary.Wpf

type Address =
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
    { Address.Zero with
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
    InvoiceAddress    : Address
    DeliveryAddress   : Address option
  }
  static member Zero = { Kind = CustomerKind.Zero; InvoiceAddress = Address.Zero; DeliveryAddress = None }

let customerFormlet  =
  // labeled is a labeled textbox with some validation
  let inline labeled l v = Input.text "" |> v |> EnhanceWith.errorVisualizer |> EnhanceWith.label l

  let any l       = labeled l            <| id
  let notEmpty l  = labeled l            <| Ensure.notEmpty
  let socialNo    = labeled "Social No"  <| Ensure.usingRegEx @"^\d{6}-\d{4}$"  "Social No must look like: YYMMDD-XXXX"
  let companyNo   = labeled "Company No" <| Ensure.usingRegEx @"^\d{6}$"        "Company No must look like: 123456"
  let taxNo       = labeled "Tax No"     <| Ensure.usingRegEx @"^MVA\d{6}$"     "Tax No must look like: MVA123456"

  // A "simple" formlet collect address info
  let address l =
    Formlet.return_ Address.New
    <&> any       "C/O"
    <&> notEmpty  "Street"
    <&> notEmpty  "Zip"
    <&> notEmpty  "City"
    <&> any       "County"
    <&> notEmpty  "Country"
    |>  EnhanceWith.group l

  let invoiceAddress  = address "Invoice Address"

  // A "simple" formlet collect person info
  let person =
    Formlet.return_ Person.New
    <&> notEmpty "First name"
    <&> notEmpty "Last name"
    <&> socialNo
    |>> CustomerKind.Person
    |>  EnhanceWith.group "Person"

  // A "simple" formlet collect company info
  let company =
    Formlet.return_ Company.New
    <&> notEmpty "Name"
    <&> companyNo
    <&> taxNo
    |>> CustomerKind.Company
    |>  EnhanceWith.group "Company"

  // A "cool" formlet that uses the mustSelect to select either the person/company
  // formlet and then collect it to produce the value we are actually interested in
  let kind =
    Input.choice 0 [|"Person", person; "Company", company|]
    |> Formlet.join

  // Delivery address is optional
  let deliveryAddress =
    let f         = address "Delivery Address" |>> Some
    let disabled  = f |> MakeElement.disabled None
    let enabled   = f |> MakeElement.enabled

    Input.check "Use separate delivery address" disabled enabled
    |> Formlet.join

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
    } |> EnhanceWith.submit

  customer

