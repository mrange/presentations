// ----------------------------------------------------------------------------------------------
// Copyright (c) Mårten Rånge.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A
// copy of the license can be found in the License.html file at the root of this distribution.
// If you cannot locate the  Microsoft Public License, please send an email to
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------
open System
open System.Globalization
open System.Text
open System.Threading
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module CalculatorModule =
  open FParsec.Primitives
  open FParsec.CharParsers

  type UserState  = unit
  type Parser<'t> = Parser<'t, UserState>

  type BinaryOp =
    | Add
    | Subtract
    | Multiply
    | Divide

  type AST =
    | Integer   of int
    | Variable  of string
    | Binary    of AST*BinaryOp*AST

  let rec eval (lookup : string -> int) ast =
    match ast with
    | Integer i       -> i
    | Variable v      -> lookup v
    | Binary (l,op,r) ->
      let fop=
        match op with
        | Add       -> ( + )
        | Subtract  -> ( - )
        | Multiply  -> ( * )
        | Divide    -> ( / )

      let lv = eval lookup l
      let rv = eval lookup r

      fop lv rv

  let toString ast =
    let sb = StringBuilder ()

    let rec str ast =
      match ast with
      | Integer i       -> ignore <| sb.Append i
      | Variable v      -> ignore <| sb.Append v
      | Binary (l,op,r) ->
        let sop=
          match op with
          | Add       -> " + "
          | Subtract  -> " - "
          | Multiply  -> " * "
          | Divide    -> " / "

        ignore <| sb.Append '('
        str l
        ignore <| sb.Append sop
        str r
        ignore <| sb.Append ')'

    str ast

    sb.ToString ()

  let past : Parser<AST> =
    let charToOp = function
        | '+' -> Add
        | '-' -> Subtract
        | '*' -> Multiply
        | '/' -> Divide
        | op -> failwith "Unexpected op : %A" op

    let pexpr, prexpr     = createParserForwardedToRef<AST, unit> ()
    let ptoken ch         = skipChar ch .>> spaces
    let pcharToBinary ops = anyOf ops .>> spaces |>> fun ch -> fun l r -> Binary (l, charToOp ch, r)
    let pint              = pint32 |>> Integer
    let pvar              = many1Satisfy2L Char.IsLetter Char.IsLetterOrDigit "variable" |>> Variable
    let psub              = between (ptoken '(') (ptoken ')') pexpr
    let pterm             = choice [pvar; pint; psub] .>> spaces
    let p0                = chainl1 pterm (pcharToBinary "*/")
    let p1                = chainl1 p0 (pcharToBinary "+-")
    do prexpr             := p1

    spaces >>. pexpr .>> eof
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module JSONModule =
  open FParsec.Primitives
  open FParsec.CharParsers

  type UserState  = unit
  type Parser<'t> = Parser<'t, UserState>

  type JSON =
    | NullValue
    | BooleanValue  of bool
    | NumberValue   of float
    | StringValue   of string
    | ArrayValue    of JSON list
    | ObjectValue   of (string*JSON) list

  let toString json =
    let sb = StringBuilder ()

    let appStr (s : string) =
      ignore <| sb.Append '"'
      let e = s.Length - 1
      for i = 0 to e do
        ignore <| match s.[i] with
        | '\"'  -> sb.Append @"\"""
        | '\\'  -> sb.Append @"\\"
        | '/'   -> sb.Append @"\/"
        | '\b'  -> sb.Append @"\b"
        | '\f'  -> sb.Append @"\f"
        | '\n'  -> sb.Append @"\n"
        | '\r'  -> sb.Append @"\r"
        | '\t'  -> sb.Append @"\t"
        | c     -> sb.Append c
      ignore <| sb.Append '"'

    let rec impl = function
      | NullValue           ->
        ignore <| sb.Append "null"
      | BooleanValue true   ->
        ignore <| sb.Append "true"
      | BooleanValue false  ->
        ignore <| sb.Append "false"
      | NumberValue n  ->
        ignore <| sb.Append n
      | StringValue s       ->
        appStr s
      | ArrayValue vs       ->
        ignore <| sb.Append '['
        let mutable prepend = ""
        for v in vs do
          ignore <| sb.Append prepend
          impl v
          prepend <- ", "
        ignore <| sb.Append ']'
      | ObjectValue ms       ->
        ignore <| sb.Append '{'
        let mutable prepend = ""
        for k,v in ms do
          ignore <| sb.Append prepend
          appStr k
          ignore <| sb.Append ':'
          impl v
          prepend <- ", "
        ignore <| sb.Append '}'

    impl json

    sb.ToString ()

  let pjson =
    let puint64 = puint64 <?> "digit"

    let parray  , rparray   = createParserForwardedToRef<JSON, unit> ()

    let pobject , rpobject  = createParserForwardedToRef<JSON, unit> ()

    let pnull               = stringReturn "null" NullValue

    let pboolean            = stringReturn "true" (BooleanValue true) <|> stringReturn "false" (BooleanValue false)

    let prawint             = pipe3 getPosition puint64 getPosition (fun p ui n -> ui,(n.Index - p.Index))

    let pnumber =
      let psign : Parser<float->float>=
        charReturn '-' (fun d -> -d)
        <|>% id
      let pfrac =
        pipe2 (skipChar '.') prawint (fun _ (ui,i) -> (float ui) * (pown 10.0 (int -i)))
        <|>% 0.0
      let pexp =
        pipe3 (anyOf "eE") psign puint64 (fun _ sign ui -> pown 10.0 (int (sign (double ui))))
        <|>% 1.0
      let pzero =
        charReturn '0' 0.0
      let pfull =
        pipe3 puint64 pfrac pexp (fun i f e -> (float i + f)*e)
      pipe2 psign (pzero <|> pfull) (fun s n -> NumberValue (s n))

    let prawstring =
      let phex =
        hex
        |>> fun ch ->
          if Char.IsDigit ch then int ch - int '0'
          elif ch >= 'A' && ch <= 'F' then int ch - int 'A' + 10
          elif ch >= 'a' && ch <= 'f' then int ch - int 'a' + 10
          else 0
      let pstring_token = skipChar '"'
      let pesc_token    = skipChar '\\'
      let pch   = satisfyL (function '"' | '\\' -> false | _ -> true) "char"
      let pech  =
        let psimple =
          anyOf "\"\\/bfnrt"
          |>> function
            | 'b' -> '\b'
            | 'f' -> '\f'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | c   -> c
        let punicode =
          pipe5 (skipChar 'u') phex phex phex phex (fun _ v0 v1 v2 v3 ->
            let ui = (v0 <<< 12) + (v1 <<< 8) + (v2 <<< 4) + v3
            char ui
            )
        pesc_token
        >>. (psimple <|> punicode)
      let pstr  = manyChars (choice [pch; pech])
      between pstring_token pstring_token pstr

    let pstring = prawstring |>> StringValue

    let pvalue  = choice [pnull; pboolean; pnumber; pstring; parray; pobject] .>> spaces

    let ptk ch = skipChar ch .>> spaces

    let parr =
      let pvalues = sepBy pvalue (ptk ',') |>> ArrayValue
      between (ptk '[') (ptk ']') pvalues

    let pobj =
      let pmember =
        pipe3 (prawstring .>> spaces) (ptk ':') pvalue (fun k _ v -> k,v)
      let pmembers = sepBy  pmember (ptk ',') |>> ObjectValue
      between (ptk '{') (ptk '}') pmembers

    rparray   := parr
    rpobject  := pobj

    spaces >>. (pobject <|> parray) .>> eof
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open FParsec.Primitives
open FParsec.CharParsers
// ----------------------------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
  Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture

  let l = function
    | "x" -> 3
    | "y" -> 5
    | _ -> 0

  let rec loop () =
    printfn "Type expression or blank line to exit"
    let input = Console.ReadLine()

    if input.Length = 0 then
      ()
    else
      match run JSONModule.pjson input with
      | Success (ast, _, _) ->
        printfn "AST  : %A" ast
        printfn "print: %s" <| JSONModule.toString ast
//        printfn "eval : %d" <| CalculatorModule.eval l ast
      | Failure (msg, _, _) ->
        printfn "%s" msg

      loop ()

  loop ()

  0
// ----------------------------------------------------------------------------------------------
