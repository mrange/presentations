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
open System.Text

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

let pcalc: Parser<AST>= spaces >>. pexpr .>> eof

[<EntryPoint>]
let main argv =
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
      match run pcalc input with
      | Success (ast, _, _) ->
        printfn "AST  : %A" ast
        printfn "print: %s" <| toString ast
        printfn "eval : %d" <| eval l ast
      | Failure (msg, _, _) ->
        printfn "%s" msg

      loop ()

  loop ()

  0
