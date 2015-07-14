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
module ParserModule =
  type Error =
    | NoError
    | Expected    of string
    | Unexpected  of string
    | Fork        of Error*Error
    | Group       of Error list

  // A Parser is a function that:
  //  Takes an input string
  //  Takes an integer pointing to the next unconsumed character in the  input string
  //  Takes an integer pointing at the position error messages should be collected
  //  Returns an optional result
  //  Returns accumulated error messages for the error position
  //  Returns the next unconsumed position
  //  Returns the most unconsumed position
  type Parser<'T> = string*int*int -> 'T option*Error*int*int

  module Detail =
    let ueos        = Unexpected "EOS"
    let eeos        = Expected "EOS"

    let inline err pe pos epos =
      if pos = epos then
        pe
      else
        NoError
    let inline errjoin lpe rpe =
      match lpe, rpe with
      | NoError , _       -> rpe
      | _       , NoError -> lpe
      | _       , _       -> Fork (lpe, rpe)
    let inline errgroup errs  =
      let ee = errs |> List.filter (function NoError -> false | _ -> true)
      if ee.IsEmpty then NoError
      else Group ee

  open Detail

  let result  ov  err pos mpos  = ov, err, pos, mpos
  let success v   err pos mpos  = result (Some v) err pos mpos
  let failure     err pos mpos  = result None err pos mpos

  // Parser "Atoms"
  let preturn (v : 'T) : Parser<'T> =
    fun (s,pos,epos) ->
      success v NoError pos pos

  let pfail (pe : Error) : Parser<'T> =
    fun (s,pos,epos) ->
      failure (err pe pos epos) pos pos

  let peos : Parser<unit> =
    fun (s,pos,epos) ->
      if pos >= s.Length then
        success () NoError pos pos
      else
        failure (err eeos pos epos) pos pos

  let psatisfy pe f : Parser<char> =
    let pe = errgroup [ueos; pe]
    fun (s,pos,epos) ->
      if pos >= s.Length then
        failure (err pe pos epos) pos pos
      elif f s.[pos] then
        success s.[pos] NoError (pos + 1) (pos + 1)
      else
        failure (err pe pos epos) pos pos

  let pchar       = psatisfy (Expected "char")        <| fun _ -> true
  let pdigit      = psatisfy (Expected "digit")       Char.IsDigit
  let phexdigit   = psatisfy (Expected "hexdigit")    <| fun ch ->
    (Char.IsDigit ch) || (ch >= 'A' && ch <= 'F') || (ch >= 'a' && ch <= 'f')
  let pletter     = psatisfy (Expected "letter")      Char.IsLetter
  let pwhitespace = psatisfy (Expected "whitespace")  Char.IsWhiteSpace

  let pskip (ch : char) : Parser<unit> =
    let pe = errgroup [ueos; Expected <| sprintf "'%s'" (ch.ToString ())]
    fun (s,pos,epos) ->
      if pos >= s.Length then
        failure (err pe pos epos) pos pos
      elif s.[pos] = ch then
        success () NoError (pos + 1) (pos + 1)
      else
        failure (err pe pos epos) pos pos

  let pstr (exp : string) : Parser<unit> =
    let expected = Expected <| sprintf "'%s'" exp
    fun (s,pos,epos) ->
      let e = pos + exp.Length
      if e > s.Length then
        failure (err expected pos epos) pos pos
      else
        let rec loop pp =
          if pp < e then
            let c = s.[pp] = exp.[pp - pos]

            c && (loop (pp + 1))
          else
            true

        if loop pos then
          success () (err expected pos epos) e e
        else
          failure (err expected pos epos) pos pos

  let panyOf (anyOf : string) : Parser<char> =
    let pe =
      anyOf
      |> Seq.map (fun ch -> Expected <| sprintf "'%s'" (ch.ToString ()))
      |> Seq.toList
      |> errgroup
    psatisfy pe (fun ch -> (anyOf.IndexOf ch) > -1)

  // Parser modifiers
  let pbind
    (t : Parser<'T>)
    (fu : 'T -> Parser<'U>) : Parser<'U> =
    fun (s,pos,epos) ->
      let ovt,terr,tpos, tmpos = t (s,pos,epos)
      match ovt with
      | Some vt ->
        let u = fu vt
        let ovu, uerr, upos, umpos = u (s,tpos,epos)
        result ovu (errjoin terr uerr) upos (max tmpos umpos)
      | _ ->
        failure terr tpos tmpos
  let inline (>>=) t fu     = pbind t fu

  let inline pright t u     = t >>= fun _ -> u
  let inline pcombine t u   = pright t u
  let inline (>>.) t u      = pright t u

  let inline pleft t u      = t >>= fun left -> u >>= fun _ -> preturn left
  let inline (.>>) t u      = pleft t u

  let inline pbetween b e p = b >>. p .>> e

  let inline pmap p m       = p >>= fun v -> preturn (m v)
  let inline (|>>) p m      = pmap p m

  let inline (>>%) p v      = p >>= fun _ -> preturn v

  let pdebug (p : Parser<'T>) : Parser<'T> =
    fun (s,pos,epos) ->
      let ovp, perr, ppos, pmpos = p (s,pos,epos)
      result ovp perr ppos pmpos

  let ptrampoline<'T> () : Parser<'T>*Parser<'T> ref =
    let r = ref <| preturn Unchecked.defaultof<'T>
    let p =
      fun (s,pos,epos) ->
        let ovp, perr, ppos, pmpos = !r (s,pos,epos)
        result ovp perr ppos pmpos
    p, r

  let popt (p : Parser<'T>) : Parser<'T option> =
    fun (s,pos,epos) ->
      let ovp, perr, ppos, pmpos = p (s,pos,epos)
      match ovp with
      | Some vp ->
        success (Some vp) perr ppos pmpos
      | _ ->
        success None perr pos pmpos
  let inline (>>?) p v      = popt p >>= function None -> preturn v | Some vv -> preturn vv

  let pmany (p : Parser<'T>) : Parser<'T list> =
    fun (s,pos,epos) ->
      let rec loop r cpos =
        let ovp, perr, ppos, pmpos = p (s,cpos,epos)
        match ovp with
        | Some vp ->
          loop ((perr,vp)::r) ppos
        | _ ->
          let errs, vs = r |> List.rev |> List.unzip
          let group = errgroup (perr::errs)
          success vs group cpos pmpos

      loop [] pos

  let pwhitespaces= pmany pwhitespace >>% ()

  let pmany1 p = pmany p >>= fun l -> if l.IsEmpty then pfail NoError else preturn l

  let mapListToString = List.toArray >> fun cs -> String(cs)

  let pmanyStr p  = pmany p |>> mapListToString
  let pmanyStr1 p = pmany1 p |>> mapListToString

  let pmanySepBy1 (p : Parser<'T>) (psep : Parser<_>) : Parser<'T list> =
    p
    >>= fun first ->
      pmany (psep >>. p)
      >>= fun rest -> preturn (first::rest)

  let pmanySepBy (p : Parser<'T>) (psep : Parser<_>) : Parser<'T list> =
    pmanySepBy1 p psep
    >>? []

  let pchainl (p : Parser<'T>) (psep : Parser<'T -> 'T -> 'T>) : Parser<'T> =
    fun (s,pos,epos) ->
      let rec loop v errs cpos cmpos =
        let ovs, serr, spos, smpos = psep (s,cpos,epos)
        let maxmpos = max smpos cmpos
        match ovs with
        | Some vs ->
          let ovp, perr, ppos, pmpos = p (s,spos,epos)
          let maxmpos = max pmpos maxmpos
          match ovp with
          | Some vp ->
            loop (vs v vp) (perr::serr::errs) ppos maxmpos
          | _ ->
            failure (errgroup (perr::serr::errs)) ppos maxmpos
        | None ->
          success v (errgroup (serr::errs)) cpos maxmpos

      let ovp, perr, ppos, pmpos = p (s,pos,epos)
      match ovp with
      | Some vp ->
          loop vp [perr] ppos pmpos
      | _ ->
        failure perr ppos pmpos

  let pchoice (ps : Parser<'T> list) : Parser<'T> =
    fun (s,pos,epos) ->
      // eloop is used to collect errors at epos
      let rec eloop perrs mpos ps =
        match ps with
        | [] ->
          mpos, errgroup perrs
        | p::ps ->
          let _, perr, _, pmpos = p (s,pos,epos)
          eloop (perr::perrs) (max pmpos mpos) ps

      // loop tests parsers until it find the first match
      let rec loop perrs mpos ps =
        match ps with
        | [] ->
          failure (errgroup perrs) mpos mpos
        | p::ps ->
          let ovp, perr, ppos, pmpos= p (s,pos,epos)
          match ovp with
          | Some vp ->
            let lmpos = max pmpos mpos
            let lmpos, err =
              if epos = pmpos then
                eloop (perr::perrs) lmpos ps
              else
                lmpos, NoError
            success vp err ppos pmpos
          | _ ->
            loop (perr::perrs) (max pmpos mpos) ps

      loop [] 0 ps

  let pnatural : Parser<uint64*int> =
    pmany1 pdigit
    |>> fun digits ->
      let res = digits |> List.fold (fun s ch -> s * 10UL + (uint64 ch - uint64 '0')) 0UL
      res, digits.Length

  let puint64 = pnatural |>> fun (ui,_) -> ui
  let puint32 = pnatural |>> fun (ui,_) -> uint32 ui

  let pdelay (ft : unit -> Parser<'T>) : Parser<'T> =
    fun (s,pos,epos) ->
      let t = ft ()
      t (s,pos,epos)

  // Computation Expression builder
  type ParserBuilder()=
    member x.Delay(ft)      = pdelay ft
    member x.Return(v)      = preturn v
    member x.Bind(t,fu)     = pbind t fu
    member x.Combine(t,u)   = pcombine t u

  let parser = ParserBuilder()

  // Parse function
  type Result<'T> =
    | Success of 'T*int
    | Failure of string*int

  let parse (p : Parser<'T>) (s : string) : Result<'T> =
    let ov, _, pos, mpos = p (s, 0, Int32.MaxValue)
    match ov with
    | Some v -> Success (v, pos)
    | None ->
      // If parsing failed, rerun the parser with the error position set
      //  This will return all detected errors at that position
      //  This approach reduces error collection cost during successful parses
      let _, err, _, _ = p (s, 0, mpos)

      let sb = StringBuilder ()
      let newline () = ignore <| sb.AppendLine ()
      let append (ss : string) = ignore <| sb.Append ss
      let prelude = "Failed to parse: "

      append <| sprintf "%s%s" prelude s

      newline ()
      ignore <| sb.Append(' ', prelude.Length + pos)
      ignore <| sb.Append '^'

      let es = ResizeArray<string> ()
      let ues = ResizeArray<string> ()

      let rec extractError = function
      | NoError       -> ()
      | Expected e    -> es.Add e
      | Unexpected ue -> ues.Add ue
      | Fork (l,r)    ->
        extractError l
        extractError r
      | Group es ->
        es |> List.iter extractError

      extractError err

      let buildString description (vs : ResizeArray<string>) =
        let ss = vs |> Seq.sort |> Seq.distinct |> Seq.toArray

        if ss.Length = 0 then
          ()
        else
          newline ()

          append description

          let e = ss.Length - 1

          for i = 0 to e do
            let s = ss.[i]
            let prepend =
              if    i = 0 then  ""
              elif  i = e then  " or "
              else              ", "

            ignore <| sb.Append prepend
            ignore <| sb.Append (ss.[i])

      buildString "Expected " es
      buildString "Unexpected " ues

      Failure (sb.ToString (), pos)
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module CalculatorModule =
  open ParserModule
  open System.Text

  type BinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide
    | Modulo

  let binaryOperation = function
    | Add       -> ( + )
    | Subtract  -> ( - )
    | Multiply  -> ( * )
    | Divide    -> ( / )
    | Modulo    -> ( % )

  let binaryOperatorName = function
    | Add       -> '+'
    | Subtract  -> '-'
    | Multiply  -> '*'
    | Divide    -> '/'
    | Modulo    -> '%'

  type Expression =
    | Value     of  int
    | Variable  of  string
    | Binary    of  Expression*BinaryOperator*Expression

  let mapBinaryOperator =
    let bin op l r = Binary (l, op, r)
    function
    | '+' -> bin Add
    | '-' -> bin Subtract
    | '*' -> bin Multiply
    | '/' -> bin Divide
    | '%' -> bin Modulo
    | _   -> bin Add

  let rec eval (lookup : string->int) = function
    | Value     i     -> i
    | Variable  v     -> lookup v
    | Binary (l,bop,r)->
      let lv  = eval lookup l
      let rv  = eval lookup r
      let bf  = binaryOperation bop
      bf lv rv

  let stringifyExpression e =
    let sb = StringBuilder ()
    let rec impl = function
      | Value     i     ->
        ignore <| sb.Append i
      | Variable  v     ->
        ignore <| sb.Append '\''
        ignore <| sb.Append v
        ignore <| sb.Append '\''
      | Binary (l,bop,r)->
        ignore <| sb.Append '('
        impl l
        ignore <| sb.Append (binaryOperatorName bop)
        impl r
        ignore <| sb.Append ')'

    impl e

    sb.ToString ()

  let pexpression =
    let ptk ch        = pskip ch .>> pwhitespaces

    let pexpr, rpexpr = ptrampoline<Expression> ()
    let pvalue        = puint32 |>> fun ui -> Value (int ui)
    let pvariable     = pmanyStr1 pletter |>> Variable
    let psubexpr      = pbetween (ptk '(') (ptk ')') pexpr
    let pterm         = pchoice [pvalue; pvariable; psubexpr] .>> pwhitespaces
    let pop0          = panyOf "*/%" |>> mapBinaryOperator    .>> pwhitespaces
    let plevel0       = pchainl pterm pop0
    let pop1          = panyOf "+-" |>> mapBinaryOperator     .>> pwhitespaces
    let plevel1       = pchainl plevel0 pop1
    rpexpr := plevel1
    pwhitespaces >>. plevel1 .>> peos

// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
module JSONModule =
  open ParserModule
  open System.Text

  type JSON =
    | NullValue
    | BooleanValue  of bool
    | NumberValue   of float
    | StringValue   of string
    | ArrayValue    of JSON list
    | ObjectValue   of (string*JSON) list

  let stringifyJSON json =
    let sb = StringBuilder ()

    let appStr (s : string) =
      ignore <| sb.Append '"'
      ignore <| sb.Append s     // TODO: Add escaping
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
    let parray  , rparray   = ptrampoline<JSON> ()

    let pobject , rpobject  = ptrampoline<JSON> ()

    let pnull   = pstr "null" >>% NullValue

    let pboolean=
      pchoice
        [
          pstr "true"   >>% BooleanValue true
          pstr "false"  >>% BooleanValue false
        ]

    let pnumber =
      let psign : Parser<float->float>=
        pskip '-' >>% fun d -> -d
        >>? id

      let pfrac =
        parser {
          do!   pskip '.'
          let!  ui,i  = pnatural
          return (float ui) * (pown 10.0 -i)
        } >>? 0.0

      let pexp =
        parser {
          do!   pchoice [pskip 'e'; pskip 'E']
          let!  sign  = psign
          let!  ui,_  = pnatural
          let scale   = sign (double ui)
          return pown 10.0 (int scale)
        } >>? 1.0

      let pzero =
        pskip '0'
        |>> fun ch -> 0.0

      let pfull =
        parser {
          let! i,_  = pnatural
          let! f    = pfrac
          let! e    = pexp

          return (float i + f)*e
        }

      parser {
        let! s  = psign
        let! n  = pchoice [pzero; pfull]

        return NumberValue (s n)
      }

    let prawstring =

      let phex =
        phexdigit
        |>> fun ch ->
          if Char.IsDigit ch then int ch - int '0'
          elif ch >= 'A' && ch <= 'F' then int ch - int 'A' + 10
          elif ch >= 'a' && ch <= 'f' then int ch - int 'a' + 10
          else 0


      let pstring_token = pskip '"'
      let pch   = psatisfy (Expected "char") (function '"' | '\\' -> false | _ -> true)
      let pech  =
        parser {
          do!   pskip '\\'
          let!  c = panyOf "\"\\/bfnrt"
          let   r =
            match c with
            | 'b' -> '\b'
            | 'f' -> '\f'
            | 'n' -> '\n'
            | 'r' -> '\r'
            | 't' -> '\t'
            | _   -> c
          return r
        }
      let puch  =
        parser {
          do!   pskip '\\'
          do!   pskip 'u'
          let!  v0  = phex
          let!  v1  = phex
          let!  v2  = phex
          let!  v3  = phex
          let ui = (v0 <<< 12) + (v1 <<< 8) + (v2 <<< 4) + v3
          return char ui
        }
      let pstr  = pmany (pchoice [pch; pech; puch]) |>> (List.toArray >> fun cs -> String(cs))
      pbetween pstring_token pstring_token pstr

    let pstring = prawstring |>> StringValue

    let pvalue  = pchoice [pnull; pboolean; pnumber; pstring; parray; pobject] .>> pwhitespaces

    let ptk ch = pskip ch .>> pwhitespaces

    let parr =
      let pvalues = pmanySepBy pvalue (ptk ',') |>> ArrayValue
      pbetween (ptk '[') (ptk ']') pvalues

    let pobj =
      let pmember =
        parser {
          let!  name  = prawstring .>> pwhitespaces
          do!   ptk ':'
          let!  value = pvalue

          return name, value
        }
      let pmembers = pmanySepBy pmember (ptk ',') |>> ObjectValue
      pbetween (ptk '{') (ptk '}') pmembers

    rparray   := parr
    rpobject  := pobj

    pwhitespaces >>. pchoice [pobject; parray] .>> peos
// ----------------------------------------------------------------------------------------------

// ----------------------------------------------------------------------------------------------
open ParserModule
open CalculatorModule
open JSONModule
// ----------------------------------------------------------------------------------------------
let parseAndPrint (p : Parser<'T>) (a : 'T -> string) s =
  printfn "Input: %s" s
  let res = parse p s
  match res with
  | Success (v,pos) ->
    printfn "Success : Parsing stopped at pos: %d\nAST     : %A\nFriendly: %s" pos v (a v)
  | Failure (msg, pos) ->
    printfn "Failed  : Parsing stopped at pos: %d\n%s" pos msg
// ----------------------------------------------------------------------------------------------
[<EntryPoint>]
let main argv =
  Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture

  let parse s = parseAndPrint pjson stringifyJSON s

  let rec loop () =
    printfn "-- \nEnter expression to be parsed"
    let line = Console.ReadLine ()
    if line.Length > 0 then
      parse line
      loop ()
    else
      ()

  loop ()

  0
// ----------------------------------------------------------------------------------------------
