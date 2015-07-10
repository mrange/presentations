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

type ParserError =
  | NoError
  | Expected    of string
  | Unexpected  of string
  | Fork        of ParserError*ParserError
  | Group       of ParserError list

type Parser<'T> = string*int*int -> 'T option*ParserError*int

let err pe pos epos = 
  if pos = epos then 
    pe 
  else 
    NoError
let errjoin lpe rpe = 
  match lpe, rpe with
  | NoError , _       -> rpe
  | _       , NoError -> lpe
  | _       , _       -> Fork (lpe, rpe)

module Errors =
  let eos         = Unexpected "EOS"

let psatisfy pe f : Parser<char> =
  let eos = Group [Errors.eos; pe]
  fun (s,pos,epos) ->
    if pos >= s.Length then
      None, err eos pos epos, pos
    elif f s.[pos] then
      Some s.[pos], NoError, pos + 1
    else
      None, err pe pos epos, pos

let pchar   = psatisfy (Expected "char")    <| fun _ -> true
let pdigit  = psatisfy (Expected "digit")   Char.IsDigit
let pletter = psatisfy (Expected "letter")  Char.IsLetter

let preturn (v : 'T) : Parser<'T> =
  fun (s,pos,epos) ->
    Some v, NoError, pos

let pfail pe : Parser<'T> =
  fun (s,pos,epos) ->
    None, err pe pos epos, pos

let pbind
  (t : Parser<'T>)
  (fu : 'T -> Parser<'U>) : Parser<'U> =
  fun (s,pos,epos) ->
    let ovt,terr,tpos = t (s,pos,epos)
    match ovt with
    | Some vt ->
      let u = fu vt
      let ovu, uerr, upos = u (s,tpos,epos)
      ovu, errjoin terr uerr , upos
    | _ ->
      None, terr, tpos

let inline (>>=) t fu = pbind t fu

let pmap (p : Parser<'T>) (m : 'T -> 'U) : Parser<'U> =
  p
  >>= fun v -> preturn (m v)

let inline (|>>) p m = pmap p m

let popt (p : Parser<'T>) : Parser<'T option> =
  fun (s,pos, epos) ->
    let ovp, perr, ppos = p (s,pos, epos)
    match ovp with
    | Some vp -> 
      Some (Some vp), perr, ppos
    | _ -> 
      Some None, perr, pos

let pmany (p : Parser<'T>) : Parser<'T list> =
  fun (s,pos,epos) ->
    let rec loop r cpos =
      let ovp, perr, ppos= p (s,cpos,epos)
      match ovp with
      | Some vp -> loop ((perr,vp)::r) ppos
      | _ -> 
        let errs, vs = r |> List.rev |> List.unzip
        let group = Group (perr::errs)
        Some vs, group, cpos

    loop [] pos

let pmany1 (p : Parser<'T>) : Parser<'T list> =
  pmany p
  >>= fun l -> if l.IsEmpty then pfail NoError else preturn l

let pskip ch : Parser<unit> =
  psatisfy (Expected <| sprintf "'%s'" (String(ch, 1))) (fun c -> ch = c)
  |>> fun _ -> ()

let pdelay (ft : unit -> Parser<'T>) : Parser<'T> =
  fun (s,pos,epos) ->
    let t = ft ()
    t (s,pos,epos)

type ParserBuilder()=
  member x.Delay(ft)  = pdelay ft
  member x.Return(v)  = preturn v
  member x.Bind(t,fu) = pbind t fu

let parser = ParserBuilder()

let pint : Parser<int> =
  pmany1 pdigit
  |>> List.fold (fun s ch -> s * 10 + (int ch - int '0')) 0

let pidentifier : Parser<string> =
  pmany1 pletter
  |>> (List.toArray >> String)

let passignment_ : Parser<string*int> =
  pidentifier
  >>= fun id ->
    pskip '='
    >>= fun _ ->
      pint
      >>= fun i ->
        pskip ';'
        >>= fun _ ->
          preturn (id, i)

let passignment : Parser<string*int> =
  parser {
    let! id = pidentifier
    do! pskip '='
    let! i = pint
    do! pskip ';'
    return id, i
  }

let passignments : Parser<(string*int) list> =
  pmany passignment

let parseAndPrint s (p : Parser<'T>)=
  let prelude = "Input: "
  printfn "%s%s" prelude s
  let ov, _, pos = p (s, 0, Int32.MaxValue)
  let indicator = String(' ', pos + prelude.Length)
  printfn "%s^" indicator
  match ov with
  | Some v ->
    printfn "Success: Parsed %d characters as: %A" pos v
  | _ ->

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


    let _, err, _ = p (s, 0, pos)

    extractError err

    let buildString (input : ResizeArray<string>) = 
      let ss = input |> Seq.sort |> Seq.distinct |> Seq.toArray

      if ss.Length = 0 then
        ""
      else
        let sb = StringBuilder ()
        let e = ss.Length - 1

        for i = 0 to e do
          let s = ss.[i]
          let prepend =
            match i with
            | 0 -> ""
            | e -> " or "
            |_ -> ", "

          ignore <| sb.Append prepend
          ignore <| sb.Append (ss.[i])

        sb.ToString ()

    let expected   = buildString es
    let unexpected = buildString ues

    printfn "Failed: Parse failed at position: %d" pos
    if expected.Length > 0 then
      printfn "Expected %s" expected
    if unexpected.Length > 0 then
      printfn "Unexpected %s" unexpected

[<EntryPoint>]
let main argv =

  let rec loop () =
    let line = Console.ReadLine ()
    if line.Length > 0 then
      parseAndPrint line passignment
      loop ()
    else
      ()

  loop ()

  0
