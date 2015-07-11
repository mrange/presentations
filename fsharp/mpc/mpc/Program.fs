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

module ParserModule =
  type Error =
    | NoError
    | Expected    of string
    | Unexpected  of string
    | Fork        of Error*Error
    | Group       of Error list

  type Parser<'T> = string*int*int -> 'T option*Error*int

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

  open Detail

  let preturn (v : 'T) : Parser<'T> =
    fun (s,pos,epos) ->
      Some v, NoError, pos

  let pfail (pe : Error) : Parser<'T> =
    fun (s,pos,epos) ->
      None, err pe pos epos, pos

  let peos : Parser<unit> =
    fun (s,pos,epos) ->
      if pos >= s.Length then
        Some (), NoError, pos
      else
        None, err eeos pos epos, pos

  let psatisfy pe f : Parser<char> =
    let eos = Group [ueos; pe]
    fun (s,pos,epos) ->
      if pos >= s.Length then
        None, err eos pos epos, pos
      elif f s.[pos] then
        Some s.[pos], NoError, pos + 1
      else
        None, err pe pos epos, pos

  let pchar       = psatisfy (Expected "char")        <| fun _ -> true
  let pdigit      = psatisfy (Expected "digit")       Char.IsDigit
  let pletter     = psatisfy (Expected "letter")      Char.IsLetter
  let pwhitespace = psatisfy (Expected "whitespacew") Char.IsWhiteSpace

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

  let inline (>>=) t fu   = pbind t fu

  let inline pright t u   = t >>= fun _ -> u
  let inline pcombine t u = pright t u
  let inline (>>.) t u    = pright t u

  let inline pleft t u    = t >>= fun left -> u >>= fun _ -> preturn left
  let inline (.>>) t u    = pleft t u

  let inline pmap p m     = p >>= fun v -> preturn (m v)
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

  let pmany1 p = pmany p >>= fun l -> if l.IsEmpty then pfail NoError else preturn l

  let pskip (ch : char) : Parser<unit> =
    let expected = Expected <| sprintf "'%s'" (String(ch, 1))
    psatisfy expected (fun c -> ch = c) |>> fun _ -> ()

  let pdelay (ft : unit -> Parser<'T>) : Parser<'T> =
    fun (s,pos,epos) ->
      let t = ft ()
      t (s,pos,epos)

  type ParserBuilder()=
    member x.Delay(ft)      = pdelay ft
    member x.Return(v)      = preturn v
    member x.Bind(t,fu)     = pbind t fu
    member x.Combine(t,u)   = pcombine t u

  let parser = ParserBuilder()

  type Result<'T> =
    | Success of 'T*int
    | Failure of string*int

  let parse (p : Parser<'T>) (s : string) : Result<'T> =
    let ov, _, pos = p (s, 0, Int32.MaxValue)
    match ov with
    | Some v -> Success (v, pos)
    | None ->
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
      let _, err, _ = p (s, 0, pos)

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

open ParserModule

// Parses an integer: 2130904
let pint : Parser<int> =
  pmany1 pdigit
  |>> List.fold (fun s ch -> s * 10 + (int ch - int '0')) 0

// Parses an identifier: abc
let pidentifier : Parser<string> =
  pmany1 pletter
  |>> (List.toArray >> String)

// Parses an assignment: abc=343;
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

// Parses an assignment: abc=343;
let passignment : Parser<string*int> =
  parser {
    let! id = pidentifier
    do! pskip '='
    let! i = pint
    do! pskip ';'
    return id, i
  }

// Parses assignments: abc=343;dcef=322;
let passignments : Parser<(string*int) list> =
  pmany passignment

let parseAndPrint s (p : Parser<'T>)=
  let res = parse p s
  match res with
  | Success (v,pos) ->
    printfn "Success: Parsed %d characters as: %A" pos v
  | Failure (msg, pos) ->
    printfn "Failed: Parsing stopped at pos: %d\n%s" pos msg

[<EntryPoint>]
let main argv =
  let rec loop () =
    printfn "Enter the text to be parsed (example: abc=343;)"
    let line = Console.ReadLine ()
    if line.Length > 0 then
      parseAndPrint line (passignment .>> peos)
      loop ()
    else
      ()

  loop ()

  0
