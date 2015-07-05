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

type Parser<'T> = string*int -> 'T option*int

let psatisfy f : Parser<char> =
  fun (s,pos) ->
    if pos < s.Length && f s.[pos] then
      Some s.[pos],pos + 1
    else
      None,pos

let pchar   = psatisfy <| fun _ -> true
let pdigit  = psatisfy Char.IsDigit
let pletter = psatisfy Char.IsLetter

let preturn (v : 'T) : Parser<'T> =
  fun (s,pos) ->
    Some v,pos

let pfail : Parser<'T> =
  fun (s,pos) ->
    None,pos

let pbind
  (t : Parser<'T>)
  (fu : 'T -> Parser<'U>) : Parser<'U> =
  fun (s,pos) ->
    let ovt,tpos = t (s,pos)
    match ovt with
    | Some vt ->
      let u = fu vt
      u (s,tpos)
    | _ ->
      None, tpos

let inline (>>=) t fu = pbind t fu

let pmap (p : Parser<'T>) (m : 'T -> 'U) : Parser<'U> =
  p
  >>= fun v -> preturn (m v)

let inline (|>>) p m = pmap p m

let pmany (p : Parser<'T>) : Parser<'T list> =
  fun (s,pos) ->
    let rec loop r c =
      let ov, n = p (s,c)
      match ov with
      | Some v -> loop (v::r) n
      | _ -> Some (List.rev r), c

    loop [] pos

let pmany1 (p : Parser<'T>) : Parser<'T list> =
  pmany p
  >>= fun l -> if l.IsEmpty then pfail else preturn l

let pskip ch : Parser<unit> =
  psatisfy (fun c -> ch = c)
  |>> fun _ -> ()

let pdelay (ft : unit -> Parser<'T>) : Parser<'T> =
  fun (s,pos) ->
    let t = ft ()
    t (s,pos)

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
    pskip '=' >>= fun _ ->
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
  let ov, pos = p (s, 0)
  let indicator = String(' ', pos + prelude.Length)
  printfn "%s^" indicator
  match ov with
  | Some v ->
    printfn "Success: Parsed %d characters as: %A" pos v
  | _ ->
    printfn "Failed: Parse failed at position: %d" pos

[<EntryPoint>]
let main argv =
  parseAndPrint "a=3;" passignments

  0
