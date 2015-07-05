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
    let ovt,vpos = t (s,pos)
    match ovt with
    | Some vt ->
      let u = fu vt
      u (s,vpos)
    | _ ->
      None, vpos

let inline (>>=) t fu = pbind t fu

let pmap (p : Parser<'T>) (m : 'T -> 'U) : Parser<'U> =
  p
  >>= fun v -> preturn (m v)

let inline (|>>) p m = pmap p m

let pmany (p : Parser<'T>) : Parser<'T list> =
  fun (s,pos) ->
    let rec impl r c =
      let ov, n = p (s,c)
      match ov with
      | Some v -> impl (v::r) n
      | _ -> Some (List.rev r), c

    impl [] pos

let pmany1 (p : Parser<'T>) : Parser<'T list> =
  pmany p
  >>= fun l -> if l.IsEmpty then pfail else preturn l

let pint : Parser<int> =
  pmany1 pdigit
  |>> List.fold (fun s ch -> s * 10 + (int ch - int '0')) 0

let pidentifier : Parser<string> =
  pmany1 pletter
  |>> (List.toArray >> System.String)

let pskip ch : Parser<unit> =
  psatisfy (fun c -> ch = c)
  |>> fun _ -> ()

let passignment : Parser<string*int> =
  pidentifier
  >>= fun id ->
    pskip '='
    >>= fun _ ->
      pint
      >>= fun i ->
        preturn (id, i)

let pdelay (ft : unit -> Parser<'T>) : Parser<'T> =
  fun (s,pos) ->
    let t = ft ()
    t (s,pos)

type ParserBuilder()=
  member x.Delay(ft)  = pdelay ft
  member x.Return(v)  = preturn v
  member x.Bind(t,fu) = pbind t fu

let parser = ParserBuilder()

let passignment2 : Parser<string*int> =
  parser {
    let! id = pidentifier
    do! pskip '='
    let! i = pint
    return id, i
  }


let parseAndPrint s (p : Parser<'T>)=
  printfn "Input: %s" s
  let ov, pos = p (s, 0)
  match ov with
  | Some v ->
    printfn "Success: Parsed %d characters as: %A" pos v
  | _ ->
    printfn "Failed: Parsed %d characters" pos

[<EntryPoint>]
let main argv =

  let p = pint

  parseAndPrint "avs" pidentifier

  0
