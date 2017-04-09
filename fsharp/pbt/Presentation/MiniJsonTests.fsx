#r "../packages/FsCheck.2.8.0/lib/net452/FsCheck.dll"

#load "MiniJson.fsx"

open MiniJson.JsonModule

module Tests =
  open FsCheck
  open System
  open System.Globalization
  open System.Text

  type Whitespace =
    | Tab
    | NewLine
    | CarriageReturn
    | Space

    member x.AsChar =
      match x with
      | Tab             -> '\t'
      | NewLine         -> '\n'
      | CarriageReturn  -> '\r'
      | Space           -> ' '

  type Ws = Whitespace [] * Whitespace []

  let noWs : Ws = [||], [||]

  let coalesce s = if s <> null then s else ""

  let (|PositiveInf|NegativeInf|NaN|Normal|) d =
    if    Double.IsPositiveInfinity d then PositiveInf
    elif  Double.IsNegativeInfinity d then NegativeInf
    elif  Double.IsNaN              d then NaN
    else  Normal d

  type WsJson =
    | WsJsonNull    of Ws
    | WsJsonBoolean of Ws*bool
    | WsJsonNumber  of Ws*float
    | WsJsonString  of Ws*string
    | WsJsonArray   of Ws*WsJson []
    | WsJsonObject  of Ws*(Ws*string*WsJson) []

    member x.MakeValidRoot () =
      match x with
      | WsJsonNull    _
      | WsJsonBoolean _
      | WsJsonNumber  _
      | WsJsonString  _ -> WsJsonArray (noWs, [|x|])  // In order to be valid JSON
      | WsJsonArray   _
      | WsJsonObject  _ -> x

    member x.IsEqual (j : Json) =
      let rec loop wsj j =
        match wsj, j with
        | WsJsonNull      _               , JsonNull          -> true
        | WsJsonBoolean   (_, wsb)        , JsonBoolean jb    -> wsb = jb
        | WsJsonNumber    (_, PositiveInf), JsonString  "+Inf"-> true     // PositiveInf serialized as "+Inf"
        | WsJsonNumber    (_, NegativeInf), JsonString  "-Inf"-> true     // NegativeInf serialized as "-Inf"
        | WsJsonNumber    (_, NaN)        , JsonString  "NaN" -> true     // NaN serialized as "NaN"
        | WsJsonNumber    (_, Normal n)   , JsonNumber  jn    ->
          if    n > 1.78e308   && jn > 1.78e308 then true   // TODO: MiniJson don't handle very large floats exactly correct
          elif  n < -1.78e308  && jn < -1.78e308 then true  // TODO: MiniJson don't handle very large floats exactly
          else
            let d = n - jn |> abs
            d < 0.00000001 // TODO: Too primitive technique to handle roundings
        | WsJsonString    (_, wss)      , JsonString  js      -> coalesce wss = coalesce js
        | WsJsonArray     (_, wsvs)     , JsonArray   jvs     -> Array.forall2 loop wsvs jvs
        | WsJsonObject    (_, wsms)     , JsonObject  jms     -> Array.forall2 (fun (_, wsk, wsv) (jk, jv) -> coalesce wsk = coalesce jk && loop wsv jv) wsms jms
        | _                             , _                   -> false
      loop x j

    override x.ToString () : string =
      let sb = StringBuilder ()

      let inline str (s : string)  = ignore <| sb.Append s
      let inline ch  (c : char)    = ignore <| sb.Append c
      let inline num (f : float)   =
        if Double.IsNaN f then
          ignore <| sb.AppendFormat "\"NaN\""   // JSON numbers doesn't support NaN
        elif Double.IsPositiveInfinity f then
          ignore <| sb.AppendFormat "\"+Inf\""  // JSON numbers doesn't support +Inf
        elif Double.IsNegativeInfinity f then
          ignore <| sb.AppendFormat "\"-Inf\""  // JSON numbers doesn't support -Inf
        else
          ignore <| sb.AppendFormat (CultureInfo.InvariantCulture, "{0:G}", f)

      let estr (s : string) =
        ch '"'
        let s = coalesce s
        let e =s.Length - 1
        for i = 0 to e do
          match s.[i] with
          | '\"'            -> str @"\"""
          | '\\'            -> str @"\\"
          | '/'             -> str @"\/"
          | c when c < ' '  -> str ToStringDetails.nonPrintableChars.[int c]
          | c               -> ch c
        ch '"'

      let inline rws (pre, post) f =
        let inline g ws =
          for (w : Whitespace) in ws do
            ch w.AsChar
        g pre
        f ()
        g post

      let values b e (vs : 'T array) (a : 'T -> unit) =
        ch b
        let ee = vs.Length - 1
        for i = 0 to ee do
          let v = vs.[i]
          a v
          if i < ee then
            ch ','
        ch e

      let rec impl j =
        match j with
        | WsJsonNull    ws          -> rws ws <| fun () -> str Tokens.Null
        | WsJsonBoolean (ws, true)  -> rws ws <| fun () -> str Tokens.True
        | WsJsonBoolean (ws, false) -> rws ws <| fun () -> str Tokens.False
        | WsJsonNumber  (ws, n)     -> rws ws <| fun () -> num n
        | WsJsonString  (ws, s)     -> rws ws <| fun () -> estr s
        | WsJsonArray   (ws, vs)    -> rws ws <| fun () -> values '[' ']' vs impl
        | WsJsonObject  (ws, ms)    -> rws ws <| fun () -> values '{' '}' ms implkv
      and implkv (ws,k,v) =
        rws ws <| fun () -> estr k
        ch ':'
        impl v

      let json = x.MakeValidRoot ()

      impl json

      sb.ToString ()

  type Properties () =

    static member ``parse can parse JSON documents with whitspaces`` (wsj : WsJson) =
      let wsj = wsj.MakeValidRoot ()
      let j   = wsj.ToString ()
      match parse true j with
      | Failure (msg, _)  ->
        printfn "%A" msg
        false
      | Success a         ->
        if wsj.IsEqual a then
          true
        else
          printfn "%A" wsj
          printfn "%A" a
          false

  let run () =
    let config = { Config.Quick with MaxTest = 1000; MaxFail = 1000 }
    Check.All<Properties> config

// Tests.run ()
