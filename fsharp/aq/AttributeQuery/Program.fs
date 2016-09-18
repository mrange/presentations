type ProductData() =
  member x.Get (path : string) = "TODO:"

type SysData() =
  member x.Get (path : string) = "TODO:"

type Gpp3Data() =
  member x.Get (path : string) (key : string) = "TODO:"

type QueryContext =
  {
    ProductData   : ProductData
    SysData       : SysData
    Gpp3Data      : Gpp3Data
    Errors        : string []
  }
  static member New pd sd gd : QueryContext = { ProductData = pd; SysData = sd; Gpp3Data = gd; Errors = [||] }

type AttributeQuery<'T> = QueryContext -> 'T

// Monad

let qreturn v : AttributeQuery<'T> =
  fun ctx ->
    v

let qbind (t : AttributeQuery<'T>) (uf : 'T -> AttributeQuery<'U>) : AttributeQuery<'U> =
  fun ctx ->
    let tv  = t ctx
    let u   = uf tv
    let uv  = u ctx
    uv
let inline (>>=) t uf   = qbind t uf

// Arrow

let qarr     f          = f >> qreturn
let qkleisli tf uf      = fun v -> (tf v) >>= uf
let inline (>=>) tf uf  = qkleisli tf uf

// Functor

let qmap (m : 'T -> 'U) (t : AttributeQuery<'T>) : AttributeQuery<'U> =
  t >>= (m >> qreturn)
let inline (>>!) t m = qmap m t

// Applicative

let qpure    v          = qreturn v
let qap  tf u           = tf >>= fun f -> u >>! f
let inline (<*>) tf u   = qap tf u

// Queries

let split (sep : char) (id : string) : AttributeQuery<string[]> =
  fun ctx -> id.Split sep

let at (idx : int) (vs : 'T []) : AttributeQuery<'T> =
  fun ctx -> vs.[idx]

let select idx = split ',' >=> at idx

let iwd v : AttributeQuery<string> =
  fun ctx -> string v

let gpp3 path key : AttributeQuery<string> =
  fun ctx -> "TODO:"

let prodDataXml path : AttributeQuery<string> =
  fun ctx -> "TODO:"

let sysDataParam path : AttributeQuery<string> =
  fun ctx -> "TODO:"

open FSharp.Data.UnitSystems.SI.UnitNames
open FSharp.Data.UnitSystems.SI.UnitSymbols

type FrequencyUnit =
  | FU_Hz   of decimal

type PowerUnit =
  | PU_W    of decimal
  | PU_dBm  of decimal

type GainUnit =
  | GU_dB   of decimal

type UnitConverter =
  | BandLow   of FrequencyUnit*FrequencyUnit
  | BandHigh  of FrequencyUnit*FrequencyUnit
  | BandWidth of FrequencyUnit*FrequencyUnit
  | Power     of PowerUnit*PowerUnit
  | Gain      of GainUnit*GainUnit
  | Scalar
  | Unknown

let dBm0_1  = PU_dBm 0.1M
let kHz     = FU_Hz 1000M

let freqClassUsage = prodDataXml "/board/freqClassUsage"

let capabilityInstructions =
  [|
    "CRBS_TRS_CAP_CARDINALITY_SUPPORT",
    [|
      "capabilityIdentity"         , Scalar                                                 , iwd 1
      "numberOfDevices"            , Scalar                                                 , iwd 0                                             (* IWD: "Number of static devices" *)
    |]
    "CRBS_TRS_CAP_RF_CHAR_SUPPORT",
    [|
      "capabilityIdentity"         , Scalar                                                 , iwd 2
      "txPortMaximumOutputPower"   , Power (dBm0_1 (* IWD *) , dBm0_1  (* not always! *))   , prodDataXml   "/board/powerClassUsage"
      "txPortMaximumPar"           , Power (dBm0_1 (* IWD *) , dBm0_1  (* sysDataParam *))  , sysDataParam  "capMaxPar"
      "duplexMode"                 , Scalar                                                 , sysDataParam  "capabilityDuplexMode"
      "txOperationBandLowEdge"     , BandLow   (kHz (* IWD *), kHz (* gpp3 *))              , freqClassUsage >>= gpp3 "bandLimits" >>= select 0 (* select "DL_f_min" *)
      "txOperationBandHighEdge"    , BandHigh  (kHz (* IWD *), kHz (* gpp3 *))              , freqClassUsage >>= gpp3 "bandLimits" >>= select 1 (* select "DL_f_max" *)
      "rxOperationBandLowEdge"     , BandLow   (kHz (* IWD *), kHz (* gpp3 *))              , freqClassUsage >>= gpp3 "bandLimits" >>= select 2 (* select "UL_f_min" *)
      "rxOperationBandHighEdge"    , BandHigh  (kHz (* IWD *), kHz (* gpp3 *))              , freqClassUsage >>= gpp3 "bandLimits" >>= select 3 (* select "UL_f_max" *)
      "txMaximumBandwidth"         , BandWidth (kHz (* IWD *), kHz (* match IWD range *))   , sysDataParam "capabilityDlBw"                     (* Observed values in sysDataParam 10000-75000*)
      "rxMaximumBandwidth"         , BandWidth (kHz (* IWD *), kHz (* match IWD range *))   , sysDataParam "capabilityUlBw"                     (* Observed values in sysDataParam 10000-75000*)
//      "rxMaximumGainArpToRuInput"  , dBm0_1 (* IWD *)          , __  (*?range -500to500?*) , subtract_str(sysDataParam("ulGainSetting").then(split(select(2))), sysDataParam("ulGainSetting").then(split(select(1)))) } (* GAIN_UL_HIGH_EDGE - GAIN_UL_LOW_EDGE. Observed value outside IWD range *)
    |]
  |]

[<EntryPoint>]
let main argv =
  printfn "%A" argv
  0
