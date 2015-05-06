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
open System.Diagnostics                

open Canvas

open SharpDX

module StateMonad =

    type State<'T,'S> = 'S -> 'T*'S


    module State =

        let returnValue (v : 'T)            : State<'T,'S>  = fun s -> v,s
        let returnFrom  (t : State<'T,'S>)  : State<'T,'S>  = t
        let zero        ()                  : State<unit,'S>= returnValue ()

        let delay (ft : unit -> State<'T,'S>) : State<'T,'S> = 
            fun s ->
                let t = ft ()
                t s

        let bind    (t : State<'T,'S>) (fu : 'T -> State<'U,'S>): State<'U,'S> =
            fun s ->
                let tv,ts   = t s
                let u       = fu tv
                let uv,us   = u ts

                uv,us
        let combine (t : State<unit,'S>) (u : State<'U,'S>)     : State<'U,'S> =
            fun s ->
                let tv,ts   = t s
                let uv,us   = u ts

                uv,us

        let whileDo (test : unit -> bool) (t : State<unit,'S>) : State<unit, 'S> =
            fun s ->
                let mutable ms = s

                while test () do
                    let _,ns = t ms
                    ms <- ns

                (), ms

        let inline pushFrame<'S when 'S : (member PushFrame : unit -> unit)> s: unit =
            (^S : (member PushFrame : unit -> unit) s)
            
        let inline popFrame<'S when 'S : (member PopFrame : unit -> unit)> s: unit =
            (^S : (member PopFrame : unit -> unit) s)

        let inline run (t : State<'T,'S>) : State<'T, 'S> =
            fun s ->
                pushFrame s
                let r = t s
                popFrame s
                r

        type StateBuilder() =
            member inline x.Bind(t,fu)      = bind t fu
            member inline x.Combine(t,u)    = combine t u
            member inline x.Delay(ft)       = delay ft
            member inline x.While(test,t)   = whileDo test t
            member inline x.Return(v)       = returnValue v
            member inline x.ReturnFrom(t)   = t
            member inline x.Zero()          = zero ()

            member inline x.Run(t)          = run t

    let state = State.StateBuilder()

type Witness<'W> = Witness of ('W -> unit)

module Sorting =

    open System

    open StateMonad

    let inline increment (r : 'V ref) (i : 'V) : unit =
        r := !r + i

    let inline clamp v b e =
        if v < b then b
        elif v > e then e
        else v

    type InplaceRange<'T> =
        {
            Begin   : int
            End     : int
            Values  : 'T []
        }

        static member New b e (vs : 'T []) = 
            { 
                Begin   = clamp b 0 vs.Length
                End     = clamp e b vs.Length
                Values  = vs 
            }

        member x.IsEmpty    = x.Length = 0

        member x.InRange i  = x.Begin <= i && i < x.End

        member x.ValidateInRange i = if not (x.InRange i) then raise <| IndexOutOfRangeException("i")

        member x.Item
            with get i  =
                x.ValidateInRange i
                x.Values.[i]
            and set i v =
                x.ValidateInRange i
                x.Values.[i] <- v

        member x.Length     = x.End - x.Begin

        override x.ToString () =
            let sub = Array.sub x.Values x.Begin x.Length
            sprintf "range {%d,%d,%A}" x.Begin x.End sub

    let rangeFromArray (vs : 'V []) : InplaceRange<'V> = InplaceRange<_>.New 0 vs.Length vs

    let subRange (b : int) (e : int) (r : InplaceRange<'V>) : InplaceRange<'V> =
        InplaceRange<_>.New b e r.Values

    type ArrayAction = 
        | PushArray         of Array
        | PopArray      
        | PushFrame
        | PopFrame
        | SwapValues        of int*int
        | PickPivot         of int
        | CompareToPivot    of int
        | RangeBegin        of int
        | RangeEnd          of int
//        | Range         of int*int

    type SorterState =
        {
            Random  : Random
            Witness : Witness<ArrayAction>
        }

        static member New r w = { Random = r; Witness = w }

        member x.PushFrame () = x.Raise PushFrame

        member x.PopFrame () = x.Raise PopFrame

        member x.Raise (aa : ArrayAction) = 
            let (Witness f) = x.Witness
            f aa

        member x.Next (b : int) (e : int) : int =
            x.Random.Next (b,e)


    type Sorter<'T> = State<'T,SorterState>

    let doNothing : Sorter<unit> =
        fun s ->
            (),s


    let raise (aa : ArrayAction) : Sorter<unit>  =
        fun s ->
            s.Raise aa
            (),s

    let processRange (r : InplaceRange<'V>) : Sorter<(int*int) option> =
        fun s ->
            let b = r.Begin
            let e = r.End
            // + 1 because we don't process ranges of length = 1
            if b + 1 < e then
                s.Raise <| RangeBegin b
                s.Raise <| RangeEnd e
                (Some (b,e)),s
            else
                None, s

    let incrementBegin (b : int ref) (i : int) : Sorter<unit> =
        fun s ->
            increment b i
            s.Raise <| RangeBegin !b
            (),s

    let incrementEnd (e : int ref) (i : int) : Sorter<unit> =
        fun s ->
            increment e i
            s.Raise <| RangeEnd !e
            (),s

    let pickPivot (p : int) (r : InplaceRange<'V>) : Sorter<int*'V> =
        fun s ->
            if r.IsEmpty then
                failwith "Input array range is empty, can't pick pivot"
            else
                let v = r.[p]
                s.Raise <| PickPivot p
                (p,v), s

    let pickRandomPivot (r : InplaceRange<'V>) : Sorter<int*'V> =
        fun s ->
            let p = s.Next r.Begin r.End
            pickPivot p r s

    let compareToPivot (c : 'V -> bool) (i : int) (r : InplaceRange<'V>) : Sorter<bool> =
        fun s ->
            let v = r.[i]
            s.Raise <| ArrayAction.CompareToPivot i
            (c v),s

    let swapValues (f : int) (l : int) (r : InplaceRange<'V>) : Sorter<unit> =
        fun s ->
            let t = r.[f]
            r.[f] <- r.[l]
            r.[l] <- t

            s.Raise <| SwapValues (f,l)

            (),s

    let splitRange (p : int) (r : InplaceRange<'V>) : Sorter<InplaceRange<'V>*InplaceRange<'V>> =
        fun s ->
            let lr = subRange r.Begin   p       r
            let rr = subRange (p + 1)   r.End   r
            (lr,rr),s
    

    let run (random : Random) (witness : Witness<ArrayAction>) (s : Sorter<'T>) : 'T = 
        let ss = SorterState.New random witness
        let r,_ = s ss
        r

    let inplaceSorter (impl : InplaceRange<'V> -> Sorter<unit>) (vs : 'V []) : Sorter<'V []> = 
        state {
            let r = vs |> Array.copy |> rangeFromArray 
            do! raise <| PushArray vs
            do! impl r
            do! raise PopArray
            return r.Values
        }

    let sorter (impl : 'V [] -> InplaceRange<'V> -> Sorter<unit>) (vs : 'V []) : Sorter<'V []> = 
        state {
            let r = vs |> rangeFromArray 
            let a = Array.zeroCreate vs.Length
            do! raise <| PushArray vs
            do! impl a r
            do! raise PopArray
            return a
        }

    module Details =
        type InplaceSorter<'V> = InplaceRange<'V> -> Sorter<unit>

        let inplaceNullSort (r : InplaceRange<'V>) : Sorter<unit> =
            let rec impl (r : InplaceRange<'V>) : Sorter<unit> =
                state {
                    return ()
                }
            impl r

        let inplaceInsertionSort (r : InplaceRange<'V>) : Sorter<unit> =
            let rec impl (r : InplaceRange<'V>) : Sorter<unit> =
                state {
                    let! range = processRange r
                    match range with
                    | None ->
                        return ()
                    | Some (b,e) ->
                        do! impl <| subRange b (e - 1) r

                        let re      = ref e
                        do! incrementEnd re -1
                        let! _,p    = pickPivot !re r

                        while b < !re do
                        
                            let c = !re - 1
                            let! result = compareToPivot (fun v -> v > p) c r
                            if result then
                                do! swapValues c !re r
                                do! incrementEnd re -1
                            else
                                // This exits the loop
                                re := b
                }
            impl r

        let inplaceHybridSort ((cutoff,inner) : int*InplaceSorter<'V>)  (r : InplaceRange<'V>) : Sorter<unit> =
            let rec impl (r : InplaceRange<'V>) : Sorter<unit> =
                state {
                    let! range = processRange r
                    match range with
                    | None ->
                        return ()
                    | Some (b, e) when e - b < cutoff -> 
                        return! inner r
                    | Some (b,e) ->
                        let rb   = ref b
                        let re   = ref e

                        // Store the pivot
                        let! pi,p= pickRandomPivot r
                        let ps = !re - 1
                        do! swapValues pi ps r
                        do! incrementEnd re -1

                        let comparer v = v >= p

                        // + 1 because no use swapping to same position
                        while (!rb + 1) < !re do

                            let c = !rb
                            let! result = compareToPivot comparer c r
                            if result then
                                do! swapValues c (!re - 1) r
                                do! incrementEnd re -1
                            else
                                do! incrementBegin rb 1

                        let c = !rb
                        let! result = compareToPivot comparer c r

                        if result then
                            // c should be member of right range
                            // Swap pivot into place
                            do! swapValues c ps r
                            let! lr, rr = splitRange c r
                            do! impl lr
                            do! impl rr
                        elif c + 1 < ps then
                            // c should be member of left range
                            // Swap pivot into place
                            do! swapValues (c + 1) ps r
                            let! lr, rr = splitRange (c + 1) r
                            do! impl lr
                            do! impl rr
                        else
                            // c should be member of left range
                            // No need to swap as c + 1 is pointing to pivot pos
                            // right range will be empty
                            let! lr, rr = splitRange (c + 1) r
                            Debug.Assert rr.IsEmpty
                            do! impl lr
                }
            impl r

    let rec insertionSort (vs : 'V []) : Sorter<'V []> =
        inplaceSorter Details.inplaceInsertionSort vs

    let rec quickSort (vs : 'V []) : Sorter<'V []> =
        inplaceSorter (Details.inplaceHybridSort (0, Details.inplaceNullSort)) vs

    let rec hybridSort (cutoff : int) (vs : 'V []) : Sorter<'V []> =
        inplaceSorter (Details.inplaceHybridSort (cutoff, Details.inplaceInsertionSort)) vs

    let rec mergeSort (vs : 'V []) : Sorter<'V []> =
        let rec impl (cpy : 'V []) (r : InplaceRange<'V>) : Sorter<unit> =
            state {
                let! range = processRange r
                match range with
                | None ->
                    return ()
                | Some (b,e) ->
                    let vs = r.Values

                    let m = (b + e) / 2

                    do! impl cpy <| subRange b m r
                    do! impl cpy <| subRange m e r

                    let rf = ref b
                    let rs = ref m
                    let rc = ref b
    
                    while !rf < m && !rs < e do
                        let f = !rf
                        let s = !rs
                        let c = !rc

                        if vs.[f] < vs.[s] then
                            cpy.[c] <- vs.[f]
                            increment rf 1
                            increment rc 1
                        else
                            cpy.[c] <- vs.[s]
                            increment rs 1
                            increment rc 1

                    while !rf < m do
                        let f = !rf
                        let c = !rc
                        cpy.[c] <- vs.[f]
                        increment rf 1
                        increment rc 1

                    while !rs < m do
                        let s = !rs
                        let c = !rc
                        cpy.[c] <- vs.[s]
                        increment rs 1
                        increment rc 1

            }
        sorter impl vs

open Sorting

module Trickle =

    let createTrickle (delayInMs : int) (b : int) (e : int) : unit -> int =
        if delayInMs < 1 then
            fun () -> e
        else
            let sw = Stopwatch ()
            sw.Start ()
            fun () ->
                let i = int <| sw.ElapsedMilliseconds / (int64 delayInMs)
                clamp (i + b) b e


type VisualState =
    {
        Values      : int []
        MaxValue    : int
        PivotIndex  : int option
        BeginIndex  : int option
        EndIndex    : int option
    }

    static member Empty = 
        {
            Values      = [||]
            MaxValue    = 0
            PivotIndex  = None
            BeginIndex  = None
            EndIndex    = None
        }

let animateArrayAction (name : string) (aas : ArrayAction []) =

    let trickle = Trickle.createTrickle 10 0 aas.Length

    let lastId  = ref 0

    let state   = ref []

    let width   = 1600.F
    let height  = 1200.F

    Window.Show ("Interactive sort - " + name) (int width) (int height) ignore <| fun dev renderTarget ->
        renderTarget.Clear Color.Black

        let t = trickle ()
        for i in !lastId..(t - 1) do
            let aa = aas.[i]

            let next =
                match !state, aa with
                | s         , PushArray a       -> 
                    let vs  = a :?> int[]
                    let max = vs |> Array.max   // TODO: Handle empty
                    { VisualState.Empty with Values = vs; MaxValue = max }::s
                | _::rest   , PopArray          -> rest                     
                | s::rest   , PushFrame         -> s::s::rest
                | _::rest   , PopFrame          -> rest
                | s::rest   , SwapValues (f,t)  ->
                    let tmp = s.Values.[t]
                    s.Values.[t] <- s.Values.[f]
                    s.Values.[f] <- tmp
                    let ns = 
                        match s.PivotIndex with
                        | Some p when p = t -> { s with PivotIndex = Some f }
                        | Some p when p = f -> { s with PivotIndex = Some t }
                        | _ -> s
                    ns::rest
                | s::rest   , PickPivot p       ->
                    let ns = { s with PivotIndex = Some p }
                    ns::rest
                | s::rest   , RangeBegin b      ->
                    let ns = { s with BeginIndex = Some b }
                    ns::rest
                | s::rest   , RangeEnd e        ->
                    let ns = { s with EndIndex = Some e }
                    ns::rest
                | s         , _                 -> s

            state := next

        lastId := t


        match !state with
        | []    -> ()
        | s::_  ->
            let inline solid c  = dev.GetBrush (SolidBrush c)
            let baseBrush       = solid Color.BlueViolet
            let valueFillBrush  = solid Color.YellowGreen
            let valueStrokeBrush= solid Color.White
            let pivotFillBrush  = solid Color.Red
            let rangeBrush      = solid Color.LightBlue
            let textBrush       = solid Color.White
            let textFormat      = dev.GetTextFormat (SimpleTextFormat ("Consolas", 36.F))

            let rect = RectangleF (100.F, 100.F, 800.F, 40.F)

            let step = rect.Width / float32 s.Values.Length
            renderTarget.FillRectangle (rect, baseBrush)

            let multiplier = 1.F

            let makeV2 i =
                let x = rect.X + (float32 i) * step
                let y = rect.Bottom
                v2 x y

            let makeRect i = 
                let v = s.Values.[i]
                let x = rect.X + (float32 i) * step
                let y = rect.Bottom
                let w = step
                let h = multiplier * float32 v
                let r = rectf x y w h
                r

            match s.BeginIndex, s.EndIndex with
            | Some b, Some e ->
                let bv2 = makeV2 b
                let ev2 = makeV2 e + (v2 0.F (multiplier * float32 s.MaxValue + 50.F))
                let r   = bounds bv2 ev2
                renderTarget.FillRectangle (r, rangeBrush)
            | _ -> ()

            for i in 0..(s.Values.Length - 1) do
                let r       = makeRect i
                renderTarget.FillRectangle (r, valueFillBrush)
                renderTarget.DrawRectangle (r, valueStrokeBrush, 2.0F)

            match s.PivotIndex with
            | Some i -> 
                let r       = makeRect i
                renderTarget.FillRectangle (r, pivotFillBrush)
            | _ -> ()

            renderTarget.DrawText ("QuickSort", textFormat, rect, textBrush)

let animateSorter (random : Random) (vs : 'V []) (name : string) (sorter : 'V [] -> Sorter<_>) =

    let actions     = ResizeArray<ArrayAction>()
    let witness     = Witness actions.Add

    let s = sorter vs

    ignore <| run random witness s

    animateArrayAction name <| actions.ToArray ()


[<EntryPoint>]
let main argv = 
    let random      = Random(19740531)

    let vs = [| for x in 0..100 -> random.Next (300) |]

    let sorters =
        [|
            "insertionSort" , insertionSort
            "quickSort"     , quickSort
            "hybridSort"    , hybridSort 8
        |]

    let name, sorter = sorters.[2]

    animateSorter random vs name sorter

    0
