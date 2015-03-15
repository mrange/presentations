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

    let rec quickSort (vs : 'V []) : Sorter<'V []> =
        let rec impl (r : InplaceRange<'V>) : Sorter<unit> =
            state {
                let! range = processRange r
                match range with
                | None ->
                    return ()
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
        inplaceSorter impl vs

    let rec insertionSort (vs : 'V []) : Sorter<'V []> =
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
                            // This exists the loop
                            re := b
            }
        inplaceSorter impl vs

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

[<EntryPoint>]
let main argv = 

    let random      = Random(19740531)

    let sorters =
        [|
            "mergeSort"     , mergeSort
            "quickSort"     , quickSort
            "insertionSort" , insertionSort
        |]

    let testDatum = 
        [|
            [| |]
            [| 1 |]
            [| 1; 1 |]
            [| 1; 2 |]
            [| 1; 2; 3; 4 |]
            [| 4; 3; 2; 1 |]
            [| 3; 1; 4; 1; 5; 9; 2; 6; 5; 4 |]
            [| for x in 0..10 -> random.Next(1000) |]
        |]

    let testRun (testData : int[]) (name : string) (s : int [] -> Sorter<int[]>) : unit = 
        printfn "Running test: %s\nInput:%A" name testData

        let cost = ref 0
        let witness = Witness <| fun aa -> 
            let c = 
                match aa with 
                | PushArray _       -> 0
                | PopArray          -> 0
                | PushFrame         -> 0
                | PopFrame          -> 0
                | SwapValues _      -> 1
                | PickPivot _       -> 0
                | CompareToPivot _  -> 1
                | RangeBegin _      -> 0
                | RangeEnd _        -> 0
            increment cost c

        let expected= testData |> Array.sort
        let actual  = testData |> s |> (run random witness)

        let e,r = 
            if expected = actual then
                true, "SUCCESS"
            else
                false, "FAILURE"

        if e then
            printfn "%s - Cost:%d" r !cost
        else
            printfn "%s - Cost:%d\nActual:%A\nExpected:%A" r !cost actual expected

    for testData in testDatum do
        for name, sorter in sorters do
            testRun testData name sorter


    0
