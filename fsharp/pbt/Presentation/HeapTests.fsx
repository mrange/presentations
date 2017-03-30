#r "../packages/FsCheck.2.8.0/lib/net452/FsCheck.dll"

#load "Heap.fsx"

open Heap

module Tests =
  open FsCheck
  open System.Collections.Generic

  type HeapActions =
    | Push of int*int64
    | Pop

  [<Sealed>]
  type Properties () =
    static let heap (input : ('TKey*'TValue) []) : Heap<'TKey, 'TValue> =
      let heap = Heap<'TKey, 'TValue> ()
      for k, v in input do
        heap.Push k v
      heap

    static let unheap (heap : Heap<'TKey, 'TValue>) : ('TKey*'TValue) [] =
      let ra = ResizeArray<'TKey*'TValue> 16
      let rec loop () =
        match heap.TryPop () with
        | Some v -> ra.Add v; loop ()
        | None -> ()
      loop ()
      ra.ToArray ()

    static member ``Heap count matches input count`` (input : (int*int64) []) =
      let heap      = input |> heap
      heap.Count    = input.Length

    static member ``Heap count is 0 after unheap`` (input : (int*int64) []) =
      let heap      = input |> heap
      let _         = heap  |> unheap
      heap.Count    = 0

    static member ``Input keys are sorted by heap`` (input : (int*int64) []) =
      let sorted    = input |> heap |> unheap

      let keys      = Array.map fst
      let expected  = input   |> keys |> Array.sort
      let actual    = sorted  |> keys

      expected       = actual

    static member ``Input and heap content is equivalent`` (input : (int*int64) []) =
      let sorted    = input |> heap |> unheap

      // We need to group on keys and sort values as heap sorting isn't stable
      let content i =
        i
        |> Seq.groupBy fst
        |> Seq.map (fun (k, vs) -> k, vs |> Seq.sort |> Seq.toArray)
        |> Seq.sortBy fst
        |> Seq.toArray
      let expected  = input   |> content
      let actual    = sorted  |> content

      expected      = actual

    static member ``Heap should behave identical to a heap based on SortedDictionary`` (actions : HeapActions list) =
      let heap  = Heap<int, int64> ()
      let sd    = SortedDictionary<int, int64> ()

      let tryPush k v =
        if sd.ContainsKey k then false
        else sd.Add (k, v); true

      let tryPop () =
        use mutable e = sd.GetEnumerator ()
        if e.MoveNext () then
          sd.Remove e.Current.Key |> ignore
          (e.Current.Key, e.Current.Value) |> Some
        else
          None

      let count () = sd.Count

      let rec loop actions =
        match actions with
        | [] -> true
        | Push (k, v)::acs ->
          // SortedDictionary doesn't support duplicates
          //  the Heap does but we also a have the problem
          //  of equivalent sorting of keys of same value.
          //  Other property tests cover the property of multiple
          //  keys
          if tryPush k v then
            heap.Push k v
          loop acs
        | Pop::acs ->
          let expected  = tryPop ()
          let actual    = heap.TryPop ()
          expected = actual && loop acs

      loop actions

  let run () =
    let config = { Config.Quick with MaxTest = 1000; MaxFail = 1000 }
    Check.All<Properties> config

// do Tests.run ()