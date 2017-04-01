open System.Collections.Generic
open System.Diagnostics

[<Sealed>]
type Heap<'TKey, 'TValue when 'TKey : comparison>() =
  let elements = ResizeArray<'TKey*'TValue> 16

  let swap f t =
    let tmp = elements.[t]
    elements.[t]  <- elements.[f]
    elements.[f]  <- tmp

  let rec bubbleUp current =
    if current > 0 then
      let up    = (current - 1) / 2
      let k, _  = elements.[current]
      let uk, _ = elements.[up]
      if uk > k then
        swap up current
        bubbleUp up

  let rec siftDown current end_ =
    let left    = 2*current + 1
    let right   = 2*current + 2
    if left < end_ then
      let k, _    = elements.[current]
      let lk, _   = elements.[left]
      let smallest= if k < lk then current else left
      let smallest=
        if right < end_ then
          let rk, _   = elements.[right]
          let kk, _   = elements.[smallest]
          if kk < rk then smallest else right
        else
          smallest
      if smallest <> current then
        swap smallest current
        siftDown smallest end_

  member x.Count = elements.Count

  [<Conditional ("DEBUG")>]
  member x.CheckInvariant () : bool =
    let rec loop current =
      let left  = 2*current + 1
      let right = 2*current + 2
      if left < elements.Count then
        let k, _    = elements.[current]
        let lk, _   = elements.[left]
        let result  = k <= lk
        let result  =
          if right < elements.Count then
            let rk, _   = elements.[right]
            result && k <= rk
          else
            true
        if not result then
          printfn "Oops: %d, %d, %d" current left right
        result && loop left && loop right
      else
        true
    loop 0

  member x.Push (k : 'TKey) (v : 'TValue) : unit =
    let last  = elements.Count
    let kv    = k, v
    elements.Add kv
    bubbleUp last
#if DEBUG
    x.CheckInvariant () |> Debug.Assert
#endif

  member x.TryPop () : ('TKey*'TValue) option =
    match elements.Count with
    | 0 -> None
    | 1 ->
      let r = Some elements.[0]
      elements.Clear ()
      r
    | n ->
      let last    = n - 1
      let result  = elements.[0]
      elements.[0] <- elements.[last]
      elements.RemoveAt last
      siftDown 0 last
#if DEBUG
      x.CheckInvariant () |> Debug.Assert
#endif
      result |> Some

