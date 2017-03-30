module RedBlackTree =
  type Color =
    | Red
    | Black
  type Tree<'K, 'V> =
    | Leaf
    | Node  of Color*Tree<'K, 'V>*'K*'V*Tree<'K, 'V>

  [<GeneralizableValue>]
  let empty = Leaf

  let rec depth t =
    match t with
    | Leaf -> 0
    | Node (_, l, _, _, r) -> (max (depth l) (depth r)) + 1

  let rec lookup k t =
    match t with
    | Leaf -> None
    | Node (_, ll, kk, vv, rr) ->
      if k < kk then
        lookup k ll
      elif k > kk then
        lookup k rr
      else
        Some vv

  let set k v t =
    let inline balance c l k v r =
      match c, l, k, v, r with
      | Black, Node (Red, Node (Red, a, xk, xv, b), yk, yv, c), zk, zv, d
      | Black, Node (Red, a, xk, xv, Node (Red, b, yk, yv, c)), zk, zv, d
      | Black, a, xk, xv, Node (Red, Node (Red, b, yk, yv, c), zk, zv, d)
      | Black, a, xk, xv, Node (Red, b, yk, yv, Node (Red, c, zk, zv, d)) ->
        Node (Red, Node (Black, a, xk, xv, b), yk, yv, Node (Black, c, zk, zv, d))
      | _ -> Node (c, l, k, v, r)
    let rec loop t =
      match t with
      | Leaf -> Node (Red, Leaf, k, v, Leaf)
      | Node (c, ll, kk, vv, rr)->
        if k < kk then
          balance c (loop ll) kk vv rr
        elif k > kk then
          balance c ll kk vv (loop rr)
        else
          Node (c, ll, k, v, rr)
    match loop t with
    | Node (Red, ll, kk, vv, rr) -> Node (Black, ll, kk, vv, rr)
    | t -> t

  let fromArray (vs : _ []) =
    let rec loop t i =
      if i < vs.Length then
        let k, v = vs.[i]
        let nt = set k v t
        loop nt (i + 1)
      else
        t
    loop empty 0

  module Tests =

    type Key =
      | IntKey    of int
      | StringKey of string
//      | FloatKey  of float

    type Value =
      | IntValue    of int
      | StringValue of string

    type Properties() =
      static let isBlack t =
        match t with
        | Leaf
        | Node (Black, _, _, _, _) -> true
        | _ -> false

      // Checks the invariants specified here: https://en.wikipedia.org/wiki/Red–black_tree
      //  1. Root node is black
      //  2. If a node is red it has no red children
      //  3. Every path from a node to a leaf contain the same number of black nodes
      //  4. In addition since it is a binary search tree it checks that
      //      left/key < key < right/key
      static let checkInvariant t =
        let rec loop lvl cp t =
          match t with
          | Leaf -> Some 1
          | Node (c, l, k, _, r) ->
            match c, loop (lvl + 1) ((>) k) l, loop (lvl + 1) ((<) k) r with
            | Red, Some ld, Some rd when cp k && lvl > 0 && isBlack l && isBlack r && ld = rd ->
              Some ld
            | Black, Some ld, Some rd when cp k && ld = rd ->
              Some (ld + 1)
            | _ -> None
        (loop 0 (fun _ -> true) t).IsSome

      static member ``RedBlack invariants must hold for any key-values`` (vs : (Key*int64) []) =
        let rec loop t i =
          if i < vs.Length then
            let k, v = vs.[i]
            let nt = set k v t
            checkInvariant nt && loop nt (i + 1)
          else
            true
        loop empty 0

      static member ``Tree must contain the key-values`` (vs : (Key*int64) []) =
        let uvs = vs |> Seq.groupBy fst |> Seq.toArray
        let t = fromArray vs
        let rec loop i =
          if i < uvs.Length then
            let k, gvs = uvs.[i]
            // Pick the last of the grouped values because set overwrites the values
            let _, v = gvs |> Seq.last
            match lookup k t with
            | Some vv when vv = v -> loop (i + 1)
            | _ -> false
          else
            true
        loop 0

      static member ``Tree depth is at most 2[log2 (n + 1)]`` (vs : (Key*int64) []) =
        let log2 v = log v / log 2.
        let uvs = vs |> Seq.distinctBy fst |> Seq.toArray
        let t = fromArray uvs
        let d = depth t
        float d <= 2.0 * (uvs.Length + 1 |> float |> log2)

    open FsCheck

    let run () =
      let config = { Config.Quick with MaxTest = 1000 }
      Check.All<Properties> config

[<EntryPoint>]
let main argv =
  RedBlackTree.Tests.run ()
  0
