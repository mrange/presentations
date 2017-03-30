module RedBlackTree =
  type Color =
    | Red
    | Black
  type Tree<'K, 'V> =
    | Leaf
    | Node  of Color*Tree<'K, 'V>*'K*'V*Tree<'K, 'V>

  [<GeneralizableValue>]
  let empty = Leaf

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

