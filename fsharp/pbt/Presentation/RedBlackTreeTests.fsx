#r "../packages/FsCheck.2.8.0/lib/net452/FsCheck.dll"

#load "RedBlackTree.fsx"

open RedBlackTree.RedBlackTree

module Tests =
  open FsCheck

  let rec depth t =
    match t with
    | Leaf -> 0
    | Node (_, l, _, _, r) -> (max (depth l) (depth r)) + 1

  let log2 v = log v / log 2.

  type Key =
    | Int     of int
    | String  of string
//    | Float   of float

  type Properties () =

    // §1 - Lookup after set shall succeed
    //  §1.1 - The tree shall contain all distinct key/values
    // §2 - Lookup after unset shall fail

    // §3 - Parent key is greater than left child, smaller than right child

    // §4 - No Red Node has a Red child
    // §5 - Every path from the Root to a Leaf contains the same number of Black nodes
    // §6 - §4 + §5 => Tree depth at most 2[log2 (n + 1)]

    static member ``§1 - Lookup after set shall succeed`` (k : Key) (v : int) (vs : (Key*int) []) =
      let tree  = fromArray vs
      let tree  = set k v tree

      match lookup k tree with
      | Some tv -> tv = v
      | None    -> false

    static member ``§1.1 - The tree shall contain all distinct key/values`` (vs : (Key*int) []) =
      let vs    = vs |> Array.distinctBy fst
      let tree  = fromArray vs

      let rec loop i =
        if i < vs.Length then
          let k, v = vs.[i]
          match lookup k tree with
          | Some tv -> tv = v && loop (i + 1)
          | None    -> false
        else
          true

      loop 0

    static member ``§3 - Parent node is greater than left child, smaller than right child`` (vs : (Key*int) []) =
      let tree  = fromArray vs

      let rec loop t =
        match t with
        | Leaf  -> true
        | Node (_, l, k, _, r) ->
          let leftIsOk =
            match l with
            | Leaf                  -> true
            | Node (_, _, lk, _, _) -> lk < k
          let rightIsOk =
            match r with
            | Leaf                  -> true
            | Node (_, _, rk, _, _) -> k < rk
          leftIsOk && rightIsOk && loop l && loop r

      loop tree

    static member ``§4 - No Red Node has a Red child`` (vs : (Key*int) []) =
      let tree = fromArray vs

      let rec test t =
        match t with
        | Leaf                                        -> true
        | Node (Red, Node (Red, _, _, _, _), _, _, _)
        | Node (Red, _, _, _, Node (Red, _, _, _, _)) -> false
        | Node (_, l, _, _, r)                        -> test l && test r

      test tree

    static member ``§5 - Every path from the Root to a Leaf contains the same number of Black nodes`` (vs : (Key*int) []) =
      let tree = fromArray vs

      let rec test bns t =
        match t with
        | Leaf                  -> bns + 1
        | Node (c, l, _, _, r)  ->
          let bns =
            match c with
            | Black -> bns + 1
            | Red   -> bns
          let lbns = test bns l
          let rbns = test bns r
          if lbns = rbns then
            lbns
          else
            -1

      test 0 tree > -1

    static member ``§6 - §4 + §5 => Tree depth at most 2[log2 (n + 1)]`` (vs : (Key*int) []) =
      let vs    = vs |> Array.distinctBy fst
      let tree  = fromArray vs
      let depth = depth     tree
      float depth <= 2.0 * (log2 (float (vs.Length + 1)))

  let run () =
    let config = { Config.Quick with MaxTest = 1000; MaxFail = 1000 }
    Check.All<Properties> config

// Tests.run ()
