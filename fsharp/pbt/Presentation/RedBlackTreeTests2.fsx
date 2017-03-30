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

  type Properties () =

    // §1 - Lookup after set shall succeed
    //  §1.1 - The tree shall contain all distinct key/values
    // §2 - Lookup after unset shall fail

    // §3 - Parent key is greater than left child, smaller than right child

    // §4 - No Red Node has a Red child
    // §5 - Every path from the Root to a Leaf contains the same number of Black nodes
    // §6 - §4 + §5 => Tree depth at most 2[log2 (n + 1)]

    static member dummy () =
      true

  let run () =
    let config = { Config.Quick with MaxTest = 100; MaxFail = 1000 }
    Check.All<Properties> config

// Tests.run ()
