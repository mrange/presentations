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


// Record
[<NoComparison>]
type Complex =
    {
        Real    : double
        Imag    : double
    }

    static member New re im = { Real = re; Imag = im }

    static member ( + ) (l : Complex, r : Complex) =  Complex.New (l.Real + r.Real) (l.Imag + r.Imag)
    static member ( * ) (l : Complex, r : Complex) =  Complex.New (l.Real*r.Real - l.Imag*r.Imag) (2.*l.Real*l.Imag)

    member x.Radius = sqrt (x.Real*x.Real + x.Imag*x.Imag)
    member x.Angle  = atan2 x.Imag x.Real

module BinaryTree =
    // ADT
    type Tree<'TKey, 'TValue> =
        | Empty
        | Leaf of 'TKey*'TValue
        | Node of 'TKey*'TValue*Tree<'TKey, 'TValue>*Tree<'TKey, 'TValue>

    let aTree = Node (2, 20, Leaf (1, 10), Node (3, 30, Leaf (4, 40), Leaf (5, 50)))

    let rec find f t =
        match t with
        | Empty -> None
        | Leaf (k,v) when f = v -> Some v
        | Leaf _ -> None
        | Node (k,v,_,_) when f = k -> Some v
        | Node (k,_,l,_) when f < k -> find f l
        | Node (_,_,_,r) -> find f r

open System
// Primary constructors
type X(id : int, name : string) =

    interface IDisposable with
        member x.Dispose () = printfn "Disposed"

    member x.Id     = id
    member x.Name   = name

open BinaryTree

// Csv provider
type Netflix =  FSharp.Data.CsvProvider<"netflix.csv">

[<EntryPoint>]
let main argv =
    // 1. Hello world
    printfn "Hello world"

    // 2. Values
    let x = 3
    let y = "Hello"
    let z : int = 5

    // 3. Functions
    let mad x y z = x*y + z
    let result = mad 1 2 3
    let mad (x : double) (y : double) (z : double) = x*y + z
    let result = mad 1. 2. 3.
    let inline mad x y z = x*y + z
    let result = mad 1. 2. 3.
    let result = mad 1 2 3

    let curried = mad 1 2
    let result = curried 3

    // 4. Records
    let x = Complex.New 1. 2.
    let y = Complex.New 2. 1.
    let z = Complex.New 1. 0.

    let result = mad x y z
    printfn "%A" result

    // 5. List comphrension
    let v = [| for x in 0..3 -> x |]
    let v = [| for x in 0..3 -> x*x |]
    let i = [| for x in 0..3 ->
                [| for y in 0..3 ->
                    if x = y then 1. else 0.
                |]
            |]
    printfn "Identity: %A" i
    let i = [| for x in 0..3 do
                for y in 0..3 do yield if x = y then 1. else 0.
            |]
    printfn "Identity: %A" i

    // 6. "LINQ"
    let ones = i |> Array.filter (fun x -> x <> 0.)
    printfn "Ones: %A" ones

    let ones = i |> Array.filter ((<>) 0.)
    printfn "Ones: %A" ones

    let ones = i |> Array.filter ((<>) 0.) |> Array.map (fun x -> x.ToString ())
    printfn "Ones: %A" ones

    // 7. ADT
    let v1 = find 3 aTree
    let v2 = find 10 aTree

    printfn "Tree: %A" aTree
    printfn "v1: %A" v1
    printfn "v2: %A" v2

    // 8. No nulls
    match v1 with
    | Some v -> printfn "Has value: %A" v
    | _ -> printfn "No value"

    match v2 with
    | Some v -> printfn "Has value: %A" v
    | _ -> printfn "No value"

    // 9. Types
    use x = new X(1, "Testing")
    printfn "Id: %d, Name:%s" x.Id x.Name

    // 10. printf
    printfn "An integer: %d" 1
    printfn "A double: %f" 3.14
    printfn "A string: %s" "Testing"
    printfn "%d, %f, %s" 1 3.14 "Testing"

    // 11. Fsharp.Data
    let ratings = (Netflix.Load "netflix.csv").Rows |> Seq.toArray

    let sorted  = ratings |> Array.sortBy (fun rating -> rating.Rating)
    printfn "Sorted: %A" sorted

    let rated  = ratings |> Array.sortBy (fun rating -> rating.Date)
    printfn "Sorted: %A" rated

    // 12. Reflections
    //   1. No nulls
    //   2. Prefer Immutability
    //   3. Excellent language reference
    //      https://msdn.microsoft.com/en-us/library/dd233181.aspx
    //   4. Excellent and interesting libraries
    //      WebSharper
    //      FunScript
    //      FSharp.Data
    //      FParsec
    //   5. Computation expressions
    //      The most powerful construct in .NET universe


    0
