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

namespace Canvas

open System
open System.Collections.Generic

open SharpDX

[<AutoOpen>]
module internal Utils =
    let DefaultOf<'T>  = Unchecked.defaultof<'T>

    let Dispose (d : IDisposable) =
        if d <> null then
            try
                d.Dispose ()
            with
            | e -> () // TODO: Trace

    let DisposeValuesInDictionary (d : IDictionary<'K, #IDisposable>) : unit =
        try
            for kv in d do
                Dispose kv.Value
        finally
            d.Clear ()

    type ActionDisposable(a : unit -> unit) =
        interface IDisposable with
            member x.Dispose () = 
                try
                    a ()
                with
                | e -> () // TODO: Trace
                

    let OnExit (a : unit -> unit) : IDisposable = upcast new ActionDisposable (a)

    type IDictionary<'K,'V> with
        member inline x.GetOrCreate (k : 'K) (creator : 'K -> 'V) =
            let mutable v = DefaultOf<'V>
            if x.TryGetValue (k, &v) then
                v
            else
                v <- creator k
                x.Add (k, v)
                v

