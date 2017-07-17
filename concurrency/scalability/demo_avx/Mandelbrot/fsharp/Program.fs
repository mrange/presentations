// ----------------------------------------------------------------------------------------------
// Copyright 2017 Mårten Rånge
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
// ----------------------------------------------------------------------------------------------

let clock =
  let sw = System.Diagnostics.Stopwatch ()
  sw.Start ()
  fun () -> sw.ElapsedMilliseconds

let timeIt a =
  System.GC.Collect (2, System.GCCollectionMode.Forced, true)

  let inline cc g       = System.GC.CollectionCount g
  let bcc0, bcc1, bcc2  = cc 0, cc 1, cc 2
  let before            = clock ()

  let r                 = a ()

  let after             = clock ()
  let acc0, acc1, acc2  = cc 0, cc 1, cc 2

  after - before, acc0 - bcc0, acc1 - bcc1, acc2 - bcc2, r

open System.Numerics
open System.Threading.Tasks

type FloatN = Vector<float32>

let float4 v0 v1 v2 v3 =
  let vs = Array.zeroCreate FloatN.Count
  vs.[0] <- v0
  vs.[1] <- v1
  vs.[2] <- v2
  vs.[3] <- v3
  FloatN vs

let float8 v0 v1 v2 v3 v4 v5 v6 v7 =
  let vs = Array.zeroCreate FloatN.Count
  vs.[0] <- v0
  vs.[1] <- v1
  vs.[2] <- v2
  vs.[3] <- v3
  vs.[4] <- v4
  vs.[5] <- v5
  vs.[6] <- v6
  vs.[7] <- v7
  FloatN vs

let inline float4_1 (v : float32) = FloatN v


type Mandelbrot =
  class

    static member inline step x y cx cy =
      let inline ( * ) x y = (x : FloatN)*y
      let inline ( + ) x y = (x : FloatN)+y

      let xy = x * y
      let x2 = x * x
      let y2 = y * y
      let yy = xy + xy + cy
      let xx = x2 - y2 + cx

      xx, yy

    static member inline step2 x y cx cy =
      let inline ( * ) x y = (x : FloatN)*y
      let inline ( + ) x y = (x : FloatN)+y

      let xy = x * y
      let x2 = x * x
      let y2 = y * y
      let yy = xy + xy + cy
      let xx = x2 - y2 + cx

      xx, yy, x2, y2

    // The mandelbrot equation: Z' = Z^2 + C
    static member mandelbrot_sse cx_0 cy_0 cx_1 cy_1 cx_2 cy_2 cx_3 cy_3 : uint16 =
      let rec loop rem x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3 =

        if rem > 0 then
          // #0
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #1
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #2
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #3
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #4
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #5
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #6
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #7
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          let inline cmp (r : FloatN) i =
            r.[i] <= 4.F
            // EXPERIMENTAL: Inline ILAsm
            //let f = r.[i]
            //(# "clt" f 4.F : byte #)

          let c =
               cmp r2_0 0
            || cmp r2_0 1
            || cmp r2_0 2
            || cmp r2_0 3
            || cmp r2_1 0
            || cmp r2_1 1
            || cmp r2_1 2
            || cmp r2_1 3
            || cmp r2_2 0
            || cmp r2_2 1
            || cmp r2_2 2
            || cmp r2_2 3
            || cmp r2_3 0
            || cmp r2_3 1
            || cmp r2_3 2
            || cmp r2_3 3

          if c then
              loop (rem - 1) x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3
            else
              0us
        else
          // #48
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #49
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          let inline bit (r : FloatN) i s =
            // EXPERIMENTAL: Inline ILAsm
            //let f = r.[i]
            //let c = (# "clt" f 4.F : byte #)
            //(# "shl" c s : byte #)
            if    r.[i] <= 4.F then (1us <<< s)
            else  0us

          let r =
                bit r2_0 0 0x7
            ||| bit r2_0 1 0x6
            ||| bit r2_0 2 0x5
            ||| bit r2_0 3 0x4
            ||| bit r2_1 0 0x3
            ||| bit r2_1 1 0x2
            ||| bit r2_1 2 0x1
            ||| bit r2_1 3 0x0
            ||| bit r2_2 0 0xF
            ||| bit r2_2 1 0xE
            ||| bit r2_2 2 0xD
            ||| bit r2_2 3 0xC
            ||| bit r2_3 0 0xB
            ||| bit r2_3 1 0xA
            ||| bit r2_3 2 0x9
            ||| bit r2_3 3 0x8

          r

      loop 6 cx_0 cy_0 cx_0 cy_0 cx_1 cy_1 cx_1 cy_1 cx_2 cy_2 cx_2 cy_2 cx_3 cy_3 cx_3 cy_3

    static member mandelbrot_full_sse cx_0 cy_0 cx_1 cy_1 cx_2 cy_2 cx_3 cy_3 : uint16 =
      let rec loop rem x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3 =

        if rem > 0 then
          // #0
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #1
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #2
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #3
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #4
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #5
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #6
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #7
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          loop (rem - 1) x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3
        else
          // #48
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #49
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          let inline bit (r : FloatN) i s =
            // EXPERIMENTAL: Inline ILAsm
            //let f = r.[i]
            //let c = (# "clt" f 4.F : byte #)
            //(# "shl" c s : byte #)
            if    r.[i] <= 4.F then (1us <<< s)
            else  0us

          let r =
                bit r2_0 0 0x7
            ||| bit r2_0 1 0x6
            ||| bit r2_0 2 0x5
            ||| bit r2_0 3 0x4
            ||| bit r2_1 0 0x3
            ||| bit r2_1 1 0x2
            ||| bit r2_1 2 0x1
            ||| bit r2_1 3 0x0
            ||| bit r2_2 0 0xF
            ||| bit r2_2 1 0xE
            ||| bit r2_2 2 0xD
            ||| bit r2_2 3 0xC
            ||| bit r2_3 0 0xB
            ||| bit r2_3 1 0xA
            ||| bit r2_3 2 0x9
            ||| bit r2_3 3 0x8

          r

      loop 6 cx_0 cy_0 cx_0 cy_0 cx_1 cy_1 cx_1 cy_1 cx_2 cy_2 cx_2 cy_2 cx_3 cy_3 cx_3 cy_3

    static member mandelbrot_avx cx_0 cy_0 cx_1 cy_1 cx_2 cy_2 cx_3 cy_3 : uint32 =
      let rec loop rem x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3 =

        if rem > 0 then
          // #0
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #1
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #2
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #3
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #4
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #5
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #6
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #7
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          let inline cmp (r : FloatN) i =
            // EXPERIMENTAL: Inline ILAsm
            // let f = r.[i]
            // (# "clt" f 4.F : uint32 #)
            r.[i] <= 4.F

          let c =
               cmp r2_0 0
            || cmp r2_0 1
            || cmp r2_0 2
            || cmp r2_0 3
            || cmp r2_0 4
            || cmp r2_0 5
            || cmp r2_0 6
            || cmp r2_0 7
            || cmp r2_1 0
            || cmp r2_1 1
            || cmp r2_1 2
            || cmp r2_1 3
            || cmp r2_1 4
            || cmp r2_1 5
            || cmp r2_1 6
            || cmp r2_1 7
            || cmp r2_2 0
            || cmp r2_2 1
            || cmp r2_2 2
            || cmp r2_2 3
            || cmp r2_2 4
            || cmp r2_2 5
            || cmp r2_2 6
            || cmp r2_2 7
            || cmp r2_3 0
            || cmp r2_3 1
            || cmp r2_3 2
            || cmp r2_3 3
            || cmp r2_3 4
            || cmp r2_3 5
            || cmp r2_3 6
            || cmp r2_3 7

          if c then
              loop (rem - 1) x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3
            else
              0u
        else
          // #48
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #49
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          let inline bit (r : FloatN) i s =
            // EXPERIMENTAL: Inline ILAsm
            // let f = r.[i]
            // let c = (# "clt" f 4.F : uint32 #)
            // (# "shl" c s : uint32 #)
            if    r.[i] <= 4.F then (1u <<< s)
            else  0u

          let r =
                bit r2_0 0 0x07
            ||| bit r2_0 1 0x06
            ||| bit r2_0 2 0x05
            ||| bit r2_0 3 0x04
            ||| bit r2_0 4 0x03
            ||| bit r2_0 5 0x02
            ||| bit r2_0 6 0x01
            ||| bit r2_0 7 0x00
            ||| bit r2_1 0 0x0F
            ||| bit r2_1 1 0x0E
            ||| bit r2_1 2 0x0D
            ||| bit r2_1 3 0x0C
            ||| bit r2_1 4 0x0B
            ||| bit r2_1 5 0x0A
            ||| bit r2_1 6 0x09
            ||| bit r2_1 7 0x08
            ||| bit r2_2 0 0x17
            ||| bit r2_2 1 0x16
            ||| bit r2_2 2 0x15
            ||| bit r2_2 3 0x14
            ||| bit r2_2 4 0x13
            ||| bit r2_2 5 0x12
            ||| bit r2_2 6 0x11
            ||| bit r2_2 7 0x10
            ||| bit r2_3 0 0x1F
            ||| bit r2_3 1 0x1E
            ||| bit r2_3 2 0x1D
            ||| bit r2_3 3 0x1C
            ||| bit r2_3 4 0x1B
            ||| bit r2_3 5 0x1A
            ||| bit r2_3 6 0x19
            ||| bit r2_3 7 0x18

          r

      loop 6 cx_0 cy_0 cx_0 cy_0 cx_1 cy_1 cx_1 cy_1 cx_2 cy_2 cx_2 cy_2 cx_3 cy_3 cx_3 cy_3

    static member mandelbrot_full_avx cx_0 cy_0 cx_1 cy_1 cx_2 cy_2 cx_3 cy_3 : uint32 =
      let rec loop rem x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3 =

        if rem > 0 then
          // #0
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #1
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #2
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #3
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #4
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #5
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #6
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #7
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          loop (rem - 1) x_0 y_0 cx_0 cy_0 x_1 y_1 cx_1 cy_1 x_2 y_2 cx_2 cy_2 x_3 y_3 cx_3 cy_3
        else
          // #48
          let x_0, y_0 = Mandelbrot.step x_0 y_0 cx_0 cy_0
          let x_1, y_1 = Mandelbrot.step x_1 y_1 cx_1 cy_1
          let x_2, y_2 = Mandelbrot.step x_2 y_2 cx_2 cy_2
          let x_3, y_3 = Mandelbrot.step x_3 y_3 cx_3 cy_3

          // #49
          let x_0, y_0, x2_0, y2_0 = Mandelbrot.step2 x_0 y_0 cx_0 cy_0
          let x_1, y_1, x2_1, y2_1 = Mandelbrot.step2 x_1 y_1 cx_1 cy_1
          let x_2, y_2, x2_2, y2_2 = Mandelbrot.step2 x_2 y_2 cx_2 cy_2
          let x_3, y_3, x2_3, y2_3 = Mandelbrot.step2 x_3 y_3 cx_3 cy_3

          let r2_0 = x2_0 + y2_0
          let r2_1 = x2_1 + y2_1
          let r2_2 = x2_2 + y2_2
          let r2_3 = x2_3 + y2_3

          let inline bit (r : FloatN) i s =
            // EXPERIMENTAL: Inline ILAsm
            // let f = r.[i]
            // let c = (# "clt" f 4.F : uint32 #)
            // (# "shl" c s : uint32 #)
            if    r.[i] <= 4.F then (1u <<< s)
            else  0u

          let r =
                bit r2_0 0 0x07
            ||| bit r2_0 1 0x06
            ||| bit r2_0 2 0x05
            ||| bit r2_0 3 0x04
            ||| bit r2_0 4 0x03
            ||| bit r2_0 5 0x02
            ||| bit r2_0 6 0x01
            ||| bit r2_0 7 0x00
            ||| bit r2_1 0 0x0F
            ||| bit r2_1 1 0x0E
            ||| bit r2_1 2 0x0D
            ||| bit r2_1 3 0x0C
            ||| bit r2_1 4 0x0B
            ||| bit r2_1 5 0x0A
            ||| bit r2_1 6 0x09
            ||| bit r2_1 7 0x08
            ||| bit r2_2 0 0x17
            ||| bit r2_2 1 0x16
            ||| bit r2_2 2 0x15
            ||| bit r2_2 3 0x14
            ||| bit r2_2 4 0x13
            ||| bit r2_2 5 0x12
            ||| bit r2_2 6 0x11
            ||| bit r2_2 7 0x10
            ||| bit r2_3 0 0x1F
            ||| bit r2_3 1 0x1E
            ||| bit r2_3 2 0x1D
            ||| bit r2_3 3 0x1C
            ||| bit r2_3 4 0x1B
            ||| bit r2_3 5 0x1A
            ||| bit r2_3 6 0x19
            ||| bit r2_3 7 0x18

          r

      loop 6 cx_0 cy_0 cx_0 cy_0 cx_1 cy_1 cx_1 cy_1 cx_2 cy_2 cx_2 cy_2 cx_3 cy_3 cx_3 cy_3
  end

[<EntryPoint>]
let main argv =
  // Argument is the desired x/y size of the set
  let dim       =
    let dim = if argv.Length > 0 then int argv.[0] else 200
    max dim 200
  let dimf      = float32 dim
  let width     = (dim + 7) / 8

  // What part of the mandelbrot set is rendered
  let minX      = -1.5F
  let minY      = -1.0F
  let maxX      =  0.5F
  let maxY      =  1.0F
  // More iterations means a more accurate visualization of the mandelbrot set
  let maxIter   =  50

  let scaleX    = (maxX - minX) / dimf;
  let scaleY    = (maxY - minY) / dimf;

  let pixels    = Array.zeroCreate (width*dim)

  let minX4     = float4_1 minX
  let scaleX4   = float4_1 scaleX

  let mandelbrotSet_sse () =
    let lshiftX4  = float4 0.F 1.F 2.F 3.F
    let ushiftX4  = float4 4.F 5.F 6.F 7.F

    Parallel.For (0, dim / 2, fun hy ->
      let y       = hy*2
      let yoffset = y*width
      let cy_0    = float4_1 (scaleY*(float32 (y    )) + minY)
      let cy_1    = float4_1 (scaleY*(float32 (y + 1)) + minY)
      let mutable last_full = false
      for w = 0 to (width - 1) do
        let x     = w*8
        let x4    = float4_1 (float32 x)
        let cx_0  = minX4 + (x4 + lshiftX4)*scaleX4
        let cx_1  = minX4 + (x4 + ushiftX4)*scaleX4
        let bits  =
          if last_full then
            Mandelbrot.mandelbrot_full_sse cx_0 cy_0 cx_1 cy_0 cx_0 cy_1 cx_1 cy_1
          else
            Mandelbrot.mandelbrot_sse cx_0 cy_0 cx_1 cy_0 cx_0 cy_1 cx_1 cy_1
        pixels.[yoffset         + w] <- byte (bits      )
        pixels.[yoffset + width + w] <- byte (bits >>> 8)

        last_full <- bits <> 0us
      )

  let mandelbrotSet_avx () =
    let shiftX8 = float8 0.F 1.F 2.F 3.F 4.F 5.F 6.F 7.F

    Parallel.For (0, dim / 4, fun hy ->
      let y       = hy*4
      let yoffset = y*width
      let cy_0    = float4_1 (scaleY*(float32 (y    )) + minY)
      let cy_1    = float4_1 (scaleY*(float32 (y + 1)) + minY)
      let cy_2    = float4_1 (scaleY*(float32 (y + 2)) + minY)
      let cy_3    = float4_1 (scaleY*(float32 (y + 3)) + minY)
      let mutable last_full = false
      for w = 0 to (width - 1) do
        let x     = w*8
        let x4    = float4_1 (float32 x)
        let cx_0  = minX4 + (x4 + shiftX8)*scaleX4
        let bits  =
          if last_full then
            Mandelbrot.mandelbrot_full_avx cx_0 cy_0 cx_0 cy_1 cx_0 cy_2 cx_0 cy_3
          else
            Mandelbrot.mandelbrot_avx cx_0 cy_0 cx_0 cy_1 cx_0 cy_2 cx_0 cy_3
        pixels.[yoffset           + w] <- byte (bits       )
        pixels.[yoffset +   width + w] <- byte (bits >>> 8 )
        pixels.[yoffset + 2*width + w] <- byte (bits >>> 16)
        pixels.[yoffset + 3*width + w] <- byte (bits >>> 24)

        last_full <- bits <> 0u
      )

  let mandelbrotSet =
    if FloatN.Count = 4 then
      printfn "Using SSE"
      mandelbrotSet_sse
    else
      printfn "Using AVX2"
      mandelbrotSet_avx

  printfn "Generating mandelbrot set: %dx%d(%d)" dim dim maxIter
  let ms, cc0, cc1, cc2, _ = timeIt mandelbrotSet
  printfn "  ... generating mandelbrot set: %d ms, (%d, %d, %d) GC" ms cc0 cc1 cc2

  // Writes the pixels as PBM
  //  Can be viewed using: http://paulcuth.me.uk/netpbm-viewer/
  do
//    let fs = Console.OpenStandardOutput()
    use fs = System.IO.File.Create "mandelbrot_fsharp.pbm"
    use ss = new System.IO.StreamWriter (fs)
    ss.Write (sprintf "P4\n%d %d\n" dim dim)
    ss.Flush ()
    fs.Write (pixels, 0, pixels.Length)

  0
