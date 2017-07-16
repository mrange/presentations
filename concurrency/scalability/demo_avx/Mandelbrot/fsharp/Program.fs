﻿// ----------------------------------------------------------------------------------------------
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

type SSEVector = Vector<float32>

type Mandelbrot =
  class

    static member inline step x y cx cy =
      let inline ( * ) x y = (x : SSEVector)*y
      let inline ( + ) x y = (x : SSEVector)+y

      let xy = x * y
      let x2 = x * x
      let y2 = y * y
      let yy = xy + xy + cy
      let xx = x2 - y2 + cx

      xx, yy

    static member inline step2 x y cx cy =
      let inline ( * ) x y = (x : SSEVector)*y
      let inline ( + ) x y = (x : SSEVector)+y

      let xy = x * y
      let x2 = x * x
      let y2 = y * y
      let yy = xy + xy + cy
      let xx = x2 - y2 + cx

      xx, yy, x2, y2

    // The mandelbrot equation: Z' = Z^2 + C
    static member mandelbrot cx_0 cy_0 cx_1 cy_1 cx_2 cy_2 cx_3 cy_3 : uint16 =
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

          let inline cmp (r : SSEVector) i =
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

          let inline bit (r : SSEVector) i s =
            // EXPERIMENTAL: Inline ILAsm
            //let f = r.[i]
            //let c = (# "clt" f 4.F : byte #)
            //(# "shl" c s : byte #)
            if    r.[i] <= 4.F then (1us <<< s)
            else  0us

          let r =
                bit r2_0 0 0xF
            ||| bit r2_0 1 0xE
            ||| bit r2_0 2 0xD
            ||| bit r2_0 3 0xC
            ||| bit r2_1 0 0xB
            ||| bit r2_1 1 0xA
            ||| bit r2_1 2 0x9
            ||| bit r2_1 3 0x8
            ||| bit r2_2 0 0x7
            ||| bit r2_2 1 0x6
            ||| bit r2_2 2 0x5
            ||| bit r2_2 3 0x4
            ||| bit r2_3 0 0x3
            ||| bit r2_3 1 0x2
            ||| bit r2_3 2 0x1
            ||| bit r2_3 3 0x0

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

  let minX4     = SSEVector minX
  let scaleX4   = SSEVector scaleX
  let lshiftX4  = SSEVector [|0.F; 1.F; 2.F; 3.F|]
  let ushiftX4  = SSEVector [|4.F; 5.F; 6.F; 7.F|]

  let mandelbrotSet () =
    Parallel.For (0, dim / 2, fun hy ->
      let y       = hy*2
      let yoffset = y*width
      let cy_0    = SSEVector (scaleY*(float32 (y    )) + minY)
      let cy_1    = SSEVector (scaleY*(float32 (y + 1)) + minY)
      for w = 0 to (width - 1) do
        let x     = w*8
        let x4    = SSEVector (float32 x)
        let cx_0  = minX4 + (x4 + lshiftX4)*scaleX4
        let cx_1  = minX4 + (x4 + ushiftX4)*scaleX4
        let bits  = Mandelbrot.mandelbrot cx_0 cy_0 cx_1 cy_0 cx_0 cy_1 cx_1 cy_1
        pixels.[yoffset         + w] <- byte (bits >>> 8)
        pixels.[yoffset + width + w] <- byte (bits      )
      )

  printfn "Generating mandelbrot set: %dx%d(%d)" dim dim maxIter
  let ms, cc0, cc1, cc2, _ = timeIt mandelbrotSet
  printfn "  ... generating mandelbrot set: %d ms, (%d, %d, %d) GC" ms cc0 cc1 cc2

  // Writes the pixels as PBM
  //  Can be viewed using: http://paulcuth.me.uk/netpbm-viewer/
  do
//    let fs = Console.OpenStandardOutput()
    use fs = System.IO.File.Create "mandelbrot_fsharp2.pbm"
    use ss = new System.IO.StreamWriter (fs)
    ss.Write (sprintf "P4\n%d %d\n" dim dim)
    ss.Flush ()
    fs.Write (pixels, 0, pixels.Length)

  0
