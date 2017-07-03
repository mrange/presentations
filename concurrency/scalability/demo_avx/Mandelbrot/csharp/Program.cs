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

namespace csharp
{
  using System;
  using System.Diagnostics;
  using System.IO;
  using System.Numerics;
  using System.Runtime.CompilerServices;
  using System.Threading.Tasks;

  using Float4 = System.Numerics.Vector<float>;

  static class Program
  {
    static readonly Stopwatch clock;

    static Program()
    {
      clock = new Stopwatch();
      clock.Start();
    }

    struct MeasuredResult<T>
    {
      public readonly T     Result      ;
      public readonly long  Milliseconds;
      public readonly int   CC0         ;
      public readonly int   CC1         ;
      public readonly int   CC2         ;

      public MeasuredResult(T result, long milliSeconds, int cc0, int cc1, int cc2)
      {
        Result        = result      ;
        Milliseconds  = milliSeconds;
        CC0           = cc0         ;
        CC1           = cc1         ;
        CC2           = cc2         ;
      }
    }

    static MeasuredResult<T> MeasureFunction<T>(Func<T> f)
    {
      GC.Collect(2, System.GCCollectionMode.Forced, true);

      var bcc0 = GC.CollectionCount(0);
      var bcc1 = GC.CollectionCount(1);
      var bcc2 = GC.CollectionCount(2);

      var before = clock.ElapsedMilliseconds;
      var result = f();
      var after  = clock.ElapsedMilliseconds;

      var acc0 = GC.CollectionCount(0);
      var acc1 = GC.CollectionCount(1);
      var acc2 = GC.CollectionCount(2);

      return new MeasuredResult<T>(result, after - before, acc0 - bcc0, acc1 - bcc1, acc2 - bcc2);
    }

    static Float4 float4(float v0, float v1, float v2, float v3)
    {
      return new Float4 (new [] {v0, v1, v2, v3 });
    }

    static Float4 float4_1(float v0)
    {
      return new Float4 (v0);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static int Bit (Float4 r, int i, int bit)
    {
      return r[i] < 4 ? bit : 0;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    static byte Mandelbrot(Float4 cx_0, Float4 cy_0, Float4 cx_1, Float4 cy_1)
    {
      var x_0   = cx_0;
      var y_0   = cy_0;
      var x_1   = cx_1;
      var y_1   = cy_1;

      var r2_0  = new Float4(0);      
      var r2_1  = new Float4(0);      

      for (var iter = maxIter; iter > 0; --iter)
      {
        var x2_0 = x_0*x_0;
        var y2_0 = y_0*y_0;

        r2_0 = x2_0 + y2_0;

        var x2_1 = x_1*x_1;
        var y2_1 = y_1*y_1;

        r2_1 = x2_1 + y2_1;

        var cont =
                r2_0[0] < 4
            ||  r2_0[1] < 4
            ||  r2_0[2] < 4
            ||  r2_0[3] < 4
            ||  r2_1[0] < 4
            ||  r2_1[1] < 4
            ||  r2_1[2] < 4
            ||  r2_1[3] < 4
            ;
        if (!cont) return 0;

        var xy_0 = x_0*y_0;
        var xy_1 = x_1*y_1;

        y_0 = xy_0 + xy_0 + cy_0;
        x_0 = x2_0 - y2_0 + cx_0;

        y_1 = xy_1 + xy_1 + cy_1;
        x_1 = x2_1 - y2_1 + cx_1;
      }

      var mask =
          Bit(r2_0, 0, 0x80)
        | Bit(r2_0, 1, 0x40)
        | Bit(r2_0, 2, 0x20)
        | Bit(r2_0, 3, 0x10)
        | Bit(r2_1, 0, 0x08)
        | Bit(r2_1, 1, 0x04)
        | Bit(r2_1, 2, 0x02)
        | Bit(r2_1, 3, 0x01)
        ;

      return (byte)mask;
    }

    static void SequentialFor(int inclusiveFrom, int exclusiveTo, Action<int> a)
    {
      for (var iter = inclusiveFrom; iter < exclusiveTo; ++iter)
      {
        a(iter);
      }
    }

    static Byte[] GenerateMandelbrot(int dim)
    {
      var width   = (dim + 7) / 8;

      var bytes   = new Byte[width*dim];

      var scalex  = (maxX - minX) / dim;
      var scaley  = (maxY - minY) / dim;

      var incx_0  = float4(
          0 * scalex
        , 1 * scalex
        , 2 * scalex
        , 3 * scalex
        );
      var incx_1  = float4(
          4 * scalex
        , 5 * scalex
        , 6 * scalex
        , 7 * scalex
        );

      Parallel.For(0, dim, y => {
//      SequentialFor(0, dim, y => {
        var yoffset = y*width;
        for (int w = 0; w < width; ++w)
        {
          var x     = w*8                         ;
          var cx    = scalex*x + minX             ;
          var cy    = scaley*y + minY             ;
          var cx_0  = float4_1(cx) + incx_0       ;
          var cx_1  = float4_1(cx) + incx_1       ;
          var cy_0  = float4_1(cy)                ;
          var cy_1  = float4_1(cy)                ;

          var bits  = Mandelbrot(cx_0, cy_0, cx_1, cy_1);

          bytes[yoffset + w] = bits;
        }
      });

      return bytes;
    }

    const float minX        = -1.5F;
    const float minY        = -1.0F;
    const float maxX        =  0.5F;
    const float maxY        =  1.0F;
    const int   maxIter     =  50  ;
    const int   defaultSize =  200 ;

    static int ParseDim(string s)
    {
      var res = Int32.TryParse(s, out int x);
      return res ? x : defaultSize;
    }

    static void Main(string[] args)
    {
      var dim =
            args.Length < 1
          ? defaultSize
          : ParseDim(args[0])
          ;
      Console.WriteLine("Generating mandelbrot set: {0}x{0}({1})", dim, maxIter);
      var result = MeasureFunction(() => GenerateMandelbrot(dim));
      Console.WriteLine(
          "  ... generating mandelbrot set: {0} ms, ({1}, {2}, {3}) GC"
        , result.Milliseconds
        , result.CC0
        , result.CC1
        , result.CC2
        );

      // Writes the pixels as PBM
      //  Can be viewed using: http://paulcuth.me.uk/netpbm-viewer/

      using(var fs = File.Create("mandelbrot_csharp.pbm"))
      using(var ss = new StreamWriter (fs))
      {
        var bytes = result.Result;
        ss.Write (String.Format("P4\n{0} {0}\n", dim));
        ss.Flush ();
        fs.Write (bytes, 0, bytes.Length);
      }
    }
  }
}
