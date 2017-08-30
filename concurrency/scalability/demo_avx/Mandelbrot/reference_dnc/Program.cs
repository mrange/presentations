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

    static bool Mandelbrot(double cx, double cy)
    {
      var x  = cx;
      var y  = cy;

      for (var iter = maxIter; iter > 0; --iter)
      {
        var x2 = x*x;
        var y2 = y*y;
        var r2 = x2 + y2;

        if (r2 > 4) return false;

        var xy = x*y;

        y = xy + xy + cy;
        x = x2 - y2 + cx;
      }

      return true;
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

//      Parallel.For(0, dim, y => {
      SequentialFor(0, dim, y => {
        var yoffset = y*width;
        for (int w = 0; w < width; ++w)
        {
          byte bits = 0;
          for (int i = 0; i < 8; ++i)
          {
            var x     = w*8 + i         ;
            var cx    = scalex*x + minX ;
            var cy    = scaley*y + minY ;

            var inSet = Mandelbrot(cx, cy);
            if(inSet)
            {
              bits |= (byte)(1 << (7 - i));
            }
          }

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

      using(var fs = File.Create("mandelbrot_reference_dnc.pbm"))
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
