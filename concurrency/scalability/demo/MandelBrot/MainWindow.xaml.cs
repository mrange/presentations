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
using System;
using System.Collections.Concurrent;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Threading;

namespace MandelBrot
{
    public partial class MainWindow
    {
        readonly static Color[] s_colors  =
        {
            Colors.Red          ,
            Colors.Yellow       ,
            Colors.Green        ,
            Colors.Cyan         ,
            Colors.Blue         ,
            Colors.Magenta      ,
            Colors.Red          ,
        };

        const int       IndicatorSize = 8                       ;

        const int       MaxIter     = 2048                      ;
        const int       ImageWidth  = 2048                      ;
        const int       ImageHeight = 2048                      ;

        const double    CenterX     = 0.001643721971153         ;
        const double    CenterY     = 0.822467633298876         ;

        const double    ZoomX       = 0.00000000010             ;
        const double    ZoomY       = 0.00000000010             ;

        WriteableBitmap                 m_wb                    ;
        readonly DispatcherTimer        m_renderTimer           ;
        readonly ConcurrentQueue<Line>  m_queue                 = new ConcurrentQueue<Line> ();


        readonly Color[]                m_colorPalette          ;

        readonly Stopwatch              m_stopwatch             = new Stopwatch ();

        Task m_renderTask;


        struct Line
        {
            public Line (int lineNo)
                :   this ()
            {
                LineNo  = lineNo            ;
                Pixels  = new byte[ImageWidth*4] ;
            }


            public int      LineNo  ;
            public byte[]   Pixels  ;
        }


        public MainWindow ()
        {
            InitializeComponent ();

            m_renderTimer   = new DispatcherTimer (
                TimeSpan.FromMilliseconds(200)      , 
                DispatcherPriority.ApplicationIdle  , 
                OnRenderBitmap                      , 
                Dispatcher
                );

            var steps   = 16  ;

            var interpolatedColors = Enumerable
                .Range (0, s_colors.Length)
                .SelectMany (i => 
                {
                    var from    = s_colors[i % s_colors.Length].ToVector4();
                    var to      = s_colors[(i + 1) % s_colors.Length].ToVector4();
                    return Enumerable
                        .Range (0, steps)
                        .Select (ii => (((double)ii) / steps).Lerp(from, to).ToColor());
                })
                .ToArray ()
                ;

            m_colorPalette = new Color[MaxIter + 1];

            for (var iter = 0; iter < MaxIter; ++iter)
            {
                m_colorPalette[iter] = interpolatedColors[iter % interpolatedColors.Length];
            }

            m_colorPalette[MaxIter] = Colors.Black;

        }

        void OnRenderBitmap(object sender, EventArgs e)
        {
            if (m_wb == null)
            {
                return;
            }

            Line line;
            m_wb.Lock();
            try 
            {
                while (m_queue.TryDequeue (out line))
                {
                    if (
                            line.LineNo >= 0 
                        &&  line.LineNo < ImageHeight
                        &&  line.Pixels != null
                        &&  line.Pixels.Length/4 == ImageHeight
                        )
                    {
                        m_wb.WritePixels (new Int32Rect (line.LineNo, 0, 1, ImageHeight), line.Pixels, 4, 0);
                    }
                }
            }
            finally
            {
                m_wb.Unlock();
            }
        }

        void Render_Click (object sender, RoutedEventArgs e)
        {
            if (m_renderTask != null)
            {
                return;
            }

            Info.Text = "Rendering...";

            m_wb            = new WriteableBitmap (ImageWidth, ImageHeight, 96.0, 96.0, PixelFormats.Bgr32, null);
            Image.Source    = m_wb;

            m_stopwatch.Restart(); 
            m_renderTask = Task.Factory.StartNew (RenderMandelBrot, TaskCreationOptions.LongRunning);
        }



        void RenderMandelBrot(object arg)
        {
            var tx = ZoomX/ImageWidth;
            var mx = CenterX - ZoomX / 2;
            var ty = ZoomY/ImageHeight;
            var my = CenterY - ZoomY / 2;


            for (var ix = 0; ix < ImageWidth; ++ix)
            {
                var tid = Thread.CurrentThread.ManagedThreadId;
                var line = new Line (ix);

                for (var iy = 0; iy < ImageHeight; ++iy)
                {
                    var x = tx * ix + mx;
                    var y = ty * iy + my;
                    var iter = MandelBrot (x,y, MaxIter);
                    var color = m_colorPalette[iter];

                    line.Pixels[4*iy + 0] = color.B;
                    line.Pixels[4*iy + 1] = color.G;
                    line.Pixels[4*iy + 2] = color.R;
                    line.Pixels[4*iy + 3] = color.A;
                }


                var threadColor = s_colors[tid%s_colors.Length];
                for (var iy = 0; iy < IndicatorSize; ++iy)
                {
                    line.Pixels[4*iy + 0] = threadColor.B;
                    line.Pixels[4*iy + 1] = threadColor.G;
                    line.Pixels[4*iy + 2] = threadColor.R;
                    line.Pixels[4*iy + 3] = threadColor.A;
                }

                m_queue.Enqueue(line);
            }

            Action onInvoke = FinalizeRender;

            Dispatcher.BeginInvoke(
                DispatcherPriority.ApplicationIdle,
                onInvoke
                );

        }

        void FinalizeRender()
        {
            m_renderTask = null;
            m_stopwatch.Stop(); 
            Info.Text = string.Format (
                CultureInfo.InvariantCulture, 
                "Done, it took: {0:#,0} ms",
                m_stopwatch.ElapsedMilliseconds
                );
        }


        int MandelBrot (double x, double y, int iter)
        {
            var ix = x;
            var iy = y;

            var i = 0;

            // Zn+1 = Zn^2 + C

            for (; (i < iter) & ((ix * ix + iy * iy) < 4); ++i)
            {
                var tx = ix * ix - iy * iy + x;
                iy = 2 * ix * iy + y;
                ix = tx;
            }

            return i;
        }
    }
}
