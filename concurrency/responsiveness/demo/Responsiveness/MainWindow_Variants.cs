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
using System.Diagnostics;
using System.IO;
using System.Net;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
namespace Responsiveness
{
    partial class MainWindow
    {
        void RenderBitmap(CancellationToken ct)
        {
            TraceThreadId();

//            RenderBitmap_Trivial(ct);

//            RenderBitmap_NaiveTask(ct)
//            RenderBitmap_Task(ct)
            RenderBitmap_Async(ct)
                //.ContinueWith(t => 
                //    {
                //        TraceThreadId();

                //        if (t.IsCanceled || !t.Result)
                //        {
                //            DisplayError (new Exception ("Cancelled"));
                //        }
                //    })
                .HandleFaults()
                ;
        }

        void RenderBitmap_Trivial(CancellationToken ct)
        {
            RenderMandelbrot(ct);
        }

        Task<bool> RenderBitmap_NaiveTask(CancellationToken ct)
        {
            return Task.Run(() => RenderMandelbrot(ct), ct);
        }

        Task<bool> RenderBitmap_Task(CancellationToken ct)
        {
            var width = m_bitmap.PixelWidth;
            var height = m_bitmap.PixelHeight;

            return Task.Run(() => RenderMandelbrot(
                ct,
                width,
                height,
                (pixels, x) => Dispatcher.InvokeAsync(() => WritePixels(pixels, x))
                ), 
                ct);
        }

        async Task<bool> RenderBitmap_Async(CancellationToken ct)
        {
            TraceThreadId();

            var result = await RenderMandelbrotAsync(ct);

            TraceThreadId();

            if (!result)
            {
                DisplayError (new Exception ("Cancelled"));
            }

            return result;
        }
    
    }
}
