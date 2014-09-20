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
using System.Linq;
using System.Net;
using System.Threading;
using System.Threading.Tasks;
using System.Windows;
namespace Responsiveness
{
    partial class MainWindow
    {
        void RenderBitmap1 (CancellationToken ct)
        {
            TraceThreadId();

            //RenderBitmap_Trivial(ct);
            //RenderBitmap_NaiveTask(ct)
            //RenderBitmap_Task(ct)
            //.HandleFaults();
        }

        void RenderBitmap4 (CancellationToken ct)
        {
            TraceThreadId();

            Render4Bitmap_Task (ct)
                .HandleResult()
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
                0,
                0,
                width,
                height,
                AsyncWritePixels
                ),
                ct);
        }

        async Task<bool> Render4Bitmap_Task (CancellationToken ct)
        {
            var width = m_bitmap.PixelWidth;
            var height = m_bitmap.PixelHeight;

            Func<int, Task<bool>> taskCreator =
                i => Task.Factory.StartNew(
                    () => RenderMandelbrot(
                        ct,
                        (i % 2) * width / 2,
                        (i / 2) * height / 2,
                        width/2,
                        height/2,
                        AsyncWritePixels
                        ),
                    ct,
                    TaskCreationOptions.LongRunning,
                    defaultTaskScheduler
                    );

            var task0 = taskCreator (0);
            var task1 = taskCreator (1);
            var task2 = taskCreator (2);
            var task3 = taskCreator (3);

            var result0 = await task0;
            var result1 = await task1;
            var result2 = await task2;
            var result3 = await task3;

            return
                    result0
                &&  result1
                &&  result2
                &&  result3
                ;
        }
    }
}
