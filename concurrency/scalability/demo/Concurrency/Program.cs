// ----------------------------------------------------------------------------------------------
// Copyright (c) WCOM AB.
// ----------------------------------------------------------------------------------------------
// This source code is subject to terms and conditions of the Microsoft Public License. A 
// copy of the license can be found in the License.html file at the root of this distribution. 
// If you cannot locate the  Microsoft Public License, please send an email to 
// dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
//  by the terms of the Microsoft Public License.
// ----------------------------------------------------------------------------------------------
// You must not remove this notice, or any other, from this software.
// ----------------------------------------------------------------------------------------------

// ReSharper disable InconsistentNaming

using System;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Concurrency
{

    class PerformanceTest
    {
        const int OuterLoop = 2000  ;
        const int InnerLoop = 50000 ;
        const int Modulus   = 20    ;

        public void PlainSequentialSum()
        {
            var result = 0L;
            const long count = OuterLoop * InnerLoop;

            for (var iter = 0L; iter < count; ++iter)
            {
                ++result;
            }

            Console.WriteLine ("Result: {0}", result);
        }

        public void NaiveParallelSum()
        {
            var result = 0L;
            const long count = OuterLoop * InnerLoop;

            Parallel.For(
                0, 
                count,
                i => ++result
                );

            Console.WriteLine ("Result: {0}", result);
        }

        public void SlowParallelSum()
        {
            var result = 0L;
            const long count = OuterLoop * InnerLoop;

            Parallel.For(
                0, 
                count,
                i => Interlocked.Increment(ref result)
                );

            Console.WriteLine ("Result: {0}", result);
        }

        public void PlainSequentialRandom()
        {
            var ints = new int[InnerLoop];

            var random = new Random();

            for (var outer = 0; outer < OuterLoop; ++outer)
            {
                for (var inner = 0; inner < InnerLoop; ++inner)
                {
                    ints[inner] = random.Next();
                }
            }
        }

        public void NaiveParallelRandom()
        {
            var ints = new int[InnerLoop];

            var random = new Random();


            for (var outer = 0; outer < OuterLoop; ++outer)
            {
                Parallel.For(
                    0, 
                    InnerLoop,
                    i => ints[i] = random.Next()
                    );
            }
        }
        
        public void TrivialParallelRandom()
        {
            var ints = new int[InnerLoop];

            for (var outer = 0; outer < OuterLoop; ++outer)
            {
                Parallel.For(
                    0, 
                    InnerLoop,
                    () => new Random(),
                    (i, pls, random) => {ints[i] = random.Next(); return random;},
                    random => {}
                    );
            }
        }

        public void BetterParallelRandom()
        {
            var ints = new int[InnerLoop];

            for (var outer = 0; outer < OuterLoop; ++outer)
            {
                Parallel.For(
                    0, 
                    InnerLoop / Modulus,
                    () => new Random(),
                    (i, pls, random) =>
                        {
                            var begin   = i * Modulus       ;
                            var end     = begin + Modulus   ;
                            for (var iter = begin; iter < end; ++iter)
                            {
                                ints[iter] = random.Next(); 
                            }
                            return random;
                        },
                    random => {}
                    );
            }
        }

        sealed class Counter
        {
            int m_counter = OuterLoop;

            public bool CountDown ()
            {
                var counter = Interlocked.Decrement (ref m_counter);
                return counter > 0;
            }
        }

        public void IndependentRandomize ()
        {
            var counter = new Counter ();

            var tasks = Enumerable
                .Range (0, 8)
                .Select (i => Task.Factory.StartNew (
                    () =>
                    {
                        var ints = new int[InnerLoop];
                        var random = new Random();

                        while (counter.CountDown ())
                        {
                            for (var inner = 0; inner < InnerLoop; ++inner)
                            {
                                ints[inner] = random.Next();
                            }
                        }
                    },
                    TaskCreationOptions.LongRunning))
                .ToArray ()
                ;

            Task.WaitAll (tasks);
        }

    }

    class Program
    {
        static void TimeIt (string name, Action action)
        {
            var stopwatch = new Stopwatch ();
            stopwatch.Start ();

            try
            {
                action ();
                stopwatch.Stop ();
            }
            catch (Exception exc)
            {
                stopwatch.Stop ();
                Console.WriteLine("Caught: {0}", exc.Message);
            }

            Console.WriteLine ("Executing {0} took {1:0,000} ms", name, stopwatch.ElapsedMilliseconds);
        }

        static void Main(string[] args)
        {
            Console.BackgroundColor = ConsoleColor.White;
            Console.ForegroundColor = ConsoleColor.Black;

            Console.Clear();

            //Process.GetCurrentProcess().ProcessorAffinity = new IntPtr(1);

            var per = new PerformanceTest();

            //TimeIt("Plain sequential sum"           , per.PlainSequentialSum    );
            //TimeIt("Naive parallel sum"             , per.NaiveParallelSum     );
            //TimeIt("Slow parallel sum"              , per.SlowParallelSum       );

            TimeIt("Plain sequential randomize"     , per.PlainSequentialRandom );
            TimeIt("Naive parallel randomize"       , per.NaiveParallelRandom   );

            //TimeIt("Trivial parallel randomize"     , per.TrivialParallelRandom );
            //TimeIt("Better parallel randomize"      , per.BetterParallelRandom  );
            //TimeIt("Independent randomize"          , per.IndependentRandomize   );
        }
    }
}
