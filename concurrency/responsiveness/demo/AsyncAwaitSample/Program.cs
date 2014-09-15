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
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Windows.Threading;

namespace AsyncAwaitSample
{
    static class Program
    {
        [STAThread]
        static void Main (string[] args)
        {
            //SynchronizationContext.SetSynchronizationContext (new SynchronizationContext ());
            //SynchronizationContext.SetSynchronizationContext (new DispatcherSynchronizationContext ());
            //SynchronizationContext.SetSynchronizationContext (new WindowsFormsSynchronizationContext ());

            TestCase1.Run ();
            //TestCase2.Run ();
            //TestCase3.Run ();
        }

        static class TestCase1 
        {
            public static void Run ()
            {
                Console.WriteLine ("Running TestCase1...");

                var readTask = ReadSomeTextAsync ("SomeText.txt");

                var text = readTask.Result;

                Console.WriteLine ("TestCase1 done");
            }

            static async Task<string> ReadSomeTextAsync (string fileName)
            {    
                using (var sr = new StreamReader (fileName))    
                {        
                    var result = await sr.ReadToEndAsync();        
                    return result;    
                }
            }
        }

        static class TestCase2
        {
            static int m_readingFiles;

            public static void Run ()
            {
                Console.WriteLine ("Running TestCase2...");

                var tasks = 
                    Enumerable
                        .Range (0, 1000)
                        .Select (i => ReadSomeTextAsync ("SomeText.txt"))
                        .ToArray ()
                        ;

                while (!tasks.All (t => t.IsCompleted))
                {
                    Application.DoEvents ();
                    Thread.Sleep (10);
                }


                Console.WriteLine ("m_readingFiles should be 0, but is {0}", m_readingFiles);
                Console.WriteLine ("TestCase2 done");
            }

            static async Task<string> ReadSomeTextAsync (string fileName)
            {    
                ++m_readingFiles;
                var result = await ReadFileAsync (fileName);
                --m_readingFiles;
                return result;    
            }

            static async Task<string> ReadFileAsync (string fileName)
            {
                await Task.Delay(1);
                return "Testing";
            }
        }

        static class TestCase3
        {
            public static void Run ()
            {
                Console.WriteLine ("Running TestCase3...");

                var readTask = ReadSomeTextAsync ("SomeText.txt");

                readTask.WaitForTask ();

                Console.WriteLine ("TestCase3 done");
            }

            static async Task<string> ReadSomeTextAsync (string fileName)
            {    
                TraceThreadId ();
                using (var sr = new StreamReader (fileName))    
                {        
                    var result = await sr.ReadToEndAsync();        
                    TraceThreadId ();
                    return result;    
                }
            }

            static void TraceThreadId()
            {
                Console.WriteLine ("Thread Id: {0}", Thread.CurrentThread.ManagedThreadId);
            }
        }

        static class TestCase4
        {
            static async Task<string[][]> ReadCsvAsync (string fileName)
            {
               var result = new List<string[]> ();
 
                using (var sr = new StreamReader (fileName))    
                {        
                    var header = await sr.ReadLineAsync ();
                    if (header != null)
                    {
                        var headers = header.Split('\t');
                        result.Add (headers);

                        string line;

                        while ((line = await sr.ReadLineAsync ()) != null)
                        {
                            var fields = line.Split('\t');
                            if (fields.Length == headers.Length)
                            {
                                result.Add (fields);
                            }
                        }
                    }

                }

                return result.ToArray ();
            }

            static string[][] ReadCsv (string fileName)
            {
               var result = new List<string[]> ();
 
                using (var sr = new StreamReader (fileName))    
                {        
                    var header = sr.ReadLine ();
                    if (header != null)
                    {
                        var headers = header.Split('\t');
                        result.Add (headers);

                        string line;

                        while ((line = sr.ReadLine ()) != null)
                        {
                            var fields = line.Split('\t');
                            if (fields.Length == headers.Length)
                            {
                                result.Add (fields);
                            }
                        }
                    }

                }

                return result.ToArray ();
            }

        }

        public static void WaitForTask (this Task task)
        {
            while (!task.IsCompleted)
            {
                Application.DoEvents ();
                Thread.Sleep (10);
            }
        }


    }
}
