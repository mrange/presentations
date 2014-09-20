
// ############################################################################
// #                                                                          #
// #        ---==>  T H I S  F I L E  I S   G E N E R A T E D  <==---         #
// #                                                                          #
// # This means that any edits to the .cs file will be lost when its          #
// # regenerated. Changes should instead be applied to the corresponding      #
// # text template file (.tt)                                                      #
// ############################################################################



// ############################################################################
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Extensions/BasicExtensions.cs
// @@@ INCLUDE_FOUND: ../Common/Array.cs
// @@@ INCLUDE_FOUND: ../Common/Config.cs
// @@@ INCLUDE_FOUND: ../Common/Log.cs
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Concurrency/TaskSchedulers.cs
// @@@ INCLUDE_FOUND: ../Common/Log.cs
// @@@ INCLUDE_FOUND: ShutDownable.cs
// @@@ INCLUDE_FOUND: RemainingTime.cs
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Common/Array.cs
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Common/Config.cs
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Common/Log.cs
// @@@ INCLUDE_FOUND: Config.cs
// @@@ INCLUDE_FOUND: Generated_Log.cs
// @@@ SKIPPING (Already seen): https://raw.github.com/mrange/T4Include/master/Common/Log.cs
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Concurrency/ShutDownable.cs
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Concurrency/RemainingTime.cs
// @@@ SKIPPING (Already seen): https://raw.github.com/mrange/T4Include/master/Common/Config.cs
// @@@ INCLUDING: https://raw.github.com/mrange/T4Include/master/Common/Generated_Log.cs
// ############################################################################
// Certains directives such as #define and // Resharper comments has to be
// moved to top in order to work properly
// ############################################################################
// ReSharper disable InconsistentNaming
// ReSharper disable PartialMethodWithSinglePart
// ReSharper disable PartialTypeWithSinglePart
// ReSharper disable RedundantNameQualifier
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Extensions/BasicExtensions.cs
namespace Responsiveness
{
    // ----------------------------------------------------------------------------------------------
    // Copyright (c) M�rten R�nge.
    // ----------------------------------------------------------------------------------------------
    // This source code is subject to terms and conditions of the Microsoft Public License. A
    // copy of the license can be found in the License.html file at the root of this distribution.
    // If you cannot locate the  Microsoft Public License, please send an email to
    // dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
    //  by the terms of the Microsoft Public License.
    // ----------------------------------------------------------------------------------------------
    // You must not remove this notice, or any other, from this software.
    // ----------------------------------------------------------------------------------------------



    namespace Source.Extensions
    {
        using System;
        using System.Collections.Generic;
        using System.Globalization;
        using System.IO;
        using System.Reflection;

        using Source.Common;

        static partial class BasicExtensions
        {
            public static bool IsNullOrWhiteSpace (this string v)
            {
                return string.IsNullOrWhiteSpace (v);
            }

            public static bool IsNullOrEmpty (this string v)
            {
                return string.IsNullOrEmpty (v);
            }

            public static T FirstOrReturn<T>(this T[] values, T defaultValue)
            {
                if (values == null)
                {
                    return defaultValue;
                }

                if (values.Length == 0)
                {
                    return defaultValue;
                }

                return values[0];
            }

            public static T FirstOrReturn<T>(this IEnumerable<T> values, T defaultValue)
            {
                if (values == null)
                {
                    return defaultValue;
                }

                foreach (var value in values)
                {
                    return value;
                }

                return defaultValue;
            }

            public static void Shuffle<T>(this T[] values, Random random)
            {
                if (values == null)
                {
                    return;
                }

                if (random == null)
                {
                    return;
                }

                for (var iter = 0; iter < values.Length; ++iter)
                {
                    var swapWith = random.Next (iter, values.Length);

                    var tmp = values[iter];

                    values[iter] = values[swapWith];
                    values[swapWith] = tmp;
                }

            }

            public static string DefaultTo (this string v, string defaultValue = null)
            {
                return !v.IsNullOrEmpty () ? v : (defaultValue ?? "");
            }

            public static IEnumerable<T> DefaultTo<T>(
                this IEnumerable<T> values,
                IEnumerable<T> defaultValue = null
                )
            {
                return values ?? defaultValue ?? Array<T>.Empty;
            }

            public static T[] DefaultTo<T>(this T[] values, T[] defaultValue = null)
            {
                return values ?? defaultValue ?? Array<T>.Empty;
            }

            public static T DefaultTo<T>(this T v, T defaultValue = default (T))
                where T : struct, IEquatable<T>
            {
                return !v.Equals (default (T)) ? v : defaultValue;
            }

            public static string FormatWith (this string format, CultureInfo cultureInfo, params object[] args)
            {
                return string.Format (cultureInfo, format ?? "", args.DefaultTo ());
            }

            public static string FormatWith (this string format, params object[] args)
            {
                return format.FormatWith (Config.DefaultCulture, args);
            }

            public static TValue Lookup<TKey, TValue>(
                this IDictionary<TKey, TValue> dictionary,
                TKey key,
                TValue defaultValue = default (TValue))
            {
                if (dictionary == null)
                {
                    return defaultValue;
                }

                TValue value;
                return dictionary.TryGetValue (key, out value) ? value : defaultValue;
            }

            public static TValue GetOrAdd<TKey, TValue>(
                this IDictionary<TKey, TValue> dictionary,
                TKey key,
                TValue defaultValue = default (TValue))
            {
                if (dictionary == null)
                {
                    return defaultValue;
                }

                TValue value;
                if (!dictionary.TryGetValue (key, out value))
                {
                    value = defaultValue;
                    dictionary[key] = value;
                }

                return value;
            }

            public static TValue GetOrAdd<TKey, TValue>(
                this IDictionary<TKey, TValue> dictionary,
                TKey key,
                Func<TValue> valueCreator
                )
            {
                if (dictionary == null)
                {
                    return valueCreator ();
                }

                TValue value;
                if (!dictionary.TryGetValue (key, out value))
                {
                    value = valueCreator ();
                    dictionary[key] = value;
                }

                return value;
            }

            public static void DisposeNoThrow (this IDisposable disposable)
            {
                try
                {
                    if (disposable != null)
                    {
                        disposable.Dispose ();
                    }
                }
                catch (Exception exc)
                {
                    Log.Exception ("DisposeNoThrow: Dispose threw: {0}", exc);
                }
            }

            public static TTo CastTo<TTo> (this object value, TTo defaultValue)
            {
                return value is TTo ? (TTo) value : defaultValue;
            }

            public static string Concatenate (this IEnumerable<string> values, string delimiter = null, int capacity = 16)
            {
                values = values ?? Array<string>.Empty;
                delimiter = delimiter ?? ", ";

                return string.Join (delimiter, values);
            }

            public static string GetResourceString (this Assembly assembly, string name, string defaultValue = null)
            {
                defaultValue = defaultValue ?? "";

                if (assembly == null)
                {
                    return defaultValue;
                }

                var stream = assembly.GetManifestResourceStream (name ?? "");
                if (stream == null)
                {
                    return defaultValue;
                }

                using (stream)
                using (var streamReader = new StreamReader (stream))
                {
                    return streamReader.ReadToEnd ();
                }
            }

            public static IEnumerable<string> ReadLines (this TextReader textReader)
            {
                if (textReader == null)
                {
                    yield break;
                }

                string line;

                while ((line = textReader.ReadLine ()) != null)
                {
                    yield return line;
                }
            }

    #if !NETFX_CORE
            public static IEnumerable<Type> GetInheritanceChain (this Type type)
            {
                while (type != null)
                {
                    yield return type;
                    type = type.BaseType;
                }
            }
    #endif
        }
    }
}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Extensions/BasicExtensions.cs
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Concurrency/TaskSchedulers.cs
namespace Responsiveness
{
    // ----------------------------------------------------------------------------------------------
    // Copyright (c) M�rten R�nge.
    // ----------------------------------------------------------------------------------------------
    // This source code is subject to terms and conditions of the Microsoft Public License. A
    // copy of the license can be found in the License.html file at the root of this distribution.
    // If you cannot locate the  Microsoft Public License, please send an email to
    // dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
    //  by the terms of the Microsoft Public License.
    // ----------------------------------------------------------------------------------------------
    // You must not remove this notice, or any other, from this software.
    // ----------------------------------------------------------------------------------------------



    namespace Source.Concurrency
    {
        using System;
        using System.Collections.Concurrent;
        using System.Collections.Generic;
        using System.Threading;
        using System.Threading.Tasks;

        using Source.Common;

        sealed partial class SequentialTaskScheduler : TaskScheduler, IShutDownable
        {
            const int                           DefaultTimeOutInMs = 250;
            public readonly string              Name    ;
            public readonly TimeSpan            TimeOut ;

            readonly BlockingCollection<Task>   m_tasks = new BlockingCollection<Task>();
            Thread                              m_executingThread   ;
            bool                                m_done              ;

            int                                 m_taskFailureCount;


            partial void Partial_ThreadCreated (Thread thread);

            partial void Partial_TaskFailed (Task task, Exception exc, int failureCount, ref bool done);

            public SequentialTaskScheduler (string name, TimeSpan? timeOut = null, ApartmentState apartmentState = ApartmentState.Unknown)
            {
                Name                = name      ?? "UnnamedTaskScheduler";
                TimeOut             = timeOut   ?? TimeSpan.FromMilliseconds (DefaultTimeOutInMs);
                m_executingThread   = new Thread (OnRun)
                               {
                                   IsBackground = true
                               };

                m_executingThread.SetApartmentState (apartmentState);

                Partial_ThreadCreated (m_executingThread);

                m_executingThread.Start ();
            }

            void OnRun (object context)
            {
                while (!m_done)
                {
                    Task task;
                    try
                    {
                        if (m_tasks.TryTake (out task, TimeOut))
                        {
                            // null task means exit
                            if (task == null)
                            {
                                m_done = true;
                                continue;
                            }

                            if (!TryExecuteTask (task))
                            {
                                Log.Warning (
                                    "SequentialTaskScheduler.OnRun: {0} - TryExecuteTask failed for task: {1}",
                                    Name,
                                    task.Id
                                    );
                            }
                        }
                    }
                    catch (Exception exc)
                    {
                        ++m_taskFailureCount;

                        Log.Exception (
                            "SequentialTaskScheduler.OnRun: {0} - Caught exception: {1}",
                            Name,
                            exc
                            );

                        Partial_TaskFailed (task, exc, m_taskFailureCount, ref m_done);
                    }
                }
            }

            protected override bool TryDequeue (Task task)
            {
                Log.Warning ("SequentialTaskScheduler.TryDequeue: {0} - Task dequeing not supported", Name);
                return false;
            }

            protected override void QueueTask (Task task)
            {
                m_tasks.Add (task);
            }

            protected override bool TryExecuteTaskInline (Task task, bool taskWasPreviouslyQueued)
            {
                return false;
            }

            protected override IEnumerable<Task> GetScheduledTasks ()
            {
                return m_tasks.ToArray ();
            }

            public int TasksInQueue
            {
                get { return m_tasks.Count; }
            }

            public bool IsDisposed
            {
                get { return m_executingThread == null; }
            }

            public void SignalShutDown ()
            {
                if (!m_done)
                {
                    m_done = true;
                    // null task to wake up thread
                    m_tasks.Add (null);
                }
            }

            public void ShutDown (RemainingTime remainingTime)
            {
                var thread = Interlocked.Exchange (ref m_executingThread, null);
                if (thread != null)
                {
                    try
                    {
                        SignalShutDown ();
                        var joinTimeOut = (int)remainingTime.Remaining.TotalMilliseconds/2;
                        if (!thread.Join (joinTimeOut))
                        {
                            Log.Warning (
                                "SequentialTaskScheduler.Dispose: {0} - Executing thread didn't shutdown, aborting it...",
                                Name
                                );

                            thread.Abort ();
                            var abortTimeOut = remainingTime.Remaining;
                            if (!thread.Join (abortTimeOut))
                            {
                                Log.Warning (
                                    "SequentialTaskScheduler.Dispose: {0} - Executing thread didn't shutdown after abort, ignoring it...",
                                    Name
                                    );
                            }
                        }
                    }
                    catch (Exception exc)
                    {
                        Log.Exception (
                            "SequentialTaskScheduler.Dispose: {0} - Caught exception: {1}",
                            Name,
                            exc
                            );
                    }
                }

            }

            public void Dispose ()
            {
                ShutDown (new RemainingTime (TimeOut));
            }
        }
    }
}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Concurrency/TaskSchedulers.cs
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Array.cs
namespace Responsiveness
{
    // ----------------------------------------------------------------------------------------------
    // Copyright (c) M�rten R�nge.
    // ----------------------------------------------------------------------------------------------
    // This source code is subject to terms and conditions of the Microsoft Public License. A
    // copy of the license can be found in the License.html file at the root of this distribution.
    // If you cannot locate the  Microsoft Public License, please send an email to
    // dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
    //  by the terms of the Microsoft Public License.
    // ----------------------------------------------------------------------------------------------
    // You must not remove this notice, or any other, from this software.
    // ----------------------------------------------------------------------------------------------

    namespace Source.Common
    {
        static class Array<T>
        {
            public static readonly T[] Empty = new T[0];
        }
    }
}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Array.cs
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Config.cs
namespace Responsiveness
{
    // ----------------------------------------------------------------------------------------------
    // Copyright (c) M�rten R�nge.
    // ----------------------------------------------------------------------------------------------
    // This source code is subject to terms and conditions of the Microsoft Public License. A
    // copy of the license can be found in the License.html file at the root of this distribution.
    // If you cannot locate the  Microsoft Public License, please send an email to
    // dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
    //  by the terms of the Microsoft Public License.
    // ----------------------------------------------------------------------------------------------
    // You must not remove this notice, or any other, from this software.
    // ----------------------------------------------------------------------------------------------


    namespace Source.Common
    {
        using System.Globalization;

        sealed partial class InitConfig
        {
            public CultureInfo DefaultCulture = CultureInfo.InvariantCulture;
        }

        static partial class Config
        {
            static partial void Partial_Constructed(ref InitConfig initConfig);

            public readonly static CultureInfo DefaultCulture;

            static Config ()
            {
                var initConfig = new InitConfig();

                Partial_Constructed (ref initConfig);

                initConfig = initConfig ?? new InitConfig();

                DefaultCulture = initConfig.DefaultCulture;
            }
        }
    }
}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Config.cs
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Log.cs
namespace Responsiveness
{
    // ----------------------------------------------------------------------------------------------
    // Copyright (c) M�rten R�nge.
    // ----------------------------------------------------------------------------------------------
    // This source code is subject to terms and conditions of the Microsoft Public License. A
    // copy of the license can be found in the License.html file at the root of this distribution.
    // If you cannot locate the  Microsoft Public License, please send an email to
    // dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
    //  by the terms of the Microsoft Public License.
    // ----------------------------------------------------------------------------------------------
    // You must not remove this notice, or any other, from this software.
    // ----------------------------------------------------------------------------------------------



    namespace Source.Common
    {
        using System;
        using System.Globalization;

        static partial class Log
        {
            static partial void Partial_LogLevel (Level level);
            static partial void Partial_LogMessage (Level level, string message);
            static partial void Partial_ExceptionOnLog (Level level, string format, object[] args, Exception exc);

            public static void LogMessage (Level level, string format, params object[] args)
            {
                try
                {
                    Partial_LogLevel (level);
                    Partial_LogMessage (level, GetMessage (format, args));
                }
                catch (Exception exc)
                {
                    Partial_ExceptionOnLog (level, format, args, exc);
                }

            }

            static string GetMessage (string format, object[] args)
            {
                format = format ?? "";
                try
                {
                    return (args == null || args.Length == 0)
                               ? format
                               : string.Format (Config.DefaultCulture, format, args)
                        ;
                }
                catch (FormatException)
                {

                    return format;
                }
            }
        }
    }
}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Log.cs
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Concurrency/ShutDownable.cs
namespace Responsiveness
{
    // ----------------------------------------------------------------------------------------------
    // Copyright (c) M�rten R�nge.
    // ----------------------------------------------------------------------------------------------
    // This source code is subject to terms and conditions of the Microsoft Public License. A
    // copy of the license can be found in the License.html file at the root of this distribution.
    // If you cannot locate the  Microsoft Public License, please send an email to
    // dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
    //  by the terms of the Microsoft Public License.
    // ----------------------------------------------------------------------------------------------
    // You must not remove this notice, or any other, from this software.
    // ----------------------------------------------------------------------------------------------

    namespace Source.Concurrency
    {
        using System;

        using Source.Common;

        partial interface IShutDownable : IDisposable
        {
            /// <summary>
            /// SignalShutDown - signals the object to shutdown
            /// Shall not Throw
            /// Shall not Block
            /// </summary>
            void SignalShutDown ();

            /// <summary>
            /// ShutDown - waits for the object to shutdown
            /// Should not Throw
            /// May Block
            /// </summary>
            /// <param name="remainingTime"></param>
            void ShutDown (RemainingTime remainingTime);
        }

        static partial class ShutDownable
        {
            public static void ShutDown (RemainingTime remainingTime, params IShutDownable[] shutDownables)
            {
                if (shutDownables == null)
                {
                    return;
                }

                foreach (var shutDownable in shutDownables)
                {
                    if (shutDownable != null)
                    {
                        shutDownable.SignalShutDown ();
                    }
                }

                foreach (var shutDownable in shutDownables)
                {
                    if (shutDownable != null)
                    {
                        try
                        {
                            shutDownable.ShutDown (remainingTime);
                        }
                        catch (Exception exc)
                        {
                            Log.Exception ("Failed to shutdown: {0}", exc);
                        }
                    }
                }

            }

        }
    }
}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Concurrency/ShutDownable.cs
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Concurrency/RemainingTime.cs
namespace Responsiveness
{
    // ----------------------------------------------------------------------------------------------
    // Copyright (c) M�rten R�nge.
    // ----------------------------------------------------------------------------------------------
    // This source code is subject to terms and conditions of the Microsoft Public License. A
    // copy of the license can be found in the License.html file at the root of this distribution.
    // If you cannot locate the  Microsoft Public License, please send an email to
    // dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound
    //  by the terms of the Microsoft Public License.
    // ----------------------------------------------------------------------------------------------
    // You must not remove this notice, or any other, from this software.
    // ----------------------------------------------------------------------------------------------

    namespace Source.Concurrency
    {
        using System;
        using System.Diagnostics;

        partial struct RemainingTime
        {
            public readonly TimeSpan    TimeOut     ;
            readonly        Stopwatch   m_sw        ;

            public RemainingTime (TimeSpan timeOut)
            {
                TimeOut     = timeOut           ;
                m_sw        = new Stopwatch ()  ;

                m_sw.Start ();
            }

            public TimeSpan Remaining
            {
                get
                {
                    var elapsed = m_sw.Elapsed;

                    var remaining = TimeOut - elapsed;

                    if (remaining < TimeSpan.Zero)
                    {
                        return TimeSpan.Zero;
                    }

                    return remaining;
                }
            }

            public bool IsTimedOut
            {
                get
                {
                    return Remaining == TimeSpan.Zero;
                }
            }

        }
    }
}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Concurrency/RemainingTime.cs
// ############################################################################

// ############################################################################
// @@@ BEGIN_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Generated_Log.cs
namespace Responsiveness
{
    // ############################################################################
    // #                                                                          #
    // #        ---==>  T H I S  F I L E  I S   G E N E R A T E D  <==---         #
    // #                                                                          #
    // # This means that any edits to the .cs file will be lost when its          #
    // # regenerated. Changes should instead be applied to the corresponding      #
    // # template file (.tt)                                                      #
    // ############################################################################






    namespace Source.Common
    {
        using System;

        partial class Log
        {
            public enum Level
            {
                Success = 1000,
                HighLight = 2000,
                Info = 3000,
                Warning = 10000,
                Error = 20000,
                Exception = 21000,
            }

            public static void Success (string format, params object[] args)
            {
                LogMessage (Level.Success, format, args);
            }
            public static void HighLight (string format, params object[] args)
            {
                LogMessage (Level.HighLight, format, args);
            }
            public static void Info (string format, params object[] args)
            {
                LogMessage (Level.Info, format, args);
            }
            public static void Warning (string format, params object[] args)
            {
                LogMessage (Level.Warning, format, args);
            }
            public static void Error (string format, params object[] args)
            {
                LogMessage (Level.Error, format, args);
            }
            public static void Exception (string format, params object[] args)
            {
                LogMessage (Level.Exception, format, args);
            }
    #if !NETFX_CORE && !SILVERLIGHT && !WINDOWS_PHONE
            static ConsoleColor GetLevelColor (Level level)
            {
                switch (level)
                {
                    case Level.Success:
                        return ConsoleColor.Green;
                    case Level.HighLight:
                        return ConsoleColor.White;
                    case Level.Info:
                        return ConsoleColor.Gray;
                    case Level.Warning:
                        return ConsoleColor.Yellow;
                    case Level.Error:
                        return ConsoleColor.Red;
                    case Level.Exception:
                        return ConsoleColor.Red;
                    default:
                        return ConsoleColor.Magenta;
                }
            }
    #endif
            static string GetLevelMessage (Level level)
            {
                switch (level)
                {
                    case Level.Success:
                        return "SUCCESS  ";
                    case Level.HighLight:
                        return "HIGHLIGHT";
                    case Level.Info:
                        return "INFO     ";
                    case Level.Warning:
                        return "WARNING  ";
                    case Level.Error:
                        return "ERROR    ";
                    case Level.Exception:
                        return "EXCEPTION";
                    default:
                        return "UNKNOWN  ";
                }
            }

        }
    }

}
// @@@ END_INCLUDE: https://raw.github.com/mrange/T4Include/master/Common/Generated_Log.cs
// ############################################################################

// ############################################################################
namespace Responsiveness.Include
{
    static partial class MetaData
    {
        public const string RootPath        = @"https://raw.github.com/";
        public const string IncludeDate     = @"2014-02-10T21:19:04";

        public const string Include_0       = @"https://raw.github.com/mrange/T4Include/master/Extensions/BasicExtensions.cs";
        public const string Include_1       = @"https://raw.github.com/mrange/T4Include/master/Concurrency/TaskSchedulers.cs";
        public const string Include_2       = @"https://raw.github.com/mrange/T4Include/master/Common/Array.cs";
        public const string Include_3       = @"https://raw.github.com/mrange/T4Include/master/Common/Config.cs";
        public const string Include_4       = @"https://raw.github.com/mrange/T4Include/master/Common/Log.cs";
        public const string Include_5       = @"https://raw.github.com/mrange/T4Include/master/Concurrency/ShutDownable.cs";
        public const string Include_6       = @"https://raw.github.com/mrange/T4Include/master/Concurrency/RemainingTime.cs";
        public const string Include_7       = @"https://raw.github.com/mrange/T4Include/master/Common/Generated_Log.cs";
    }
}
// ############################################################################


