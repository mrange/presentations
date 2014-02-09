using System;
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

using System.Threading.Tasks;
using System.Windows.Threading;

namespace Responsiveness
{
    public partial class App
    {
        
        App ()
        {
            InitializeComponent ();

            DispatcherUnhandledException += App_DispatcherUnhandledException;

            AppDomain.CurrentDomain.UnhandledException += CurrentDomain_UnhandledException;
        }

        internal static void HandleFaults(Exception e)
        {
            App.Current.Dispatcher.InvokeAsync(() => 
                {
                    var mainWindow = App.Current.MainWindow as MainWindow;
                    if (mainWindow != null)
                    {
                        mainWindow.DisplayError(e);
                    }
                });
        }

        void CurrentDomain_UnhandledException(object sender, UnhandledExceptionEventArgs e)
        {
            HandleFaults(e.ExceptionObject as Exception);
        }

        void App_DispatcherUnhandledException(object sender, DispatcherUnhandledExceptionEventArgs e)
        {
            HandleFaults(e.Exception);
        }

    }
}
