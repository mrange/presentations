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
using System.ComponentModel;
using System.Text;

namespace ViewModel
{
    public enum OrderStatus
    {
        Created         ,
        InvoiceSent     ,
        PartiallyPaid   ,
        FullyPaid       ,
        Provisioned     ,
        Return          ,
        Closed          ,
    }

    public enum OrderRowStatus
    {
        Created         ,
        Picked          ,
        Packed          ,
        OutOfStock      ,
        Sent            ,
        Return          ,
    }

    public interface IPrettyPrintable
    {
        void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent);
    }

    public static partial class PrettyPrint
    {
        const string NullLiteral = "<NULL>";
        static void Indent (StringBuilder sb, int indent)
        {
            sb.Append (' ', indent * 2);
        }

        static void Prelude (StringBuilder sb, int indent, string name)
        {
            Indent (sb, indent);
            sb.Append (name);
            sb.Append (" = ");
            sb.AppendLine ();
        }

        public static void PrintLiteral (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string value)
        {
            Indent (sb, indent);
            sb.Append (value ?? NullLiteral);
            sb.AppendLine ();
        }


        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string value)
        {
            Indent (sb, indent);
            if (value != null)
            {
                sb.Append ('"');
                sb.Append (value);
                sb.Append ('"');
            }
            else
            {
                sb.Append (NullLiteral);
            }
            sb.AppendLine ();
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, bool value)
        {
            Indent (sb, indent);
            sb.Append (value ? "true" : "false");
            sb.AppendLine ();
        }

        /*
        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, object value)
        {
            var prettyPrintable = value as IPrettyPrintable;
            if (prettyPrintable != null)
            {
                PrintValue (sb, indent + 1, prettyPrintable);
            }
            else
            {
                Prelude (sb, indent, name);
                sb.Append (value ?? NullLiteral);
                sb.AppendLine ();
            }
        }
        */

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, IPrettyPrintable value)
        {
            if (value != null)
            { 
                if (seen.Contains (value))
                {
                    PrintValue (sb, seen, indent, "<PrintedEarlier>");
                }
                else
                {
                    seen.Add (value);
                    value.PrintValue (sb, seen, indent);
                    seen.Remove (value);
                }
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }

        public static string Print (IPrettyPrintable prettyPrintable)
        {
            var sb      = new StringBuilder ();
            var seen    = new HashSet<IPrettyPrintable> ();
            PrintValue (sb, seen, 0, prettyPrintable);
            return sb.ToString ();
        }
    }

    public abstract class BaseViewModel : INotifyPropertyChanged, IPrettyPrintable
    {
        public sealed override string ToString ()
        {
            return PrettyPrint.Print (this); 
        }

        public event PropertyChangedEventHandler PropertyChanged;

        protected void OnPropertyChanged (string name)
        {
            var pc = PropertyChanged;
            if (pc != null)
            {
                pc (this, new PropertyChangedEventArgs (name));
            }
        }

        protected abstract void OnPrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent);

        void IPrettyPrintable.PrintValue(StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent)
        {
            OnPrintValue (sb, seen, indent);
        }
    }

    public static class Common
    {
        public static Customer CreateTestCustomer ()
        {
            var article = new Article
                {
                    Id              = 123L                  ,
                    Created         = DateTime.Now          ,
                    Name            = "Coffe"               ,
                    Description     = "Coffe"               ,
                };
            var orderRow = new OrderRow
                {
                    Id              = 213L                  ,
                    Created         = DateTime.Now          ,
                    Article         = article               ,
                    Name            = article.Name          ,
                    Description     = article.Description   ,
                    TotalAmount     = 100M                  ,
                    VatAmount       = 20M                   ,
                    Quantity        = 2M                    ,
                };
            var order = new Order
                {
                    Id              = 321L                  ,
                    Created         = DateTime.Now          ,
                    Description     = ""                    ,
                    IsRefund        = false                 ,
                    Status          = OrderStatus.Created   ,
                    Rows            = new [] { orderRow }   ,
                };
            var customer = new Customer 
                {
                    Id          = 007L                  ,
                    Created     = DateTime.Now          ,
                    FirstName   = "Swe"                 ,
                    LastName    = "Tugg"                ,
                    BirthDate   = DateTime.Now          ,
                    Orders      = new [] { order }      ,
                };

            return customer;
        }
    }
}
