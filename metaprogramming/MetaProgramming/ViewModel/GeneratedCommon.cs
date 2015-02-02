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

// ############################################################################
// #                                                                          #
// #        ---==>  T H I S  F I L E  I S   G E N E R A T E D  <==---         #
// #                                                                          #
// # This means that any edits to the .cs file will be lost when its          #
// # regenerated. Changes should instead be applied to the corresponding      #
// # template file (.tt)                                                      #
// ############################################################################







using System;
using System.Collections.Generic;
using System.Text;

namespace ViewModel
{
    public static partial class PrettyPrint
    {
        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, IPrettyPrintable value)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, value);
        }

        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, IPrettyPrintable[] values)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, values);
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, IPrettyPrintable[] values)
        {
            if (values != null)
            {
                Indent (sb, indent);
                sb.Append ("[");
                sb.AppendLine ();

                foreach (var value in values)
                {
                    PrintValue 
                    (sb, seen, indent + 1, value);
                }

                Indent (sb, indent);
                sb.Append ("]");
                sb.AppendLine ();
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }
        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, string value)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, value);
        }

        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, string[] values)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, values);
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string[] values)
        {
            if (values != null)
            {
                Indent (sb, indent);
                sb.Append ("[");
                sb.AppendLine ();

                foreach (var value in values)
                {
                    PrintValue 
                    (sb, seen, indent + 1, value);
                }

                Indent (sb, indent);
                sb.Append ("]");
                sb.AppendLine ();
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }
        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, bool value)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, value);
        }

        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, bool[] values)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, values);
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, bool[] values)
        {
            if (values != null)
            {
                Indent (sb, indent);
                sb.Append ("[");
                sb.AppendLine ();

                foreach (var value in values)
                {
                    PrintValue 
                    (sb, seen, indent + 1, value);
                }

                Indent (sb, indent);
                sb.Append ("]");
                sb.AppendLine ();
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }
        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, OrderStatus value)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, value);
        }

        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, OrderStatus[] values)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, values);
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, OrderStatus[] values)
        {
            if (values != null)
            {
                Indent (sb, indent);
                sb.Append ("[");
                sb.AppendLine ();

                foreach (var value in values)
                {
                    PrintValue 
                    (sb, seen, indent + 1, value);
                }

                Indent (sb, indent);
                sb.Append ("]");
                sb.AppendLine ();
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }
        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, long value)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, value);
        }

        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, long[] values)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, values);
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, long[] values)
        {
            if (values != null)
            {
                Indent (sb, indent);
                sb.Append ("[");
                sb.AppendLine ();

                foreach (var value in values)
                {
                    PrintValue 
                    (sb, seen, indent + 1, value);
                }

                Indent (sb, indent);
                sb.Append ("]");
                sb.AppendLine ();
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }
        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, decimal value)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, value);
        }

        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, decimal[] values)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, values);
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, decimal[] values)
        {
            if (values != null)
            {
                Indent (sb, indent);
                sb.Append ("[");
                sb.AppendLine ();

                foreach (var value in values)
                {
                    PrintValue 
                    (sb, seen, indent + 1, value);
                }

                Indent (sb, indent);
                sb.Append ("]");
                sb.AppendLine ();
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }
        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, DateTime value)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, value);
        }

        public static void PrintNamedValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, string name, DateTime[] values)
        {
            Prelude (sb, indent, name);
            PrintValue (sb, seen, indent + 1, values);
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, DateTime[] values)
        {
            if (values != null)
            {
                Indent (sb, indent);
                sb.Append ("[");
                sb.AppendLine ();

                foreach (var value in values)
                {
                    PrintValue 
                    (sb, seen, indent + 1, value);
                }

                Indent (sb, indent);
                sb.Append ("]");
                sb.AppendLine ();
            }
            else
            {
                PrintValue (sb, seen, indent, NullLiteral);
            }
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, OrderStatus value)
        {
            Indent (sb, indent);
            sb.Append (value);
            sb.AppendLine ();
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, long value)
        {
            Indent (sb, indent);
            sb.Append (value);
            sb.AppendLine ();
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, decimal value)
        {
            Indent (sb, indent);
            sb.Append (value);
            sb.AppendLine ();
        }

        public static void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent, DateTime value)
        {
            Indent (sb, indent);
            sb.Append (value);
            sb.AppendLine ();
        }

    }
}
