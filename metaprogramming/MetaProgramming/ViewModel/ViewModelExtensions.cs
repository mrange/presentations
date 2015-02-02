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
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ViewModel
{
    partial class Order
    {
        public void RecomputeAmount ()
        {
            if (Rows != null && Rows.Length > 0)
            { 
                var totalAmount = Rows.Sum (r => r.Quantity * r.TotalAmount);
                var vatAmount   = Rows.Sum (r => r.Quantity * r.VatAmount)  ;

                TotalAmount     = totalAmount   ;
                VatAmount       = vatAmount     ;
            }
            else
            {
                TotalAmount     = 0.0M  ;
                VatAmount       = 0.0M  ;
            }
        }

        partial void OnChangedRows(OrderRow[] oldValue, OrderRow[] newValue)
        {
            if (oldValue != null)
            {
                foreach (var row in oldValue)
                {
                    row.ParentOrder = null;
                }
            }
            if (newValue != null)
            {
                foreach (var row in newValue)
                {
                    row.ParentOrder = this;
                }
            }

            RecomputeAmount ();
        }

    }

    partial class OrderRow
    {
        public void RecomputeAmount ()
        {
            var parent = ParentOrder;
            if (parent != null)
            {
                parent.RecomputeAmount ();
            }
        }
        partial void OnChangedQuantity(decimal oldValue, decimal newValue)
        {
            RecomputeAmount ();
        }

        partial void OnChangedTotalAmount(decimal oldValue, decimal newValue)
        {
            RecomputeAmount ();
        }

        partial void OnChangedVatAmount(decimal oldValue, decimal newValue)
        {
            RecomputeAmount ();
        }
    }
}
