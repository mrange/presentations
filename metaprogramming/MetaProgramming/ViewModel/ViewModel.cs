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
using System.ComponentModel;
using System.Text;
using System.Collections.Generic;



namespace ViewModel
{
    // ------------------------------------------------------------------------
    // ViewModel Customer
    // ------------------------------------------------------------------------
    public partial class Customer : INotifyPropertyChanged, IPrettyPrintable
    {
        // --------------------------------------------------------------------        
        // Property Id (long)
        // --------------------------------------------------------------------        
        public long Id
        {
            get
            {
                return m_pId;
            }
            set
            {
                if (m_pId != value)
                {
                    var old = m_pId;
                    m_pId = value;
                    OnChangedId (old, m_pId);
                    OnPropertyChanged ("Id");
                }
            }
        }
        // --------------------------------------------------------------------        
        long m_pId;
        // --------------------------------------------------------------------        
        partial void OnChangedId (
            long oldValue, 
            long newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Created (DateTime)
        // --------------------------------------------------------------------        
        public DateTime Created
        {
            get
            {
                return m_pCreated;
            }
            set
            {
                if (m_pCreated != value)
                {
                    var old = m_pCreated;
                    m_pCreated = value;
                    OnChangedCreated (old, m_pCreated);
                    OnPropertyChanged ("Created");
                }
            }
        }
        // --------------------------------------------------------------------        
        DateTime m_pCreated;
        // --------------------------------------------------------------------        
        partial void OnChangedCreated (
            DateTime oldValue, 
            DateTime newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property FirstName (string)
        // --------------------------------------------------------------------        
        public string FirstName
        {
            get
            {
                return m_pFirstName;
            }
            set
            {
                if (m_pFirstName != value)
                {
                    var old = m_pFirstName;
                    m_pFirstName = value;
                    OnChangedFirstName (old, m_pFirstName);
                    OnPropertyChanged ("FirstName");
                }
            }
        }
        // --------------------------------------------------------------------        
        string m_pFirstName;
        // --------------------------------------------------------------------        
        partial void OnChangedFirstName (
            string oldValue, 
            string newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property LastName (string)
        // --------------------------------------------------------------------        
        public string LastName
        {
            get
            {
                return m_pLastName;
            }
            set
            {
                if (m_pLastName != value)
                {
                    var old = m_pLastName;
                    m_pLastName = value;
                    OnChangedLastName (old, m_pLastName);
                    OnPropertyChanged ("LastName");
                }
            }
        }
        // --------------------------------------------------------------------        
        string m_pLastName;
        // --------------------------------------------------------------------        
        partial void OnChangedLastName (
            string oldValue, 
            string newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property BirthDate (DateTime)
        // --------------------------------------------------------------------        
        public DateTime BirthDate
        {
            get
            {
                return m_pBirthDate;
            }
            set
            {
                if (m_pBirthDate != value)
                {
                    var old = m_pBirthDate;
                    m_pBirthDate = value;
                    OnChangedBirthDate (old, m_pBirthDate);
                    OnPropertyChanged ("BirthDate");
                }
            }
        }
        // --------------------------------------------------------------------        
        DateTime m_pBirthDate;
        // --------------------------------------------------------------------        
        partial void OnChangedBirthDate (
            DateTime oldValue, 
            DateTime newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Orders (Order[])
        // --------------------------------------------------------------------        
        public Order[] Orders
        {
            get
            {
                return m_pOrders;
            }
            set
            {
                if (m_pOrders != value)
                {
                    var old = m_pOrders;
                    m_pOrders = value;
                    OnChangedOrders (old, m_pOrders);
                    OnPropertyChanged ("Orders");
                }
            }
        }
        // --------------------------------------------------------------------        
        Order[] m_pOrders;
        // --------------------------------------------------------------------        
        partial void OnChangedOrders (
            Order[] oldValue, 
            Order[] newValue);
        // --------------------------------------------------------------------        

        

        public void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent)
        {
            PrettyPrint.PrintLiteral (sb, seen, indent, "class Customer");
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Id", Id);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Created", Created);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "FirstName", FirstName);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "LastName", LastName);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "BirthDate", BirthDate);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Orders", Orders);
        }


        public override string ToString ()
        {
            return PrettyPrint.Print (this); 
        }

        void OnPropertyChanged (string name)
        {
            var pc = PropertyChanged;
            if (pc != null)
            {
                pc (this, new PropertyChangedEventArgs (name));
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;
    }
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // ViewModel Article
    // ------------------------------------------------------------------------
    public partial class Article : INotifyPropertyChanged, IPrettyPrintable
    {
        // --------------------------------------------------------------------        
        // Property Id (long)
        // --------------------------------------------------------------------        
        public long Id
        {
            get
            {
                return m_pId;
            }
            set
            {
                if (m_pId != value)
                {
                    var old = m_pId;
                    m_pId = value;
                    OnChangedId (old, m_pId);
                    OnPropertyChanged ("Id");
                }
            }
        }
        // --------------------------------------------------------------------        
        long m_pId;
        // --------------------------------------------------------------------        
        partial void OnChangedId (
            long oldValue, 
            long newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Created (DateTime)
        // --------------------------------------------------------------------        
        public DateTime Created
        {
            get
            {
                return m_pCreated;
            }
            set
            {
                if (m_pCreated != value)
                {
                    var old = m_pCreated;
                    m_pCreated = value;
                    OnChangedCreated (old, m_pCreated);
                    OnPropertyChanged ("Created");
                }
            }
        }
        // --------------------------------------------------------------------        
        DateTime m_pCreated;
        // --------------------------------------------------------------------        
        partial void OnChangedCreated (
            DateTime oldValue, 
            DateTime newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Name (string)
        // --------------------------------------------------------------------        
        public string Name
        {
            get
            {
                return m_pName;
            }
            set
            {
                if (m_pName != value)
                {
                    var old = m_pName;
                    m_pName = value;
                    OnChangedName (old, m_pName);
                    OnPropertyChanged ("Name");
                }
            }
        }
        // --------------------------------------------------------------------        
        string m_pName;
        // --------------------------------------------------------------------        
        partial void OnChangedName (
            string oldValue, 
            string newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Description (string)
        // --------------------------------------------------------------------        
        public string Description
        {
            get
            {
                return m_pDescription;
            }
            set
            {
                if (m_pDescription != value)
                {
                    var old = m_pDescription;
                    m_pDescription = value;
                    OnChangedDescription (old, m_pDescription);
                    OnPropertyChanged ("Description");
                }
            }
        }
        // --------------------------------------------------------------------        
        string m_pDescription;
        // --------------------------------------------------------------------        
        partial void OnChangedDescription (
            string oldValue, 
            string newValue);
        // --------------------------------------------------------------------        

        

        public void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent)
        {
            PrettyPrint.PrintLiteral (sb, seen, indent, "class Article");
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Id", Id);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Created", Created);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Name", Name);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Description", Description);
        }


        public override string ToString ()
        {
            return PrettyPrint.Print (this); 
        }

        void OnPropertyChanged (string name)
        {
            var pc = PropertyChanged;
            if (pc != null)
            {
                pc (this, new PropertyChangedEventArgs (name));
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;
    }
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // ViewModel Order
    // ------------------------------------------------------------------------
    public partial class Order : INotifyPropertyChanged, IPrettyPrintable
    {
        // --------------------------------------------------------------------        
        // Property Id (long)
        // --------------------------------------------------------------------        
        public long Id
        {
            get
            {
                return m_pId;
            }
            set
            {
                if (m_pId != value)
                {
                    var old = m_pId;
                    m_pId = value;
                    OnChangedId (old, m_pId);
                    OnPropertyChanged ("Id");
                }
            }
        }
        // --------------------------------------------------------------------        
        long m_pId;
        // --------------------------------------------------------------------        
        partial void OnChangedId (
            long oldValue, 
            long newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Created (DateTime)
        // --------------------------------------------------------------------        
        public DateTime Created
        {
            get
            {
                return m_pCreated;
            }
            set
            {
                if (m_pCreated != value)
                {
                    var old = m_pCreated;
                    m_pCreated = value;
                    OnChangedCreated (old, m_pCreated);
                    OnPropertyChanged ("Created");
                }
            }
        }
        // --------------------------------------------------------------------        
        DateTime m_pCreated;
        // --------------------------------------------------------------------        
        partial void OnChangedCreated (
            DateTime oldValue, 
            DateTime newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Description (string)
        // --------------------------------------------------------------------        
        public string Description
        {
            get
            {
                return m_pDescription;
            }
            set
            {
                if (m_pDescription != value)
                {
                    var old = m_pDescription;
                    m_pDescription = value;
                    OnChangedDescription (old, m_pDescription);
                    OnPropertyChanged ("Description");
                }
            }
        }
        // --------------------------------------------------------------------        
        string m_pDescription;
        // --------------------------------------------------------------------        
        partial void OnChangedDescription (
            string oldValue, 
            string newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property IsRefund (bool)
        // --------------------------------------------------------------------        
        public bool IsRefund
        {
            get
            {
                return m_pIsRefund;
            }
            set
            {
                if (m_pIsRefund != value)
                {
                    var old = m_pIsRefund;
                    m_pIsRefund = value;
                    OnChangedIsRefund (old, m_pIsRefund);
                    OnPropertyChanged ("IsRefund");
                }
            }
        }
        // --------------------------------------------------------------------        
        bool m_pIsRefund;
        // --------------------------------------------------------------------        
        partial void OnChangedIsRefund (
            bool oldValue, 
            bool newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Status (OrderStatus)
        // --------------------------------------------------------------------        
        public OrderStatus Status
        {
            get
            {
                return m_pStatus;
            }
            set
            {
                if (m_pStatus != value)
                {
                    var old = m_pStatus;
                    m_pStatus = value;
                    OnChangedStatus (old, m_pStatus);
                    OnPropertyChanged ("Status");
                }
            }
        }
        // --------------------------------------------------------------------        
        OrderStatus m_pStatus;
        // --------------------------------------------------------------------        
        partial void OnChangedStatus (
            OrderStatus oldValue, 
            OrderStatus newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property TotalAmount (decimal)
        // --------------------------------------------------------------------        
        public decimal TotalAmount
        {
            get
            {
                return m_pTotalAmount;
            }
            set
            {
                if (m_pTotalAmount != value)
                {
                    var old = m_pTotalAmount;
                    m_pTotalAmount = value;
                    OnChangedTotalAmount (old, m_pTotalAmount);
                    OnPropertyChanged ("TotalAmount");
                }
            }
        }
        // --------------------------------------------------------------------        
        decimal m_pTotalAmount;
        // --------------------------------------------------------------------        
        partial void OnChangedTotalAmount (
            decimal oldValue, 
            decimal newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property VatAmount (decimal)
        // --------------------------------------------------------------------        
        public decimal VatAmount
        {
            get
            {
                return m_pVatAmount;
            }
            set
            {
                if (m_pVatAmount != value)
                {
                    var old = m_pVatAmount;
                    m_pVatAmount = value;
                    OnChangedVatAmount (old, m_pVatAmount);
                    OnPropertyChanged ("VatAmount");
                }
            }
        }
        // --------------------------------------------------------------------        
        decimal m_pVatAmount;
        // --------------------------------------------------------------------        
        partial void OnChangedVatAmount (
            decimal oldValue, 
            decimal newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Rows (OrderRow[])
        // --------------------------------------------------------------------        
        public OrderRow[] Rows
        {
            get
            {
                return m_pRows;
            }
            set
            {
                if (m_pRows != value)
                {
                    var old = m_pRows;
                    m_pRows = value;
                    OnChangedRows (old, m_pRows);
                    OnPropertyChanged ("Rows");
                }
            }
        }
        // --------------------------------------------------------------------        
        OrderRow[] m_pRows;
        // --------------------------------------------------------------------        
        partial void OnChangedRows (
            OrderRow[] oldValue, 
            OrderRow[] newValue);
        // --------------------------------------------------------------------        

        

        public void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent)
        {
            PrettyPrint.PrintLiteral (sb, seen, indent, "class Order");
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Id", Id);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Created", Created);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Description", Description);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "IsRefund", IsRefund);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Status", Status);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "TotalAmount", TotalAmount);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "VatAmount", VatAmount);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Rows", Rows);
        }


        public override string ToString ()
        {
            return PrettyPrint.Print (this); 
        }

        void OnPropertyChanged (string name)
        {
            var pc = PropertyChanged;
            if (pc != null)
            {
                pc (this, new PropertyChangedEventArgs (name));
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;
    }
    // ------------------------------------------------------------------------

    // ------------------------------------------------------------------------
    // ViewModel OrderRow
    // ------------------------------------------------------------------------
    public partial class OrderRow : INotifyPropertyChanged, IPrettyPrintable
    {
        // --------------------------------------------------------------------        
        // Property Id (long)
        // --------------------------------------------------------------------        
        public long Id
        {
            get
            {
                return m_pId;
            }
            set
            {
                if (m_pId != value)
                {
                    var old = m_pId;
                    m_pId = value;
                    OnChangedId (old, m_pId);
                    OnPropertyChanged ("Id");
                }
            }
        }
        // --------------------------------------------------------------------        
        long m_pId;
        // --------------------------------------------------------------------        
        partial void OnChangedId (
            long oldValue, 
            long newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Created (DateTime)
        // --------------------------------------------------------------------        
        public DateTime Created
        {
            get
            {
                return m_pCreated;
            }
            set
            {
                if (m_pCreated != value)
                {
                    var old = m_pCreated;
                    m_pCreated = value;
                    OnChangedCreated (old, m_pCreated);
                    OnPropertyChanged ("Created");
                }
            }
        }
        // --------------------------------------------------------------------        
        DateTime m_pCreated;
        // --------------------------------------------------------------------        
        partial void OnChangedCreated (
            DateTime oldValue, 
            DateTime newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Article (Article)
        // --------------------------------------------------------------------        
        public Article Article
        {
            get
            {
                return m_pArticle;
            }
            set
            {
                if (m_pArticle != value)
                {
                    var old = m_pArticle;
                    m_pArticle = value;
                    OnChangedArticle (old, m_pArticle);
                    OnPropertyChanged ("Article");
                }
            }
        }
        // --------------------------------------------------------------------        
        Article m_pArticle;
        // --------------------------------------------------------------------        
        partial void OnChangedArticle (
            Article oldValue, 
            Article newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property ParentOrder (Order)
        // --------------------------------------------------------------------        
        public Order ParentOrder
        {
            get
            {
                return m_pParentOrder;
            }
            set
            {
                if (m_pParentOrder != value)
                {
                    var old = m_pParentOrder;
                    m_pParentOrder = value;
                    OnChangedParentOrder (old, m_pParentOrder);
                    OnPropertyChanged ("ParentOrder");
                }
            }
        }
        // --------------------------------------------------------------------        
        Order m_pParentOrder;
        // --------------------------------------------------------------------        
        partial void OnChangedParentOrder (
            Order oldValue, 
            Order newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Quantity (decimal)
        // --------------------------------------------------------------------        
        public decimal Quantity
        {
            get
            {
                return m_pQuantity;
            }
            set
            {
                if (m_pQuantity != value)
                {
                    var old = m_pQuantity;
                    m_pQuantity = value;
                    OnChangedQuantity (old, m_pQuantity);
                    OnPropertyChanged ("Quantity");
                }
            }
        }
        // --------------------------------------------------------------------        
        decimal m_pQuantity;
        // --------------------------------------------------------------------        
        partial void OnChangedQuantity (
            decimal oldValue, 
            decimal newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Name (string)
        // --------------------------------------------------------------------        
        public string Name
        {
            get
            {
                return m_pName;
            }
            set
            {
                if (m_pName != value)
                {
                    var old = m_pName;
                    m_pName = value;
                    OnChangedName (old, m_pName);
                    OnPropertyChanged ("Name");
                }
            }
        }
        // --------------------------------------------------------------------        
        string m_pName;
        // --------------------------------------------------------------------        
        partial void OnChangedName (
            string oldValue, 
            string newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property Description (string)
        // --------------------------------------------------------------------        
        public string Description
        {
            get
            {
                return m_pDescription;
            }
            set
            {
                if (m_pDescription != value)
                {
                    var old = m_pDescription;
                    m_pDescription = value;
                    OnChangedDescription (old, m_pDescription);
                    OnPropertyChanged ("Description");
                }
            }
        }
        // --------------------------------------------------------------------        
        string m_pDescription;
        // --------------------------------------------------------------------        
        partial void OnChangedDescription (
            string oldValue, 
            string newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property TotalAmount (decimal)
        // --------------------------------------------------------------------        
        public decimal TotalAmount
        {
            get
            {
                return m_pTotalAmount;
            }
            set
            {
                if (m_pTotalAmount != value)
                {
                    var old = m_pTotalAmount;
                    m_pTotalAmount = value;
                    OnChangedTotalAmount (old, m_pTotalAmount);
                    OnPropertyChanged ("TotalAmount");
                }
            }
        }
        // --------------------------------------------------------------------        
        decimal m_pTotalAmount;
        // --------------------------------------------------------------------        
        partial void OnChangedTotalAmount (
            decimal oldValue, 
            decimal newValue);
        // --------------------------------------------------------------------        

        // --------------------------------------------------------------------        
        // Property VatAmount (decimal)
        // --------------------------------------------------------------------        
        public decimal VatAmount
        {
            get
            {
                return m_pVatAmount;
            }
            set
            {
                if (m_pVatAmount != value)
                {
                    var old = m_pVatAmount;
                    m_pVatAmount = value;
                    OnChangedVatAmount (old, m_pVatAmount);
                    OnPropertyChanged ("VatAmount");
                }
            }
        }
        // --------------------------------------------------------------------        
        decimal m_pVatAmount;
        // --------------------------------------------------------------------        
        partial void OnChangedVatAmount (
            decimal oldValue, 
            decimal newValue);
        // --------------------------------------------------------------------        

        

        public void PrintValue (StringBuilder sb, HashSet<IPrettyPrintable> seen, int indent)
        {
            PrettyPrint.PrintLiteral (sb, seen, indent, "class OrderRow");
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Id", Id);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Created", Created);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Article", Article);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "ParentOrder", ParentOrder);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Quantity", Quantity);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Name", Name);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "Description", Description);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "TotalAmount", TotalAmount);
            PrettyPrint.PrintNamedValue (sb, seen, indent + 1, "VatAmount", VatAmount);
        }


        public override string ToString ()
        {
            return PrettyPrint.Print (this); 
        }

        void OnPropertyChanged (string name)
        {
            var pc = PropertyChanged;
            if (pc != null)
            {
                pc (this, new PropertyChangedEventArgs (name));
            }
        }

        public event PropertyChangedEventHandler PropertyChanged;
    }
    // ------------------------------------------------------------------------

}

