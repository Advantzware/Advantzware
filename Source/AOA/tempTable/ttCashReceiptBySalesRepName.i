/* ttCashReceiptBySalesRepName.i */

/* Cash Receipt By SalesRep Name.rpa */
DEFINE TEMP-TABLE ttCashReceiptBySalesRepName NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD salesRep  AS CHARACTER LABEL "Sales Rep"     FORMAT "x(3)"
    FIELD salesName AS CHARACTER LABEL "Sales Name"    FORMAT "x(30)"
    FIELD custNo    AS CHARACTER LABEL "Customer"      FORMAT "x(8)"
    FIELD custName  AS CHARACTER LABEL "Customer Name" FORMAT "x(30)"
    FIELD terms     AS CHARACTER LABEL "Terms"         FORMAT "x(8)"
    FIELD invoiceNo AS INTEGER   LABEL "Invoice"       FORMAT ">>>>>>>>"
    FIELD invDate   AS DATE      LABEL "Inv Date"      FORMAT 99/99/9999
    FIELD chkDate   AS DATE      LABEL "Check Date"    FORMAT 99/99/9999
    FIELD Aging     AS DECIMAL   LABEL "Aging"         FORMAT "$->>,>>9.99" 
    FIELD invAmt    AS DECIMAL   LABEL "Inv Amount"    FORMAT "$->>>,>>>,>>9.99"
    FIELD amtPaid   AS DECIMAL   LABEL "Amt Paid"      FORMAT "$->>>,>>>,>>9.99"
    FIELD discount  AS DECIMAL   LABEL "Discount"      FORMAT "$->>>>,>>9.99"
    FIELD balAftPay AS DECIMAL   LABEL "Bal Aft Pymt"  FORMAT "$->>>,>>>,>>9.99"
    FIELD commAmt   AS DECIMAL   LABEL "Comm Amt"      FORMAT "$->>,>>>,>>9.99"
    FIELD commPct   AS DECIMAL   LABEL "Comm Pct"      FORMAT "->>9.99" 
    FIELD xxSort    AS CHARACTER LABEL "Sort"          FORMAT "x(50)"
    FIELD rec-id    AS RECID
    FIELD row-id    AS ROWID
        INDEX sortBy IS PRIMARY rowType xxSort
        .
