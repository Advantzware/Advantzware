/* ttCommissionCashReceipt.i */

/* Commision Cash Receipt.rpa */
DEFINE TEMP-TABLE ttCommissionCashReceipt NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD salesRep    AS CHARACTER LABEL "Sales Rep"    FORMAT "x(3)"
    FIELD custNo      AS CHARACTER LABEL "Customer"     
    FIELD invoiceNo   AS INTEGER   LABEL "Invoice"      FORMAT ">>>>>9"
    FIELD invDate     AS DATE      LABEL "Inv Date"     FORMAT "99/99/9999"
    FIELD custPart    AS CHARACTER LABEL "Cust Part"    FORMAT "x(15)"
    FIELD orderNo     AS INTEGER   LABEL "Order"        FORMAT ">>>>>9"
    FIELD invQty      AS DECIMAL   LABEL "Quantity"
    FIELD invAmt      AS DECIMAL   LABEL "Inv Amt"      FORMAT "$->>>,>>9.99"
    FIELD cashDate    AS DATE      LABEL "Cash Date"    FORMAT "99/99/9999"
    FIELD delta       AS DECIMAL   LABEL "Delta"        FORMAT "$->>>,>>9.99"
    FIELD grossProfit AS DECIMAL   LABEL "Gross Profit" FORMAT "$->>>,>>9.99"
    FIELD commission  AS DECIMAL   LABEL "Commission"   FORMAT "$->>>,>>9.99"
    FIELD commAmt     AS DECIMAL   LABEL "Comm Amt"     FORMAT "$->>>,>>9.99"
    FIELD basis       AS CHARACTER LABEL "Basis"        FORMAT "x"
    FIELD totalCost   AS DECIMAL   LABEL "Total Cost"
    FIELD xxAmtPaid   AS DECIMAL
    FIELD xxAmtD      AS DECIMAL
    FIELD xxCost      AS DECIMAL
    FIELD xxCommPct   AS DECIMAL
    FIELD rec-id      AS RECID
    FIELD row-id      AS ROWID
        INDEX sortBy IS PRIMARY rowType salesRep custNo invoiceNo invDate
        .
