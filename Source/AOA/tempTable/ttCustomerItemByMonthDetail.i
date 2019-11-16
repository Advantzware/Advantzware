/* ttCustomerItemByMonthDetail.i */

/* Customer Item By Month.rpa */
DEFINE TEMP-TABLE ttCustomerItemByMonthDetail NO-UNDO 
    {aoa/tempTable/ttFields.i}
    FIELD custNo      AS CHARACTER LABEL "Customer"       FORMAT "X(8)"
    FIELD custName    AS CHARACTER LABEL "Customer Name"  FORMAT "X(30)"
    FIELD itemNo      AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD itemDscr    AS CHARACTER LABEL "FG Description" FORMAT "X(30)"
    FIELD custPart    AS CHARACTER LABEL "Customer Part"  FORMAT "X(15)"
    FIELD invDate     AS DATE      LABEL "Invoice Date"   FORMAT "99/99/9999"
    FIELD invTotal    AS DECIMAL   LABEL "Invoice Total"  FORMAT "->>,>>>,>>9.99"
    FIELD qtyInvoiced AS INTEGER   LABEL "Qty Invoiced"   FORMAT "->,>>>,>>>,>>9"
    FIELD qtyShipped  AS INTEGER   LABEL "Qty Shipped"    FORMAT "->,>>>,>>>,>>9"
        INDEX ttCustomerItemByMonthDetail IS PRIMARY custNo itemNo invDate
        .
