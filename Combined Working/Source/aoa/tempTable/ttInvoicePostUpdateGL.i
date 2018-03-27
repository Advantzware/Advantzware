/* ttInvoicePostUpdateGL.i */

/* Invoice Post Update GL.rpa */
DEFINE TEMP-TABLE ttInvoicePostUpdateGL
    {aoa/tempTable/ttFields.i}
    FIELD invNo             AS INTEGER   LABEL "Invoice Num"   FORMAT ">9"     
    FIELD invDate           AS DATE      LABEL "Invoice Dt"    FORMAT "99/99/99"
    FIELD custNo            AS CHARACTER LABEL "Cust Num"      FORMAT "x(8)"
    FIELD custName          AS CHARACTER LABEL "Cust Name"     FORMAT "x(25)"
    FIELD orderNumber       AS INTEGER   LABEL "Order Number"  FORMAT "9"
    FIELD invoiceQty        AS INTEGER   LABEL "Invoice Qty"   FORMAT "9"
    FIELD totInvoicefreight AS DECIMAL   LABEL "Total Freight" FORMAT "9.9"
    FIELD totInvoiceTax     AS DECIMAL   LABEL "Total Tax"     FORMAT "9.9"
    FIELD miscTot           AS DECIMAL   LABEL "Misc Total"    FORMAT "9.9"
    FIELD lineTot           AS DECIMAL   LABEL "Line Total"    FORMAT "9.9"
    FIELD iInvRev           AS INTEGER   LABEL "Invoice Rev"   FORMAT "99"
    FIELD weightPerTon      AS DECIMAL   LABEL "Weight"        FORMAT "9.9"
    FIELD pricePerTon       AS DECIMAL   LABEL "Ton Price"     FORMAT "9.9"
    FIELD iNo               AS CHARACTER LABEL "Item"          FORMAT "x(16)"
    FIELD iName             AS CHARACTER LABEL "Description"   FORMAT "x(25)"
    FIELD qty               AS INTEGER   LABEL "Order"         FORMAT "->>,>>>,>>9"
    FIELD invQty            AS INTEGER   LABEL "Qty Invoiced"  FORMAT "->>,>>>,>>9"
    FIELD shipQty           AS INTEGER   LABEL "Qty Shipped"   FORMAT "->>,>>>,>>9"
    FIELD cost              AS DECIMAL   LABEL "Cost"          FORMAT "->>>,>>9.99<<<<"
    FIELD price             AS DECIMAL   LABEL "Price"         FORMAT "->>>,>>9.99<<<<" DECIMALS 6
    FIELD uom               AS CHARACTER LABEL "UOM"           FORMAT "x(3)"
    FIELD TotPrice          AS DECIMAL   LABEL "Amt Per Ton"   FORMAT "9.9"
    FIELD profit            AS DECIMAL   LABEL "Profit"        FORMAT "->>>9.99%"
    .
/* Invoice Post Update GL.rpa */
