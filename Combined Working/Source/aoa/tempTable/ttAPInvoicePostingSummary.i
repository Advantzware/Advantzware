/* ttAPInvoicePostingSummary.i */

/* AP Invoice Posting.rpa */
DEFINE TEMP-TABLE ttAPInvoicePostingSummary NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD glAcct    AS CHARACTER LABEL "GL Account"     FORMAT "x(25)"
    FIELD glDscr    AS CHARACTER LABEL "GL Description" FORMAT "x(45)"
    FIELD poNo      AS INTEGER   LABEL "PO"             FORMAT ">>>>>9"
    FIELD invDate   AS DATE      LABEL "Inv Date"       FORMAT "99/99/9999"
    FIELD vendor    AS CHARACTER LABEL "Vendor"         FORMAT "x(8)"
    FIELD invoice   AS CHARACTER LABEL "Invoice"        FORMAT "x(12)"
    FIELD lineNo    AS DECIMAL   LABEL "Line"           FORMAT ">>9.9"
    FIELD dscr      AS CHARACTER LABEL "Description"    FORMAT "x(35)"
    FIELD qty       AS INTEGER   LABEL "Qty"            FORMAT "->>>,>>>,>>9.9<<<<<"
    FIELD unitPrice AS DECIMAL   LABEL "Unit Price"     FORMAT "->,>>>,>>9.99<<<<"  
    FIELD glAmount  AS DECIMAL   LABEL "Amount"         FORMAT "->,>>>,>>9.99"
    FIELD xxID      AS INTEGER   LABEL "ID"             FORMAT ">>>>>9" INITIAL 999999
    FIELD xxOrder   AS INTEGER   LABEL "Order"          FORMAT ">>>>>9" INITIAL 999999
        INDEX ID IS PRIMARY
              xxID
              xxOrder
    .
