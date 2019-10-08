/* ttShipmentReportDetail.i */

/* Shipment Report.rpa */
DEFINE TEMP-TABLE ttShipmentReportDetail NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD itemNo     AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD itemDscr   AS CHARACTER LABEL "FG Description" FORMAT "X(30)"
    FIELD custPart   AS CHARACTER LABEL "Customer Part"  FORMAT "X(15)"
    FIELD invDate    AS DATE      LABEL "Inv Date"       FORMAT "99/99/9999"
    FIELD invNo      AS INTEGER   LABEL "Invoice"        FORMAT ">>>>>9"
    FIELD jobNo      AS CHARACTER LABEL "Job"            FORMAT "X(10)"
    FIELD skids      AS INTEGER   LABEL "Skids"          FORMAT "->>,>>>,>>9"
    FIELD weight100  AS DECIMAL   LABEL "Weight/100"     FORMAT "->>>,>>9.99"
    FIELD frtCharge  AS DECIMAL   LABEL "Freight Charge" FORMAT "->>,>>>,>>9.99"
    FIELD qtyShipped AS INTEGER   LABEL "Qty Shipped"    FORMAT "->,>>>,>>>,>>9"
        INDEX ttShipmentReportDetail IS PRIMARY itemNo invDate invNo
        .
