/* ttShipmentReportSummary.i */

/* Shipment Report.rpa */
DEFINE TEMP-TABLE ttShipmentReportSummary NO-UNDO 
    {aoa/tempTable/ttFields.i}
    FIELD itemNo     AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD itemDscr   AS CHARACTER LABEL "FG Description" FORMAT "X(30)"
    FIELD custPart   AS CHARACTER LABEL "Customer Part"  FORMAT "X(15)"
    FIELD qtyYear    AS INTEGER   LABEL "Year"           FORMAT "9999"
    FIELD qtyMonth   AS INTEGER   LABEL "Month"          FORMAT "99"
    FIELD qtyShipped AS INTEGER   LABEL "Qty Shipped"    FORMAT "->,>>>,>>>,>>9" 
        INDEX ttShipmentReportSummary IS PRIMARY itemNo qtyYear DESCENDING qtyMonth DESCENDING
        .
