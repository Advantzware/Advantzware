/* ttShipmentReport.i */

/* Shipment Report.rpa */
DEFINE TEMP-TABLE ttShipmentReport NO-UNDO 
    {aoa/tempTable/ttFields.i}
    FIELD xxOrder      AS INTEGER   LABEL "Order"          FORMAT ">>>,>>9"
    FIELD itemNo       AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD itemDscr     AS CHARACTER LABEL "FG Description" FORMAT "X(30)"
    FIELD custPart     AS CHARACTER LABEL "Customer Part"  FORMAT "X(15)"
    FIELD qtyShipped1  AS CHARACTER LABEL "Qty Shipped 1"  FORMAT "X(15)"
    FIELD qtyShipped2  AS CHARACTER LABEL "Qty Shipped 2"  FORMAT "X(15)"
    FIELD qtyShipped3  AS CHARACTER LABEL "Qty Shipped 3"  FORMAT "X(15)"
    FIELD qtyShipped4  AS CHARACTER LABEL "Qty Shipped 4"  FORMAT "X(15)"
    FIELD qtyShipped5  AS CHARACTER LABEL "Qty Shipped 5"  FORMAT "X(15)"
    FIELD qtyShipped6  AS CHARACTER LABEL "Qty Shipped 6"  FORMAT "X(15)"
    FIELD qtyShipped7  AS CHARACTER LABEL "Qty Shipped 7"  FORMAT "X(15)"
    FIELD qtyShipped8  AS CHARACTER LABEL "Qty Shipped 8"  FORMAT "X(15)"
    FIELD qtyShipped9  AS CHARACTER LABEL "Qty Shipped 9"  FORMAT "X(15)"
    FIELD qtyShipped10 AS CHARACTER LABEL "Qty Shipped 10" FORMAT "X(15)"
    FIELD qtyShipped11 AS CHARACTER LABEL "Qty Shipped 11" FORMAT "X(15)"
    FIELD qtyShipped12 AS CHARACTER LABEL "Qty Shipped 12" FORMAT "X(15)"
    FIELD qtyShipped13 AS CHARACTER LABEL "Qty Shipped 13" FORMAT "X(15)"
    FIELD qtyShipped14 AS CHARACTER LABEL "Qty Shipped 14" FORMAT "X(15)"
    FIELD qtyShipped15 AS CHARACTER LABEL "Qty Shipped 15" FORMAT "X(15)"
    FIELD qtyShipped16 AS CHARACTER LABEL "Qty Shipped 16" FORMAT "X(15)"
    FIELD qtyShipped17 AS CHARACTER LABEL "Qty Shipped 17" FORMAT "X(15)"
    FIELD qtyShipped18 AS CHARACTER LABEL "Qty Shipped 18" FORMAT "X(15)"
    FIELD qtyShipped19 AS CHARACTER LABEL "Qty Shipped 19" FORMAT "X(15)"
    FIELD qtyShipped20 AS CHARACTER LABEL "Qty Shipped 20" FORMAT "X(15)"
    FIELD qtyShipped21 AS CHARACTER LABEL "Qty Shipped 21" FORMAT "X(15)"
    FIELD qtyShipped22 AS CHARACTER LABEL "Qty Shipped 22" FORMAT "X(15)"
    FIELD qtyShipped23 AS CHARACTER LABEL "Qty Shipped 23" FORMAT "X(15)"
    FIELD qtyShipped24 AS CHARACTER LABEL "Qty Shipped 24" FORMAT "X(15)"
    FIELD qtyShipped25 AS CHARACTER LABEL "Qty Shipped 25" FORMAT "X(15)"
    FIELD qtyShipped26 AS CHARACTER LABEL "Qty Shipped 26" FORMAT "X(15)"
    FIELD qtyShipped27 AS CHARACTER LABEL "Qty Shipped 27" FORMAT "X(15)"
        INDEX ttShipmentReport IS PRIMARY xxOrder
        .
