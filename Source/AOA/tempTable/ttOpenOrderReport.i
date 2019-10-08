/* ttOpenOrderReport.i */

/* Open Order Report.rpa */ 
DEFINE TEMP-TABLE ttOpenOrderReport NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD custNo      AS CHARACTER LABEL "Cust No"          FORMAT "x(8)"
    FIELD lineDueDate AS DATE      LABEL "Line Due Dt"      FORMAT 99/99/9999
    FIELD relDueDate  AS DATE      LABEL "Rel Due Dt"       FORMAT 99/99/9999
    FIELD custPartNo  AS CHARACTER LABEL "Cust Part"        FORMAT "x(15)"
    FIELD fgItemName  AS CHARACTER LABEL "Item Description" FORMAT "x(30)"
    FIELD fgItemNo    AS CHARACTER LABEL "FG Item"          FORMAT "x(15)"
    FIELD orderNo     AS INTEGER   LABEL "Order No"         FORMAT ">>>>>>"
    FIELD cadNo       AS CHARACTER LABEL "CAD"              FORMAT "x(15)"
    FIELD poNo        AS CHARACTER LABEL "PO No"            FORMAT "x(10)"
    FIELD qtyOrd      AS INTEGER   LABEL "Order Qty"        FORMAT "->,>>>,>>9"
    FIELD qtyOnhand   AS INTEGER   LABEL "Qty Onhand"       FORMAT "->,>>>,>>9"
    FIELD qtyShipped  AS INTEGER   LABEL "Qty Shipped"      FORMAT "->,>>>,>>9"
    FIELD qtyActRel   AS INTEGER   LABEL "Qty ActRel"       FORMAT "->,>>>,>>9"
    FIELD qtyWIP      AS INTEGER   LABEL "Qty WIP"          FORMAT "->,>>>,>>9"
    FIELD qtyAvail    AS INTEGER   LABEL "Qty Avail"        FORMAT "->,>>>,>>9"
    FIELD salesRep    AS CHARACTER LABEL "Rep"              FORMAT "x(3)"
    FIELD unit        AS INTEGER   LABEL "Unit"             FORMAT ">>>>9"
    FIELD pallet      AS INTEGER   LABEL "Pallet"           FORMAT ">>9"
    FIELD palletCount AS INTEGER   LABEL "Pallet Count"     FORMAT ">>>,>>9"
    FIELD sellPrice   AS DECIMAL   LABEL "Sell Price"       FORMAT "->,>>>,>>9.99"
    FIELD cstatus     AS CHARACTER LABEL "Status"           FORMAT "x(20)"
    FIELD xxSort1     AS CHARACTER LABEL "Sort 1"           FORMAT "x(100)"
    FIELD xxSort2     AS CHARACTER LABEL "Sort 2"           FORMAT "x(100)"
    FIELD xxIndex     AS INTEGER   LABEL "Index"            FORMAT ">>>>>>9"
        INDEX ttOpenOrderReport IS PRIMARY rowType xxSort1 xxSort2
        .
