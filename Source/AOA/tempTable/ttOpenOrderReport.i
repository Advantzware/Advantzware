/* ttOpenOrderReport.i */

/* Open Order Report.rpa */ 
DEFINE TEMP-TABLE ttOpenOrderReport NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD custNo       AS CHARACTER LABEL "Cust No"          FORMAT "x(8)"
    FIELD lineDueDate  AS DATE      LABEL "Line Due Dt"      FORMAT 99/99/9999
    FIELD relDueDate   AS DATE      LABEL "Rel Due Dt"       FORMAT 99/99/9999
    FIELD custPartNo   AS CHARACTER LABEL "Cust Part"        FORMAT "x(15)"
    FIELD fgItemName   AS CHARACTER LABEL "Item Description" FORMAT "x(30)"
    FIELD fgItemNo     AS CHARACTER LABEL "FG Item"          FORMAT "x(15)"
    FIELD orderNo      AS INTEGER   LABEL "Order No"         FORMAT ">>>>>>>>"
    FIELD cadNo        AS CHARACTER LABEL "CAD"              FORMAT "x(15)"
    FIELD poNo         AS CHARACTER LABEL "PO No"            FORMAT "x(10)"
    FIELD qtyOrd       AS INTEGER   LABEL "Order Qty"        FORMAT "->,>>>,>>9"
    FIELD qtyOnhand    AS INTEGER   LABEL "Qty Onhand"       FORMAT "->,>>>,>>9"
    FIELD qtyShipped   AS INTEGER   LABEL "Qty Shipped"      FORMAT "->,>>>,>>9"
    FIELD qtyActRel    AS INTEGER   LABEL "Qty ActRel"       FORMAT "->,>>>,>>9"
    FIELD qtyWIP       AS INTEGER   LABEL "Qty WIP"          FORMAT "->,>>>,>>9"
    FIELD qtyAvail     AS INTEGER   LABEL "Qty Avail"        FORMAT "->,>>>,>>9"
    FIELD salesRep     AS CHARACTER LABEL "Rep"              FORMAT "x(3)"
    FIELD unit         AS INTEGER   LABEL "Unit"             FORMAT ">>>>9"
    FIELD pallet       AS INTEGER   LABEL "Pallet"           FORMAT ">>9"
    FIELD palletCount  AS INTEGER   LABEL "Pallet Count"     FORMAT ">>>,>>9"
    FIELD sellPrice    AS DECIMAL   LABEL "Sell Price"       FORMAT "->,>>>,>>9.99"
    FIELD cstatus      AS CHARACTER LABEL "Status"           FORMAT "x(20)"
    FIELD orderValue   AS DECIMAL   LABEL "Order Value"      FORMAT "->>>,>>>,>>9.99"
    FIELD ackDate      AS DATE      LABEL "Ack Date"         FORMAT "99/99/9999"
    FIELD ordStartDate AS DATE      LABEL "Order Start"      FORMAT "99/99/9999"
    FIELD csr          AS CHARACTER LABEL "CSR"              FORMAT "x(8)"
    FIELD jobNo        AS CHARACTER LABEL "Job"              FORMAT "x(10)"
    FIELD die          AS CHARACTER LABEL "Die"              FORMAT "x(15)"
    FIELD style        AS CHARACTER LABEL "Style"            FORMAT "x(6)"
    FIELD dueDate      AS DATE      LABEL "Due Date"         FORMAT "99/99/9999"
    FIELD runEndDate   AS DATE      LABEL "End Run"          FORMAT "99/99/9999"
    FIELD sheeted      AS CHARACTER LABEL "Sheeted"          FORMAT "x(3)"
    FIELD printed      AS CHARACTER LABEL "Printed"          FORMAT "x(3)"
    FIELD dueCut       AS CHARACTER LABEL "Die Cut"          FORMAT "x(3)"
    FIELD glued        AS CHARACTER LABEL "Glued"            FORMAT "x(3)"
    FIELD xxIndex      AS INTEGER   LABEL "Index"            FORMAT ">>>>>>9"
        INDEX ttOpenOrderReport IS PRIMARY rowType
        .
