/* ttOrdersBookedByOrderNo.i */

DEFINE TEMP-TABLE ttOrdersBookedByOrderNo NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD orderNo      AS INTEGER   LABEL "OrderNo"        FORMAT ">>>>>>"  
    FIELD estNo        AS CHARACTER LABEL "EstNo"          FORMAT "X(8)"
    FIELD jobNo        AS CHARACTER LABEL "JobNo"          FORMAT "X(9)"
    FIELD orddate      AS DATE      LABEL "Date"           FORMAT 99/99/9999
    FIELD custNo       AS CHARACTER LABEL "CustNo"         FORMAT "X(8)"
    FIELD custName     AS CHARACTER LABEL "Customer Name"  FORMAT "X(30)"
    FIELD fgItem       AS CHARACTER LABEL "Item"           FORMAT "X(15)"
    FIELD fgItemName   AS CHARACTER LABEL "Description"    FORMAT "X(30)"
    FIELD fgOrderQty   AS INTEGER   LABEL "FG Order Qty"   FORMAT "->>>,>>>,>>9"
    FIELD fgCost       AS DECIMAL   LABEL "FG Cost"        FORMAT "->>>,>>>,>>9.99"
    FIELD price        AS DECIMAL   LABEL "Price"          FORMAT ">>,>>>,>>9.99"
    FIELD uom          AS CHARACTER LABEL "UOM"            FORMAT "X(3)"
    FIELD extPrice     AS DECIMAL   LABEL "Ext Price"      FORMAT "->,>>>,>>9.99"
    FIELD fgItemProfit AS DECIMAL   LABEL "FG Item Profit" FORMAT "->,>>>,>>9.99"
    FIELD poMsf        AS DECIMAL   LABEL "PO MSF"         FORMAT ">>>9.99"
    FIELD fgShipped    AS INTEGER   LABEL "FG Shipped"     FORMAT "->>,>>>,>>9"
    FIELD poProfit     AS DECIMAL   LABEL "PO Profit"      FORMAT ">,>>>,>>9.99"
    FIELD poNo         AS INTEGER   LABEL "PO No"          FORMAT ">>>>>>>>9"
    FIELD poQty        AS INTEGER   LABEL "PO Qty"         FORMAT "->>,>>>,>>9"
    FIELD poCost       AS DECIMAL   LABEL "PO Cost"        FORMAT ">>>>,>>9.99"
    FIELD poTotalCost  AS DECIMAL   LABEL "PO Total Cost"  FORMAT "->,>>>,>>9.99"
    FIELD poReceived   AS INTEGER   LABEL "PO Received"    FORMAT "->>,>>>,>>9"
    FIELD orderProfit  AS DECIMAL   LABEL "Order Profit"   FORMAT "->>>>>,>>>99%"
    FIELD msfReceived  AS DECIMAL   LABEL "MSF Recvd"      FORMAT "->,>>9.99"
    FIELD fgShipDate   AS DATE      LABEL "FG Ship Date"   FORMAT 99/99/9999
    FIELD poRecDate    AS DATE      LABEL "PO Rec Date"    FORMAT 99/99/9999
    FIELD fgExtPrice   AS DECIMAL   LABEL "FG Ext Price"   FORMAT "->>>>,>>9.99"
    FIELD poRecCost    AS DECIMAL   LABEL "PO Rec Cost"    FORMAT  "->>>,>>9.99"
    FIELD profSold     AS DECIMAL   LABEL "ProfitSold"     FORMAT "->>>>>,>>9.99"
    FIELD profSoldp    AS DECIMAL   LABEL "ProfitSoldPct"  FORMAT "->>>>>,>>>99%"
    FIELD unitBoard    AS INTEGER   LABEL "Units Board"    FORMAT "->>>>>>,>>9"
    FIELD unitWaste    AS DECIMAL   LABEL "Unit Waste"     FORMAT "->>>>,>>9.99"
    FIELD lossp        AS DECIMAL   LABEL "Loss"           FORMAT ">>9.99%"
    FIELD bolNo        AS INTEGER   LABEL "BOL No"         FORMAT ">>>>>>>>"
    FIELD invoiceNo    AS INTEGER   LABEL "Invoice No"     FORMAT ">>>>>>"
    .
