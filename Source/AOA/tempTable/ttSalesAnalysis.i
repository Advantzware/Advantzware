/* ttSalesAnalysis.i */

/* Sales Analysis.rpa */
DEFINE TEMP-TABLE ttSalesAnalysis NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD cCustNo      AS CHARACTER              LABEL "Customer"
    FIELD cShipTo      AS CHARACTER              LABEL "Ship To"
    FIELD cCity        AS CHARACTER              LABEL "City"           FORMAT "x(15)"
    FIELD cState       AS CHARACTER              LABEL "State"          FORMAT "x(5)"
    FIELD cSname       AS CHARACTER              LABEL "Rep"            FORMAT "x(15)"
    FIELD iInvNo       AS INTEGER                LABEL "Inv#"           FORMAT ">>>>>>>>"
    FIELD iMonth       AS INTEGER                LABEL "Month"          FORMAT ">>>>>"
    FIELD iYear        AS INTEGER                LABEL "Year"           FORMAT "9999"
    FIELD cInvDate     AS CHARACTER              LABEL "Inv Date"       FORMAT "99/99/9999"
    FIELD cFgItem      AS CHARACTER              LABEL "Fg Item#"       FORMAT "x(15)"
    FIELD cProCode     AS CHARACTER              LABEL "Pro Code"       
    FIELD cOrderNo     AS CHARACTER              LABEL "Order #"        FORMAT ">>>>>>>>"
    FIELD iQtyShip     AS INTEGER                LABEL "Qty Shipped"    FORMAT "->>>,>>>,>>9"
    FIELD dUnitPrice   AS DECIMAL                LABEL "Unit Price"     FORMAT "->>>,>>>,>>9.99"
    FIELD cUom         AS CHARACTER              LABEL "UOM"            FORMAT "x(4)"
    FIELD dInvAmt      AS DECIMAL                LABEL "Invoice Amt"    FORMAT "->,>>>,>>>,>>9.99"
    FIELD cBolWhse     AS CHARACTER              LABEL "BOL Whse"       
    FIELD cShipToName  AS CHARACTER              LABEL "Ship To Name"   FORMAT "x(30)"    
    FIELD cShipAdd1    AS CHARACTER              LABEL "Ship Address 1" FORMAT "x(30)"    
    FIELD cShipAdd2    AS CHARACTER              LABEL "Ship Address 2" FORMAT "x(30)"    
    FIELD cShipAdd3    AS CHARACTER              LABEL "Ship Address 3" FORMAT "x(30)"
    FIELD cShipCity    AS CHARACTER              LABEL "Ship To City"   FORMAT "x(15)"
    FIELD cShipState   AS CHARACTER              LABEL "Ship To ST"     FORMAT "x(10)"
    FIELD cShipZip     AS CHARACTER              LABEL "Ship To Zip"    FORMAT "x(12)"
    FIELD cBolDate     AS CHARACTER              LABEL "BOL Date"       FORMAT "x(10)"
    .
