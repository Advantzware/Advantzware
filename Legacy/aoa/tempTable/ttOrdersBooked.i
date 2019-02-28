/* ttOrdersBooked.i */

/* Orders Booked.rpa */
DEFINE TEMP-TABLE ttOrdersBooked NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD salesRep     AS CHARACTER LABEL "Sales Rep"      FORMAT "X(3)"
    FIELD salesRepName AS CHARACTER LABEL "Sales Rep Name" FORMAT "X(30)"
    FIELD dueDate      AS DATE      LABEL "Due Date"       FORMAT 99/99/9999
    FIELD orderNo      AS INTEGER   LABEL "Order No"       FORMAT ">>>>>>>"
    FIELD custNo       AS CHARACTER LABEL "Cust No"        FORMAT "X(8)"
    FIELD custName     AS CHARACTER LABEL "Customer Name"  FORMAT "X(30)"
    FIELD commPer      AS DECIMAL   LABEL "Comm Pct"       FORMAT ">>>>>9.99"
    FIELD prodCode     AS CHARACTER LABEL "Prod Code"      FORMAT "x(8)"
    FIELD qtyOrdEa     AS INTEGER   LABEL "Qty Ordered"    FORMAT ">,>>>,>>9"
    FIELD custPartNo   AS CHARACTER LABEL "Customer Part"  FORMAT "x(15)" 
    FIELD fgItemNo     AS CHARACTER LABEL "FG Item"        FORMAT "X(15)"
    FIELD fgItemName   AS CHARACTER LABEL "FG Item Name"   FORMAT "X(30)"
    FIELD sqFt         AS DECIMAL   LABEL "Sq Ft"          FORMAT ">>,>>>.999999"
    FIELD totalSqft    AS DECIMAL   LABEL "Total SqFt"     FORMAT "->,>>>.999"
    FIELD msfPrice     AS DECIMAL   LABEL "MSF"            FORMAT "->>,>>9.99"
    FIELD price        AS DECIMAL   LABEL "Price"          FORMAT ">,>>>,>>9.99"
    FIELD orderAmount  AS DECIMAL   LABEL "Order Amount"   FORMAT "->,>>>,>>9.99"
    FIELD profitPer    AS DECIMAL   LABEL "Profit"         FORMAT "->,>>>,>>9.9"
    FIELD totalTons    AS DECIMAL   LABEL "Total Tons"     FORMAT "->,>>>.999999"
    FIELD ton          AS DECIMAL   LABEL "Ton"            FORMAT "->>,>>9.99"
    FIELD custPO       AS CHARACTER LABEL "Cust PO"        FORMAT "x(15)"
    FIELD orderDate    AS DATE      LABEL "Order Date"     FORMAT 99/99/9999
    FIELD dieNo        AS CHARACTER LABEL "Die No"         FORMAT "x(15)"
    FIELD vUserID      AS CHARACTER LABEL "User ID"        FORMAT "x(8)"
    FIELD prUOM        AS CHARACTER LABEL "UOM"            FORMAT "x(6)" 
    FIELD machineCode  AS CHARACTER LABEL "Machine"        FORMAT "x(30)"
    FIELD inksCode     AS CHARACTER LABEL "Inks"           FORMAT "x(40)"
    FIELD printSheet   AS CHARACTER LABEL "Prints Sheet"   FORMAT "x(20)"
    FIELD dCstPerM     AS DECIMAL   LABEL "Cost/$m"        FORMAT "->>>,>>>,>>9.99"
    FIELD dTotStdCost  AS DECIMAL   LABEL "Total Std Cost" FORMAT "->>>>>>>9.99"
    FIELD dFullCost    AS DECIMAL   LABEL "Full Cost"      FORMAT "->>>>>>>9.99"
    FIELD cEnterBy     AS CHARACTER LABEL "Entered By"     FORMAT "x(10)"
    FIELD cStatus      AS CHARACTER LABEL "Status"         FORMAT "x(20)"
    FIELD xxCost       AS DECIMAL   LABEL "Cost"           FORMAT "->>,>>9.99"   
    FIELD xxSort       AS CHARACTER LABEL "Sort"           FORMAT "x(100)"
        INDEX ttOrdersBooked IS PRIMARY rowType xxSort
        .
