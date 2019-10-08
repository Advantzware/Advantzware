/* ttBOLPackingList.i */

/* BOL Packing List.rpa */
DEFINE TEMP-TABLE ttBOLPackingList NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD bolNo      AS INTEGER   LABEL "BOL No"       FORMAT ">>>>>>>9"
    FIELD shipDate   AS DATE      LABEL "Shipped"      FORMAT "99/99/9999"
    FIELD pickTicket AS CHARACTER LABEL "Pick Ticket"  FORMAT "x(8)"
    FIELD custName   AS CHARACTER LABEL "Customer"     FORMAT "x(30)"
    FIELD custNo     AS CHARACTER LABEL "Cust No"      FORMAT "x(8)"
    FIELD itemNo     AS CHARACTER LABEL "Item ID"      FORMAT "x(15)"
    FIELD poNo       AS INTEGER   LABEL "PO No"        FORMAT ">>>>>9"
    FIELD relNo      AS INTEGER   LABEL "Release No"   FORMAT ">>>>>9"
    FIELD orderNo    AS INTEGER   LABEL "Order No"     FORMAT ">>>>>9"
    FIELD jobNo      AS CHARACTER LABEL "Job No"       FORMAT "x(6)"
    FIELD ticket     AS CHARACTER LABEL "Ticket"       FORMAT "x(8)"
    FIELD tagDate    AS DATE      LABEL "Prod Date"    FORMAT "99/99/9999"
    FIELD qtyCase    AS INTEGER   LABEL "Cases"        FORMAT "->>>,>>9"
    FIELD caseBundle AS INTEGER   LABEL "Cartons/Case" FORMAT ">>>,>>9"
    FIELD partial    AS DECIMAL   LABEL "Partials"     FORMAT ">>>,>>9"
    FIELD weight     AS INTEGER   LABEL "Wgt/Case"     FORMAT ">>>>9"
    FIELD prntr      AS CHARACTER LABEL "Printer"      FORMAT "x(8)"
    FIELD xxSort     AS CHARACTER LABEL "Sort"         FORMAT "x(50)"
        INDEX sortBy IS PRIMARY rowType xxSort
        .
