/* ttFGInventoryStatus.i */

/* FG Inventory Status.rpa */
DEFINE TEMP-TABLE ttFGInventoryStatus NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD cCustNo      AS CHARACTER              LABEL "Customer"
    FIELD cPo          LIKE itemfg.cust-po-no    LABEL "PO #"           FORMAT "x(15)"
    FIELD cSman        LIKE cust.sman            LABEL "Rep"            FORMAT "x(4)"
    FIELD cItemNo      LIKE itemfg.i-no          LABEL "Item #"         FORMAT "x(15)"
    FIELD cPartNo      LIKE itemfg.part-no       LABEL "Cust Part #"    FORMAT "x(15)"
    FIELD cItemName    LIKE itemfg.i-name        LABEL "Description"    FORMAT "x(15)"
    FIELD cJobNo       AS CHARACTER              LABEL "Job"            FORMAT "x(13)"
    FIELD iQtyOnHand   AS INTEGER                LABEL "Qty OnHand"     FORMAT "->>>>,>>>,>>9"
    FIELD cRcptDate    AS CHARACTER              LABEL "Rect. Date"     FORMAT "99/99/9999"
    FIELD dSellPrice   AS DECIMAL                LABEL "Sell Price"     FORMAT ">>,>>>,>>9.99"
    FIELD dTotalValue  AS DECIMAL                LABEL "Total Value"    FORMAT "->>>,>>>,>>9.99"
    FIELD iCommitted   AS INTEGER                LABEL "Committed"      FORMAT "->>,>>>,>>9"
    FIELD iQtyCase     AS INTEGER                LABEL "Qty/Case"       FORMAT "->>,>>9"
    FIELD cFgLot       AS CHARACTER              LABEL "FG Lot #"       FORMAT "x(20)"
    FIELD cCustLot     AS CHARACTER              LABEL "Customer Lot #" FORMAT "x(15)"
    FIELD cOrdDueDate  AS CHARACTER              LABEL "Order Due Date" FORMAT "99/99/9999"
    FIELD cJobDueDate  AS CHARACTER              LABEL "Job Due Date"   FORMAT "99/99/9999"
    FIELD cCSR         AS CHARACTER              LABEL "CSR"            FORMAT "x(8)"
    .
