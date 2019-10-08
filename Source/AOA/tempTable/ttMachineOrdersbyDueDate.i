/* ttMachineOrdersbyDueDate.i */

/* Machine Orders by Due Date.rpa */ 
DEFINE TEMP-TABLE ttMachineOrdersbyDueDate NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD machine        AS CHARACTER LABEL "Machine"        FORMAT "x(8)"
    FIELD orderNo        AS INTEGER   LABEL "Order"          FORMAT ">>>>>9"
    FIELD dueDate        AS DATE      LABEL "Due Date"       FORMAT "99/99/9999"
    FIELD fgItem         AS CHARACTER LABEL "FG Item"        FORMAT "x(30)"
    FIELD inventory      AS LOGICAL   LABEL "Inventory"      FORMAT "Yes/"
    FIELD custName       AS CHARACTER LABEL "Customer"       FORMAT "x(30)"
    FIELD qtyOrdered     AS DECIMAL   LABEL "Qty Ordered"    FORMAT "->>,>>>,>>9.9<<"
    FIELD numberOut      AS INTEGER   LABEL "Out"            FORMAT ">>9" 
    FIELD sheetsOrdered  AS LOGICAL   LABEL "Sheets Ordered" FORMAT "Yes/"
    FIELD qtyOnHand      AS DECIMAL   LABEL "Qty On Hand"    FORMAT "->>>,>>9.9<<<<<"
        INDEX ttMachineOrdersbyDueDate IS PRIMARY rowType machine dueDate orderNo
        .
