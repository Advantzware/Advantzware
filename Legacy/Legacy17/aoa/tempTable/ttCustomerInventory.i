/* ttCustomerInventory.i */

/* Customer Inventory.rpa */
DEFINE TEMP-TABLE ttCustomerInventory NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD custNo      AS CHARACTER          LABEL "Cust No"
    FIELD custName    LIKE cust.name        LABEL "Customer Name"
    FIELD itemClass   LIKE itemfg.class     LABEL "Class"
    FIELD itemNo      LIKE itemfg.i-no      LABEL "Item Number"
    FIELD partNo      LIKE itemfg.part-no   LABEL "Part Number"
    FIELD itemName    LIKE itemfg.i-name    LABEL "Description"
    FIELD orderLevel  LIKE itemfg.ord-level LABEL "Order Level"
    FIELD releasePO   LIKE oe-rel.po-no     LABEL "Release PO"
    FIELD qtyOnHand   AS INTEGER            LABEL "Qty On Hand"  FORMAT "->>>>,>>>,>>>"
    FIELD palletCount AS INTEGER            LABEL "Pallet Count" FORMAT "->>>,>>>,>>>"
    FIELD releaseQty  AS CHARACTER          LABEL "Release Qty"  FORMAT "x(11)" INITIAL "___________"
    FIELD requestDate AS DATE               LABEL "Date"         FORMAT "99/99/9999"
    FIELD xxSort1     AS CHARACTER          LABEL "Sort 1"       FORMAT "x(500)"
    FIELD xxSort2     AS CHARACTER          LABEL "Sort 2"       FORMAT "x(500)"
        INDEX sortBy IS PRIMARY rowType xxSort1 xxSort2
        .
