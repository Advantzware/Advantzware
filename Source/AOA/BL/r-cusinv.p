/*------------------------------------------------------------------------
  File: r-cusinv.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Customer Inventory.rpa */
{aoa/tempTable/ttCustomerInventory.i}

{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttCustomerInventory.
{aoa/includes/pCustomerInventory.i}

/* local variables */
DEFINE VARIABLE iPalletCount AS INTEGER   NO-UNDO.
DEFINE VARIABLE rRowID       AS ROWID     NO-UNDO.
DEFINE VARIABLE cStatus      AS CHARACTER NO-UNDO.

/* subject business logic */
FOR EACH cust NO-LOCK
    WHERE cust.company EQ ipcCompany
      AND cust.cust-no GE cStartCustNo
      AND cust.cust-no LE cEndCustNo
      AND (IF NOT lIncludeInactiveCustomers THEN cust.active EQ "A" ELSE TRUE),
    EACH itemfg NO-LOCK
    WHERE itemfg.company EQ cust.company
      AND itemfg.cust-no EQ cust.cust-no
      AND CAN-DO(cInventoryClasses,itemfg.class) EQ TRUE
    :
    IF lCustList AND
       NOT CAN-FIND(FIRST ttCustList
                    WHERE ttCustList.cust-no EQ cust.cust-no
                      AND ttCustList.log-fld EQ TRUE) THEN
    NEXT.
    ASSIGN
        iPalletCount = 0
        rRowID  = ?
        .
    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
        USE-INDEX co-ino
        BREAK BY fg-bin.qty DESC:
        ASSIGN
            iPalletCount = (IF fg-bin.case-count EQ 0 THEN 1
                       ELSE fg-bin.case-count) *
                        (IF fg-bin.cases-unit    EQ 0 THEN 1
                       ELSE fg-bin.cases-unit) *
                        (IF fg-bin.units-pallet  EQ 0 THEN 1
                       ELSE fg-bin.units-pallet)
            rRowID = ROWID(fg-bin)
            .
        LEAVE.
    END. /* each fg-bin */
    IF NOT lIncludeZeroQty AND iPalletCount EQ 0 THEN NEXT.
    FIND fg-bin NO-LOCK WHERE ROWID(fg-bin) EQ rRowID NO-ERROR.
    RELEASE oe-ordl.
    RELEASE oe-rel.
    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company  EQ itemfg.company
          AND oe-ordl.i-no     EQ itemfg.i-no,
        FIRST oe-ord OF oe-ordl NO-LOCK
        WHERE oe-ord.cust-no   EQ cust.cust-no
        BY oe-ordl.req-date DESC
        BY oe-ordl.ord-no   DESC
        :
        LEAVE.
    END. /* each oe-ordl */
    IF AVAIL oe-ordl THEN
    FOR EACH oe-rel NO-LOCK
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
        BY oe-rel.rel-date:
        {oe/rel-stat.i cStatus}
        IF INDEX("CZ",cStatus) EQ 0 THEN LEAVE.
    END. /* each oe-rel */
    CREATE ttCustomerInventory.
    ASSIGN
        ttCustomerInventory.xxSort1     = cust.cust-no + itemfg.class
        ttCustomerInventory.xxSort2     = IF cSort EQ "Item" THEN itemfg.i-no + itemfg.part-no
                                                             ELSE itemfg.part-no + itemfg.i-no
        ttCustomerInventory.custNo      = cust.cust-no
        ttCustomerInventory.custName    = cust.name
        ttCustomerInventory.itemClass   = itemfg.class
        ttCustomerInventory.itemNo      = itemfg.i-no
        ttCustomerInventory.partNo      = itemfg.part-no
        ttCustomerInventory.itemName    = itemfg.i-name
        ttCustomerInventory.orderLevel  = itemfg.ord-level
        ttCustomerInventory.releasePO   = oe-rel.po-no WHEN AVAILABLE oe-rel
        ttCustomerInventory.qtyOnHand   = itemfg.q-onh
        ttCustomerInventory.palletCount = iPalletCount
        ttCustomerInventory.requestDate = oe-ordl.req-date WHEN AVAILABLE oe-rel
        .
END. /* each cust */

{aoa/BL/pBuildCustList.i}
