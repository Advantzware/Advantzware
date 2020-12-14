/* -------------------------------------------------- fg/updfgcst.p 03/97 JLF */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipcItemID LIKE itemfg.i-no.
DEFINE INPUT PARAMETER iplCalcCostFromHist AS LOGICAL NO-UNDO.

{sys/inc/var.i shared}
{Inventory/ttInventory.i "NEW SHARED"}
DEFINE BUFFER bf-itemfg FOR itemfg.

DEFINE VARIABLE hdInventoryProcs AS HANDLE.
RUN inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.

FOR EACH bf-itemfg
    WHERE bf-itemfg.company EQ cocode
    AND (IF ipcItemID EQ "*" THEN YES
    ELSE bf-itemfg.i-no EQ ipcItemID)
    NO-LOCK:

    STATUS DEFAULT "Please wait...  Updating item: " + TRIM(bf-itemfg.i-no).

    IF bf-itemfg.isaset AND bf-itemfg.alloc THEN
        RUN util/fixfgcst.p (ROWID(bf-itemfg)).
    ELSE
        RUN fg/updfgcs1.p (RECID(bf-itemfg), YES, iplCalcCostFromHist).
    
    FIND CURRENT bf-itemfg EXCLUSIVE-LOCK.
    RUN Inventory_GetAverageCostFG IN hdInventoryProcs (bf-itemfg.company, bf-itemfg.i-no , OUTPUT bf-itemfg.avg-cost).
    FIND CURRENT bf-itemfg NO-LOCK.
END.
DELETE OBJECT hdInventoryProcs.
RELEASE bf-itemfg.
STATUS DEFAULT "".

/* end ---------------------------------- copr. 1997  Advanced Software, Inc. */

