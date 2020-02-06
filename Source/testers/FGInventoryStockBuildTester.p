{inventory/ttinventory.i "NEW SHARED"}

DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.
DEFINE VARIABLE iCount           AS INTEGER NO-UNDO.
DEFINE VARIABLE totCount         AS INTEGER NO-UNDO.
DEFINE VARIABLE itemCount        AS INTEGER NO-UNDO.
DEFINE VARIABLE itemLimitToLoad  AS INTEGER NO-UNDO INITIAL 10. /* Modify the limit as necessary */
 
DISABLE TRIGGERS FOR LOAD OF inventoryStock.

RUN inventory/inventoryProcs.p PERSISTENT SET hdInventoryProcs.

LOG-MANAGER:LOGFILE-NAME = "C:\temp\FGInventoryLoad_DisabledTriggers.log".
LOG-MANAGER:WRITE-MESSAGE("Begin Load").

ETIME(YES).

FOR EACH itemfg NO-LOCK:
    IF itemCount GE itemLimitToLoad THEN
        LEAVE.

    iCount  = 0.

    RUN BuildFGInventoryStockForItem in hdInventoryProcs(
        ROWID(itemfg),
        TODAY,
        INPUT-OUTPUT iCount
        ).
        
    totCount = totCount + iCount.

    itemCount = itemCount + 1.

    LOG-MANAGER:WRITE-MESSAGE("Item: " + itemfg.i-no + "  Total inventory tags created for item: " + string(iCount)).
END.

LOG-MANAGER:WRITE-MESSAGE("End load").
LOG-MANAGER:WRITE-MESSAGE("Total item count: " + string(itemCount)).
LOG-MANAGER:WRITE-MESSAGE("Total tag created: " + string(totCount)).
LOG-MANAGER:WRITE-MESSAGE("Total time elapsed: " + STRING(ETIME / 1000) + "s").
LOG-MANAGER:CLOSE-LOG().

DELETE OBJECT hdInventoryProcs.
