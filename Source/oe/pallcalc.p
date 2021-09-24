/*----------------------------------------------------oe/pallcalc.p 05/05 JLF */
/* Calculate the number of pallets needed.                                    */
/*----------------------------------------------------------------------------*/

DEF INPUT  PARAM ip-rowid   AS ROWID NO-UNDO.
DEF OUTPUT PARAM op-pallets AS INT NO-UNDO.


DEFINE VARIABLE iPallet AS INTEGER NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE NO-UNDO.
RUN inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.

FIND oe-boll WHERE ROWID(oe-boll) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-boll THEN DO:
  
  RUN Inventory_GetQuantityOfUnitsForOEBoll IN hdInventoryProcs (rowid(oe-boll), OUTPUT iPallet).
  op-pallets = iPallet.
END.

IF VALID-HANDLE(hdInventoryProcs) THEN
DELETE PROCEDURE hdInventoryProcs.
