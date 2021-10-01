/*----------------------------------------------------oe/pallcalc2.p  */
/* Calculate the number of pallets needed.                                    */
/*----------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcItemID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcBin AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcTag AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipiOrderID AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiQuantity AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER ipiQuantityPerSubUnit AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opiQuantityOfUnits AS INTEGER NO-UNDO.

DEFINE VARIABLE iPallet          AS INTEGER NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.

RUN inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.

RUN Inventory_GetQuantityOfUnitsForBinAndOrder IN hdInventoryProcs (ipcCompany, ipcItemID, ipcJobID, ipiJobID2, ipcLocationID, ipcBin, ipcTag, ipiOrderID, ipiQuantity, ipiQuantityPerSubUnit, OUTPUT opiQuantityOfUnits).
  

IF VALID-HANDLE(hdInventoryProcs) THEN
    DELETE PROCEDURE hdInventoryProcs.


