
/*------------------------------------------------------------------------
    File        : InventoryTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Mon Mar 04 21:58:33 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdInventoryProcs AS HANDLE.
RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
{Inventory/ttInventory.i "NEW SHARED"}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEFINE VARIABLE lCreated AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

FIND FIRST job-mch NO-LOCK 
    WHERE job-mch.company EQ '001'
    AND job-mch.job-no EQ 'W13648'
    AND job-mch.job-no2 EQ 0
    AND job-mch.m-code EQ 'SHT1'
    AND job-mch.frm EQ 1
    NO-ERROR.

FIND FIRST job-mat NO-LOCK 
    WHERE job-mat.company EQ job-mch.company
    AND job-mat.job-no EQ job-mch.job-no
    AND job-mat.job-no2 EQ job-mch.job-no2
    AND job-mat.frm EQ job-mch.frm
    AND job-mat.rm-i-no EQ "SBS18"
    NO-ERROR.
  
RUN CreatePreLoadtagsFromInputsWIP IN hdInventoryProcs (ROWID(job-mch), ROWID(job-mat), 
    job-mat.qty, 500, 1, "EA", OUTPUT lCreated, OUTPUT cMessage).

/*FOR EACH ttInventoryStockPreLoadtag:                      */
/*    DISPLAY                                               */
/*        ttInventoryStockPreLoadtag.InventoryStockID       */
/*        ttInventoryStockPreLoadtag.JobID                  */
/*        ttInventoryStockPreLoadtag.Quantity               */
/*        ttInventoryStockPreLoadtag.QuantityOriginal       */
/*        ttInventoryStockPreLoadtag.QuantityPerSubUnit     */
/*        ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit*/
/*        ttInventoryStockPreLoadtag.QuantityOfSubUnits     */
/*        ttInventoryStockPreLoadtag.QuantityOfUnits        */
/*        ttInventoryStockPreLoadtag.QuantityPartial        */
/*        .                                                 */
/* END.                                                     */
/*UI Code to manipulate the ttInventoryStockPreLoadtag table here*/
RUN CreateInventoryLoadtagsFromPreLoadtags IN hdInventoryProcs.

EMPTY TEMP-TABLE InventoryStock.
FOR EACH ttInventoryStockLoadtag:
    ASSIGN 
        lCreated = NO
        cMessage = ""
        . 
    RUN CreateInventoryStockFromLoadtag IN hdInventoryProcs (ttInventoryStockLoadtag.InventoryStockID, YES, NO, OUTPUT lCreated, OUTPUT cMessage).
/*    DISPLAY                                                */
/*        ttInventoryStockLoadtag.InventoryStockID           */
/*        ttInventoryStockLoadtag.JobID                      */
/*        ttInventoryStockLoadtag.Quantity                   */
/*        ttInventoryStockLoadtag.QuantityOriginal           */
/*        ttInventoryStockLoadtag.QuantityPerSubUnit         */
/*        ttInventoryStockLoadtag.QuantitySubUnitsPerUnit    */
/*        ttInventoryStockLoadtag.QuantityOfSubUnits         */
/*        ttInventoryStockLoadtag.QuantityOfUnits            */
/*        ttInventoryStockLoadtag.QuantityPartial            */
/*        ttInventoryStockLoadtag.StockIDAlias FORMAT "x(30)"*/
/*        lCreated                                           */
/*        cMessage                                           */
/*        .                                                  */
               
END.


/*UI Code to manipulate (delete?) the ttInventoryStockLoadtag table records here.*/  

/*Conditional post */
FOR EACH InventoryTransaction NO-LOCK:
    DISPLAY InventoryTransaction.
END.


FOR EACH InventoryStock NO-LOCK,
FIRST InventoryStockAlias NO-LOCK
    WHERE InventoryStockAlias.Company EQ InventoryStock.Company
    AND InventoryStockAlias.InventoryStockID EQ InventoryStock.InventoryStockID:
    DISPLAY
        InventoryStock.InventoryStockID FORMAT "x(30)"
        InventoryStockAlias.StockIDAlias FORMAT "x(30)"
        InventoryStock.Quantity
        InventoryStock.QuantityOriginal
/*        InventoryStock.QuantityPerSubUnit     */
/*        InventoryStock.QuantitySubUnitsPerUnit*/
/*        InventoryStock.QuantityOfSubUnits     */
/*        InventoryStock.QuantityOfUnits        */
/*        InventoryStock.QuantityPartial        */
        InventoryStock.InventoryStatus FORMAT "x(30)"
        InventoryStock.PrimaryID FORMAT "x(30)"
        
        .
END.


FOR EACH InventoryTransaction NO-LOCK:
    RUN PostTransaction IN hdInventoryProcs (InventoryTransaction.InventoryTransactionID).
    DISPLAY InventoryTransaction.
END.

FOR EACH InventoryStock NO-LOCK,
FIRST InventoryStockAlias NO-LOCK
    WHERE InventoryStockAlias.Company EQ InventoryStock.Company
    AND InventoryStockAlias.InventoryStockID EQ InventoryStock.InventoryStockID:
    DISPLAY
        InventoryStock.InventoryStockID FORMAT "x(30)"
        InventoryStockAlias.StockIDAlias FORMAT "x(30)"
        InventoryStock.Quantity
        InventoryStock.QuantityOriginal
        InventoryStock.WarehouseID
        InventoryStock.LocationID
/*        InventoryStock.QuantityPerSubUnit     */
/*        InventoryStock.QuantitySubUnitsPerUnit*/
/*        InventoryStock.QuantityOfSubUnits     */
/*        InventoryStock.QuantityOfUnits        */
/*        InventoryStock.QuantityPartial        */
        InventoryStock.InventoryStatus FORMAT "x(30)"
        InventoryStock.PrimaryID FORMAT "x(30)"
        
        .
END.

FIND FIRST InventoryStock NO-LOCK 
    NO-ERROR.

IF AVAILABLE InventoryStock THEN     
    RUN CreateTransactionTransfer IN hdInventoryProcs (InventoryStock.Company, InventoryStock.StockIDAlias, "MAIN", "A-100", "YES", OUTPUT lCreated, OUTPUT cMessage).

FOR EACH InventoryStock NO-LOCK,
FIRST InventoryStockAlias NO-LOCK
    WHERE InventoryStockAlias.Company EQ InventoryStock.Company
    AND InventoryStockAlias.InventoryStockID EQ InventoryStock.InventoryStockID:
    DISPLAY
        InventoryStock.InventoryStockID FORMAT "x(30)"
        InventoryStockAlias.StockIDAlias FORMAT "x(30)"
        InventoryStock.Quantity
        InventoryStock.QuantityOriginal
        InventoryStock.WarehouseID
        InventoryStock.LocationID
/*        InventoryStock.QuantityPerSubUnit     */
/*        InventoryStock.QuantitySubUnitsPerUnit*/
/*        InventoryStock.QuantityOfSubUnits     */
/*        InventoryStock.QuantityOfUnits        */
/*        InventoryStock.QuantityPartial        */
        InventoryStock.InventoryStatus FORMAT "x(30)"
        InventoryStock.PrimaryID FORMAT "x(30)"
        
        .
END.
/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetSheetsFromLF PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdQuantityInLF AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdSheetLengthInLF AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantityInSheets AS INTEGER NO-UNDO.

    opiQuantityInSheets = INTEGER(TRUNC(ipdQuantityInLF / ipdSheetLengthInLF, 0)) + INTEGER((ipdQuantityInLF MODULO ipdSheetLengthInLF) NE 0).

END PROCEDURE.

