
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
DEFINE VARIABLE lCreated AS LOGICAL NO-UNDO.
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

FOR EACH ttInventoryStockPreLoadtag:
    DISPLAY ttInventoryStockPreLoadtag.JobNo ttInventoryStockPreLoadtag.Quantity ttInventoryStockPreLoadtag.QuantityPerSubUnit ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit ttInventoryStockPreLoadtag.QuantityOfSubUnits ttInventoryStockPreLoadtag.QuantityOfUnits ttInventoryStockPreLoadtag.QuantityPartial.
END.

RUN CreateInventoryLoadtagsFromPreLoadtags IN hdInventoryProcs.

FOR EACH ttInventoryStockLoadtag:
    DISPLAY ttInventoryStockLoadtag.JobNo ttInventoryStockLoadtag.QuantityOriginal ttInventoryStockLoadtag.QuantityPerSubUnit ttInventoryStockLoadtag.QuantitySubUnitsPerUnit ttInventoryStockLoadtag.QuantityOfSubUnits ttInventoryStockLoadtag.QuantityOfUnits ttInventoryStockLoadtag.QuantityPartial.
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

