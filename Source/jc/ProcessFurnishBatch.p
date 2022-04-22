
/*------------------------------------------------------------------------
    File        : ProcessFurnishBatch.p
    Purpose     : 

    Syntax      :

    Description : Custom Program to Create RM inventory from an estimate

    Author(s)   : BV
    Created     : Tue Feb 02 21:26:14 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcTag        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdtTransDate AS DATETIME  NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocation   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplExportOnly AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER oplError      AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

{inventory/ttBrowseInventory.i}

DEFINE TEMP-TABLE ttRMToProcess NO-UNDO 
    FIELD company         AS CHARACTER 
    FIELD itemID          AS CHARACTER 
    FIELD quantity        AS DECIMAL 
    FIELD quantityUOM     AS CHARACTER
    FIELD transactionType AS CHARACTER 
    .
    
DEFINE TEMP-TABLE ttRMTransaction NO-UNDO
    FIELD company           AS CHARACTER 
    FIELD itemID            AS CHARACTER 
    FIELD quantity          AS DECIMAL  
    FIELD quantityUOM       AS CHARACTER 
    FIELD costPerUOM        AS DECIMAL 
    FIELD costUOM           AS CHARACTER
    FIELD costTotal         AS DECIMAL
    FIELD tag               AS CHARACTER
    FIELD rmBinRowID        AS ROWID
    FIELD rmRctdRowID       AS ROWID
    FIELD isError           AS LOGICAL
    FIELD errorMessage      AS CHARACTER
    FIELD transactionStatus AS CHARACTER
    FIELD transactionType   AS CHARACTER  
    FIELD transactionDate   AS DATETIME
    FIELD RMLot             AS CHARACTER
    .

DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.

RUN spGetSessionParam (
    INPUT  "Company",
    OUTPUT cCompany
    ).
IF ipcLocation EQ "" THEN 
    RUN spGetSessionParam (
        INPUT  "Location",
        OUTPUT cLocation
        ).
ELSE 
    cLocation = ipcLocation.
        
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetFilePath RETURNS CHARACTER PRIVATE
    (ipcFolder AS CHARACTER,
    ipcFile AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */
RUN pProcessFurnishBatch(
    INPUT  ipcCompany, 
    INPUT  ipcEstimateID, 
    INPUT  ipcTag, 
    INPUT  ipdtTransDate,
    INPUT  iplExportOnly, 
    OUTPUT oplError, 
    OUTPUT opcMessage
    ).


/* **********************  Internal Procedures  *********************** */

PROCEDURE pAddRMToProcess PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Creates an RMToProcess record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty             AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTransactionType AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError           AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-item FOR ITEM.
    
    FIND FIRST bf-item NO-LOCK 
         WHERE bf-item.company EQ ipcCompany
           AND bf-item.i-no    EQ ipcItemID
         NO-ERROR.
    IF NOT AVAILABLE bf-item THEN DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid item on estimate: " + ipcItemID
            .
        RETURN.
    END.
    
    FIND FIRST ttRMToProcess
         WHERE ttRMToProcess.company         EQ ipcCompany
           AND ttRMToProcess.itemID          EQ ipcItemID
           AND ttRMToProcess.transactionType EQ ipcTransactionType
         NO-ERROR.
    IF NOT AVAILABLE ttRMToProcess THEN DO:
        CREATE ttRMToProcess.
        ASSIGN 
            ttRMToProcess.company         = ipcCompany
            ttRMToProcess.itemID          = ipcItemID
            ttRMToProcess.quantityUOM     = bf-item.cons-uom
            ttRMToProcess.transactionType = ipcTransactionType
            .
    END.
    
    ttRMToProcess.quantity = ttRMToProcess.quantity + ipdQty.

END PROCEDURE.

PROCEDURE pAddRMTransactionFromBin PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-rm-bin FOR rm-bin.
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantity AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdCostTotal AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcTransType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtTransDate AS DATETIME NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    
    CREATE ttRMTransaction.
    ASSIGN
        ttRMTransaction.company           = ipbf-rm-bin.company
        ttRMTransaction.itemID            = ipbf-rm-bin.i-no
        ttRMTransaction.transactionType   = ipcTransType
        ttRMTransaction.quantity          = IF ipbf-rm-bin.qty GT 0 THEN MINIMUM(iopdQuantity, ipbf-rm-bin.qty) ELSE iopdQuantity
        ttRMTransaction.tag               = ipbf-rm-bin.tag
        ttRMTransaction.transactionDate   = ipdtTransDate
        ttRMTransaction.quantityUOM       = ipcQuantityUOM
        ttRMTransaction.costPerUOM        = ipbf-rm-bin.cost
        ttRMTransaction.costTotal         = ipbf-rm-bin.cost * ttRMTransaction.quantity
        ttRMTransaction.costUOM           = "EA"
        ttRMTransaction.rmBInRowID        = ROWID(ipbf-rm-bin)
        ttRMTransaction.transactionStatus = "Created"
        iopdQuantity                      = iopdQuantity - ttRMTransaction.quantity
        iopdCostTotal                     = iopdCostTotal + ttRMTransaction.costTotal
        .

END PROCEDURE.

PROCEDURE pBuildRMToProcess PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an estimate, this builds the necessary RMs that need to be processed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-est         FOR est.
    DEFINE BUFFER bf-ef          FOR ef.
    DEFINE BUFFER bf-est-qty     FOR est-qty.
    DEFINE BUFFER bf-estMaterial FOR estMaterial.
    
    DEFINE VARIABLE iIndex     AS INTEGER NO-UNDO.
    DEFINE VARIABLE dMasterQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dSpecQty   AS DECIMAL NO-UNDO.
    
    FIND FIRST bf-est NO-LOCK
         WHERE bf-est.company EQ ipcCompany
           AND bf-est.est-no  EQ ipcEstimateID
         NO-ERROR.
    IF AVAILABLE bf-est THEN DO:
        FIND FIRST bf-est-qty NO-LOCK 
             WHERE bf-est-qty.company EQ bf-est.company
               AND bf-est-qty.est-no  EQ bf-est.est-no
             NO-ERROR.
        IF AVAILABLE bf-est-qty THEN
            dMasterQty = bf-est-qty.eqty.
        ELSE
            dMasterQty = 1200.  /*Refactor - pull from estimate quantity if not always 1200 lbs*/
            
        FIND FIRST bf-ef OF bf-est NO-LOCK NO-ERROR.
        IF AVAILABLE bf-ef THEN DO:
            DO iIndex = 1 TO EXTENT(bf-ef.spec-no):
                IF bf-ef.spec-no[iIndex] NE "" THEN DO:
                    RUN custom/extradec.p (
                        INPUT  0.0001,
                        INPUT  bf-ef.spec-qty[iIndex] * dMasterQty,
                        OUTPUT dSpecQty
                        ).
                        
                    RUN pAddRMToProcess(
                        INPUT  ipcCompany, 
                        INPUT  bf-ef.spec-no[iIndex], 
                        INPUT  dSpecQty, 
                        INPUT  "I", 
                        OUTPUT oplError, 
                        OUTPUT opcMessage
                        ).
                    IF oplError THEN 
                        RETURN.
                END.  /*bf-ef.spec-no[iIndex] NE ""*/
            END. /* iIndex = 1 to 8 */
            FOR EACH bf-estMaterial NO-LOCK 
                WHERE bf-estMaterial.company EQ bf-est.company
                AND bf-estMaterial.estimateNo EQ bf-est.est-no:
                
                IF bf-estMaterial.quantityPer EQ "Lot" THEN 
                    dSpecQty = bf-estMaterial.quantity.
                ELSE 
                    dSpecQty = bf-estMaterial.quantity * dMasterQty.
                    
                RUN pAddRMToProcess(
                    INPUT ipcCompany, 
                    INPUT bf-estMaterial.itemID,
                    INPUT dSpecQty,
                    INPUT "I",
                    OUTPUT oplError,
                    OUTPUT opcMessage
                    ).
            END.    
            RUN pAddRMToProcess(
                INPUT  ipcCompany, 
                INPUT  bf-ef.board, 
                INPUT  dMasterQty, 
                INPUT  "R", 
                OUTPUT oplError, 
                OUTPUT opcMessage
                ).        
        END. /* AVAILABLE bf-ef */
    END. /* AVAILABLE bf-est */
    ELSE 
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid estimate: " + ipcEstimateID
            .

END PROCEDURE.

PROCEDURE pBuildRMTransactions PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Process the ttRMToProcess set to create the transactions required.
     First, there should be "Issue" transactions created for those with "I"  
     There should be one Receipt transaction for the "Board" (furnish) that will be a fixed quantity.
     The cost of this receipt should be the combined cost of the materials in the 
     ttRMToProcess list
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcTag AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtTransDate AS DATETIME NO-UNDO.
    
    DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dQuantity   AS DECIMAL   NO-UNDO.
    
    DEFINE BUFFER bf-rm-bin FOR rm-bin.
    DEFINE BUFFER bf-item   FOR item.
    
    FOR EACH ttRMToProcess
        WHERE ttRMToProcess.transactionType EQ "I":
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ ttRMToProcess.company
               AND bf-item.i-no    EQ ttRMToProcess.itemID
             NO-ERROR.
        IF NOT AVAILABLE bf-item THEN
            NEXT.
            
        dQuantity = ttRMToProcess.quantity.

    /*FIFO select the bins (there may or may not be tags) that will be depleted for each item*/
    /*Build up a running total cost of that inventory (store in dCostTotal)*/
    /*Add ttRMTransaction record to process later*/
        FOR EACH bf-rm-bin NO-LOCK
            WHERE bf-rm-bin.company EQ ttRMToProcess.company
              AND bf-rm-bin.i-no    EQ ttRMToProcess.itemID
              AND bf-rm-bin.loc     EQ cLocation
              AND bf-rm-bin.qty     GT 0
            BY bf-rm-bin.rec_key:
            IF dQuantity LE 0 THEN
                LEAVE.
            RUN pAddRMTransactionFromBin(BUFFER bf-rm-bin, INPUT-OUTPUT dQuantity, INPUT-OUTPUT dCostTotal, ttRMToProcess.transactionType, ipdtTransDate, bf-item.cons-uom).
        END.
        
        IF dQuantity LE 0 THEN
            NEXT.

        FOR EACH bf-rm-bin NO-LOCK
            WHERE bf-rm-bin.company EQ ttRMToProcess.company
              AND bf-rm-bin.i-no    EQ ttRMToProcess.itemID
              AND bf-rm-bin.qty     GT 0
            BY bf-rm-bin.rec_key:
            IF dQuantity LE 0 THEN
                LEAVE.
            RUN pAddRMTransactionFromBin(BUFFER bf-rm-bin, INPUT-OUTPUT dQuantity, INPUT-OUTPUT dCostTotal, ttRMToProcess.transactionType, ipdtTransDate, bf-item.cons-uom).
        END.
        
        IF dQuantity LE 0 THEN
            NEXT.
        
        /* IF rm-bin doesn't have enough quantity. Reduce total quantity from first available rm-bin */
        FOR EACH bf-rm-bin NO-LOCK
            WHERE bf-rm-bin.company EQ ttRMToProcess.company
              AND bf-rm-bin.i-no    EQ ttRMToProcess.itemID
            BY bf-rm-bin.rec_key:
            IF dQuantity LE 0 THEN
                LEAVE.
            RUN pAddRMTransactionFromBin(BUFFER bf-rm-bin, INPUT-OUTPUT dQuantity, INPUT-OUTPUT dCostTotal, ttRMToProcess.transactionType, ipdtTransDate, bf-item.cons-uom).
        END.  
        
    END.
    
    FOR EACH ttRMToProcess
        WHERE ttRMToProcess.transactionType EQ "R":
        FIND FIRST bf-item NO-LOCK
             WHERE bf-item.company EQ ttRMToProcess.company
               AND bf-item.i-no    EQ ttRMToProcess.itemID
             NO-ERROR.
        IF NOT AVAILABLE bf-item THEN
            NEXT.
            
        /*Add ttRMTransaction record to process later - calculate the cost per LB and store that in UOM cost*/
        CREATE ttRMTransaction.
        ASSIGN
            ttRMTransaction.company           = ttRMToProcess.company
            ttRMTransaction.itemID            = ttRMToProcess.itemID
            ttRMTransaction.transactionType   = ttRMToProcess.transactionType
            ttRMTransaction.quantity          = IF ttRMToProcess.quantity GT 0 THEN ttRMToProcess.quantity ELSE 1
            ttRMTransaction.RMLot             = ipcTag
            ttRMTransaction.transactionDate   = ipdtTransDate
            ttRMTransaction.quantityUOM       = bf-item.cons-uom
            ttRMTransaction.costPerUOM        = dCostTotal / ttRMTransaction.quantity
            ttRMTransaction.costTotal         = dCostTotal
            ttRMTransaction.transactionStatus = "Created"
            ttRMTransaction.costUOM           = "EA"
            .        
    END.
    
END PROCEDURE.

PROCEDURE pExportTempTables PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Exports temp-tables to defined location for testing purposes
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdOutput    AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdTempTable AS HANDLE    NO-UNDO.
    DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFolder     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS LOGICAL   NO-UNDO.
    
    RUN system\OutputProcs.p PERSISTENT SET hdOutput.
    RUN FileSys_GetTempDirectory (
        OUTPUT cFolder
        ).
    
    ASSIGN 
        cFile       = fGetFilePath(cFolder, "RMToProcess")
        hdTempTable = TEMP-TABLE ttRMToProcess:HANDLE
        .
        
    RUN Output_TempTableToCSV IN hdOutput (
        INPUT  hdTempTable, 
        INPUT  cFile, 
        INPUT  YES, 
        INPUT  YES, 
        OUTPUT lSuccess, 
        OUTPUT opcMessage
        ).
    
    ASSIGN 
        cFile       = fGetFilePath(cFolder, "RMTransactions")
        hdTempTable = TEMP-TABLE ttRMTransaction:HANDLE
        .
        
    RUN Output_TempTableToCSV IN hdOutput (
        input  hdTempTable, 
        input  cFile, 
        input  YES, 
        input  YES, 
        OUTPUT lSuccess, 
        OUTPUT opcMessage
        ).
    
   
    IF NOT lSuccess THEN 
        oplError = YES.
    ELSE 
        opcMessage = "Files Exported to " + cFolder.
        
END PROCEDURE.

PROCEDURE pProcessFurnishBatch PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstimateID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcTag        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdtTransDate AS DATETIME  NO-UNDO.
    DEFINE INPUT  PARAMETER iplExportOnly AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError      AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttRMToProcess.
    EMPTY TEMP-TABLE ttRMTransaction.

    /*This procedure builds the ttRMToProcess temp-table based on the estimate passed in.
    It will create one "Board" to receive and an a list of Special Materials that will need to be consumed/issued*/
    RUN pBuildRMToProcess(
        INPUT  ipcCompany, 
        INPUT  ipcEstimateID, 
        OUTPUT oplError, 
        OUTPUT opcMessage
        ).  

    IF oplError THEN
        RETURN.

    /*This is the framework for processing the ttRMToProcess temp-table between these two procedures.
    Note if legacy auto issue logic is used, it may not be practical to generate the pending inventory and GL transactions.
    This is the desired model that is similar to process of PostInvoices.p that builds up a "work list" that will then be processed
    at the very end in one transaction that does the DB creates and writes. */
    RUN pBuildRMTransactions(
        INPUT ipcTag,
        INPUT ipdtTransDate
        ).

    RUN pProcessTransactions (
        OUTPUT oplError, 
        OUTPUT opcMessage
        ).

    IF iplExportOnly THEN  
        RUN pExportTempTables (
            OUTPUT oplError, 
            OUTPUT opcMessage
            ).

END PROCEDURE.

PROCEDURE pProcessTransactions PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes the RM and GL Transactions to create actual transactions
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplError   AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.
    DEFINE VARIABLE lSuccess         AS LOGICAL NO-UNDO.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

    MAIN-BLOCK:
    DO TRANSACTION ON ERROR UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
        ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:      
        FOR EACH ttRMTransaction
            WHERE ttRMTransaction.transactionStatus EQ "Created":
            /*Create rm-rctd and post it.  This needs to update all summary item fields as well as create the appropriate GL (if RMPOSTGL is on)
            This can either be the legacy procedures or a scaled-down rewrite*/
           
            IF ttRMTransaction.transactionType EQ "R" THEN DO:
                RUN Inventory_CreateRMTransactionForDate IN hdInventoryProcs (
                    INPUT  ttRMTransaction.company, 
                    INPUT  ttRMTransaction.transactionDate, 
                    INPUT  ttRMTransaction.itemID, 
                    INPUT  ttRMTransaction.tag, 
                    INPUT  cLocation, 
                    INPUT  "",  /* Bin */ 
                    INPUT  ttRMTransaction.transactionType, 
                    INPUT  ttRMTransaction.quantity, 
                    INPUT  ttRMTransaction.costPerUOM, 
                    INPUT  "",  /* Reason Code */ 
                    INPUT  ttRMTransaction.rmLot,
                    OUTPUT ttRMTransaction.rmRctdRowID, 
                    OUTPUT oplError, 
                    OUTPUT opcMessage
                    ).        
            END.
            ELSE IF ttRMTransaction.transactionType EQ "I" THEN DO:
                RUN Inventory_CreateRMTransactionFromRMBinForDate IN hdInventoryProcs (
                    INPUT  ttRMTransaction.rmBInRowID, 
                    INPUT  ttRMTransaction.transactionDate,
                    INPUT  ttRMTransaction.transactionType, 
                    INPUT  ttRMTransaction.quantity, 
                    INPUT  "",  /* Reason Code */ 
                    INPUT  FALSE,  /* Update Job Details */
                    OUTPUT ttRMTransaction.rmRctdRowID, 
                    OUTPUT oplError, 
                    OUTPUT opcMessage
                    ).
            END.
            
            ASSIGN
                ttRMTransaction.isError      = oplError
                ttRMTransaction.errorMessage = opcMessage
                .
                
            IF oplError THEN
                UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.

            ttRMTransaction.transactionStatus = "Pending".
        END.
        
        FOR EACH ttRMTransaction
            WHERE ttRMTransaction.transactionStatus EQ "Pending":
            RUN Inventory_BuildRawMaterialToPost IN hdInventoryProcs (
                INPUT  ttRMTransaction.rmRctdRowID,
                INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
                ).    
            
            ttRMTransaction.transactionStatus = "Processing".
        END.   
        
        RUN Inventory_PostRawMaterials IN hdInventoryProcs (
            INPUT  cCompany,
            INPUT  ipdtTransDate,
            OUTPUT lSuccess,
            OUTPUT opcMessage,
            INPUT-OUTPUT TABLE ttBrowseInventory BY-REFERENCE
            ).
       
        oplError = NOT lSuccess.
       
        IF oplError THEN
            UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK.
         
        FOR EACH ttRMTransaction
            WHERE ttRMTransaction.transactionStatus EQ "Processing":           
            ttRMTransaction.transactionStatus = "Posting".            
        END.
    END.
        
    DELETE PROCEDURE hdInventoryProcs.
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetFilePath RETURNS CHARACTER PRIVATE
    (ipcFolder AS CHARACTER, ipcFile AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:  Given inputs, validate folder and build new file name.  Return complete path.
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cFullFilePath AS CHARACTER NO-UNDO.
    
    cFullFilePath = ipcFolder + "\" + ipcFile + ".csv".
    
    RETURN cFullFilePath.
            
END FUNCTION.
        
