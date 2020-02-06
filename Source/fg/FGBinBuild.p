
/*------------------------------------------------------------------------
    File        : FGBinBuild.p
    Purpose     : 

    Syntax      :

    Description : Builds Bins from history based on a given as of date.
                  Also has argument to purge history records processed

    Author(s)   : BV
    Created     : Thu Feb 14 13:53:01 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{inventory/ttInventory.i "NEW SHARED"}
{fg/ttFGBins.i "SHARED"}
{fg/ttInventoryTables.i}

DEFINE STREAM sExport1.
DEFINE STREAM sExport2.
DEFINE VARIABLE gcOutputFile1       AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcOutputFile2       AS CHARACTER NO-UNDO.
DEFINE VARIABLE gcDefaultExportPath AS CHARACTER NO-UNDO INITIAL "C:\Temp".
DEFINE VARIABLE gcEmptyTagPrefix    AS CHARACTER NO-UNDO INITIAL "id:".
DEFINE VARIABLE hdInventoryProcs    AS HANDLE    NO-UNDO.

DEFINE VARIABLE iInventoryStockSeq        AS INTEGER NO-UNDO.
DEFINE VARIABLE iInventoryTransactionSeq  AS INTEGER NO-UNDO.
DEFINE VARIABLE iInventoryStockAliasSeq   AS INTEGER NO-UNDO.

RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
PROCEDURE BuildBinsForItemAndPurge:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper Proc that purges
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriFGItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcExportFilePath AS CHARACTER NO-UNDO.
    
    RUN pBuildBinsForItem(ipriFGItem, ipdtAsOf, INPUT-OUTPUT iopiCountOfProcessed, YES, ipcExportFilePath).
    
END PROCEDURE.

PROCEDURE BuildBinsForItem:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper Proc that doesn't purge
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriFGItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER NO-UNDO.
    
    RUN pBuildBinsForItem(ipriFGItem, ipdtAsOf, INPUT-OUTPUT iopiCountOfProcessed, NO, "").
    
END PROCEDURE.

PROCEDURE CreateCycleCountTransactions:
/*------------------------------------------------------------------------------
 Purpose: Given an AsOf Date, create Cycle Count Transactions for ttFGBins      
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.

DEFINE VARIABLE iNextRNo AS INTEGER NO-UNDO.

FOR EACH ttFGBins 
    WHERE ttFGBins.qty NE 0 NO-LOCK:
    DO TRANSACTION:  /*To complete Create Trigger processing*/
        
        FIND LAST fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAIL fg-rctd AND fg-rctd.r-no GT iNextRNo THEN iNextRNo = fg-rctd.r-no.
        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAIL fg-rcpth AND fg-rcpth.r-no GT iNextRNo THEN iNextRNo = fg-rcpth.r-no.
        iNextRNo = iNextRNo + 1.
        
        CREATE fg-rcpth.
        ASSIGN
            fg-rcpth.company = ttFGBins.company
            fg-rcpth.r-no = iNextRNo
            fg-rcpth.adjustmentCode = "PRG"
            fg-rcpth.spare-char-5 = "Added Through Purge"
            fg-rcpth.create-by = USERID("asi")
            fg-rcpth.i-name = ttFGBins.itemfgIName
            fg-rcpth.i-no = ttFGBins.i-no
            fg-rcpth.job-no = ttFGBins.job-no
            fg-rcpth.job-no2 = ttFGBins.job-no2
            fg-rcpth.loc = ttFGBins.loc
            fg-rcpth.po-no = ttFGBins.po-no
            fg-rcpth.post-date = ipdtAsOf
            fg-rcpth.pur-uom = ttFGBins.pur-uom
            fg-rcpth.rita-code = "C"
            fg-rcpth.trans-date = ipdtAsOf
            .
    
    END.
    CREATE fg-rdtlh.
    ASSIGN
        fg-rdtlh.company = fg-rcpth.company
        fg-rdtlh.rita-code = fg-rcpth.rita-code
        fg-rdtlh.r-no = fg-rcpth.r-no
        fg-rdtlh.i-no = fg-rcpth.i-no
      
        fg-rdtlh.cust-no = ttFGBins.cust-no
        fg-rdtlh.loc = ttFGBins.loc
        fg-rdtlh.loc-bin = ttFGBins.loc-bin
        fg-rdtlh.job-no = ttFGBins.job-no
        fg-rdtlh.job-no2 = ttFGBins.job-no2
        fg-rdtlh.last-cost = ttFGBins.std-tot-cost
        fg-rdtlh.avg-cost = ttFGBins.std-tot-cost
        fg-rdtlh.cost = ttFGBins.std-tot-cost
        fg-rdtlh.qty = ttFGBins.qty
        fg-rdtlh.stacks-unit  = ttFGBins.units-pallet /* Stacks-Units = Units/Pallet */
        fg-rdtlh.cases        = TRUNCATE(ttFGBins.qty / ttFGBins.case-count, 0)
        fg-rdtlh.qty-case     = ttFGBins.case-count
        fg-rdtlh.partial      = ttFGBins.qty MODULO ttFgBins.case-count       
        fg-rdtlh.std-fix-cost = ttFGBins.std-fix-cost
        fg-rdtlh.std-lab-cost = ttFGBins.std-lab-cost
        fg-rdtlh.std-mat-cost = ttFGBins.std-mat-cost
        fg-rdtlh.std-tot-cost = ttFGBins.std-tot-cost
        fg-rdtlh.std-var-cost = ttFGBins.std-var-cost
        fg-rdtlh.tag = ttFGBins.tag
        fg-rdtlh.trans-date = ipdtAsOf
        fg-rdtlh.trans-time = 86399
        .

            

END.

END PROCEDURE.

PROCEDURE CreateInventoryFromTTInventory:
    /*------------------------------------------------------------------------------
     Purpose: Create Inventory tables from ttFGBins table
     Notes:
    ------------------------------------------------------------------------------*/
    /*RUN pValidateTTInventory.*/
    
    FOR EACH ttInventoryStock       
        ON ERROR UNDO, NEXT:         
        CREATE inventoryStock.
        BUFFER-COPY ttInventoryStock EXCEPT rec_key inventoryStockID 
            TO inventoryStock.
        
        inventoryStock.inventoryStockID = DYNAMIC-FUNCTION (
                                              "fGetNextStockID" IN hdInventoryProcs,
                                               ttInventoryStock.itemType
                                               ).
                                               
        IF inventoryStock.tag EQ "" THEN
            inventoryStock.tag = gcEmptyTagPrefix + inventoryStock.inventoryStockID.
        
        FOR EACH ttInventoryTransaction
            WHERE ttInventoryTransaction.inventoryStockID EQ ttInventoryStock.inventoryStockID
              AND ttInventoryTransaction.valid:
            CREATE inventoryTransaction.
            BUFFER-COPY ttInventoryTransaction EXCEPT rec_key inventoryStockID inventoryTransactionID tag 
                TO inventoryTransaction.

            ASSIGN
                inventoryTransaction.inventoryStockID       = inventoryStock.inventoryStockID
                inventoryTransaction.tag                    = inventoryStock.tag
                inventoryTransaction.inventoryTransactionID = DYNAMIC-FUNCTION (
                                                                  "fGetNextTransactionID" IN hdInventoryProcs
                                                                  )
                .
        END.
    END.
END PROCEDURE.

PROCEDURE BuildTTInventoryForItem:
    /*------------------------------------------------------------------------------
     Purpose: Given a FG Item, builds inventory 
     Notes: This procedure creates Inventory temp-tables which is modelled on the 
            logic used to update fg-bin records when "Recalculate Qtys" button is 
            clicked in I-F-1 screen  
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipriItemFG           AS ROWID     NO-UNDO.
    DEFINE INPUT        PARAMETER ipdtAsOf             AS DATE      NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-oe-bolh FOR oe-bolh.

    EMPTY TEMP-TABLE ttInventoryStock.
    EMPTY TEMP-TABLE ttInventoryTransaction.
    
    FIND FIRST itemfg NO-LOCK
        WHERE ROWID(itemfg) EQ ipriItemFG
        NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company    EQ itemfg.company
              AND fg-rcpth.i-no       EQ itemfg.i-no          
            USE-INDEX tran,
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
              AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no:
            
            iopiCountOfProcessed = iopiCountOfProcessed + 1.

            FIND FIRST ttInventoryStock
                 WHERE ttInventoryStock.company      EQ fg-rcpth.company
                   AND ttInventoryStock.fgItemID     EQ fg-rcpth.i-no
                   AND ttInventoryStock.tag          EQ fg-rdtlh.tag
                   AND ttInventoryStock.jobID        EQ fg-rcpth.job-no
                   AND ttInventoryStock.jobID2       EQ fg-rcpth.job-no2
                   AND ttInventoryStock.warehouseID  EQ fg-rdtlh.loc
                   AND ttInventoryStock.locationID   EQ fg-rdtlh.loc-bin
                   AND ttInventoryStock.customerID   EQ fg-rdtlh.cust-no
                 NO-ERROR.
            IF NOT AVAILABLE ttInventoryStock THEN DO:            
                iInventoryStockSeq = iInventoryStockSeq + 1.
                CREATE ttInventoryStock.
                ASSIGN
                    ttInventoryStock.company                    = fg-rcpth.company
                    ttInventoryStock.jobID                      = fg-rcpth.job-no
                    ttInventoryStock.jobID2                     = fg-rcpth.job-no2
                    ttInventoryStock.warehouseID                = fg-rdtlh.loc
                    ttInventoryStock.locationID                 = fg-rdtlh.loc-bin
                    ttInventoryStock.customerID                 = fg-rdtlh.cust-no
                    ttInventoryStock.fgItemID                   = fg-rcpth.i-no
                    ttInventoryStock.quantityUOM                = gcFGUOM
                    ttInventoryStock.itemType                   = gcItemTypeFG
                    ttInventoryStock.lot                        = fg-rdtlh.stack-code
                    ttInventoryStock.createdTime                = DATETIME(fg-rcpth.trans-date, fg-rdtlh.trans-time)
                    ttInventoryStock.createdBy                  = fg-rcpth.create-by
                    ttInventoryStock.lastTransBy                = fg-rcpth.update-by
                    ttInventoryStock.dimEachUOM                 = gcUOMInches
                    ttInventoryStock.inventoryStockUOM          = gcUOMInches
                    ttInventoryStock.basisWeightUOM             = gcUOMWeightBasis
                    ttInventoryStock.weightUOM                  = gcUOMWeight
                    ttInventoryStock.costStandardMat            = fg-rdtlh.std-mat-cost
                    ttInventoryStock.costStandardLab            = fg-rdtlh.std-lab-cost
                    ttInventoryStock.costStandardVOH            = fg-rdtlh.std-var-cost
                    ttInventoryStock.costStandardFOH            = fg-rdtlh.std-fix-cost
                    ttInventoryStock.sourceID                   = STRING(fg-rdtlh.r-no)
                    ttInventoryStock.sourceType                 = gcInventorySourceTypeFG
                    ttInventoryStock.inventoryStatus            = gcStatusStockInitial
                    ttInventoryStock.primaryID                  = ttInventoryStock.fgItemID
                    ttInventoryStock.inventoryStockID           = STRING(iInventoryStockSeq)
                    ttInventoryStock.tag                        = fg-rdtlh.tag
                    ttInventoryStock.poLine                     = IF ttInventoryStock.poID NE 0 THEN
                                                                      ttInventoryStock.poID
                                                                  ELSE
                                                                      1
                    ttInventoryStock.quantityOriginal           = fg-rdtlh.qty
                    ttInventoryStock.quantityPartialOriginal    = fg-rdtlh.partial
                    ttInventoryStock.quantityOfSubUnitsOriginal = fg-rdtlh.qty-case
                    ttInventoryStock.quantitySubUnitsPerUnit    = fg-rdtlh.stacks-unit
                    ttInventoryStock.quantityOfUnitsOriginal    = TRUNC((fg-rdtlh.qty - fg-rdtlh.partial) / fg-rdtlh.qty-case,0)
                    ttInventoryStock.quantityOfSubUnits         = fg-rdtlh.qty-case
                    .

                ttInventoryStock.poID = INTEGER(fg-rcpth.po-no) NO-ERROR.

                FIND FIRST fg-bin NO-LOCK
                     WHERE fg-bin.company EQ ttInventoryStock.company
                       AND fg-bin.po-no   EQ fg-rcpth.po-no
                       AND fg-bin.i-no    EQ ttInventoryStock.fgItemID
                       AND fg-bin.tag     EQ ttInventoryStock.tag
                       AND fg-bin.job-no  EQ ttInventoryStock.jobID
                       AND fg-bin.job-no2 EQ ttInventoryStock.jobID2
                       AND fg-bin.loc     EQ ttInventoryStock.warehouseID
                       AND fg-bin.loc-bin EQ ttInventoryStock.locationID
                       AND fg-bin.cust-no EQ ttInventoryStock.customerID
                     NO-ERROR.
                IF AVAILABLE fg-bin THEN
                    ASSIGN
                        ttInventoryStock.costStandardMat = fg-bin.std-mat-cost
                        ttInventoryStock.costStandardLab = fg-bin.std-lab-cost
                        ttInventoryStock.costStandardVOH = fg-bin.std-var-cost
                        ttInventoryStock.costStandardFOH = fg-bin.std-fix-cost
                        ttInventoryStock.costUOM         = fg-bin.pur-uom
                        .

                ASSIGN
                    ttInventoryStock.dimEachLen        = itemfg.t-len
                    ttInventoryStock.dimEachWid        = itemfg.t-wid
                    ttInventoryStock.dimEachDep        = itemfg.t-dep
                    ttInventoryStock.inventoryStockLen = itemfg.unitLength
                    ttInventoryStock.inventoryStockWid = itemfg.unitWidth
                    ttInventoryStock.inventoryStockDep = itemfg.unitHeight
                    ttInventoryStock.basisWeight       = itemfg.weight-100
                    ttInventoryStock.costUOM           = itemfg.prod-uom
                    .

                FIND FIRST bf-oe-bolh NO-LOCK
                     WHERE bf-oe-bolh.company EQ fg-rdtlh.company
                       AND bf-oe-bolh.b-no    EQ fg-rcpth.b-no
                     NO-ERROR.
                IF AVAILABLE bf-oe-bolh THEN
                    ttInventoryStock.bolID = STRING(bf-oe-bolh.bol-no).
            END.

            IF ttInventoryStock.poID EQ 0 AND fg-rcpth.po-no NE "" THEN
                ttInventoryStock.poID = INTEGER(fg-rcpth.po-no) NO-ERROR.
                            
            CREATE ttInventoryTransaction.
            ASSIGN
                ttInventoryTransaction.company           = ttInventoryStock.company
                ttInventoryTransaction.createdBy         = ttInventoryStock.createdBy
                ttInventoryTransaction.createdTime       = ttInventoryStock.createdTime
                ttInventoryTransaction.scannedBy         = ttInventoryStock.lastTransBy
                ttInventoryTransaction.scannedTime       = DATETIME(fg-rcpth.upd-date, fg-rdtlh.upd-time)
                ttInventoryTransaction.postedBy          = ttInventoryStock.lastTransBy
                ttInventoryTransaction.postedTime        = DATETIME(fg-rcpth.upd-date, fg-rdtlh.upd-time)
                ttInventoryTransaction.quantityChange    = fg-rdtlh.qty
                ttInventoryTransaction.quantityUOM       = ttInventoryStock.quantityUOM
                ttInventoryTransaction.warehouseID       = ttInventoryStock.warehouseID
                ttInventoryTransaction.locationID        = ttInventoryStock.locationID
                ttInventoryTransaction.transactionTime   = DATETIME(fg-rcpth.trans-date, fg-rdtlh.trans-time)
                ttInventoryTransaction.transactionStatus = gcStatusTransactionPosted
                ttInventoryTransaction.inventoryStockID  = ttInventoryStock.inventoryStockID
                ttInventoryTransaction.tag               = ttInventoryStock.tag
                .
            
            CASE fg-rcpth.rita-code:
                WHEN "R" THEN
                    ttInventoryTransaction.transactionType = gcTransactionTypeReceive.
                WHEN "T" THEN
                    ttInventoryTransaction.transactionType = gcTransactionTypeTransfer.
                WHEN "A" THEN
                    ttInventoryTransaction.transactionType = gcTransactionTypeAdjustQty.
                WHEN "I" THEN
                    ttInventoryTransaction.transactionType = gcTransactionTypeConsume.
                WHEN "S" THEN
                    ttInventoryTransaction.transactionType = gcTransactionTypeShip.
                WHEN "C" THEN
                    ttInventoryTransaction.transactionType = gcTransactionTypeCompare.
                WHEN "E" THEN
                    ttInventoryTransaction.transactionType = gcTransactionTypeReturns.
                OTHERWISE
                    ttInventoryTransaction.transactionType = fg-rcpth.rita-code.
            END CASE.

            IF INDEX("RACT",fg-rcpth.rita-code) GT 0 THEN DO:
                IF fg-rdtlh.qty-case GT 0 AND
                  (ttInventoryStock.quantityOfSubUnits LE 0 OR ttInventoryStock.quantity EQ 0 OR INDEX("AC",fg-rcpth.rita-code) GT 0) THEN
                    ttInventoryStock.quantityOfSubUnits = fg-rdtlh.qty-case.

                IF ttInventoryStock.quantitySubUnitsPerUnit GT 0 AND
                  (ttInventoryStock.quantityOfSubUnits LE 0 OR ttInventoryStock.quantity EQ 0 OR INDEX("AC",fg-rcpth.rita-code) GT 0) THEN
                    ttInventoryStock.quantityOfSubUnits = fg-rdtlh.stacks-unit.
            END.

            IF fg-rcpth.rita-code EQ "C" THEN /* Physical Count */
                ASSIGN
                    ttInventoryStock.quantity        = fg-rdtlh.qty
                    ttInventoryStock.quantityPartial = fg-rdtlh.partial
                    .
            ELSE IF INDEX("RATE",fg-rcpth.rita-code) NE 0 THEN
                ttInventoryStock.quantity = fg-rdtlh.qty + ttInventoryStock.quantity.
            ELSE
                ttInventoryStock.quantity = ttInventoryStock.quantity - fg-rdtlh.qty.

            IF fg-rcpth.rita-code NE "C" THEN DO:
                FIND FIRST fg-rctd NO-LOCK
                     WHERE fg-rctd.r-no EQ fg-rcpth.r-no
                     NO-ERROR.
                IF AVAILABLE fg-rctd AND fg-rctd.partial NE 0 THEN
                    ttInventoryStock.quantityPartial = ttInventoryStock.quantityPartial +
                                                      (fg-rctd.partial * IF INDEX("TS",fg-rcpth.rita-code) GT 0 THEN -1 ELSE 1).
                ELSE IF fg-rdtlh.qty-case NE 0 THEN
                    ttInventoryStock.quantityPartial = ttInventoryStock.quantityPartial +
                                                       ((fg-rdtlh.qty - (fg-rdtlh.cases * fg-rdtlh.qty-case)) *
                                                         IF INDEX("TS",fg-rcpth.rita-code) GT 0 THEN -1 ELSE 1).
            END.

            ttInventoryStock.quantityOfUnits = TRUNC((fg-rdtlh.qty - fg-rdtlh.partial) / fg-rdtlh.qty-case,0).

            IF ttInventoryStock.quantity EQ 0 THEN
                ASSIGN
                    ttInventoryStock.consumedTime    = DATETIME(fg-rcpth.trans-date, fg-rdtlh.trans-time)
                    ttInventoryStock.quantityOfUnits = 0
                    ttInventoryStock.quantityPartial = 0
                    .
            ELSE DO:
                IF ttInventoryStock.quantityOfSubUnits GT 0 THEN
                    ttInventoryStock.quantityPartial = IF ttInventoryStock.quantityPartial LT 0 AND ttInventoryStock.quantity GT ttInventoryStock.quantityPartial * -1 THEN
                                                           ttInventoryStock.quantity - (TRUNC(ttInventoryStock.quantity / ttInventoryStock.quantityOfSubUnits,0) * ttInventoryStock.quantityOfSubUnits)
                                                       ELSE
                                                           ttInventoryStock.quantity - (TRUNC((ttInventoryStock.quantity - ttInventoryStock.quantityPartial) / ttInventoryStock.quantityOfSubUnits,0) * ttInventoryStock.quantityOfSubUnits).

                IF ttInventoryStock.quantityOfSubUnits LE 0 OR
                  (ttInventoryStock.quantity LT ttInventoryStock.quantityOfSubUnits AND ttInventoryStock.quantity GT 0) THEN
                    ASSIGN
                        ttInventoryStock.quantityOfSubUnits = ttInventoryStock.quantity
                        ttInventoryStock.quantityPartial    = 0
                        .
            END.

            ttInventoryStock.lastTransTime = DATETIME(fg-rcpth.trans-date, fg-rdtlh.trans-time).

            RELEASE ttInventoryTransaction.
            RELEASE ttInventoryStock.
        END.
    END.
    RELEASE bf-oe-bolh.        
END PROCEDURE.

PROCEDURE pValidateTTInventory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to create Inventory Stock table records from ttFGBins table 
     Notes:
    ------------------------------------------------------------------------------*/    
    FOR EACH ttInventoryStock:
        IF ttInventoryStock.company EQ "" THEN DO:
            ASSIGN
                ttInventoryStock.valid   = FALSE
                ttInventoryStock.comment = "Empty company"
                .                
            NEXT.
        END.
        
        FIND FIRST company NO-LOCK
             WHERE company.company EQ ttInventoryStock.company
             NO-ERROR.
        IF NOT AVAILABLE company THEN DO:
            ASSIGN
                ttInventoryStock.valid   = FALSE
                ttInventoryStock.comment = "Invalid company"
                .                
            NEXT.        
        END.

        IF ttInventoryStock.fgItemID EQ "" THEN DO:
            ASSIGN
                ttInventoryStock.valid   = FALSE
                ttInventoryStock.comment = "Empty Item number"
                .                
            NEXT.
        END.
    END.
END PROCEDURE.

PROCEDURE pBuildBinsForItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a FG Item, builds bins - will purge and export too 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriFGItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiCountOfProcessed AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplPurge AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcExportPath AS CHARACTER NO-UNDO.
    
    EMPTY TEMP-TABLE ttFGBins.
    EMPTY TEMP-TABLE ttHistDelete.
    IF iplPurge THEN DO:
        RUN pInitializeOutput(ipcExportPath, ipdtAsOf).
    END.
    FIND FIRST itemfg NO-LOCK
        WHERE ROWID(itemfg) EQ ipriFGItem
        NO-ERROR.
    IF AVAILABLE itemfg THEN 
    DO:
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company    EQ itemfg.company
            AND fg-rcpth.i-no       EQ itemfg.i-no
            AND fg-rcpth.trans-date LE ipdtAsOf
            /*
            AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
             GE ""
            AND STRING(FILL(" ",6 - LENGTH(TRIM(fg-rcpth.job-no))) +
                TRIM(fg-rcpth.job-no) + STRING(fg-rcpth.job-no2,"99"))
             LE "ZZZZZZZZZZZZ"
            */
            USE-INDEX tran,
            EACH fg-rdtlh NO-LOCK
            WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
              /*
              AND fg-rdtlh.loc       GE ""
              AND fg-rdtlh.loc       LE "ZZZZZZZZZZZ"
              AND fg-rdtlh.loc-bin   GE ""
              AND fg-rdtlh.loc-bin   LE "ZZZZZZZZZZZ"
              AND fg-rdtlh.cust-no   EQ ""
              */
            BY fg-rcpth.trans-date
            BY fg-rdtlh.trans-time
            BY fg-rcpth.r-no
            :
            FIND FIRST ttFGBins
                WHERE ttFGBins.company EQ fg-rcpth.company
                AND ttFGBins.i-no    EQ fg-rcpth.i-no
                AND ttFGBins.job-no  EQ fg-rcpth.job-no
                AND ttFGBins.job-no2 EQ fg-rcpth.job-no2
                AND ttFGBins.loc     EQ fg-rdtlh.loc
                AND ttFGBins.loc-bin EQ fg-rdtlh.loc-bin
                AND ttFGBins.tag     EQ fg-rdtlh.tag
                AND ttFGBins.cust-no EQ fg-rdtlh.cust-no
                NO-ERROR.
            IF NOT AVAILABLE ttFGBins THEN 
            DO:
                CREATE ttFGBins.
                ASSIGN
                    ttFGBins.company      = fg-rcpth.company
                    ttFGBins.job-no       = fg-rcpth.job-no
                    ttFGBins.job-no2      = fg-rcpth.job-no2
                    ttFGBins.loc          = fg-rdtlh.loc
                    ttFGBins.loc-bin      = fg-rdtlh.loc-bin
                    ttFGBins.tag          = fg-rdtlh.tag
                    ttFGBins.cust-no      = fg-rdtlh.cust-no
                    ttFGBins.i-no         = fg-rcpth.i-no
                    ttFGBins.po-no        = fg-rcpth.po-no
                    ttFGBins.aging-date   = fg-rcpth.trans-date
                    ttFGBins.pur-uom      = (IF fg-rcpth.pur-uom GT "" THEN fg-rcpth.pur-uom ELSE itemfg.prod-uom)
                    ttFGBins.std-tot-cost = fg-rdtlh.std-tot-cost
                    ttFGBins.std-mat-cost = fg-rdtlh.std-mat-cost
                    ttFGBins.std-lab-cost = fg-rdtlh.std-lab-cost
                    ttFGBins.std-var-cost = fg-rdtlh.std-var-cost
                    ttFGBins.std-fix-cost = fg-rdtlh.std-fix-cost
                    ttFGBins.rita-code    = fg-rcpth.rita-code
                    ttFGBins.itemFGCustNo = itemfg.cust-no
                    ttFGBins.itemFGINo    = itemfg.i-no
                    ttFGBins.itemFGPartNo = itemfg.part-no
                    ttFGBins.itemFGIName  = itemfg.i-name
                    .
                FIND FIRST uom NO-LOCK
                    WHERE uom.uom  EQ itemfg.prod-uom
                    AND uom.mult NE 0
                    NO-ERROR.
                ttFGBins.uomMult = IF AVAILABLE uom THEN uom.mult ELSE ?.
                FIND FIRST fg-bin NO-LOCK
                    WHERE fg-bin.company EQ ttFGBins.company
                    AND fg-bin.i-no    EQ ttFGBins.i-no
                    AND fg-bin.job-no  EQ ttFGBins.job-no
                    AND fg-bin.job-no2 EQ ttFGBins.job-no2
                    AND fg-bin.loc     EQ ttFGBins.loc
                    AND fg-bin.loc-bin EQ ttFGBins.loc-bin
                    AND fg-bin.tag     EQ ttFGBins.tag
                    AND fg-bin.cust-no EQ ttFGBins.cust-no
                    NO-ERROR.
                IF AVAILABLE fg-bin THEN
                    ASSIGN
                        ttFGBins.std-tot-cost = fg-bin.std-tot-cost
                        ttFGBins.std-mat-cost = fg-bin.std-mat-cost
                        ttFGBins.std-lab-cost = fg-bin.std-lab-cost
                        ttFGBins.std-var-cost = fg-bin.std-var-cost
                        ttFGBins.std-fix-cost = fg-bin.std-fix-cost
                        .
            END.  /*create new ttFGBins*/
            CASE fg-rcpth.rita-code:
                WHEN "S" OR 
                WHEN "s" THEN
                    ttFGBins.qty = ttFGBins.qty - fg-rdtlh.qty.
                           
                WHEN "C" OR 
                WHEN "c" THEN 
                    ttFGBins.qty = fg-rdtlh.qty.
                WHEN "R" OR 
                WHEN "r" THEN 
                    DO:
                        ASSIGN 
                            ttFGBins.qty          = ttFGBins.qty + fg-rdtlh.qty
                            ttFGBins.aging-date   = fg-rcpth.trans-date
                            ttFGBins.units-pallet = fg-rdtlh.stacks-unit                            
                            ttFGBins.case-count   = fg-rdtlh.qty-case
                            .
                        IF ttFGBins.aging-date EQ ? OR
                            ttFGBins.aging-date GT fg-rcpth.trans-date THEN
                            ttFGBins.aging-date = fg-rcpth.trans-date.
                    END.
                OTHERWISE 
                ttFGBins.qty = ttFGBins.qty + fg-rdtlh.qty.
            END CASE.
            
            IF ttFGBins.case-count LE 0 AND fg-rdtlh.qty-case GT 0 THEN
                ttFGBins.case-count = fg-rdtlh.qty-case.
            IF ttFGBins.units-pallet LE 0 AND fg-rdtlh.stacks-unit GT 0 THEN
                ttFGBins.units-pallet = fg-rdtlh.stacks-unit.
            IF ttFGBins.cases LE 0 AND fg-rdtlh.cases GT 0 THEN
                ttFGBins.cases = fg-rdtlh.cases.    

            IF ttFGBins.qty EQ 0 THEN 
            DO:
                DELETE ttFGBins.
            END.
            iopiCountOfProcessed = iopiCountOfProcessed + 1.
            IF iplPurge THEN DO: 
                /*Store records to be deleted.  This is to work around the delete trigger killing one side of transfer transaction during this process*/
                CREATE ttHistDelete.
                ASSIGN 
                    ttHistDelete.riFGRcpth = ROWID(fg-rcpth)
                    ttHistDelete.riFGRdtlh = ROWID(fg-rdtlh)
                    .
            END.
        END.  /*each history record */
    END.
    IF iplPurge THEN DO:
        /*Run all deletions and export each deleted record*/
        FOR EACH ttHistDelete:
            RUN pPurgeAndExport(ttHistDelete.riFGRcpth, ttHistDelete.riFGRdtlh).
        END.
        OUTPUT STREAM sExport1 CLOSE.
        OUTPUT STREAM sExport2 CLOSE.
    END.
        
END PROCEDURE.

PROCEDURE pInitializeOutput PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcExportPath AS CHARACTER.
    DEFINE INPUT PARAMETER ipdtAsOf AS DATE.
    
    IF ipcExportPath EQ "" THEN DO:
        OS-CREATE-DIR  VALUE(gcDefaultExportPath).
        ipcExportPath = gcDefaultExportPath.
    END.
    ASSIGN 
        gcOutputFile1 = ipcExportPath + "/fg-rcpth" + STRING(ipdtAsOf,"99-99-9999") + ".d"
        gcOutputFile2 = ipcExportPath + "/fg-rdtlh" + STRING(ipdtAsOf,"99-99-9999") + ".d"
        .
     OUTPUT STREAM sExport1 to VALUE(gcOutputFile1) APPEND.
     OUTPUT STREAM sExport2 to VALUE(gcOutputFile2) APPEND.
     
END PROCEDURE.

PROCEDURE pPurgeAndExport PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriFGRcpth AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipriFGRdtlh AS ROWID NO-UNDO.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.

    FIND FIRST bf-fg-rdtlh EXCLUSIVE-LOCK
        WHERE ROWID(bf-fg-rdtlh) EQ ipriFGRdtlh
        NO-ERROR.
    IF AVAILABLE bf-fg-rdtlh THEN 
        EXPORT STREAM sExport2 bf-fg-rdtlh.
    /*Delete of fg-rdtlh handled in delete trigger of fg-rctph*/
    
    FIND FIRST bf-fg-rcpth EXCLUSIVE-LOCK
        WHERE ROWID(bf-fg-rcpth) EQ ipriFGRcpth
        NO-ERROR.
    IF AVAILABLE bf-fg-rcpth THEN DO:
        EXPORT STREAM sExport1 bf-fg-rcpth.
        DELETE bf-fg-rcpth.
    END.

    
END PROCEDURE.

