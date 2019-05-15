
/*------------------------------------------------------------------------
    File        : InventoryProcs.p
    Purpose     : 

    Syntax      :

    Description : All procedures for creating and printing Loadtags for FG, RM, and WIP

    Author(s)   : BV
    Created     : Sun Mar 03 18:31:30 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{Inventory/ttInventory.i SHARED}

DEFINE VARIABLE giLengthUniquePrefix       AS INTEGER   INITIAL 20.
DEFINE VARIABLE giLengthAlias              AS INTEGER   INITIAL 25.

DEFINE VARIABLE giIDTemp                   AS INTEGER. /*TESTING ONLY DELETE BEFORE COMMIT*/

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

FUNCTION fCanDeleteInventoryStock RETURNS LOGICAL 
	(ipcInventoryStockID AS CHARACTER) FORWARD.

FUNCTION fGetNextStockAliasID RETURNS INTEGER PRIVATE
    (  ) FORWARD.

FUNCTION fGetNextSnapshotID RETURNS INTEGER PRIVATE
    (  ) FORWARD.

FUNCTION fGetNextStockIDAlias RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcUniquePrefix AS CHARACTER) FORWARD.

FUNCTION fGetNextStockID RETURNS CHARACTER PRIVATE
    (ipcType AS CHARACTER) FORWARD.

FUNCTION fGetNextTransactionID RETURNS INTEGER PRIVATE
    (  ) FORWARD.

FUNCTION fGetNumberSuffix RETURNS INTEGER PRIVATE
    (ipcFullText AS CHARACTER,
    ipiStartChar AS INTEGER) FORWARD.

FUNCTION fGetSnapshotCompareStatus RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
     ipcInventoryStockID AS CHARACTER,
     ipdQuantity AS DECIMAL,
     ipcWarehouseID AS CHARACTER,
     ipcLocationID AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */

PROCEDURE CheckInventoryStockIDAlias:
    /*------------------------------------------------------------------------------
     Purpose: Checks to see if passed ID is an alias or a true stock ID
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLookupID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcStockIDAlias AS CHARACTER NO-UNDO.

    FIND FIRST inventoryStockAlias NO-LOCK 
        WHERE inventoryStockAlias.company EQ ipcCompany
        AND inventoryStockAlias.stockIDAlias EQ ipcLookupID
        NO-ERROR.
    IF AVAILABLE inventoryStockAlias THEN 
        ASSIGN
            opcInventoryStockID = inventoryStockAlias.inventoryStockID
            opcStockIDAlias     = inventoryStockAlias.stockIDAlias
            . 
    ELSE DO:
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company     EQ ipcCompany
               AND loadtag.tag-no      EQ ipcLookupID NO-ERROR.
        IF AVAILABLE loadtag THEN
            ASSIGN
                opcInventoryStockID = ""
                opcStockIDAlias     = ipcLookupID
                .
        ELSE       
            ASSIGN
                opcInventoryStockID = ipcLookupID
                opcStockIDAlias     = ""
                . 
    END.

END PROCEDURE.

PROCEDURE CreateInventoryStockFromLoadtag:
    /*------------------------------------------------------------------------------
     Purpose: Given existing ttInventoryStockLoadtag table, generate the "Actual"
     inventory with original quantity transfering to .quantity and registering the status as
     received.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCreateReceipt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
       
    /*Copy Loadtags to inventoryStock*/
    FIND FIRST ttInventoryStockLoadtag NO-LOCK
        WHERE ttInventoryStockLoadtag.inventoryStockID EQ ipcInventoryStockID
        NO-ERROR. 
    IF AVAILABLE ttInventoryStockLoadtag THEN 
    RUN pCreateStockFromLoadtag(BUFFER ttInventoryStockLoadtag, iplCreateReceipt, iplPost, OUTPUT oplCreated, OUTPUT opcMessage).        
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Inventory Stock ID"
            .
                  

END PROCEDURE.

PROCEDURE CreateInventoryStockFromInputsFG:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriFgBin     AS ROWID     NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSnapshotID AS INTEGER   NO-UNDO.  
    DEFINE OUTPUT PARAMETER oplCreated    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    
    FIND FIRST bf-fg-bin NO-LOCK 
        WHERE ROWID(bf-fg-bin) EQ ipriFgBin
        NO-ERROR.
    IF AVAILABLE bf-fg-bin THEN 
    DO: 

        CREATE inventoryStockSnapshot.
        ASSIGN
            inventoryStockSnapshot.inventoryStockID        = fGetNextStockID(gcItemTypeFG)
            inventoryStockSnapshot.company                 = bf-fg-bin.company
            inventoryStockSnapshot.jobID                   = bf-fg-bin.job-no
            inventoryStockSnapshot.jobID2                  = bf-fg-bin.job-no2
            inventoryStockSnapshot.inventoryStatus         = gcStatusSnapshotNotScanned
            inventoryStockSnapshot.itemType                = gcItemTypeFG
            inventoryStockSnapshot.fgItemID                = bf-fg-bin.i-no
            inventoryStockSnapshot.warehouseID             = bf-fg-bin.loc
            inventoryStockSnapshot.locationID              = bf-fg-bin.loc-bin
            inventoryStockSnapshot.orderID                 = bf-fg-bin.ord-no
            inventoryStockSnapshot.customerID              = bf-fg-bin.cust-no
            inventoryStockSnapshot.dimEachUOM              = "IN"
            inventoryStockSnapshot.quantityUOM             = bf-fg-bin.pur-uom
            inventoryStockSnapshot.costStandardPerUOM      = bf-fg-bin.std-tot-cost
            inventoryStockSnapshot.quantity                = bf-fg-bin.qty
            inventoryStockSnapshot.quantityPerSubUnit      = bf-fg-bin.qty
            inventoryStockSnapshot.lastTransTime           = NOW
            inventoryStockSnapshot.createdTime             = NOW
            inventoryStockSnapshot.lastTransBy             = USERID("asi")
            inventoryStockSnapshot.createdBy               = USERID("asi")
            inventoryStockSnapshot.inventorySnapshotID     = ipiSnapshotID
            inventoryStockSnapshot.sourceID                = inventoryStockSnapshot.inventoryStockID
            inventoryStockSnapshot.sourceType              = gcSourceTypeSnapshot
            inventoryStockSnapshot.primaryID               = inventoryStockSnapshot.fgItemID
            inventoryStockSnapshot.stockIDAlias            = bf-fg-bin.tag
            oplCreated                                     = TRUE
            opcMessage                                     = "Inventory Stock Created for " + inventoryStockSnapshot.inventoryStockID
            .
            
                    
        FIND FIRST bf-itemfg NO-LOCK 
             WHERE bf-itemfg.company EQ bf-fg-bin.company
               AND bf-itemfg.i-no    EQ bf-fg-bin.i-no
               NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
            ASSIGN 
                inventoryStockSnapshot.basisWeight    = bf-itemfg.weight-100
                inventoryStockSnapshot.basisWeightUOM = "LBS/100"
                .
                        
        IF bf-fg-bin.tag NE "" THEN 
            RUN CreateStockIDAlias (
                inventoryStockSnapshot.company, 
                inventoryStockSnapshot.inventoryStockID,
                inventoryStockSnapshot.primaryID, 
                bf-fg-bin.tag,
                OUTPUT oplCreated, 
                OUTPUT opcMessage
                ). 
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Finished Good" 
            .    


END PROCEDURE.

PROCEDURE CreatePreLoadtagsFromInputsRM:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

END PROCEDURE.

PROCEDURE CreatePreLoadtagsFromInputsWIP:
    /*------------------------------------------------------------------------------
     Purpose:  Given critical inputs for WIP process, generate the Pre-Loadtags
     for processing.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriJobMch AS ROWID NO-UNDO.  /*Last Operation*/
    DEFINE INPUT PARAMETER ipriJobMat AS ROWID NO-UNDO.  /*Board Material*/
    DEFINE INPUT PARAMETER ipdQuantityTotal AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cDefaultLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.

    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item    FOR item.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    EMPTY TEMP-TABLE ttInventoryStockPreLoadtag.
    FIND FIRST bf-job-mch NO-LOCK 
        WHERE ROWID(bf-job-mch) EQ ipriJobMch
        NO-ERROR.
    FIND FIRST bf-job-mat NO-LOCK 
        WHERE ROWID(bf-job-mat) EQ ipriJobMat
        NO-ERROR.
    IF AVAILABLE bf-job-mch AND AVAILABLE bf-job-mat THEN 
    DO:
        CREATE ttInventoryStockPreLoadtag.
        ASSIGN 
            ttInventoryStockPreLoadtag.company                 = bf-job-mch.company
            ttInventoryStockPreLoadtag.machineID               = bf-job-mch.m-code
            ttInventoryStockPreLoadtag.jobID                   = bf-job-mch.job-no
            ttInventoryStockPreLoadtag.jobID2                  = bf-job-mch.job-no2
            ttInventoryStockPreLoadtag.formNo                  = bf-job-mch.frm
            ttInventoryStockPreLoadtag.blankNo                 = bf-job-mch.blank-no
            ttInventoryStockPreLoadtag.passNo                  = bf-job-mch.pass
            ttInventoryStockPreLoadtag.inventoryStatus         = gcStatusStockPreLoadtag
            ttInventoryStockPreLoadtag.itemType                = gcItemTypeWIP
            ttInventoryStockPreLoadtag.rmItemID                = bf-job-mat.rm-i-no
            ttInventoryStockPreLoadtag.dimEachLen              = bf-job-mat.len
            ttInventoryStockPreLoadtag.dimEachWid              = bf-job-mat.wid
            ttInventoryStockPreLoadtag.dimEachDep              = bf-job-mat.dep
            ttInventoryStockPreLoadtag.dimEachUOM              = "IN"
            ttInventoryStockPreLoadtag.quantityTotal           = ipdQuantityTotal
            ttInventoryStockPreLoadtag.quantityUOM             = ipcQuantityUOM
            ttInventoryStockPreLoadtag.quantitySubUnitsPerUnit = ipiQuantitySubUnitsPerUnit
            ttInventoryStockPreLoadtag.quantityPerSubUnit      = ipdQuantityPerSubUnit
            ttInventoryStockPreLoadtag.lastTransTime           = NOW
            ttInventoryStockPreLoadtag.lastTransBy             = USERID("asi").
        RUN pGetWIPID(BUFFER ttInventoryStockPreLoadtag, OUTPUT ttInventoryStockPreLoadtag.wipItemID).
        ttInventoryStockPreLoadtag.primaryID = ttInventoryStockPreLoadtag.wipItemID.
        RUN RecalcQuantityUnits(ipdQuantityTotal, INPUT-OUTPUT ttInventoryStockPreLoadtag.quantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.quantitySubUnitsPerUnit, 
            OUTPUT ttInventoryStockPreLoadtag.quantityOfSubUnits, OUTPUT ttInventoryStockPreLoadtag.quantityOfUnits, OUTPUT ttInventoryStockPreLoadtag.quantityPartial).
            
        FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ bf-job-mat.company
            AND bf-item.i-no EQ bf-job-mat.rm-i-no
            NO-ERROR.
        IF AVAILABLE bf-item THEN 
            ASSIGN 
                ttInventoryStockPreLoadtag.basisWeight    = bf-item.basis-w
                ttInventoryStockPreLoadtag.basisWeightUOM = "LBS/MSF"
                .
        FIND FIRST bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ bf-job-mch.company
            AND bf-job-hdr.job EQ bf-job-mch.job
            AND bf-job-hdr.job-no EQ bf-job-mch.job-no
            AND bf-job-hdr.job-no2 EQ bf-job-mch.job-no2
            AND (bf-job-hdr.frm EQ bf-job-mch.frm OR bf-job-hdr.frm EQ 0)
            AND (bf-job-hdr.blank-no EQ bf-job-mch.blank-no OR bf-job-mch.blank-no EQ 0)
            NO-ERROR.
        IF AVAILABLE bf-job-hdr THEN 
            ASSIGN 
                ttInventoryStockPreLoadtag.fgItemID    = bf-job-hdr.i-no
                ttInventoryStockPreLoadtag.warehouseID = bf-job-hdr.loc
                ttInventoryStockPreLoadtag.orderID     = bf-job-hdr.ord-no
                ttInventoryStockPreLoadtag.customerID  = bf-job-hdr.cust-no
                .
        RUN sys/ref/nk1look.p (
            bf-job-mch.company,"WIPTAGSDefaultLocation","C",NO,NO,"","",
            OUTPUT cDefaultLocation,OUTPUT lFound
            ).
        IF lFound THEN
        ttInventoryStockPreLoadtag.locationID = cDefaultLocation.
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Machine or Material Inputs" 
            .    

END PROCEDURE.

PROCEDURE CreatePrintInventory:
    /*------------------------------------------------------------------------------
     Purpose: Creates temporary table to send data to a text file
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID LIKE inventoryTransaction.inventoryStockID NO-UNDO.
    
    DEFINE VARIABLE cCustName LIKE oe-ord.cust-name NO-UNDO.
    DEFINE VARIABLE cMachName LIKE mach.m-dscr      NO-UNDO.
    DEFINE VARIABLE cItemName LIKE item.i-name      NO-UNDO.
    
    FIND FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.inventoryStockID = ipcinventoryStockID
         NO-ERROR.
    IF AVAILABLE inventoryStock THEN DO:
        CREATE ttPrintInventoryStock.
        BUFFER-COPY inventoryStock TO ttPrintInventoryStock.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company = inventoryStock.company AND
                   oe-ord.ord-no  = inventoryStock.orderID AND
                   oe-ord.cust-no = inventoryStock.customerID NO-ERROR.
        IF AVAILABLE oe-ord THEN
            ASSIGN cCustName = oe-ord.cust-name.
        ELSE DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company = inventoryStock.company AND
                      cust.cust-no = inventoryStock.customerID  NO-ERROR.
            IF AVAILABLE cust THEN
                ASSIGN cCustName = cust.name.
        END.
                 
        FIND FIRST mach NO-LOCK
             WHERE mach.company = inventoryStock.company AND
                   mach.m-code  = inventoryStock.machineID NO-ERROR.
        IF AVAILABLE mach THEN
            ASSIGN cMachName = mach.m-dscr.
        
        FIND FIRST item NO-LOCK
             WHERE item.company = inventoryStock.company AND
                   item.i-no    = inventoryStock.rmItemID NO-ERROR.
        IF AVAILABLE item THEN
            ASSIGN cItemName = item.i-name.
            
        ASSIGN
            ttPrintInventoryStock.jobNumber    = LEFT-TRIM(TRIM(inventoryStock.jobID))
            ttPrintInventoryStock.jobNumber    = FILL(" ",6 - LENGTH(ttPrintInventoryStock.jobNumber)) + ttPrintInventoryStock.jobNumber
            ttPrintInventoryStock.jobRunNumber = inventoryStock.jobID2
            ttPrintInventoryStock.jobID        = ttPrintInventoryStock.jobNumber + "-" + STRING(ttPrintInventoryStock.jobRunNumber,"99")
            ttPrintInventoryStock.jobIDTrimmed = LEFT-TRIM(ttPrintInventoryStock.jobID)
            ttPrintInventoryStock.jobIDFull    = ttPrintInventoryStock.jobID + "." + STRING(inventoryStock.formNo,"99") + "." + STRING(inventoryStock.blankNo,"99")
            ttPrintInventoryStock.jobIDFullTrimmed = LEFT-TRIM(ttPrintInventoryStock.jobIDFull) 
            ttPrintInventoryStock.customerName = cCustName
            ttPrintInventoryStock.machineName  = cMachName
            ttPrintInventoryStock.rmItemName   = cItemName.
    END.

END PROCEDURE.

PROCEDURE DeleteInventoryStock:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    
    DO TRANSACTION:
        FIND FIRST inventoryStock EXCLUSIVE-LOCK
             WHERE inventoryStock.inventoryStockID EQ ipcInventoryStockID
             NO-ERROR.
        IF AVAILABLE inventoryStock THEN
        DELETE inventoryStock.
    END. /* do trans */

END PROCEDURE.

PROCEDURE DeleteInventoryTransaction:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiTransactionID AS INTEGER NO-UNDO.
    
    DO TRANSACTION:
        FIND FIRST inventoryTransaction EXCLUSIVE-LOCK
             WHERE inventoryTransaction.inventoryTransactionID EQ ipiTransactionID
             NO-ERROR.
        IF AVAILABLE inventoryTransaction THEN
            DELETE inventoryTransaction.
    END. /* do trans */

END PROCEDURE.


PROCEDURE PostReceivedInventory:
    /*------------------------------------------------------------------------------
     Purpose: Change status of inventory stock from pending to posted.
     Notes: 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany          LIKE inventoryStock.company                NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID LIKE inventoryTransaction.inventoryStockID NO-UNDO.
       
    FIND FIRST inventoryTransaction NO-LOCK
         WHERE inventoryTransaction.company          = ipcCompany AND
               inventoryTransaction.inventoryStockID = ipcInventoryStockID
         NO-ERROR.
    IF AVAILABLE inventoryTransaction THEN
        RUN PostTransaction(inventoryTransactionID).
    
END PROCEDURE.

PROCEDURE CreateTransactionInitialized:
    /*------------------------------------------------------------------------------
     Purpose: Creates the inventoryStock
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno                   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2                  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno                  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno                 AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityTotal           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit      AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                 AS CHARACTER NO-UNDO.
    
    FIND FIRST job-mch NO-LOCK
         WHERE job-mch.company  EQ ipcCompany
           AND job-mch.job-no   EQ ipcJobno
           AND job-mch.job-no2  EQ ipiJobno2
           AND job-mch.m-code   EQ ipcMachine
           AND job-mch.frm      EQ ipiFormno
           AND job-mch.blank-no EQ ipiBlankno
         NO-ERROR.
    IF NOT AVAILABLE job-mch THEN
        RETURN.

    FIND FIRST job-mat NO-LOCK  
         WHERE job-mat.company  EQ job-mch.company
           AND job-mat.job-no   EQ job-mch.job-no
           AND job-mat.job-no2  EQ job-mch.job-no2
           AND job-mat.frm      EQ job-mch.frm
         NO-ERROR.  
    IF NOT AVAILABLE job-mat THEN
        RETURN.

    RUN CreatePreLoadtagsFromInputsWIP (
        ROWID(job-mch),
        ROWID(job-mat), 
        ipdQuantityTotal,
        ipdQuantityPerSubUnit,
        ipiQuantitySubUnitsPerUnit,
        ipcQuantityUOM,
        OUTPUT oplCreated,
        OUTPUT opcMessage
        ).

    RUN CreateInventoryLoadtagsFromPreLoadtags.

    FOR EACH ttInventoryStockLoadtag:
        ASSIGN 
            oplCreated = NO
            opcMessage = "". 
        RUN CreateInventoryStockFromLoadtag (
            ttInventoryStockLoadtag.inventoryStockID,
            YES,
            NO,
            OUTPUT oplCreated,
            OUTPUT opcMessage
            ).
    END.   

END PROCEDURE.

PROCEDURE CreateTransactionReceived:
    /*------------------------------------------------------------------------------
     Purpose: Given the Loadtag buffer, create the Stock inventory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE iInventoryTransactionID AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-inventoryStock FOR inventoryStock.
    
    FIND FIRST bf-inventoryStock NO-LOCK
         WHERE bf-inventoryStock.company = ipcCompany 
           AND bf-inventoryStock.inventoryStockID = ipcInventoryStockID NO-ERROR.
    IF AVAILABLE bf-inventoryStock THEN DO:
        RUN pCreateTransactionAndReturnID(bf-inventoryStock.company, bf-inventoryStock.inventoryStockID, gcTransactionTypeReceive, 
                bf-inventoryStock.quantityOriginal, bf-inventoryStock.quantityUOM, bf-inventoryStock.warehouseID, bf-inventoryStock.locationID, 
                OUTPUT iInventoryTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
        IF iplPost THEN 
            RUN PostTransaction(iInventoryTransactionID).
    END.
    RELEASE bf-inventoryStock.
    
END PROCEDURE.

PROCEDURE CreateTransactionTransfer:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function to create a transfer
     Notes: 0 quantity transaction
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iTransactionID AS INTEGER NO-UNDO.

    RUN pCreateTransactionAndReturnID(ipcCompany, ipcInventoryStockID, gcTransactionTypeTransfer, 0, "", ipcWarehouseID, ipcLocationID, 
        OUTPUT iTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
    IF iplPost THEN 
        RUN PostTransaction(iTransactionID).

END PROCEDURE.

PROCEDURE CreateTransactionCompare:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function to create a compare transaction
     Notes: 0 quantity transaction
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStockIDAlias     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity         AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplPost             AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated          AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iTransactionID AS INTEGER NO-UNDO.
    
    FIND FIRST inventoryTransaction EXCLUSIVE-LOCK
         WHERE inventoryTransaction.company      = ipcCompany 
           AND inventoryTransaction.stockIDAlias = ipcStockIDAlias NO-ERROR.
           
    IF AVAILABLE inventoryTransaction THEN DO:
        ASSIGN
            inventoryTransaction.quantityChange         = ipdQuantity
            inventoryTransaction.quantityUOM            = ipcQuantityUOM
            inventoryTransaction.warehouseID            = ipcWarehouseID
            inventoryTransaction.locationID             = ipcLocationID
            inventoryTransaction.scannedTime            = NOW
            inventoryTransaction.scannedBy              = USERID("asi")
            oplCreated                                  = TRUE
            opcMessage                                  = "Transaction Updated"
            .
            
        RETURN.       
    END.
           
    RUN pCreateTransactionAndReturnID(ipcCompany, ipcStockIDAlias, gcTransactionTypeCompare, ipdQuantity, ipcQuantityUOM, ipcWarehouseID, ipcLocationID, 
        OUTPUT iTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).    
            
             
END PROCEDURE.

PROCEDURE CreateTransactionConsume:
    /*------------------------------------------------------------------------------
     Purpose: Wrapper function to create an Issue/Consume transaction
     Notes: No location 
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityConsumed AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iTransactionID AS INTEGER NO-UNDO.

    RUN pCreateTransactionAndReturnID(ipcCompany, ipcInventoryStockID, gcTransactionTypeConsume, - ipdQuantityConsumed, ipcQuantityUOM, "", "", 
        OUTPUT iTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
    IF iplPost THEN 
        RUN PostTransaction(iTransactionID).
    
END PROCEDURE.

PROCEDURE GenerateSnapshotRecords:
    /*------------------------------------------------------------------------------
     Purpose: Generate Snapshot records
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcType       AS CHARACTER NO-UNDO. /* FG, RM, WIP */
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouse  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iInventorySnapshotID  AS INTEGER   NO-UNDO.
    
    IF ipcType EQ "FG" THEN DO:
        IF CAN-FIND ( FIRST fg-bin NO-LOCK
                      WHERE fg-bin.company    EQ ipcCompany
                        AND fg-bin.loc        EQ ipcWarehouse
                        AND fg-bin.loc-bin    EQ ipcLocation
                        AND fg-bin.qty        NE 0
                        AND fg-bin.tag        NE "" ) THEN
            RUN pCreateSnapshotAndReturnID (
                ipcCompany,
                gcSnapshotTypeCount,
                ipcWarehouse,
                ipcLocation,
                gcSourceTypeSnapshot,
                gcItemTypeFG,
                OUTPUT iInventorySnapshotID,
                OUTPUT oplCreated,
                OUTPUT opcMessage
                ).
            

        FOR EACH fg-bin NO-LOCK
            WHERE fg-bin.company    EQ ipcCompany
              AND fg-bin.loc        EQ ipcWarehouse
              AND fg-bin.loc-bin    EQ ipcLocation
              AND fg-bin.qty        NE 0
              AND fg-bin.tag        NE "":
            RUN CreateInventoryStockFromInputsFG (
                ROWID(fg-bin),
                iInventorySnapshotID,
                OUTPUT oplCreated,
                OUTPUT opcMessage
                ).
        END.
    END.
    
END PROCEDURE.

PROCEDURE RebuildWIPBrowseTT:
    /*------------------------------------------------------------------------------
     Purpose: Rebuilds browse temp-table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno      AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcMachine    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormno     AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankno    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTotTags    AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opiTotOnHand  AS INTEGER   NO-UNDO.
    
    EMPTY TEMP-TABLE ttBrowseInventory.
    
    FOR EACH inventoryStock NO-LOCK
       WHERE inventoryStock.company   EQ ipcCompany
         AND inventoryStock.jobID     EQ ipcJobno
         AND inventoryStock.jobID2    EQ ipiJobno2   
         AND (IF ipcMachine           EQ "" THEN TRUE 
              ELSE inventoryStock.MachineID EQ ipcMachine)
         AND inventoryStock.formNo    EQ ipiFormno   
         AND inventoryStock.blankNo   EQ ipiBlankno
        :
        CREATE ttBrowseInventory.
        BUFFER-COPY inventoryStock EXCEPT inventoryStock.locationID TO ttBrowseInventory.
        ASSIGN
            ttBrowseinventory.locationID = inventoryStock.warehouseID +
                                           FILL(" ", 5 - LENGTH(inventoryStock.warehouseID)) +
                                           inventoryStock.locationID
            opiTotTags                   = opiTotTags + 1.
         
        IF inventoryStock.inventoryStatus EQ gcStatusStockReceived THEN
            opiTotOnHand = opiTotOnHand + 1.
        
    END.
        
END PROCEDURE.

PROCEDURE pCreateTransactionAndReturnID PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTransactionType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdQuantityChange AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiInventoryTransactionID AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    CREATE inventoryTransaction.
    ASSIGN 
        /*inventoryTransaction.rec_key          = DYNAMIC-FUNCTION("sfGetNextRecKey")*/
        inventoryTransaction.inventoryTransactionID = fGetNextTransactionID()
        opiInventoryTransactionID                   = inventoryTransaction.inventoryTransactionID
        inventoryTransaction.transactionType        = ipcTransactionType
        inventoryTransaction.company                = ipcCompany
        inventoryTransaction.createdBy              = USERID("asi")
        inventoryTransaction.createdTime            = NOW
        inventoryTransaction.scannedBy              = USERID("asi")
        inventoryTransaction.scannedTime            = NOW
        inventoryTransaction.quantityChange         = ipdQuantityChange
        inventoryTransaction.quantityUOM            = ipcQuantityUOM
        inventoryTransaction.warehouseID            = ipcWarehouseID
        inventoryTransaction.locationID             = ipcLocationID
        inventoryTransaction.transactionTime        = inventoryTransaction.createdTime  /*Default to Created Time, Not Posted*/
        inventoryTransaction.transactionStatus      = gcStatusTransactionInitial
        oplCreated                                  = YES
        opcMessage                                  = "Transaction Created.  ID: " + STRING(opiInventoryTransactionID)
        .
    RUN CheckInventoryStockIDAlias(ipcCompany, ipcInventoryStockID, OUTPUT inventoryTransaction.inventoryStockID, OUTPUT inventoryTransaction.stockIDAlias).
    
    RELEASE inventoryTransaction.

END PROCEDURE.

PROCEDURE pCreateSnapshotAndReturnID PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSnapshotType    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouse       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocation        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStatus AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiSnapshotID      AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage         AS CHARACTER NO-UNDO.
        
    CREATE inventorySnapshot.
    ASSIGN 
        inventorySnapshot.inventorySnapshotID  = fGetNextSnapshotID()
        inventorySnapshot.snapshotType         = ipcSnapshotType
        inventorySnapshot.itemType             = ipcItemType
        inventorySnapshot.company              = ipcCompany
        inventorySnapshot.warehouseID          = ipcWarehouse
        inventorySnapshot.locationID           = ipcLocation
        inventorySnapshot.inventoryStockStatus = ipcInventoryStatus
        inventorySnapshot.snapshotUser         = USERID("asi")
        inventorySnapshot.snapshotTime         = NOW
        oplCreated                             = YES
        opcMessage                             = "Snapshot Created. ID: " + STRING(inventorySnapshot.inventorySnapshotID)
        opiSnapshotID                          = inventorySnapshot.inventorySnapshotID
        .
    
END PROCEDURE.

PROCEDURE pAddQuantity PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds additional quantity to base quantity.  Converts UOM if necessary
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdQuantityChange AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityChangeUOM AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ipdQuantityExisting AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcQuantityExistingUOM AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dQuantityChangeInExistingUOM AS DECIMAL.

    IF ipcQuantityChangeUOM NE ipcQuantityExistingUOM THEN 
    DO:
        dQuantityChangeInExistingUOM = ipdQuantityChange.
    /*ConvertUOM and add*/
    END.
    ELSE 
        dQuantityChangeInExistingUOM = ipdQuantityChange.
    
    ipdQuantityExisting = ipdQuantityExisting + dQuantityChangeInExistingUOM.

END PROCEDURE.

PROCEDURE pCreateLoadtagFromPreLoadtag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given Buffer for pre-loadtag and overall quantity, 
        create the Loadtag 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInventoryStockPreLoadtag FOR ttInventoryStockPreLoadtag.
    DEFINE INPUT PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
 
    DEFINE VARIABLE lAliasCreated       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cAliasCreateMessage AS CHARACTER NO-UNDO.
    
    CREATE ttInventoryStockLoadtag.
    BUFFER-COPY ipbf-ttInventoryStockPreLoadtag TO ttInventoryStockLoadtag.
    ASSIGN 
        /*        ttInventoryStockLoadtag.rec_key          = DYNAMIC-FUNCTION("sfGetNextRecKey")*/
        ttInventoryStockLoadtag.inventoryStockID = fGetNextStockID(ttInventoryStockLoadtag.itemType) /*Unique ID*/
        ttInventoryStockLoadtag.quantityOriginal = ipdQuantity
        ttInventoryStockLoadtag.inventoryStatus  = gcStatusStockLoadtag
        .
    /*Ensure the partial and unit counts are calculated correctly for this specific quantity*/
    RUN RecalcQuantityUnits(ttInventoryStockLoadtag.quantityOriginal, 
        INPUT-OUTPUT ttInventoryStockLoadtag.quantityPerSubUnit, INPUT-OUTPUT ttInventoryStockLoadtag.quantitySubUnitsPerUnit,
        OUTPUT ttInventoryStockLoadtag.quantityOfSubUnits, OUTPUT ttInventoryStockLoadtag.quantityOfUnits, OUTPUT ttInventoryStockLoadtag.quantityPartial).
    
    /*Build Readable Tag Number and register it on Alias table*/
    ttInventoryStockLoadtag.stockIDAlias = fGetNextStockIDAlias(ttInventoryStockLoadtag.company, ttInventoryStockLoadtag.primaryID). 
    RUN CreateStockIDAlias(ttInventoryStockLoadtag.company, ttInventoryStockLoadtag.inventoryStockID, ttInventoryStockLoadtag.primaryID, ttInventoryStockLoadtag.stockIDAlias,
        OUTPUT lAliasCreated, OUTPUT cAliasCreateMessage). 
    
END PROCEDURE.

PROCEDURE pCreateStockFromLoadtag PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given the Loadtag buffer, create the Stock inventory
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInventoryStockLoadtag FOR ttInventoryStockLoadtag.
    DEFINE INPUT PARAMETER iplCreateReceipt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE cPrimaryID              AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iInventoryTransactionID AS INTEGER   NO-UNDO.
    
    
    CREATE inventoryStock.
    BUFFER-COPY ipbf-ttInventoryStockLoadtag TO inventoryStock.
    ASSIGN 
        /*        inventoryStock.rec_key          = DYNAMIC-FUNCTION("sfGetNextRecKey")*/
        inventoryStock.inventoryStatus = gcStatusStockInitial
        oplCreated                     = YES
        opcMessage                     = "Inventory Stock Created for " + inventoryStock.inventoryStockID
        .
    IF iplCreateReceipt THEN 
        RUN pCreateTransactionAndReturnID(inventoryStock.company, inventoryStock.inventoryStockID, gcTransactionTypeReceive, 
            inventoryStock.quantityOriginal, inventoryStock.quantityUOM, inventoryStock.warehouseID, inventoryStock.locationID, 
            OUTPUT iInventoryTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
    IF iplCreateReceipt AND iplPost THEN 
        RUN PostTransaction(iInventoryTransactionID).
    RELEASE inventoryStock.
    
END PROCEDURE.

PROCEDURE CreateStockIDAlias:
    /*------------------------------------------------------------------------------
     Purpose: Adds a record to to the stock ID Alias table given a 
     inventoryStockID, Company, PrimaryID and Alias
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUniquePrefix AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAlias AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-inventoryStockAlias FOR inventoryStockAlias.
    
    FIND FIRST bf-inventoryStockAlias NO-LOCK 
         WHERE bf-inventoryStockAlias.company      EQ ipcCompany 
           AND bf-inventoryStockAlias.stockIDAlias EQ ipcAlias
           NO-ERROR.
    
    IF NOT AVAILABLE bf-inventoryStockAlias THEN 
    DO:
        CREATE inventoryStockAlias.
        ASSIGN 
            inventoryStockAlias.inventoryStockAliasID = fGetNextStockAliasID()
            inventoryStockAlias.company               = ipcCompany
            inventoryStockAlias.inventoryStockID      = ipcInventoryStockID
            inventoryStockAlias.uniquePrefix          = ipcUniquePrefix
            inventoryStockAlias.stockIDAlias          = ipcAlias
            oplCreated                                = YES
            opcMessage                                = "Alias Created: " + ipcAlias + " = " + ipcInventoryStockID
            .
        RELEASE inventoryStockAlias.
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Alias: " + ipcAlias + " already exists in Company: " + ipcCompany
            .
            
END PROCEDURE.



PROCEDURE pGenerateLoadtagDataFile PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Generates the data file for a given loadtag
 Notes:
------------------------------------------------------------------------------*/


END PROCEDURE.

PROCEDURE GetFullUnitQuantity:
    /*------------------------------------------------------------------------------
     Purpose: Given Quantity Per SubUnit and Count of SubUnits pre Unit, return the
     quantity of a full unit
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityPerUnit AS DECIMAL NO-UNDO.

    ASSIGN 
        iopdQuantityPerSubUnit      = MAX(iopdQuantityPerSubUnit,1)
        iopiQuantitySubUnitsPerUnit = MAX(iopiQuantitySubUnitsPerUnit,1)
        opdQuantityPerUnit          = iopdQuantityPerSubUnit * iopiQuantitySubUnitsPerUnit
        .

END PROCEDURE.

PROCEDURE pGetWIPID PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Gets the WIP ID fields for a PreLoadtag buffer 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttInventoryStockPreLoadtag FOR ttInventoryStockPreLoadtag.
    DEFINE OUTPUT PARAMETER opcWIPID AS CHARACTER NO-UNDO.

    opcWIPID = STRING(ipbf-ttInventoryStockPreLoadtag.machineID,"x(6)") + STRING(ipbf-ttInventoryStockPreLoadtag.jobID,"x(6)") 
        + STRING(ipbf-ttInventoryStockPreLoadtag.jobID2,"99") + STRING(ipbf-ttInventoryStockPreLoadtag.formNo,"99")  
        + STRING(ipbf-ttInventoryStockPreLoadtag.blankNo,"99").

    IF TRIM(opcWIPID) EQ "" THEN opcWIPID = "WIPITEM".

END PROCEDURE.

PROCEDURE PostTransaction:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiInventoryTransactionID AS INTEGER NO-UNDO.

    FIND FIRST inventoryTransaction EXCLUSIVE-LOCK
        WHERE inventoryTransaction.inventoryTransactionID EQ ipiInventoryTransactionID
        NO-ERROR.
    FIND FIRST inventoryStock EXCLUSIVE-LOCK
        WHERE inventoryStock.inventoryStockID EQ inventoryTransaction.inventoryStockID
        NO-ERROR. 
    IF AVAILABLE inventoryTransaction AND AVAILABLE inventoryStock THEN 
    DO:
        IF inventoryTransaction.quantityChange NE 0 THEN 
            RUN pAddQuantity(inventoryTransaction.quantityChange, inventoryTransaction.quantityUOM, INPUT-OUTPUT inventoryStock.quantity, inventoryStock.quantityUOM). 
        IF inventoryTransaction.warehouseID NE "" THEN 
            inventoryStock.warehouseID = inventoryTransaction.warehouseID.
        IF inventoryTransaction.locationID NE "" THEN 
            inventoryStock.locationID = inventoryTransaction.locationID.
        CASE inventoryTransaction.transactionType:
            WHEN gcTransactionTypeReceive THEN 
                DO:
                    ASSIGN 
                        inventoryStock.inventoryStatus = gcStatusStockReceived.
                END.
            WHEN gcTransactionTypeTransfer THEN 
                DO:
                    ASSIGN 
                        inventoryStock.lastTransBy   = USERID("asi")
                        inventoryStock.lastTransTime = NOW
                        .           
                END.
            WHEN gcTransactionTypeConsume OR 
            WHEN gcTransactionTypeShip THEN 
                DO:
                    IF inventoryStock.quantity EQ 0 THEN 
                    DO: 
                        ASSIGN 
                            inventoryStock.consumedBy   = USERID("asi")
                            inventoryStock.consumedTime = NOW
                            inventoryStock.inventoryStatus = gcStatusStockConsumed.
                    END.
                    ELSE 
                        ASSIGN 
                            inventoryStock.consumedBy   = ""
                            inventoryStock.consumedTime = ?.
                END.
        END CASE. 
        ASSIGN 
            inventoryTransaction.transactionStatus = gcStatusTransactionPosted
            inventoryTransaction.postedBy          = USERID("asi")
            inventoryTransaction.postedTime        = NOW
            .
    END.
    RELEASE inventoryTransaction.
    RELEASE inventoryStock.

END PROCEDURE.

PROCEDURE RecalcQuantityUnits:
    /*------------------------------------------------------------------------------
     Purpose: Given a quantity and unit count, return units and partial
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipdQuantityTotal AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdQuantityPerSubUnit AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiQuantitySubUnitsPerUnit AS INTEGER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opiQuantityOfSubUnits AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQuantityOfUnits AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQuantityPartialSubUnit AS DECIMAL NO-UNDO.

    ASSIGN 
        iopdQuantityPerSubUnit      = MAX(1,iopdQuantityPerSubUnit) 
        iopiQuantitySubUnitsPerUnit = MAX(1,iopiQuantitySubUnitsPerUnit)
        opiQuantityOfSubUnits       = TRUNC(ipdQuantityTotal / iopdQuantityPerSubUnit, 0)
        opdQuantityPartialSubUnit   = ipdQuantityTotal - iopdQuantityPerSubUnit * opiQuantityOfSubUnits
        opiQuantityOfUnits          = INTEGER(TRUNC(opiQuantityOfSubUnits / iopiQuantitySubUnitsPerUnit, 0)) 
        + INTEGER((opiQuantityOfSubUnits MODULO iopiQuantitySubUnitsPerUnit) NE 0)
        .  
    
END PROCEDURE.

PROCEDURE CreateInventoryLoadtagsFromPreLoadtags:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE iCountOfLoadtags          AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQuantityPerFullLoadtag   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iCountOfFullLoadtags      AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQuantityOfPartialLoadtag AS DECIMAL NO-UNDO.
    
    EMPTY TEMP-TABLE ttInventoryStockLoadtag.
    /*Process Inputs to "explode" the loadtag records required based on inputs*/
    FOR EACH ttInventoryStockPreLoadtag:
        ttInventoryStockPreLoadtag.countOfLoadtags = MAX(ttInventoryStockPreLoadtag.countOfLoadtags,1).    

        RUN GetFullUnitQuantity(INPUT-OUTPUT ttInventoryStockPreLoadtag.quantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.quantitySubUnitsPerUnit, OUTPUT dQuantityPerFullLoadtag).

        ASSIGN 
            iCountOfFullLoadtags      = INTEGER(TRUNC(ttInventoryStockPreLoadtag.quantityTotal / dQuantityPerFullLoadtag, 0))
            dQuantityOfPartialLoadtag = ttInventoryStockPreLoadtag.quantityTotal - dQuantityPerFullLoadtag * iCountOfFullLoadtags
            .

        IF dQuantityOfPartialLoadtag NE 0 AND iCountOfFullLoadtags EQ ttInventoryStockPreLoadtag.countOfLoadtags THEN 
            ASSIGN 
                dQuantityOfPartialLoadtag = dQuantityOfPartialLoadtag + dQuantityPerFullLoadtag
                iCountOfFullLoadtags      = iCountOfFullLoadtags - 1
                .    

        DO iCountOfLoadtags = 1 TO iCountOfFullLoadtags:
            RUN pCreateLoadtagFromPreLoadtag(BUFFER ttInventoryStockPreLoadtag, dQuantityPerFullLoadtag).
        END. 
        IF dQuantityOfPartialLoadtag NE 0 THEN 
            RUN pCreateLoadtagFromPreLoadtag(BUFFER ttInventoryStockPreLoadtag, dQuantityOfPartialLoadtag).    
    END.

END PROCEDURE.

PROCEDURE ValidateBin:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcBin AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidBin AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lActiveLoc AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lActiveBin AS LOGICAL NO-UNDO.

    RUN ValidateLoc IN THIS-PROCEDURE (INPUT ipcCompany, INPUT ipcLoc, OUTPUT lActiveLoc).
    
    lActiveBin = CAN-FIND(FIRST fg-bin NO-LOCK 
        WHERE fg-bin.company EQ ipcCompany  
        AND fg-bin.loc     EQ ipcLoc 
        AND fg-bin.loc-bin EQ ipcBin
        AND fg-bin.i-no    EQ ""
        AND fg-bin.active  EQ TRUE).
    
    oplValidBin = lActiveLoc AND lActiveBin.
    

END PROCEDURE.

PROCEDURE ValidateLoc:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLoc AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidLoc AS LOGICAL NO-UNDO.
    
    oplValidLoc = CAN-FIND(FIRST loc NO-LOCK 
        WHERE loc.company EQ ipcCompany  
        AND loc.loc     EQ ipcLoc 
        AND loc.active  EQ TRUE).
        

END PROCEDURE.

PROCEDURE LocationParser:
/*------------------------------------------------------------------------------
 Purpose: Location parser
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcLocation    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcLocationID  AS CHARACTER NO-UNDO.    
    
    ASSIGN    
        opcWarehouseID = SUBSTRING(ipcLocation, 1, 5)
        opcLocationID  = SUBSTRING(ipcLocation, 6)
        .
     
END PROCEDURE.

PROCEDURE GetWarehouseList:
/*------------------------------------------------------------------------------
 Purpose: Get the list of warehouse in a string for a given company
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplActiveOnly         AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcWarehouseListItems AS CHARACTER NO-UNDO.
    
    FOR EACH loc NO-LOCK
        WHERE (IF ipcCompany EQ "" THEN
                   TRUE
               ELSE
                   loc.company = ipcCompany)
          AND (IF iplActiveOnly THEN 
                   loc.active = TRUE
               ELSE 
                   TRUE)      
        BY loc.loc:
        opcWarehouseListItems = IF opcWarehouseListItems = "" THEN 
                                    loc.loc
                                ELSE IF INDEX(opcWarehouseListItems ,loc.loc) GT 0 THEN 
                                    opcWarehouseListItems
                                ELSE
                                    opcWarehouseListItems + "," + loc.loc
                                .    
        
    END.
     
END PROCEDURE.

PROCEDURE pCanFindInventoryStock:
    /*------------------------------------------------------------------------------
     Purpose: Validate Inventory Stock
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStockIDAlias        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidInventoryStock AS LOGICAL   NO-UNDO.
    
    oplValidInventoryStock = CAN-FIND(FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.company EQ ipcCompany
           AND inventoryStock.stockIDAlias EQ ipcStockIDAlias).        

END PROCEDURE.

PROCEDURE pCanFindInventoryStockLocation:
    /*------------------------------------------------------------------------------
     Purpose: Validate Inventory Stock Location
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStockIDAlias        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidInvStockLoc    AS LOGICAL   NO-UNDO.
    
    oplValidInvStockLoc = CAN-FIND(FIRST inventoryStock NO-LOCK
         WHERE inventoryStock.company      EQ ipcCompany
           AND inventoryStock.stockIDAlias EQ ipcStockIDAlias
           AND inventoryStock.warehouseID  EQ ipcWarehouseID
           AND inventoryStock.locationID   EQ ipcLocationID).        

END PROCEDURE.

PROCEDURE pGetInventoryStockJobDetails:
    /*------------------------------------------------------------------------------
     Purpose: Fetch Job details of a Inventory Stock record
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStockIDAlias        AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipcJobno               AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER ipiJobno2              AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER ipiFormno              AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER ipiBlankno             AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER ipcMachine             AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValidInvStock       AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage             AS CHARACTER NO-UNDO.
    
    FIND FIRST inventoryStock NO-LOCK
        WHERE inventoryStock.company      EQ ipcCompany
          AND inventoryStock.StockIDAlias EQ ipcStockIDAlias NO-ERROR.
    
    IF AVAILABLE inventoryStock THEN
        ASSIGN 
            oplValidInvStock    = TRUE
            ipcJobno            = inventoryStock.jobID
            ipcMachine          = inventoryStock.machineID
            ipiJobno2           = inventoryStock.jobID2
            ipiFormno           = inventoryStock.formNo
            ipiBlankno          = inventoryStock.blankNo
            .
    ELSE
        ASSIGN
            oplValidInvStock    = FALSE
            opcMessage          = "Invalid Tag".            

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fCanDeleteInventoryStock RETURNS LOGICAL 
	(ipcInventoryStockID AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    RETURN CAN-FIND(FIRST inventoryStock
                    WHERE inventoryStock.inventoryStockID EQ ipcInventoryStockID
                      AND inventoryStock.inventoryStatus  EQ "Created").	

END FUNCTION.

FUNCTION fGetNextStockAliasID RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next stockAliasID
     Notes:
    ------------------------------------------------------------------------------*/	
    
    giIDTemp = NEXT-VALUE(invaliasid_seq).
    RETURN giIDTemp.
		
END FUNCTION.

FUNCTION fGetNextSnapshotID RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next snapshot ID
     Notes:
    ------------------------------------------------------------------------------*/	
    
    giIDTemp = giIDTemp + 1. //NEXT-VALUE(snapshotid_seq).
    RETURN giIDTemp.
		
END FUNCTION.

FUNCTION fGetNextStockIDAlias RETURNS CHARACTER PRIVATE
    ( ipcCompany AS CHARACTER , ipcUniquePrefix AS CHARACTER ):

    /*------------------------------------------------------------------------------
     Purpose: Returns the next Alias (tag number) to use for a given unique prefix
     this will search prefix across the loadtag types and return a character string for the full
     alias (Tag)
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE iNextTag   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLastFGTag AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLastRMTag AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iLastAlias AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cAlias     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartChar AS INTEGER   NO-UNDO.
    
    iStartChar = LENGTH(ipcUniquePrefix) + 1.
    FIND LAST loadtag NO-LOCK
        WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcUniquePrefix
        /*        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcUniquePrefix*/
        USE-INDEX tag NO-ERROR.
    iLastFGTag = (IF AVAILABLE loadtag THEN fGetNumberSuffix(loadtag.tag-no, iStartChar) ELSE 0) + 1.

    FIND LAST loadtag NO-LOCK
        WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ YES
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcUniquePrefix
        /*        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcUniquePrefix*/
        USE-INDEX tag NO-ERROR.
    iLastRMTag = (IF AVAILABLE loadtag THEN fGetNumberSuffix(loadtag.tag-no, iStartChar) ELSE 0) + 1.
    
    FIND LAST inventoryStockAlias NO-LOCK     
        WHERE inventoryStockAlias.company EQ ipcCompany
        AND inventoryStockAlias.uniquePrefix EQ ipcUniquePrefix
        NO-ERROR.
    iLastAlias = (IF AVAILABLE inventoryStockAlias THEN fGetNumberSuffix(inventoryStockAlias.stockIDAlias, iStartChar) ELSE 0) + 1.
    iNextTag = MAX(iLastFGTag, iLastRMTag, iLastAlias).

    cAlias = ipcUniquePrefix + FILL(" ", giLengthUniquePrefix - iStartChar + 1).
    cAlias = cAlias + STRING(iNextTag, FILL("9",giLengthAlias - LENGTH(cAlias))).

    RETURN cAlias.

		
END FUNCTION.

FUNCTION fGetNextStockID RETURNS CHARACTER PRIVATE
    (ipcType AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next stock ID
     Notes:
    ------------------------------------------------------------------------------*/	
    giIDTemp = NEXT-VALUE(invstockid_seq).
    
    RETURN ipcType + STRING(giIDTemp,"999999999999").

		
END FUNCTION.

FUNCTION fGetNextTransactionID RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next transaction ID
     Notes:
    ------------------------------------------------------------------------------*/    
    giIDTemp = NEXT-VALUE(invtrans_seq).
    RETURN giIDTemp.

END FUNCTION.

FUNCTION fGetNumberSuffix RETURNS INTEGER PRIVATE
    (ipcFullText AS CHARACTER , ipiStartChar AS INTEGER):
    /*------------------------------------------------------------------------------
     Purpose: Returns an integer given a large string with a number 
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE iCountBeginningChars AS INTEGER.
    DEFINE VARIABLE iNumberSuffix        AS INTEGER.
    

    iNumberSuffix = INTEGER(SUBSTRING(ipcFullText, ipiStartChar, (LENGTH(ipcFullText) - ipiStartChar + 1))).	

    RETURN iNumberSuffix.

	
END FUNCTION.

FUNCTION fGetSnapshotCompareStatus RETURNS CHARACTER
    (ipcCompany AS CHARACTER , ipcStockIDAlias AS CHARACTER , ipdQuantity AS DECIMAL , ipcWarehouseID AS CHARACTER , ipcLocationID AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Gets the compare status
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE opcStatus                   AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lLocationChanged            AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lQuantityChanged            AS LOGICAL   NO-UNDO.
    
    opcStatus = gcStatusSnapshotTagNotFound.

    FIND FIRST inventoryStockSnapshot NO-LOCK
         WHERE inventoryStockSnapshot.company      EQ ipcCompany
           AND inventoryStockSnapshot.stockIDAlias EQ ipcStockIDAlias NO-ERROR.

    IF AVAILABLE inventoryStockSnapshot THEN DO:
        IF ipdQuantity NE inventoryStockSnapshot.quantity THEN
            ASSIGN
                lQuantityChanged = TRUE
                opcStatus        = gcStatusSnapshotQtyChange.
        
        IF ipcWarehouseID NE inventoryStockSnapshot.warehouseID OR
           ipcLocationID  NE inventoryStockSnapshot.locationID THEN
            ASSIGN
                lLocationChanged = TRUE
                opcStatus        = gcStatusSnapshotLocChange.
           
        IF lLocationChanged AND lQuantityChanged THEN
            opcStatus = gcStatusSnapshotQtyAndLocChange.
            
        IF NOT lLocationChanged AND NOT lQuantityChanged THEN
            opcStatus = gcStatusSnapshotCompleteMatch.    
    END.
    
    FIND FIRST inventoryTransaction NO-LOCK
         WHERE inventoryTransaction.company         EQ ipcCompany
           AND inventoryTransaction.stockIDAlias    EQ ipcStockIDAlias
           AND inventoryTransaction.transactionType EQ gcTransactionTypeCompare NO-ERROR.
    IF AVAILABLE inventoryTransaction and inventoryTransaction.quantityChange EQ 0 THEN 
        opcStatus = gcStatusSnapshotNotScannedConf.
        
    RETURN opcStatus.

END FUNCTION.

