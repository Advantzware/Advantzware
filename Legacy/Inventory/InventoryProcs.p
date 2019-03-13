
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
DEFINE VARIABLE gcStatusStockPreLoadtag AS CHARACTER INITIAL "PreLoadtag".
DEFINE VARIABLE gcStatusStockLoadtag AS CHARACTER INITIAL "Loadtag".
DEFINE VARIABLE gcStatusStockInitial AS CHARACTER INITIAL "Initialized".
DEFINE VARIABLE gcStatusStockReceived AS CHARACTER INITIAL "On-Hand".

DEFINE VARIABLE gcStatusTransactionInitial AS CHARACTER INITIAL "Pending".
DEFINE VARIABLE gcStatusTransactionPosted AS CHARACTER INITIAL "Posted".

DEFINE VARIABLE gcTransactionTypeReceive AS CHARACTER INITIAL "R".
DEFINE VARIABLE gcTransactionTypeTransfer AS CHARACTER INITIAL "T".
DEFINE VARIABLE gcTransactionTypeConsume AS CHARACTER INITIAL "I".
DEFINE VARIABLE gcTransactionTypeShip AS CHARACTER INITIAL "S".

DEFINE VARIABLE gcItemTypeWIP AS CHARACTER INITIAL "WP".
DEFINE VARIABLE gcItemTypeFG AS CHARACTER INITIAL "FG".
DEFINE VARIABLE gcItemTypeRM AS CHARACTER INITIAL "RM".

DEFINE VARIABLE giLengthUniquePrefix AS INTEGER INITIAL 20.
DEFINE VARIABLE giLengthAlias AS INTEGER INITIAL 25.

DEFINE VARIABLE giIDTemp           AS INTEGER. /*TESTING ONLY DELETE BEFORE COMMIT*/

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetNextStockAliasID RETURNS INTEGER PRIVATE
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

FIND FIRST InventoryStockAlias NO-LOCK 
    WHERE InventoryStockAlias.Company EQ ipcCompany
    AND InventoryStockAlias.StockIDAlias EQ ipcLookupID
    NO-ERROR.
IF AVAILABLE InventoryStockAlias THEN 
    ASSIGN
        opcInventoryStockID = InventoryStockAlias.InventoryStockID
        opcStockIDAlias = InventoryStockAlias.StockIDAlias
        . 
ELSE 
    ASSIGN
        opcInventoryStockID = ipcLookupID
        opcStockIDAlias = ""
        . 
    

END PROCEDURE.

PROCEDURE CreateInventoryStockFromLoadtag:
    /*------------------------------------------------------------------------------
     Purpose: Given existing ttInventoryStockLoadtag table, generate the "Actual"
     inventory with original quantity transfering to .Quantity and registering the status as
     received.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCreateReceipt AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplPost AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
       
    /*Copy Loadtags to InventoryStock*/
    FIND FIRST ttInventoryStockLoadtag NO-LOCK
        WHERE ttInventoryStockLoadtag.InventoryStockID EQ ipcInventoryStockID
        NO-ERROR. 
    IF AVAILABLE ttInventoryStockLoadtag THEN DO:        
        RUN pCreateStockFromLoadtag(BUFFER ttInventoryStockLoadtag, iplCreateReceipt, iplPost, OUTPUT oplCreated, OUTPUT opcMessage).
        
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Inventory Stock ID"
            .
                  

END PROCEDURE.

PROCEDURE CreatePreLoadtagsFromInputsFG:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/


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

    DEFINE BUFFER bf-job-mch FOR job-mch.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item    FOR item.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
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
            ttInventoryStockPreLoadtag.Company                 = bf-job-mch.company
            ttInventoryStockPreLoadtag.MachineID               = bf-job-mch.m-code
            ttInventoryStockPreLoadtag.JobID                   = bf-job-mch.job-no
            ttInventoryStockPreLoadtag.JobID2                  = bf-job-mch.job-no2
            ttInventoryStockPreLoadtag.FormNo                  = bf-job-mch.frm
            ttInventoryStockPreLoadtag.BlankNo                 = bf-job-mch.blank-no
            ttInventoryStockPreLoadtag.PassNo                  = bf-job-mch.pass
            ttInventoryStockPreLoadtag.InventoryStatus         = gcStatusStockPreLoadtag
            ttInventoryStockPreLoadtag.ItemType                = gcItemTypeWIP
            ttInventoryStockPreLoadtag.RMItemID                = bf-job-mat.rm-i-no
            ttInventoryStockPreLoadtag.DimEachLen              = bf-job-mat.len
            ttInventoryStockPreLoadtag.DimEachWid              = bf-job-mat.wid
            ttInventoryStockPreLoadtag.DimEachDep              = bf-job-mat.dep
            ttInventoryStockPreLoadtag.DimEachUOM              = "IN"
            ttInventoryStockPreLoadtag.QuantityTotal           = ipdQuantityTotal
            ttInventoryStockPreLoadtag.QuantityUOM             = ipcQuantityUOM
            ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit = ipiQuantitySubUnitsPerUnit
            ttInventoryStockPreLoadtag.QuantityPerSubUnit      = ipdQuantityPerSubUnit
            .
        RUN pGetWIPID(BUFFER ttInventoryStockPreLoadtag, OUTPUT ttInventoryStockPreLoadtag.WIPItemID).
        ttInventoryStockPreLoadtag.PrimaryID = ttInventoryStockPreLoadtag.WIPItemID.
        RUN pRecalcQuantityUnits(ipdQuantityTotal, INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit, 
            OUTPUT ttInventoryStockPreLoadtag.QuantityOfSubUnits, OUTPUT ttInventoryStockPreLoadtag.QuantityOfUnits, OUTPUT ttInventoryStockPreLoadtag.QuantityPartial).
            
        FIND FIRST bf-item NO-LOCK 
            WHERE bf-item.company EQ bf-job-mat.company
            AND bf-item.i-no EQ bf-job-mat.rm-i-no
            NO-ERROR.
        IF AVAILABLE bf-item THEN 
            ASSIGN 
                ttInventoryStockPreLoadtag.BasisWeight    = bf-item.basis-w
                ttInventoryStockPreLoadtag.BasisWeightUOM = "LBS/MSF"
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
                ttInventoryStockPreLoadtag.FGItemID    = bf-job-hdr.i-no
                ttInventoryStockPreLoadtag.WarehouseID = bf-job-hdr.loc
                ttInventoryStockPreLoadtag.OrderID     = bf-job-hdr.ord-no
                ttInventoryStockPreLoadtag.CustomerID  = bf-job-hdr.cust-no
                .
    END.
    ELSE 
        ASSIGN 
            oplCreated = NO
            opcMessage = "Invalid Machine or Material Inputs" 
            .
    

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

CREATE InventoryTransaction.
ASSIGN 
    /*InventoryTransaction.rec_key          = DYNAMIC-FUNCTION("sfGetNextRecKey")*/
    InventoryTransaction.InventoryTransactionID = fGetNextTransactionID()
    opiInventoryTransactionID = InventoryTransaction.InventoryTransactionID
    InventoryTransaction.TransactionType = ipcTransactionType
    InventoryTransaction.Company = ipcCompany
    InventoryTransaction.CreatedBy = USERID("asi")
    InventoryTransaction.CreatedTime = NOW
    InventoryTransaction.QuantityChange = ipdQuantityChange
    InventoryTransaction.QuantityUOM = ipcQuantityUOM
    InventoryTransaction.WarehouseID = ipcWarehouseID
    InventoryTransaction.LocationID = ipcLocationID
    InventoryTransaction.TransactionTime = InventoryTransaction.CreatedTime  /*Default to Created Time, Not Posted*/
    InventoryTransaction.TransactionStatus = gcStatusTransactionInitial
    oplCreated = YES
    opcMessage = "Transaction Created.  ID: " + STRING(opiInventoryTransactionID)
    .
    RUN CheckInventoryStockIDAlias(ipcCompany, ipcInventoryStockID, OUTPUT InventoryTransaction.InventoryStockID, OUTPUT InventoryTransaction.StockIDAlias).
    
RELEASE InventoryTransaction.

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

IF ipcQuantityChangeUOM NE ipcQuantityExistingUOM THEN DO:
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
 
    DEFINE VARIABLE lAliasCreated AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAliasCreateMessage AS CHARACTER NO-UNDO.
    
    CREATE ttInventoryStockLoadtag.
    BUFFER-COPY ipbf-ttInventoryStockPreLoadtag TO ttInventoryStockLoadtag.
    ASSIGN 
        /*        ttInventoryStockLoadtag.rec_key          = DYNAMIC-FUNCTION("sfGetNextRecKey")*/
        ttInventoryStockLoadtag.InventoryStockID = fGetNextStockID(ttInventoryStockLoadtag.ItemType) /*Unique ID*/
        ttInventoryStockLoadtag.QuantityOriginal = ipdQuantity
        ttInventoryStockLoadtag.InventoryStatus = gcStatusStockLoadtag
        .
    /*Ensure the partial and unit counts are calculated correctly for this specific quantity*/
    RUN pRecalcQuantityUnits(ttInventoryStockLoadtag.QuantityOriginal, 
        INPUT-OUTPUT ttInventoryStockLoadtag.QuantityPerSubUnit, INPUT-OUTPUT ttInventoryStockLoadtag.QuantitySubUnitsPerUnit,
        OUTPUT ttInventoryStockLoadtag.QuantityOfSubUnits, OUTPUT ttInventoryStockLoadtag.QuantityOfUnits, OUTPUT ttInventoryStockLoadtag.QuantityPartial).
    
    /*Build Readable Tag Number and register it on Alias table*/
    ttInventoryStockLoadtag.StockIDAlias = fGetNextStockIDAlias(ttInventoryStockLoadtag.Company, ttInventoryStockLoadtag.PrimaryID). 
    RUN CreateStockIDAlias(ttInventoryStockLoadtag.Company, ttInventoryStockLoadtag.InventoryStockID, ttInventoryStockLoadtag.PrimaryID, ttInventoryStockLoadtag.StockIDAlias,
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
     
    DEFINE VARIABLE cPrimaryID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iInventoryTransactionID AS INTEGER NO-UNDO.
    
    
    CREATE InventoryStock.
    BUFFER-COPY ipbf-ttInventoryStockLoadtag TO InventoryStock.
    ASSIGN 
        /*        InventoryStock.rec_key          = DYNAMIC-FUNCTION("sfGetNextRecKey")*/
        InventoryStock.InventoryStatus = gcStatusStockInitial
        oplCreated = YES
        opcMessage = "Inventory Stock Created for " + InventoryStock.InventoryStockID
        .
    IF iplCreateReceipt THEN 
        RUN pCreateTransactionAndReturnID(InventoryStock.Company, InventoryStock.InventoryStockID, gcTransactionTypeReceive, 
            InventoryStock.QuantityOriginal, InventoryStock.QuantityUOM, InventoryStock.WarehouseID, InventoryStock.LocationID, 
            OUTPUT iInventoryTransactionID, OUTPUT oplCreated, OUTPUT opcMessage).
    IF iplCreateReceipt AND iplPost THEN 
        RUN PostTransaction(iInventoryTransactionID).
    RELEASE InventoryStock.
    
END PROCEDURE.

PROCEDURE CreateStockIDAlias:
    /*------------------------------------------------------------------------------
     Purpose: Adds a record to to the stock ID Alias table given a 
     InventoryStockID, Company, PrimaryID and Alias
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcUniquePrefix AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcAlias AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplCreated AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-InventoryStockAlias FOR InventoryStockAlias.
    
    FIND FIRST bf-InventoryStockAlias NO-LOCK 
        WHERE bf-InventoryStockAlias.Company EQ ipcCompany 
        AND bf-InventoryStockAlias.StockIDAlias EQ ipcAlias
        NO-ERROR.
    
    IF NOT AVAILABLE bf-InventoryStockAlias THEN DO:
        CREATE InventoryStockAlias.
        ASSIGN 
            InventoryStockAlias.InventoryStockAliasID = fGetNextStockAliasID()
            InventoryStockAlias.Company               = ipcCompany
            InventoryStockAlias.InventoryStockID      = ipcInventoryStockID
            InventoryStockAlias.UniquePrefix          = ipcUniquePrefix
            InventoryStockAlias.StockIDAlias = ipcAlias
            oplCreated = YES
            opcMessage = "Alias Created: " + ipcAlias + " = " + ipcInventoryStockID
            .
        RELEASE InventoryStockAlias.
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

    opcWIPID = STRING(ipbf-ttInventoryStockPreLoadtag.MachineID,"x(6)") + STRING(ipbf-ttInventoryStockPreLoadtag.JobID,"x(6)") 
        + STRING(ipbf-ttInventoryStockPreLoadtag.JobID2,"99") + STRING(ipbf-ttInventoryStockPreLoadtag.FormNo,"99")  
        + STRING(ipbf-ttInventoryStockPreLoadtag.BlankNo,"99") .

    IF TRIM(opcWIPID) EQ "" THEN opcWIPID = "WIPITEM".

END PROCEDURE.

PROCEDURE PostTransaction:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipiInventoryTransactionID AS INTEGER NO-UNDO.

FIND FIRST InventoryTransaction EXCLUSIVE-LOCK
    WHERE InventoryTransaction.InventoryTransactionID EQ ipiInventoryTransactionID
    NO-ERROR.
FIND FIRST InventoryStock EXCLUSIVE-LOCK
    WHERE InventoryStock.InventoryStockID EQ InventoryTransaction.InventoryStockID
    NO-ERROR. 
IF AVAILABLE InventoryTransaction AND AVAILABLE InventoryStock THEN DO:
    IF InventoryTransaction.QuantityChange NE 0 THEN 
        RUN pAddQuantity(InventoryTransaction.QuantityChange, InventoryTransaction.QuantityUOM, INPUT-OUTPUT InventoryStock.Quantity, InventoryStock.QuantityUOM). 
    IF InventoryTransaction.WarehouseID NE "" THEN 
        InventoryStock.WarehouseID = InventoryTransaction.WarehouseID.
    IF InventoryTransaction.LocationID NE "" THEN 
        InventoryStock.LocationID = InventoryTransaction.LocationID.
    CASE InventoryTransaction.TransactionType:
        WHEN gcTransactionTypeReceive THEN DO:
            ASSIGN 
                InventoryStock.InventoryStatus = gcStatusStockReceived.
        END.
        WHEN gcTransactionTypeTransfer THEN DO:
            
        END.
        WHEN gcTransactionTypeConsume OR WHEN gcTransactionTypeShip THEN DO:
            IF InventoryStock.Quantity EQ 0 THEN DO: 
                ASSIGN 
                    InventoryStock.ConsumedBy = USERID("asi")
                    InventoryStock.ConsumedTime = NOW
                    .
            END.
            ELSE 
                ASSIGN 
                    InventoryStock.ConsumedBy = ""
                    InventoryStock.ConsumedTime = ?
                    .
        END.
    END CASE. 
    ASSIGN 
        InventoryTransaction.TransactionStatus = gcStatusTransactionPosted
        InventoryTransaction.PostedBy = USERID("asi")
        InventoryTransaction.PostedTime = NOW
        .
END.
RELEASE InventoryTransaction.
RELEASE InventoryStock.

END PROCEDURE.

PROCEDURE pRecalcQuantityUnits PRIVATE:
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
        + INTEGER((opiQuantityOfSubUnits MODULO iopiQuantitySubUnitsPerUnit) NE 0) + INTEGER(opdQuantityPartialSubUnit GT 0)
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
        ttInventoryStockPreLoadtag.CountOfLoadtags = MAX(ttInventoryStockPreLoadtag.CountOfLoadtags,1).    
        RUN GetFullUnitQuantity(INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantityPerSubUnit, INPUT-OUTPUT ttInventoryStockPreLoadtag.QuantitySubUnitsPerUnit, OUTPUT dQuantityPerFullLoadtag).
        ASSIGN 
            iCountOfFullLoadtags      = INTEGER(TRUNC(ttInventoryStockPreLoadtag.QuantityTotal / dQuantityPerFullLoadtag, 0))
            dQuantityOfPartialLoadtag = ttInventoryStockPreLoadtag.QuantityTotal - dQuantityPerFullLoadtag * iCountOfFullLoadtags
            .
        IF dQuantityOfPartialLoadtag NE 0 AND iCountOfFullLoadtags EQ ttInventoryStockPreLoadtag.CountOfLoadtags THEN 
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


/* ************************  Function Implementations ***************** */

FUNCTION fGetNextStockAliasID RETURNS INTEGER PRIVATE
    (  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next stockAliasID
     Notes:
    ------------------------------------------------------------------------------*/	
    
    giIDTemp = giIDTemp + 1.
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
    
    FIND LAST InventoryStockAlias NO-LOCK     
        WHERE InventoryStockAlias.Company EQ ipcCompany
        AND InventoryStockAlias.UniquePrefix EQ ipcUniquePrefix
        NO-ERROR.
    iLastAlias = (IF AVAILABLE InventoryStockAlias THEN fGetNumberSuffix(InventoryStockAlias.StockIDAlias, iStartChar) ELSE 0) + 1.
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
    giIDTemp = giIDTemp + 1.
    
    RETURN ipcType + STRING(giIDTemp,"999999999999").

		
END FUNCTION.

FUNCTION fGetNextTransactionID RETURNS INTEGER PRIVATE
	(  ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the next transaction ID
     Notes:
    ------------------------------------------------------------------------------*/    
    giIDTemp = giIDTemp + 1.
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

