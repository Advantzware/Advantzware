/*------------------------------------------------------------------------
    File        : api\inbound\CreateInventoryTransfer.p
    Purpose     : Processes request data

    Syntax      :

    Description : Processes request data

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 08 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{inventory/ttinventory.i "NEW SHARED"}.

DEFINE INPUT  PARAMETER ipcCompany              AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcWareHouseID          AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockIDTag  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID            AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType             AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess              AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage              AS CHARACTER  NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.
DEFINE VARIABLE lValidBin        AS LOGICAL NO-UNDO.
DEFINE VARIABLE lValidLoc        AS LOGICAL NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL NO-UNDO.

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

ASSIGN
    oplSuccess    = YES
    lValidLoc     = YES
    lValidBin     = YES
    lValidCompany = YES
    .

/* Validate company */
lValidCompany = CAN-FIND(FIRST company NO-LOCK
                         WHERE company.company EQ ipcCompany).
        
IF NOT lValidCompany THEN DO:
    ASSIGN 
        opcMessage = "Invalid Company"
        oplSuccess = NO
        .
    RETURN.
END.
  
/* Validate warehouse */        
RUN ValidateLoc IN hdInventoryProcs (
    ipcCompany,
    ipcWareHouseID,
    OUTPUT lValidLoc
    ).

IF NOT lValidLoc THEN DO:
    ASSIGN 
        opcMessage = "Invalid WareHouseID"                     
        oplSuccess = NO
        .
    RETURN.
END.

/* Validate location */
RUN ValidateBin IN hdInventoryProcs (
    ipcCompany,
    ipcWareHouseID,
    ipcLocationID,
    OUTPUT lValidBin
    ).

IF ipcLocationID EQ "" OR NOT lValidBin THEN DO:
    ASSIGN 
        opcMessage = "Invalid LocationID"
        oplSuccess = NO 
        .
    RETURN.
END.

/* Validate tag */
IF ipcInventoryStockIDTag EQ "" THEN
    ASSIGN 
        opcMessage = "Invalid inventoryStockIDTag"
        oplSuccess = NO
        .

/* Validate primary id */
IF ipcPrimaryID EQ "" THEN
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Invalid PrimaryID"
                     ELSE
                         opcMessage + ", " + "Invalid PrimaryID"
        oplSuccess = NO
        .

/* Validate item type */
IF ipcItemType EQ "" THEN
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Invalid ItemType"
                     ELSE
                         opcMessage + ", " + "Invalid ItemType"
        oplSuccess = NO
        .

/* This block will be modified with extra logic to handle inventory transfer creation */
IF ipcItemType EQ "FG" AND oplSuccess THEN DO:     
    FIND FIRST fg-bin EXCLUSIVE-LOCK  
         WHERE fg-bin.company EQ ipcCompany
           AND fg-bin.tag     EQ ipcInventoryStockIDTag
           AND fg-bin.i-no    EQ ipcPrimaryID 
         NO-ERROR.
        
    IF NOT AVAILABLE fg-bin THEN DO:
        ASSIGN
            opcMessage = "Tag is not associated with the primary ID"
            oplSuccess = NO
            .
        RETURN.
    END.    
    ELSE      
        ASSIGN
            fg-bin.loc     = ipcWareHouseID
            fg-bin.loc-bin = ipcLocationID
            .
END.

DELETE PROCEDURE hdInventoryProcs.










    
    


