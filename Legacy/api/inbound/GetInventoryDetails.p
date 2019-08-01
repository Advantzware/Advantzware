/*------------------------------------------------------------------------
    File        : api/inbound/GetInventoryDetails.p
    Purpose     : Fetches Inventory response data for the given input data

    Syntax      :

    Description : Fetches Inventory response data for the given input data

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 06 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttfgbin.i}
{inventory/ttinventory.i "NEW SHARED"}.

DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcWareHouseID       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockID  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID         AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType          AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE                FOR ttfgbin. 

DEFINE VARIABLE iCount           AS INTEGER NO-UNDO.
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
        opcMessage = opcMessage + "Invalid Company"
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

/* Validate inventory Stock ID */
IF ipcInventoryStockID EQ "" THEN
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Invalid inventoryStockID"
                     ELSE
                         opcMessage + ", " + "Invalid inventoryStockID"
        oplSuccess = NO
        .

/* Validate  Primary ID */
IF ipcPrimaryID EQ "" THEN
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Invalid PrimaryID"
                     ELSE
                         opcMessage + ", " + "Invalid PrimaryID"
        oplSuccess = NO
        .

/* Validate Item type */
IF  ipcItemType EQ "" THEN
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Invalid ItemType"
                   ELSE
                         opcMessage + ", " + "Invalid ItemType"
        oplSuccess = NO
        .
 
/* Writes response data to temp table*/
IF ipcItemType EQ "FG" AND oplSuccess THEN DO: 
    FOR EACH fg-bin NO-LOCK 
        WHERE fg-bin.company  EQ ipcCompany
          AND fg-bin.loc      EQ ipcWareHouseID  
          AND fg-bin.loc-bin  EQ ipcLocationID
          AND fg-bin.i-no     EQ ipcPrimaryID:
                
        iCount = iCount + 1.
        
        /* Setting the maximum limit of records to fetch to 1000 */
        IF iCount GT 1000 THEN
            LEAVE.
        
        CREATE ttfgbin.
        ASSIGN 
            ttfgbin.InventoryStockID        = fg-bin.tag
            ttfgbin.PrimaryID               = fg-bin.i-no 
            ttfgbin.StockIDAlias            = fg-bin.tag
            ttfgbin.Quantity                = fg-bin.qty
            ttfgbin.QuantityUOM             = fg-bin.pur-uom
            ttfgbin.QuantityPerSubUnit      = fg-bin.cases-unit
            ttfgbin.QuantitySubUnitsPerUnit = fg-bin.case-count
            ttfgbin.QuantityPartial         = fg-bin.partial-total
            .
    END.
END.

DELETE PROCEDURE hdInventoryProcs.










    
    


