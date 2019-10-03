/*------------------------------------------------------------------------
    File        : api/inbound/GetInventoryDetails.p
    Purpose     : Fetches Inventory response data for the given input data

    Syntax      :

    Description : Fetches Inventory response data for the given input data

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 06 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{api/inbound/ttItem.i}
{inventory/ttinventory.i "NEW SHARED"}.

DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcWareHouseID       AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID        AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockID  AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID         AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType          AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL    NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage           AS CHARACTER  NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE                FOR ttItem. 

DEFINE VARIABLE iCount           AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidBin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidLoc        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cQuery           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValidTag        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidItem       AS LOGICAL   NO-UNDO.

/* The below code is added as APIInboundEvent.rec_key will be populated in the APIInboundEvent's 
   create trigger, only if session.p is running persistently, else will be populated with empty value. 
   ( refer methods/triggers/create.i ) */
DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.  
RUN system/session.p PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

ASSIGN
    oplSuccess    = YES
    lValidLoc     = IF ipcWareHouseID NE "" THEN
                        YES 
                    ELSE 
                        NO
    lValidBin     = IF ipcLocationID NE "" THEN 
                        YES
                    ELSE 
                        NO
    lValidTag     = IF ipcInventoryStockID NE "" THEN
                       YES
                    ELSE 
                        NO
    lValidItem    = IF ipcPrimaryID NE "" THEN
                        YES
                    ELSE
                        NO
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

IF NOT lValidLoc AND
   NOT lValidBin AND
   NOT lValidTag AND
   NOT lValidItem THEN DO:
    ASSIGN 
        opcMessage = "Enter value for loc&bin / tag / item to get inventory details"
        oplSuccess = NO 
        .
    RETURN.
END.   

IF lValidLoc THEN DO:
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
END.

IF lValidBin THEN DO:
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
END.

/* Validate inventory Stock ID */
IF lValidTag THEN DO:
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND (IF ipcItemType    EQ "FG" THEN
                    loadtag.item-type EQ NO
                ELSE IF ipcItemType EQ "RM" THEN
                    loadtag.item-type EQ YES
                ELSE
                    FALSE)
           AND loadtag.tag-no    EQ ipcInventoryStockID 
         NO-ERROR.
    IF NOT AVAILABLE loadtag THEN DO:
        ASSIGN 
            opcMessage = IF opcMessage EQ "" THEN 
                             "Invalid inventoryStockID"
                         ELSE
                             opcMessage + ", " + "Invalid inventoryStockID"
            oplSuccess = NO
            .
        RETURN.
    END.
END.
 
/* Validate  Primary ID */
IF lValidItem THEN DO:
    IF ipcItemType EQ "RM" THEN DO:
        FIND FIRST item NO-LOCK
             WHERE item.company EQ ipcCompany
               AND item.i-no    EQ ipcPrimaryID
             NO-ERROR.
        IF NOT AVAILABLE item THEN DO:
            ASSIGN 
                opcMessage = IF opcMessage EQ "" THEN 
                                 "Invalid PrimaryID"
                             ELSE
                                 opcMessage + ", " + "Invalid PrimaryID"
                oplSuccess = NO
                .
             RETURN.
         END.
    END.
    ELSE IF ipcItemType EQ "FG" THEN DO:
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no    EQ ipcPrimaryID
             NO-ERROR.
        IF NOT AVAILABLE itemfg THEN DO:
            ASSIGN 
                opcMessage = IF opcMessage EQ "" THEN 
                                 "Invalid PrimaryID"
                             ELSE
                                 opcMessage + ", " + "Invalid PrimaryID"
                oplSuccess = NO
                .
             RETURN.
         END.
     END.
     ELSE DO:
        ASSIGN 
            opcMessage = IF opcMessage EQ "" THEN 
                             "Invalid Item Type"
                         ELSE
                             opcMessage + ", " + "Invalid Item Type"
            oplSuccess = NO
            .
         RETURN.        
     END.     
END.

/* Validate Item type */
IF ipcItemType EQ "" THEN DO:
    ASSIGN 
        opcMessage = IF opcMessage EQ "" THEN 
                         "Invalid ItemType"
                   ELSE
                         opcMessage + ", " + "Invalid ItemType"
        oplSuccess = NO
        .
    RETURN.
END.

IF NOT oplSuccess THEN
    RETURN.
    
/* Writes response data to temp table*/
IF ipcItemType EQ "FG" THEN DO: 
    FOR EACH fg-bin NO-LOCK 
        WHERE fg-bin.company  EQ ipcCompany
          AND (IF lValidTag  THEN fg-bin.tag     EQ ipcInventoryStockID ELSE fg-bin.tag     GE "")
          AND (IF lValidItem THEN fg-bin.i-no    EQ ipcPrimaryID        ELSE fg-bin.i-no    GE "")
          AND (IF lValidLoc  THEN fg-bin.loc     EQ ipcWarehouseID      ELSE fg-bin.loc     GE "")
          AND (IF lValidBin  THEN fg-bin.loc-bin EQ ipcLocationID       ELSE fg-bin.loc-bin GE ""):
              
        iCount = iCount + 1.

        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ fg-bin.company
               AND loadtag.item-type EQ NO
               AND loadtag.tag-no    EQ fg-bin.tag 
             NO-ERROR.
                      
        /* Setting the maximum limit of records to fetch to 1000 */
        IF iCount GT 1000 THEN
            LEAVE.
        
        CREATE ttItem.
        ASSIGN 
            ttItem.WarehouseID             = fg-bin.loc
            ttItem.LocationID              = fg-bin.loc-bin
            ttItem.PrimaryID               = fg-bin.i-no  
            ttItem.InventoryStockID        = fg-bin.tag
            ttItem.Quantity                = fg-bin.qty
            ttItem.ItemType                = ipcItemType
            ttItem.StockIDAlias            = IF AVAILABLE loadtag THEN
                                                  loadtag.misc-char[1]
                                              ELSE
                                                  ""
            ttItem.QuantityUOM             = fg-bin.pur-uom /* Each */
            ttItem.QuantityPerSubUnit      = fg-bin.case-count
            ttItem.QuantitySubUnitsPerUnit = fg-bin.cases-unit
            ttItem.QuantityPartial         = fg-bin.partial-total
            .
    END.
END.
ELSE IF ipcItemType EQ "RM" THEN DO: 
    FOR EACH rm-bin NO-LOCK 
        WHERE rm-bin.company  EQ ipcCompany
          AND (IF lValidLoc  THEN rm-bin.loc     EQ ipcWarehouseID      ELSE rm-bin.loc     GE "")
          AND (IF lValidItem THEN rm-bin.i-no    EQ ipcPrimaryID        ELSE rm-bin.i-no    GE "")
          AND (IF lValidBin  THEN rm-bin.loc-bin EQ ipcLocationID       ELSE rm-bin.loc-bin GE "")
          AND (IF lValidTag  THEN rm-bin.tag     EQ ipcInventoryStockID ELSE rm-bin.tag     GE ""):
              
        iCount = iCount + 1.

        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ rm-bin.company
               AND loadtag.item-type EQ YES
               AND loadtag.tag-no    EQ rm-bin.tag 
             NO-ERROR.
        
        FIND FIRST item NO-LOCK
             WHERE item.company EQ ipcCompany
               AND item.i-no    EQ rm-bin.i-no
             NO-ERROR.           
        
        /* Setting the maximum limit of records to fetch to 1000 */
        IF iCount GT 1000 THEN
            LEAVE.
        
        CREATE ttItem.
        ASSIGN 
            ttItem.WarehouseID             = rm-bin.loc
            ttItem.LocationID              = rm-bin.loc-bin
            ttItem.PrimaryID               = rm-bin.i-no  
            ttItem.InventoryStockID        = rm-bin.tag
            ttItem.Quantity                = rm-bin.qty
            ttItem.ItemType                = ipcItemType
            ttItem.StockIDAlias            = IF AVAILABLE loadtag THEN
                                                  loadtag.misc-char[1]
                                              ELSE
                                                  ""
            ttItem.QuantityUOM             = IF AVAILABLE item THEN
                                                 item.cons-uom
                                             ELSE
                                                 ""
            ttItem.QuantityPerSubUnit      = 0
            ttItem.QuantitySubUnitsPerUnit = 0
            ttItem.QuantityPartial         = 0
            .
    END.
END.

DELETE PROCEDURE hdInventoryProcs.










    
    


