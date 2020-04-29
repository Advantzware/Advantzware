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

DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcWareHouseID      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcJobNo            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiJobNo2           AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcCustNo           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiPoID             AS INTEGER   NO-UNDO.             
DEFINE INPUT  PARAMETER iplIncludeZeroQty   AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iplIncludeEmptyTag  AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcStatus           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplOnHold           AS LOGICAL   NO-UNDO. /* Send ? as input to ignore onHold in query filter */
DEFINE INPUT  PARAMETER ipcItemType         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE               FOR ttItem. 

DEFINE VARIABLE iCount           AS INTEGER   NO-UNDO.
DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidLoc        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidBin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cQuery           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValidTag        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lItemType        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidItem       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cItemTypeFG      AS CHARACTER NO-UNDO INITIAL "FG".
DEFINE VARIABLE cItemTypeRM      AS CHARACTER NO-UNDO INITIAL "RM".
DEFINE VARIABLE hdBuffer         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTableFG         AS CHARACTER NO-UNDO INITIAL "fg-bin".
DEFINE VARIABLE cTableRM         AS CHARACTER NO-UNDO INITIAL "rm-bin".
DEFINE VARIABLE lValidJobNo      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidCustNo     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidPoID       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidStatus     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidOnHold     AS LOGICAL   NO-UNDO.

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

ASSIGN
    oplSuccess    = YES
    lValidLoc     = ipcWareHouseID NE ""
    lValidBin     = ipcLocationID NE "" 
    lValidTag     = ipcInventoryStockID NE "" 
    lValidItem    = ipcPrimaryID NE "" 
    lValidJobNo   = ipcJobNo NE "" 
    lValidCustNo  = ipcCustNo NE "" 
    lValidPoID    = ipiPoID NE 0
    lValidStatus  = ipcStatus NE ""
    lValidOnHold  = iplOnHold NE ?
    lValidCompany = YES
    ipcItemType   = IF ipcItemType EQ "" THEN
                        cItemTypeFG
                    ELSE
                        ipcItemType
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
        opcMessage = "Enter value for warehouse & location / tag / item to get inventory details"
        oplSuccess = NO 
        .
        
    RETURN.
END.   

IF (lValidLoc AND NOT lValidBin) OR
   (lValidBin AND NOT lValidLoc) OR 
   (lValidLoc AND lValidBin) THEN DO:
    
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
END.

/* Validate Item type */
IF (ipcItemType NE cItemTypeFG  AND 
    ipcItemType NE cItemTypeRM) THEN DO:
    ASSIGN 
        opcMessage = "Invalid ItemType"
        oplSuccess = NO 
        .
        
    RETURN.
END.

lItemType = IF ipcItemType EQ cItemTypeFG THEN 
                FALSE
            ELSE 
                TRUE.

/* Validate inventory Stock ID */
IF lValidTag THEN DO:    
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.item-type EQ lItemType
           AND loadtag.tag-no    EQ ipcInventoryStockID 
         NO-ERROR.
    IF NOT AVAILABLE loadtag THEN DO:
        ASSIGN 
            opcMessage = "Invalid inventoryStockID"
            oplSuccess = NO
            .
            
        RETURN.
    END.
END.

/* Validate Primary ID */
IF lValidItem THEN DO:
    IF lItemType THEN DO:
        FIND FIRST item NO-LOCK
             WHERE item.company EQ ipcCompany
               AND item.i-no    EQ ipcPrimaryID
             NO-ERROR.
        IF NOT AVAILABLE item THEN DO:
            ASSIGN 
                opcMessage = "Invalid PrimaryID"
                oplSuccess = NO
                .
                
            RETURN.
        END.
    END.
    ELSE DO:
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no    EQ ipcPrimaryID
             NO-ERROR.
        IF NOT AVAILABLE itemfg THEN DO:
            ASSIGN 
                opcMessage = "Invalid PrimaryID" 
                oplSuccess = NO
                .
                
            RETURN.
        END.
     END.
END.
  
/* Writes response data to temp table*/
IF ipcItemType EQ cItemTypeFG THEN DO:
    CREATE BUFFER hdBuffer FOR TABLE cTableFG.
    CREATE QUERY hdQuery.
    
    cQuery = "FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ '" + ipcCompany + "'"
           + (IF lValidTag    THEN " AND fg-bin.tag EQ '" + ipcInventoryStockID + "'" ELSE "")
           + (IF iplIncludeEmptyTag THEN " AND fg-bin.tag NE ''" ELSE "")
           + (IF lValidItem   THEN " AND fg-bin.i-no EQ '" + ipcPrimaryID + "'" ELSE "")
           + (IF lValidLoc    THEN " AND fg-bin.loc EQ '" + ipcWarehouseID + "'" ELSE "")
           + (IF lValidBin    THEN " AND fg-bin.loc-bin EQ '" + ipcLocationID + "'" ELSE "")
           + (IF lValidJobNo  THEN " AND fg-bin.job-no EQ '" + ipcJobNo + "'" ELSE "")
           + (IF lValidJobNo  THEN " AND fg-bin.job-no2 EQ " + STRING(ipiJobNo2) ELSE "")
           + (IF lValidCustNo THEN " AND fg-bin.cust-no EQ '" + ipcCustNo + "'" ELSE "")
           + (IF lValidPoID   THEN " AND fg-bin.po-no EQ '" + STRING(ipiPoID) + "'" ELSE "")
           + (IF lValidStatus THEN " AND fg-bin.statusID EQ '" + ipcStatus + "'" ELSE "")
           + (IF lValidOnHold THEN " AND fg-bin.onHold EQ " + STRING(iplOnHold) ELSE "") 
           + (IF iplIncludeZeroQty THEN " AND fg-bin.qty NE 0 " + " AND fg-bin.qty NE ?" ELSE "")
           .
       
    hdQuery:SET-BUFFERS(hdBuffer).
    hdQuery:QUERY-PREPARE(cQuery).
    hdQuery:QUERY-OPEN().
    hdQuery:GET-FIRST().
    
    REPEAT:
        iCount = iCount + 1.
        
        IF hdQuery:QUERY-OFF-END OR iCount GT 1000 THEN DO:
            IF iCount GT 1000 THEN
                opcMessage = "Large number of records available for this search. Limiting to 1000 records".
            LEAVE.
        END.
        
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ hdBuffer:BUFFER-FIELD("company"):BUFFER-VALUE
               AND loadtag.item-type EQ NO
               AND loadtag.tag-no    EQ hdBuffer:BUFFER-FIELD("tag"):BUFFER-VALUE
             NO-ERROR.
             
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ hdBuffer:BUFFER-FIELD("company"):BUFFER-VALUE
               AND itemfg.i-no    EQ hdBuffer:BUFFER-FIELD("i-no"):BUFFER-VALUE
             NO-ERROR.

        CREATE ttItem.
        ASSIGN
            ttItem.WarehouseID             = hdBuffer:BUFFER-FIELD("loc"):BUFFER-VALUE
            ttItem.LocationID              = hdBuffer:BUFFER-FIELD("loc-bin"):BUFFER-VALUE
            ttItem.PrimaryID               = hdBuffer:BUFFER-FIELD("i-no"):BUFFER-VALUE
            ttItem.InventoryStockID        = hdBuffer:BUFFER-FIELD("tag"):BUFFER-VALUE
            ttItem.Quantity                = hdBuffer:BUFFER-FIELD("qty"):BUFFER-VALUE
            ttItem.ItemType                = ipcItemType
            ttItem.tag                     = IF AVAILABLE loadtag THEN
                                                  loadtag.misc-char[1]
                                              ELSE
                                                  ""
            ttItem.QuantityUOM             = hdBuffer:BUFFER-FIELD("pur-uom"):BUFFER-VALUE
            ttItem.QuantityPerSubUnit      = hdBuffer:BUFFER-FIELD("case-count"):BUFFER-VALUE
            ttItem.QuantitySubUnitsPerUnit = hdBuffer:BUFFER-FIELD("cases-unit"):BUFFER-VALUE
            ttItem.QuantityPartial         = hdBuffer:BUFFER-FIELD("partial-count"):BUFFER-VALUE
            ttItem.Units                   = TRUNC((hdBuffer:BUFFER-FIELD("qty"):BUFFER-VALUE - ttItem.QuantityPartial) / ttItem.QuantityPerSubUnit,0)
            ttItem.JobNo                   = hdBuffer:BUFFER-FIELD("job-no"):BUFFER-VALUE
            ttItem.JobNo2                  = hdBuffer:BUFFER-FIELD("job-no2"):BUFFER-VALUE
            ttItem.POID                    = hdBuffer:BUFFER-FIELD("po-no"):BUFFER-VALUE
            ttItem.customerID              = hdBuffer:BUFFER-FIELD("cust-no"):BUFFER-VALUE
            ttItem.TagStatus               = hdBuffer:BUFFER-FIELD("statusId"):BUFFER-VALUE
            ttItem.OnHold                  = hdBuffer:BUFFER-FIELD("onHold"):BUFFER-VALUE
            ttItem.sourceRowID             = hdBuffer:ROWID 
            .
            
            IF ttItem.TagStatus NE "" THEN
                RUN Inventory_GetStatusDescription IN hdInventoryProcs (
                    INPUT  ttItem.TagStatus,
                    OUTPUT ttItem.StatusDescription 
                    ).
                
            IF AVAILABLE itemfg THEN
                ASSIGN
                    ttItem.UnitLength  = itemfg.unitlength
                    ttItem.UnitHeight  = itemfg.unitHeight
                    ttItem.UnitWidth   = itemfg.unitWidth
                    ttItem.StackHeight = itemfg.stackHeight
                    ttItem.itemDesc    = itemfg.i-dscr
                    .
        
        hdQuery:GET-NEXT().
    END.
    
END.
ELSE DO:
    CREATE BUFFER hdBuffer FOR TABLE cTableRM.
    CREATE QUERY hdQuery.
    
    cQuery = "FOR EACH rm-bin NO-LOCK WHERE rm-bin.company EQ '" + ipcCompany + "'"
           + (IF lValidTag  THEN " AND rm-bin.tag EQ '" + ipcInventoryStockID + "'" ELSE "")
           + (IF lValidItem THEN " AND rm-bin.i-no EQ '" + ipcPrimaryID + "'" ELSE "")
           + (IF lValidLoc  THEN " AND rm-bin.loc EQ '" + ipcWarehouseID + "'" ELSE "")
           + (IF lValidBin  THEN " AND rm-bin.loc-bin EQ '" + ipcLocationID + "'" ELSE "")
           + (IF lValidPoID THEN " AND rm-bin.po-no EQ " + STRING(ipiPoID) ELSE "")
           + (IF iplIncludeZeroQty THEN "AND rm-bin.qty NE 0 " + "AND rm-bin.qty NE ?" ELSE "")
           .
       
    hdQuery:SET-BUFFERS(hdBuffer).
    hdQuery:QUERY-PREPARE(cQuery).
    hdQuery:QUERY-OPEN().
    hdQuery:GET-FIRST().
    
    REPEAT:
        iCount = iCount + 1.
        
        IF hdQuery:QUERY-OFF-END OR iCount GT 1000 THEN DO:
            IF iCount GT 1000 THEN
                opcMessage = "Large number of records available for this search. Limiting to 1000 records".
            LEAVE.
        END.
    
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ hdBuffer:BUFFER-FIELD("company"):BUFFER-VALUE
               AND loadtag.item-type EQ YES
               AND loadtag.tag-no    EQ hdBuffer:BUFFER-FIELD("tag"):BUFFER-VALUE
             NO-ERROR.
             
        FIND FIRST item NO-LOCK
             WHERE item.company EQ hdBuffer:BUFFER-FIELD("company"):BUFFER-VALUE
               AND item.i-no    EQ hdBuffer:BUFFER-FIELD("i-no"):BUFFER-VALUE
             NO-ERROR.
             
        CREATE ttItem.
        ASSIGN
            ttItem.WarehouseID             = hdBuffer:BUFFER-FIELD("loc"):BUFFER-VALUE
            ttItem.LocationID              = hdBuffer:BUFFER-FIELD("loc-bin"):BUFFER-VALUE
            ttItem.PrimaryID               = hdBuffer:BUFFER-FIELD("i-no"):BUFFER-VALUE
            ttItem.InventoryStockID        = hdBuffer:BUFFER-FIELD("tag"):BUFFER-VALUE
            ttItem.Quantity                = hdBuffer:BUFFER-FIELD("qty"):BUFFER-VALUE
            ttItem.ItemType                = ipcItemType
            ttItem.tag                     = IF AVAILABLE loadtag THEN
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
            ttItem.POID                    = hdBuffer:BUFFER-FIELD("po-no"):BUFFER-VALUE
            ttItem.sourceRowID             = hdBuffer:ROWID
            .
        
        hdQuery:GET-NEXT().
    END.
END.
IF VALID-HANDLE(hdInventoryProcs) THEN
    DELETE PROCEDURE hdInventoryProcs.
IF VALID-HANDLE(hdBuffer) THEN
    DELETE OBJECT hdBuffer.
IF VALID-HANDLE(hdQuery) THEN DO:
    IF hdQuery:IS-OPEN THEN
        hdQuery:QUERY-CLOSE().
    DELETE OBJECT hdQuery.
END.











    
    


