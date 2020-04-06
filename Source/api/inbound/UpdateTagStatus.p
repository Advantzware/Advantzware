/*------------------------------------------------------------------------
    File        : api/inbound/UpdateTagStatus.p
    Purpose     : Fetches Inventory response data for the given input data

    Syntax      :

    Description : Fetches Inventory response data for the given input data

    Author(s)   : Vishnu Vellanki
    Created     : Tue Apr 02 07:33:22 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/
{inventory/ttinventory.i "NEW SHARED"}.

DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiPONo             AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcJobNo            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiJobNo2           AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiBOLNo            AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockID AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcWareHouseID      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcStatusID         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserName         AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess          AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage          AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidLoc        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidBin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cQuery           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValidTag        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lItemType        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cItemTypeFG      AS CHARACTER NO-UNDO INITIAL "FG".
DEFINE VARIABLE hdBuffer         AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdQuery          AS HANDLE    NO-UNDO.
DEFINE VARIABLE cTableFG         AS CHARACTER NO-UNDO INITIAL "fg-bin".
DEFINE VARIABLE lValidJobNo      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidPONo       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidBOLNo      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE rifgbin          AS ROWID     NO-UNDO.

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

ASSIGN
    oplSuccess    = YES
    lValidLoc     = ipcWareHouseID NE ""
    lValidBin     = ipcLocationID NE "" 
    lValidTag     = ipcInventoryStockID NE "" 
    lValidJobNo   = ipcJobNo NE "" 
    lValidPONo    = ipiPONo  NE 0 
    lValidBOLNo   = ipiBOLNo  NE 0 
    .
 
/* Validate company */
IF NOT CAN-FIND(FIRST company NO-LOCK
            WHERE company.company EQ ipcCompany) THEN DO:
    ASSIGN 
        opcMessage = opcMessage + "Invalid Company for item (" + ipcPrimaryID + ")" 
        oplSuccess = NO
        .
    RETURN.
END.

/* Validate StatusID */
IF NOT CAN-FIND(FIRST inventoryStatusType NO-LOCK
            WHERE inventoryStatusType.statusID EQ ipcStatusID
              AND NOT inventoryStatusType.inActive) THEN DO:
    ASSIGN 
        opcMessage = opcMessage + "Invalid StatusID for item (" + ipcPrimaryID + ")" 
        oplSuccess = NO
        .
    RETURN.
END.

/* Validate Item */
IF NOT CAN-FIND(FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipcCompany
              AND itemfg.i-no    EQ ipcPrimaryID) THEN DO:
    ASSIGN 
        opcMessage = opcMessage + "Invalid item"
        oplSuccess = NO
        .
    RETURN.
END.

/* Validate Item Type */
IF ipcItemType NE cItemTypeFG THEN DO:
    ASSIGN 
        opcMessage = opcMessage + "Invalid Item Type for item (" + ipcPrimaryID + ")" 
        oplSuccess = NO
        .
    RETURN.
END.

/* Validates WarehouseID & LocationID for non-blank values */
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
            opcMessage = "Invalid WareHouseID for item (" + ipcPrimaryID + ")" 
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
            opcMessage = "Invalid LocationID for item (" + ipcPrimaryID + ")" 
            oplSuccess = NO
            .

        RETURN.
    END.
END.

/* Validate inventory Stock ID */
IF lValidTag THEN DO:
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.item-type EQ NO
           AND loadtag.tag-no    EQ ipcInventoryStockID
         NO-ERROR.
    IF NOT AVAILABLE loadtag THEN DO:
        ASSIGN
            opcMessage = "Invalid tag for item (" + ipcPrimaryID + ")" 
            oplSuccess = NO
            .

        RETURN.
    END.
END.

/* Writes response data to temp table*/
IF ipcItemType EQ cItemTypeFG THEN DO:
    CREATE BUFFER hdBuffer FOR TABLE cTableFG.
    CREATE QUERY hdQuery.
    cQuery = "FOR EACH fg-bin NO-LOCK WHERE fg-bin.company EQ '" + ipcCompany + "' AND fg-bin.i-no EQ '" + ipcPrimaryID + "'"
           + (IF lValidTag    THEN " AND fg-bin.tag EQ '" + ipcInventoryStockID + "'" ELSE "")
           + (IF lValidLoc    THEN " AND fg-bin.loc EQ '" + ipcWarehouseID + "'" ELSE "")
           + (IF lValidBin    THEN " AND fg-bin.loc-bin EQ '" + ipcLocationID + "'" ELSE "")
           + (IF lValidJobNo  THEN " AND fg-bin.job-no EQ '" + ipcJobNo + "'" ELSE "")
           + (IF lValidJobNo  THEN " AND fg-bin.job-no2 EQ '" + STRING(ipiJobNo2) + "'" ELSE "")
           + (IF lValidPONo   THEN " AND fg-bin.po-no EQ '" + STRING(ipiPONo) + "'" ELSE "")
           + (IF lValidBOLNo  THEN " AND fg-bin.bol-no EQ '" + STRING(ipiBOLNo) + "'" ELSE "")          
           + " AND fg-bin.qty NE 0 "
           + "AND fg-bin.qty NE ?".
    hdQuery:SET-BUFFERS(hdBuffer).
    hdQuery:QUERY-PREPARE(cQuery).
    hdQuery:QUERY-OPEN().
    hdQuery:GET-FIRST(). 
    IF NOT hdBuffer:AVAILABLE THEN DO:
        ASSIGN
            opcMessage = "No FG Bins found for item (" + ipcPrimaryID + ")" 
            oplSuccess = NO
            .

        RETURN.
    END.
    DO TRANSACTION ON ERROR UNDO, LEAVE:
        REPEAT:
            IF hdQuery:QUERY-OFF-END THEN
                LEAVE.

            rifgbin = hdQuery:GET-BUFFER-HANDLE(1):ROWID.

            RUN UpdateTagStatusID IN hdInventoryProcs (
                INPUT  rifgbin,
                INPUT  ipcStatusID,
                OUTPUT oplSuccess,
                OUTPUT opcMessage
                ) NO-ERROR. 
                          
            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN
                UNDO, LEAVE.
            
            hdQuery:GET-NEXT().
            
        END.
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
