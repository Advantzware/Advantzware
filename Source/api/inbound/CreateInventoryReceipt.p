/*------------------------------------------------------------------------
    File        : api\inbound\CreateInventoryReceipt.p
    Purpose     : Processes request data

    Syntax      :

    Description : Processes request data

    Author(s)   : Vishnu Vellanki
    Created     : Tue Oct 09 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{inventory/ttinventory.i "NEW SHARED"}.
{jc/jcgl-sh.i  NEW}

DEFINE INPUT  PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcInventoryStockID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipdQuantity                AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiPONo                    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPOLine                  AS INTEGER   NO-UNDO. 
DEFINE INPUT  PARAMETER ipcJobID                   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcJobID2                  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiQuantityPerSubUnit      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcWarehouseID             AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID              AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername                AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess                 AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage                 AS CHARACTER NO-UNDO.

/* This will eventually move to setsession */
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hTags    AS HANDLE NO-UNDO.

g_company=ipcCompany.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).
RUN system/TagProcs.p PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).
{sys/inc/var.i "new shared"}

DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidPONo       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidTag        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidPOLine     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTransfer        AS CHARACTER NO-UNDO INITIAL "R".
DEFINE VARIABLE iRNo             AS INTEGER   NO-UNDO.
DEFINE VARIABLE lValidBin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidLoc        AS LOGICAL   NO-UNDO.

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

ASSIGN
    oplSuccess    = YES
    lValidPONo    = YES
    lValidCompany = YES
    lValidPOLine  = YES
    ipcQuantityUOM = IF ipcQuantityUOM NE "" THEN
                         ipcQuantityUOM 
                     ELSE
                         "EA"
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

/* Validate Tag */
IF ipcInventoryStockID  EQ "" THEN DO:
    ASSIGN 
        opcMessage = "Invalid Tag#"                     
        oplSuccess = NO
        .
    RETURN.
END.

/* Validates PO Number and Quantity when tag is not registered */
FIND FIRST loadtag NO-LOCK
     WHERE loadtag.tag-no EQ ipcInventoryStockID
       AND NOT ITEM-TYPE
     NO-ERROR.
FIND FIRST fg-bin NO-LOCK
     WHERE fg-bin.tag EQ ipcInventoryStockID
       AND NOT ITEM-TYPE
     NO-ERROR.
IF NOT AVAILABLE loadtag AND NOT AVAILABLE fg-bin THEN DO:
    IF ipiPONo EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "TAG is not registered. Please provide valid PO"                    
            oplSuccess = NO
            .
        RETURN.
    END.
    IF ipdQuantity EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "Quantity must be greater than 0"                     
            oplSuccess = NO
            .
        RETURN.
    END.
END.
     
/* Validate PO Number */
IF ipiPONo NE 0 THEN DO:
    RUN ValidatePO IN hdInventoryProcs (
        ipcCompany,
        ipiPONo,
        OUTPUT lValidPONo
        ).

    IF NOT lValidPONo THEN DO:
        ASSIGN 
            opcMessage = "Invalid POID"                     
            oplSuccess = NO
            .
        RETURN.
    END.
END.

/* Validate PO Line and if PO Line is not valid, then it dafults to 1*/
lValidPOLine = CAN-FIND(FIRST po-ordl NO-LOCK
                         WHERE po-ordl.company EQ ipcCompany
                           AND po-ordl.po-no   EQ ipiPONo
                           AND po-ordl.line    EQ ipiPOLine).
ipiPOLine = IF lValidPOLine THEN 
                ipiPOLine
            ELSE
                1.

/* Validate JobID if it is non-blank */
IF ipcJobID NE "" THEN DO:
    FIND FIRST job-hdr NO-LOCK 
         WHERE job-hdr.company EQ ipcCompany
           AND job-hdr.job-no  EQ ipcJobID
        NO-ERROR.
    IF NOT AVAILABLE job-hdr THEN 
        ASSIGN
            ipcJobID  = "0"
            ipcJobID2 = "0"
            .
END.

IF ipcWarehouseID NE "" THEN DO:
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

IF ipcLocationID NE "" THEN DO:
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

IF NOT oplSuccess THEN
    RETURN.

/* Checks fg-rctd record for the input tag and quantity */     
FIND FIRST fg-rctd NO-LOCK  
     WHERE fg-rctd.company EQ ipcCompany
       AND fg-rctd.tag     EQ ipcInventoryStockID
       AND fg-rctd.r-no    NE 0
     NO-ERROR.

/* Checks whether TAG details are already posted or not */
IF AVAILABLE fg-rctd THEN DO:
    ASSIGN
        opcMessage = "FG receipt for the TAG " + ipcInventoryStockID + " is already used"
        oplSuccess = NO
        .
    RETURN.
END. 

{sys/inc/sspostfg.i}
  
/* Checks sys-ctrl */
IF NOT SSPostFG-log THEN DO:
    ASSIGN
        opcMessage = "FG receipt can not be POSTED as sys ctrl is not available"
        oplSuccess = NO
        .
    RETURN.
END. 
    
/* Creates receipts  */	
RUN pFGRecordCreation (
    INPUT ipcCompany,
    INPUT ipcInventoryStockID,
    INPUT ipdQuantity,
    INPUT ipcQuantityUOM,
    INPUT ipiPONo,
    INPUT ipiPOLine,
    INPUT ipcJobID,
    INPUT ipcJobID2,
    INPUT ipiQuantityPerSubUnit,
    INPUT ipiQuantitySubUnitsPerUnit,
    INPUT ipcWarehouseID,
    INPUT ipcLocationID
    ) NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN
        opcMessage = "FG Recipt is not posted"
        oplSuccess = NO
        .
    RETURN.
END.

/* Posts Receipts */
RUN PostFinishedGoodsForUser IN hdInventoryProcs(
    INPUT        ipcCompany,
    INPUT        cTransfer,
    INPUT        ipcUsername,
    INPUT-OUTPUT oplSuccess,
    INPUT-OUTPUT opcMessage
    )NO-ERROR.
IF ERROR-STATUS:ERROR THEN DO:
    ASSIGN
        opcMessage = "FG Recipt is not posted"
        oplSuccess = NO
        .
    RETURN.
END.

PROCEDURE pFGRecordCreation PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new fg-rctd record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantity                AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONo                    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine                  AS INTEGER   NO-UNDO. 
    DEFINE INPUT  PARAMETER ipcJobID                   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID2                  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityPerSubUnit      AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID              AS CHARACTER NO-UNDO.
    
    /* Retrieving last record of fg-rctd table*/ 
    FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
    IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iRNo THEN
        iRNo = fg-rctd.r-no.
    
    CREATE fg-rctd.
    ASSIGN
        fg-rctd.r-no       = iRNo + 1
        fg-rctd.rct-date   = TODAY
        fg-rctd.trans-time = TIME
        fg-rctd.company    = ipcCompany
        fg-rctd.rita-code  = "R"
        fg-rctd.tag        = ipcInventoryStockID
        fg-rctd.qty        = ipdQuantity
        fg-rctd.loc        = ipcWarehouseID
        fg-rctd.loc-bin    = ipcLocationID
        fg-rctd.created-by = ipcUserName
        fg-rctd.updated-by = ipcUserName
        fg-rctd.po-no      = STRING(ipiPONo)
        fg-rctd.po-line    = ipiPOLine
        fg-rctd.qty-case   = ipiQuantityPerSubUnit
        fg-rctd.cases-unit = ipiQuantitySubUnitsPerUnit
        fg-rctd.cases      = IF ipiQuantityPerSubUnit NE 0 THEN
                                 TRUNC((ipdQuantity / ipiQuantityPerSubUnit),1)
                             ELSE
                                 0
        fg-rctd.partial    = IF ipiQuantityPerSubUnit NE 0 THEN
                                 ipdQuantity MODULO ipiQuantityPerSubUnit
                             ELSE
                                 0
        fg-rctd.pur-uom   = ipcQuantityUOM
        .
    FIND FIRST fg-rctd 
         WHERE fg-rctd.tag EQ ipcInventoryStockID
         NO-ERROR.
    IF NOT AVAILABLE fg-rctd THEN DO:
        ASSIGN
            opcMessage = "FG receipt is not created"
            oplSuccess = NO
            .
        RETURN.
    END. 

    /* Validates whether Tag is registered or not */
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.tag-no    EQ ipcInventoryStockID
           AND loadtag.item-type EQ NO
         NO-ERROR.
    IF NOT AVAILABLE loadtag THEN DO:
    
        FIND FIRST fg-bin NO-LOCK
             WHERE fg-bin.company EQ ipcCompany
               AND fg-bin.tag     EQ ipcInventoryStockID
        NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN DO:
        
            FIND FIRST po-ordl NO-LOCK
                 WHERE po-ordl.company EQ ipcCompany
                   AND po-ordl.po-no   EQ ipiPONo
                   AND po-ordl.line    EQ ipiPoLine
                 NO-ERROR.
            
            /* Gets location and bin if they are null */
            IF ipcWarehouseID EQ "" AND ipcLocationID EQ "" THEN
                
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ ipcCompany
                       AND itemfg.i-no    EQ po-ordl.i-no
                     NO-ERROR.
                IF AVAILABLE itemfg THEN
                    RUN fg/autopost.p (
                        INPUT ROWID(itemfg),
                        INPUT  ipcJobID,
                        INPUT  INT(ipcJobID2),
                        OUTPUT ipcWarehouseID, 
                        OUTPUT ipcLocationID
                        ).
            ASSIGN
                fg-rctd.i-no    = IF AVAILABLE po-ordl THEN
                                         po-ordl.i-no
                                     ELSE
                                         ""
                fg-rctd.i-name  = IF AVAILABLE po-ordl THEN
                                         po-ordl.i-name
                                     ELSE
                                         ""
                fg-rctd.job-no  = ipcJobID
                fg-rctd.job-no2 = INT(ipcJobID2)
                fg-rctd.loc     = ipcWarehouseID
                fg-rctd.loc-bin = ipcLocationID
                .
             RETURN.
        END.
        ASSIGN
            fg-rctd.i-no       = fg-bin.i-no
            fg-rctd.qty-case   = IF ipiQuantityPerSubUnit NE 0 THEN
                                     ipiQuantityPerSubUnit
                                 ELSE
                                    fg-bin.case-count
            fg-rctd.cases-unit = IF ipiQuantitySubUnitsPerUnit NE 0 THEN
                                     ipiQuantitySubUnitsPerUnit
                                 ELSE
                                    fg-bin.cases-unit
            fg-rctd.job-no   = IF ipcJobID NE "" THEN
                                     ipcJobID
                                 ELSE
                                    fg-bin.job-no
            fg-rctd.job-no2  = IF ipcJobID2 NE "" THEN
                                     INT(ipcJobID2)
                                 ELSE
                                    fg-bin.job-no2
            fg-rctd.cases      = IF fg-rctd.cases EQ 0 THEN
                                     (-1 * TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
                                 ELSE
                                     fg-rctd.cases
            fg-rctd.partial    = IF fg-rctd.partial EQ 0 THEN
                                     -1 * fg-bin.qty
                                 ELSE
                                     fg-rctd.partial
  
            fg-rctd.loc        = IF fg-rctd.loc EQ "" THEN
                                     fg-bin.loc
                                 ELSE
                                     fg-rctd.loc
            fg-rctd.loc-bin    = IF fg-rctd.loc-bin EQ "" THEN
                                     fg-bin.loc-bin
                                 ELSE
                                     fg-rctd.loc-bin
            fg-rctd.loc-bin    = IF fg-rctd.loc-bin EQ "" THEN
                                     fg-bin.loc-bin
                                 ELSE
                                     fg-rctd.loc-bin
            fg-rctd.qty       = IF fg-rctd.qty EQ 0 THEN
                                     fg-bin.qty
                                 ELSE
                                     fg-rctd.qty
  
            .
            RETURN.
  
    END.
  
    ASSIGN
        fg-rctd.i-no       = loadtag.i-no
        fg-rctd.i-name     = loadtag.i-name
        fg-rctd.stack-code = loadtag.misc-char[2]
        fg-rctd.qty-case   = IF ipiQuantityPerSubUnit NE 0 THEN
                                 ipiQuantityPerSubUnit
                             ELSE
                                loadtag.qty-case
        fg-rctd.cases-unit = IF ipiQuantitySubUnitsPerUnit NE 0 THEN
                                 ipiQuantitySubUnitsPerUnit
                             ELSE
                                loadtag.case-bundle
  
        fg-rctd.job-no    = IF ipcJobID NE "" THEN
                                 ipcJobID
                             ELSE
                                loadtag.job-no
        fg-rctd.job-no2   = IF ipcJobID2 NE "" THEN
                                 INT(ipcJobID2)
                             ELSE
                                loadtag.job-no2
        fg-rctd.cases     = IF fg-rctd.cases EQ 0 THEN
                                 loadtag.case-bundle
                             ELSE
                                 fg-rctd.cases
        fg-rctd.partial   = IF fg-rctd.partial EQ 0 THEN
                                loadtag.partial
                             ELSE
                                 fg-rctd.partial
        fg-rctd.loc       = IF fg-rctd.loc EQ "" THEN
                                 loadtag.loc
                             ELSE
                                 fg-rctd.loc
        fg-rctd.loc-bin   = IF fg-rctd.loc-bin EQ "" THEN
                                 loadtag.loc-bin
                             ELSE
                                 fg-rctd.loc-bin
        fg-rctd.qty       = IF fg-rctd.qty EQ 0 THEN
                                 loadtag.qty
                             ELSE
                                 fg-rctd.qty
        .
        RETURN.

END PROCEDURE.












    
    


