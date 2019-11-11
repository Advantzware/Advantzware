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

DEFINE VARIABLE hdInventoryProcs     AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidCompany        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidPONo           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidTag            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidPOLine         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTransfer            AS CHARACTER NO-UNDO INITIAL "R".
DEFINE VARIABLE iRNo                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE lValidBin            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidLoc            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPrimaryID           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRegTag              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iRegTagLine          AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCostUOM             AS CHARACTER NO-UNDO.
DEFINE VARIABLE dStdCost             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dExtCost             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dFrtCost             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lAverageCost         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hdCostProcs          AS HANDLE    NO-UNDO.
DEFINE VARIABLE lFGPOFrt             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFound               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturn              AS CHARACTER NO-UNDO.

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.
RUN system\CostProcs.p PERSISTENT SET hdCostProcs. 
RUN sys/ref/nk1look.p (
    INPUT ipcCompany, 
    INPUT "FGPOFRT", 
    INPUT "L", 
    INPUT NO, 
    INPUT NO, 
    INPUT "", "", 
    OUTPUT cReturn, 
    OUTPUT lFound
    ).
    
ASSIGN
    oplSuccess     = YES
    lValidPONo     = YES
    lValidCompany  = YES
    lValidPOLine   = YES
    ipcQuantityUOM = IF ipcQuantityUOM NE "EA" AND ipcQuantityUOM NE "M" THEN
                        "EA"
                     ELSE
                        ipcQuantityUOM 
    cocode         = ipcCompany
    lFGPOFrt       = lFound AND cReturn EQ "YES".
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
        opcMessage = "InventoryStockID = " + ipcInventoryStockID + " is not valid because ('it is not found' or 'it is not active')"                     
        oplSuccess = NO
        .
    RETURN.
END.

/* Validate Job Number and PO Number */
IF ipiPONo NE 0 AND ipcJobID NE "" THEN DO:
    ASSIGN 
        opcMessage = "Enter either PO Number / Job Number for InventoryStockID " + ipcInventoryStockID.                    
        oplSuccess = NO
        .
    RETURN.
END.

/* Validates PO Number and Quantity when tag is not registered */
FIND FIRST loadtag NO-LOCK
     WHERE loadtag.tag-no EQ ipcInventoryStockID
       AND NOT loadtag.ITEM-TYPE
     NO-ERROR.
FIND FIRST fg-bin NO-LOCK
     WHERE fg-bin.tag EQ ipcInventoryStockID
     NO-ERROR.

ASSIGN
    lRegTag    = AVAILABLE loadtag 
    cPrimaryID = IF AVAILABLE loadtag THEN 
                    loadtag.i-no
                 ELSE IF AVAILABLE fg-bin THEN
                    fg-bin.i-no
                 ELSE
                    ""
    iRegTagLine = IF AVAILABLE loadtag THEN 
                    loadtag.line
                  ELSE
                    1
    .
                
IF NOT AVAILABLE loadtag AND NOT AVAILABLE fg-bin THEN DO:
    IF ipiPONo EQ 0 AND ipcJobID EQ "" THEN DO:
        ASSIGN 
            opcMessage = "Tag " + ipcInventoryStockID + " is not registered. Please provide valid PO Number / Job Number"                    
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
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company   EQ ipcCompany
           AND po-ordl.po-no     EQ ipiPONo
           AND po-ordl.item-type EQ NO
           AND (po-ordl.i-no EQ cPrimaryID OR 
                cPrimaryID EQ "")
         NO-ERROR.
      
    IF NOT AVAILABLE po-ordl THEN DO:
        ASSIGN 
            opcMessage = "Invalid POID for Inventorystockid" + ipcInventoryStockID                  
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate PO Line */
    IF NOT lRegTag THEN DO:
       FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company EQ ipcCompany
               AND po-ordl.po-no   EQ ipiPONo
               AND po-ordl.line    EQ ipiPOLine
             NO-ERROR.

           ASSIGN
               lValidPOLine = AVAILABLE po-ordl
               cPrimaryID   = IF AVAILABLE po-ordl THEN
                                po-ordl.i-no
                              ELSE
                                  cPrimaryID
               .
       
    END.
    ELSE
       lValidPOLine = ipiPOLine EQ iRegTagLine.
    
    IF NOT lValidPOLine THEN DO:
    ASSIGN 
        opcMessage = "Invalid POLine for inventorystockid " + ipcInventoryStockID                    
        oplSuccess = NO
        .
        
    RETURN.

    END.
    
    RUN GetCostsFromPO (
        INPUT ipcCompany, 
        INPUT ipiPONo,
        INPUT ipiPOLine,
        INPUT cPrimaryID,
        INPUT ipdQuantity,
        OUTPUT dStdCost, 
        OUTPUT cCostUOM, 
        OUTPUT dExtCost, 
        OUTPUT dFrtCost
        ). 

END.
  

/* Validate JobID & JobID2 if it they are non-blank */
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
    IF ipcJobID NE "0" AND ipcJobID2 NE "" THEN DO:
        FOR EACH job-hdr
            WHERE job-hdr.company EQ ipcCompany
              AND job-hdr.job-no  EQ ipcJobID
              AND job-hdr.job-no2 EQ INT(ipcJobID2)
            NO-LOCK,
            FIRST job
            WHERE job.company EQ job-hdr.company
              AND job.job     EQ job-hdr.job
              AND job.job-no  EQ job-hdr.job-no
              AND job.job-no2 EQ job-hdr.job-no2
            NO-LOCK:
            LEAVE.
        END.
      
        IF NOT AVAILABLE job-hdr THEN
            FOR EACH job
                WHERE job.company EQ ipcCompany
                  AND job.job-no  EQ ipcJobID
                  AND job.job-no2 EQ INT(ipcJobID2)
                NO-LOCK,
                FIRST job-hdr
                WHERE job-hdr.company EQ job.company
                  AND job-hdr.job     EQ job.job
                  AND job-hdr.job-no  EQ job.job-no
                  AND job-hdr.job-no2 EQ job.job-no2
                NO-LOCK:
                LEAVE.
            END.

        IF NOT AVAILABLE job-hdr THEN DO:
            ASSIGN
                opcMessage = "Invalid JobID2 "                     
                oplSuccess = NO
                .
            RETURN.
        END. 
    END.
    ASSIGN
        cPrimaryID = IF AVAILABLE job-hdr THEN
                        job-hdr.i-no
                     ELSE
                         ""
        dStdCost   = job-hdr.std-mat-cost +
                     job-hdr.std-lab-cost +
                     job-hdr.std-fix-cost +
                     job-hdr.std-var-cost
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

FIND FIRST fg-ctrl NO-LOCK 
     WHERE fg-ctrl.company EQ ipcCompany
     NO-ERROR.
lAverageCost = AVAILABLE fg-ctrl AND fg-ctrl.inv-meth EQ "A".

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
        opcMessage = 'FG receipt can not be POSTED as system control ' + '"SSPostFG"' + ' is not available'
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
        opcMessage = "FG Recipt is not posted #1 - " + ERROR-STATUS:GET-MESSAGE(1)
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
        opcMessage = "FG Recipt is not posted #2 - " + ERROR-STATUS:GET-MESSAGE(1)
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
        
    FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.        
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iRNo THEN 
        iRNo = fg-rcpth.r-no.
    
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
        fg-rctd.std-cost  = dStdCost
        fg-rctd.cost-uom  = cCostUOM
        fg-rctd.ext-cost  = dExtCost
        fg-rctd.frt-cost  = dFrtCost       
        .
    FIND FIRST fg-rctd 
         WHERE fg-rctd.tag EQ ipcInventoryStockID
         NO-ERROR.
    IF NOT AVAILABLE fg-rctd THEN DO:
        ASSIGN
            opcMessage = "FG receipt for the tag " + ipcInventoryStockID + " is not created"
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

            /* Gets location and bin if they are null */
            IF ipcWarehouseID EQ "" AND ipcLocationID EQ "" THEN
                
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ ipcCompany
                       AND itemfg.i-no    EQ cPrimaryID
                       AND itemfg.stat    EQ "A"
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
                fg-rctd.i-no    = cPrimaryID
                fg-rctd.i-name  = IF AVAILABLE itemfg THEN
                                      itemfg.i-name
                                  ELSE
                                      ""
                fg-rctd.job-no  = ipcJobID
                fg-rctd.job-no2 = INT(ipcJobID2)
                fg-rctd.loc     = ipcWarehouseID
                fg-rctd.loc-bin = ipcLocationID
                .
            IF fg-rctd.loc     EQ "" OR 
               fg-rctd.loc-bin EQ "" OR 
               fg-rctd.std-cost EQ 0 THEN DO:
                RUN GetValuesFromItemFG (
                    INPUT        ipcCompany,
                    INPUT        fg-bin.i-no,
                    INPUT-OUTPUT ipcWarehouseID,
                    INPUT-OUTPUT ipcLocationID,
                    INPUT-OUTPUT dStdCost
                    ). 
                ASSIGN
                    fg-rctd.loc      = ipcWarehouseID
                    fg-rctd.loc-bin  = ipcLocationID 
                    fg-rctd.std-cost = dStdCost
                    .  
            END.
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
            
            IF fg-rctd.loc      EQ "" OR 
               fg-rctd.loc-bin  EQ "" OR 
               fg-rctd.std-cost EQ 0 THEN DO:
                RUN GetValuesFromItemFG (
                    INPUT        ipcCompany,
                    INPUT        fg-bin.i-no,
                    INPUT-OUTPUT ipcWarehouseID,
                    INPUT-OUTPUT ipcLocationID,
                    INPUT-OUTPUT dStdCost
                    ). 
                ASSIGN
                    fg-rctd.loc     = ipcWarehouseID
                    fg-rctd.loc-bin = ipcLocationID 
                    fg-rctd.std-cost = dStdCost
                    .  
            END.
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
        fg-rctd.qty       = IF fg-rctd.qty EQ 0 THEN
                                 loadtag.qty
                             ELSE
                                 fg-rctd.qty
        fg-rctd.loc       = IF fg-rctd.loc EQ "" THEN
                                loadtag.loc
                            ELSE
                                fg-rctd.loc
        fg-rctd.loc-bin   = IF fg-rctd.loc-bin EQ "" THEN
                                 loadtag.loc-bin
                             ELSE
                                 fg-rctd.loc-bin
        .
        
        IF fg-rctd.loc      EQ "" OR 
           fg-rctd.loc-bin  EQ "" OR
           fg-rctd.std-cost EQ 0 THEN DO:
            RUN GetValuesFromItemFG (
                INPUT        ipcCompany,
                INPUT        loadtag.i-no,
                INPUT-OUTPUT ipcWarehouseID,
                INPUT-OUTPUT ipcLocationID,
                INPUT-OUTPUT dStdCost
                ). 
            ASSIGN
                fg-rctd.loc      = ipcWarehouseID
                fg-rctd.loc-bin  = ipcLocationID 
                fg-rctd.std-cost = dStdCost
                .  
        END.    
        RETURN.

END PROCEDURE.

PROCEDURE GetValuesFromItemFG:
/*------------------------------------------------------------------------------
 Purpose: Gets location,bin and cost values
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcPrimaryID    AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopcLocationID  AS CHARACTER NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopdStdCost     AS DECIMAL   NO-UNDO. 
    
    FIND FIRST itemfg NO-LOCK
     WHERE itemfg.company EQ ipcCompany
       AND itemfg.i-no    EQ ipcPrimaryID
       AND itemfg.stat    EQ "A"
     NO-ERROR.
    
    IF AVAILABLE itemfg THEN
        ASSIGN
           iopcWarehouseID = IF iopcWarehouseID EQ ""  THEN
                                itemfg.def-loc
                             ELSE
                                 iopcWarehouseID 
           iopcLocationID  = IF iopcLocationID  EQ "" THEN
                                itemfg.def-loc-bin
                             ELSE 
                                 iopcLocationID
           iopdStdCost     = IF iopdStdCost NE 0 THEN
                                iopdStdCost
                             ELSE IF lAverageCost THEN
                                itemfg.avg-cost
                             ELSE
                                itemfg.last-cost
           .
           
END PROCEDURE.

PROCEDURE GetCostsFromPO:
/*------------------------------------------------------------------------------
 Purpose: Gets costs from PO values
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONumber         AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcFGItemID         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty              AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostPerUOM       AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM          AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotal        AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCostTotalFreight AS DECIMAL   NO-UNDO.

    DEFINE VARIABLE dCostPerEA        AS DECIMAL.
    DEFINE VARIABLE dCostFreight      AS DECIMAL.
    DEFINE VARIABLE dCostFreightPerEA AS DECIMAL.
    DEFINE VARIABLE lFound            AS LOGICAL.
    
    RUN GetCostForPOLine IN hdCostProcs (
        INPUT ipcCompany, 
        INPUT ipiPONumber, 
        INPUT ipiPOLine, 
        INPUT ipcFGItemID, 
        OUTPUT opdCostPerUOM, 
        OUTPUT opcCostUOM, 
        OUTPUT dCostFreight, 
        OUTPUT lFound
        ).
    
    ASSIGN
        dCostPerEA          = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, opdCostPerUOM)
        dCostFreightPerEA   = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs, opcCostUOM, "EA",0,0,0,0,1,1, dCostFreight)
        opdCostTotal        = ipdQty * dCostPerEA
        opdCostTotalFreight = ipdQty * dCostFreightPerEA
        opdCostTotal        = IF lFGPOFrt THEN 
                                  opdCostTotal + opdCostTotalFreight
                              ELSE
                                  0
        .

END PROCEDURE.








    
    


