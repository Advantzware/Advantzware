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

DEFINE INPUT        PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcInventoryStockID        AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipdQuantity                AS DECIMAL   NO-UNDO.
DEFINE INPUT        PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiPONo                   AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiPOLine                  AS INTEGER   NO-UNDO. 
DEFINE INPUT-OUTPUT PARAMETER iopcJobID                  AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcJobID2                  AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipiQuantityPerSubUnit      AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
DEFINE INPUT        PARAMETER ipcWarehouseID             AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcLocationID              AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcSSPostFG                AS CHARACTER NO-UNDO.
DEFINE INPUT        PARAMETER ipcUsername                AS CHARACTER NO-UNDO.
DEFINE OUTPUT       PARAMETER opriRctd                   AS ROWID     NO-UNDO.
DEFINE OUTPUT       PARAMETER opdFinalQuantity           AS DECIMAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER oplSuccess                 AS LOGICAL   NO-UNDO.
DEFINE OUTPUT       PARAMETER opcMessage                 AS CHARACTER NO-UNDO.

{api\inbound\ttRctd.i}

/* This will eventually move to setsession - START >>>*/
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.

g_company=ipcCompany.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).
RUN system/TagProcs.p PERSISTENT SET hdTags.
SESSION:ADD-SUPER-PROCEDURE (hdTags).
{sys/inc/var.i "new shared"}
/* END <<<*/

DEFINE VARIABLE hdInventoryProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE lValidTag          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReceipt           AS CHARACTER NO-UNDO INITIAL "R".
DEFINE VARIABLE iRNo               AS INTEGER   NO-UNDO.
DEFINE VARIABLE lValidBin          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidLoc          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPrimaryID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRegTag            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cCostUOM           AS CHARACTER NO-UNDO.
DEFINE VARIABLE dStdCost           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dExtCost           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dFrtCost           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lItemtype          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hdInventoryReceipt AS HANDLE    NO-UNDO.
DEFINE VARIABLE lPromptForClose    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lAutoIssue         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRecFound          AS LOGICAL   NO-UNDO.

RUN api\inbound\InventoryReceiptProcs.p PERSISTENT SET hdInventoryReceipt.
RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.
   
ASSIGN
    oplSuccess     = YES
    ipcQuantityUOM = IF ipcQuantityUOM NE "EA" AND ipcQuantityUOM NE "M" THEN
                         "EA" /* Each */
                     ELSE
                         ipcQuantityUOM 
    cocode         = ipcCompany
    .

/* Validate company */
IF NOT CAN-FIND(FIRST company NO-LOCK
                WHERE company.company EQ ipcCompany) THEN DO:
    ASSIGN 
        opcMessage = "Invalid Company entered for Tag  (" + ipcInventoryStockID + ")"
        oplSuccess = NO
        .
		
    RETURN.
END.

/* Validate Tag */
IF ipcInventoryStockID EQ "" THEN DO:
    ASSIGN 
        opcMessage = "Tag can not be empty"                   
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validate Job Number and PO Number */
IF iopiPONo NE 0 AND iopcJobID NE "" THEN DO:
    ASSIGN 
        opcMessage = "Enter either PO Number or Job Number for Tag  (" + ipcInventoryStockID + ")".                    
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validates PO Number and Quantity when tag is not registered */
FIND FIRST loadtag NO-LOCK
     WHERE loadtag.company   EQ ipcCompany
       AND loadtag.item-type EQ NO
       AND loadtag.tag-no    EQ ipcInventoryStockID
     NO-ERROR.
IF NOT AVAILABLE loadtag THEN DO:
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.item-type EQ YES
           AND loadtag.tag-no    EQ ipcInventoryStockID
         NO-ERROR.
END.

IF NOT AVAILABLE loadtag THEN DO:
    FIND FIRST fg-bin NO-LOCK
         WHERE fg-bin.company EQ ipcCompany
           AND fg-bin.tag     EQ ipcInventoryStockID
         NO-ERROR.
END.

IF NOT AVAILABLE fg-bin AND NOT AVAILABLE loadtag THEN DO:
    IF iopiPONo EQ 0 AND iopcJobID EQ "" THEN DO:
        ASSIGN 
            opcMessage = "Tag  (" + ipcInventoryStockID + ") is not registered. Please provide valid PO Number / Job Number"                    
            oplSuccess = NO
            .
                    
        RETURN.
    END.
    
    IF ipdQuantity EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "Quantity can not be zero for Tag (" + ipcInventoryStockID + ")"                   
            oplSuccess = NO
            .
    					
        RETURN.
    END.
    
    IF ipiQuantityPerSubUnit LE 0 THEN DO:
        ASSIGN 
            opcMessage = "Units must be greater than zero for Tag (" + ipcInventoryStockID + ")"                   
            oplSuccess = NO
            .
    					
        RETURN.
    END.
    
END.

ASSIGN
    lRegTag                    = AVAILABLE loadtag OR AVAILABLE fg-bin
    cPrimaryID                 = IF AVAILABLE loadtag THEN 
                                   loadtag.i-no
                                 ELSE IF AVAILABLE fg-bin THEN
                                   fg-bin.i-no
                                 ELSE
                                   ""
    lItemType                  = AVAILABLE loadtag AND loadtag.item-type
    ipdQuantity                = IF ipdQuantity NE 0 THEN
                                     ipdQuantity
                                 ELSE IF AVAILABLE loadtag THEN
                                     loadtag.qty
                                 ELSE IF AVAILABLE fg-bin THEN
                                     fg-bin.qty
                                 ELSE
                                     0
    ipiQuantityPerSubUnit      = IF ipiQuantityPerSubUnit NE 0 THEN
                                     ipiQuantityPerSubUnit
                                 ELSE IF AVAILABLE loadtag THEN
                                     loadtag.qty-case
                                 ELSE IF AVAILABLE fg-bin THEN
                                     fg-bin.case-count
                                 ELSE
                                     0
    ipiQuantitySubUnitsPerUnit = IF ipiQuantitySubUnitsPerUnit NE 0 THEN
                                    ipiQuantitySubUnitsPerUnit 
                                 ELSE IF AVAILABLE loadtag THEN
                                    loadtag.case-bundle
                                 ELSE IF AVAILABLE fg-bin THEN
                                    fg-bin.cases-unit
                                 ELSE
                                    0
    . 
    
/* Validate PO Number */
IF iopiPONo NE 0 THEN DO:
    FIND FIRST po-ord NO-LOCK
         WHERE po-ord.company EQ ipcCompany
           AND po-ord.po-no   EQ iopiPONo
         NO-ERROR.
    IF NOT AVAILABLE po-ord THEN DO:
        ASSIGN 
            opcMessage = "Invalid PO Number entered for Tag  (" + ipcInventoryStockID + ")"                     
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate PO Line */
    IF lRegTag THEN
        FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company EQ ipcCompany
               AND po-ordl.po-no   EQ iopiPONo
               AND po-ordl.line    EQ ipiPOLine
               AND po-ordl.i-no    EQ cPrimaryID 
             NO-ERROR.
    ELSE
        FIND FIRST po-ordl NO-LOCK
             WHERE po-ordl.company EQ ipcCompany
               AND po-ordl.po-no   EQ iopiPONo
               AND po-ordl.line    EQ ipiPOLine
             NO-ERROR.

    IF NOT AVAILABLE po-ordl THEN DO:
        ASSIGN 
            opcMessage = "Invalid POLine entered for Tag  (" + ipcInventoryStockID + ")"                  
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    IF NOT lRegTag THEN 
        ASSIGN    
            cPrimaryID = po-ordl.i-no
            lItemType  = po-ordl.item-type
            .
            
    RUN InventoryReceipt_GetCostsFromPO IN hdInventoryReceipt (
        INPUT  ipcCompany, 
        INPUT  iopiPONo,
        INPUT  ipiPOLine,
        INPUT  cPrimaryID,
        INPUT  ipdQuantity,
        OUTPUT dStdCost, 
        OUTPUT cCostUOM, 
        OUTPUT dExtCost, 
        OUTPUT dFrtCost
        ) NO-ERROR. 
        
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            opcMessage = "Error while getting FG costs for Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1)
            oplSuccess = NO
            .    
                            
        RETURN.
    END.
END.

/* Validate non-blank JobID & JobID2 */
IF iopcJobID NE "" THEN DO:
    FIND FIRST job-hdr NO-LOCK 
         WHERE job-hdr.company EQ ipcCompany
           AND job-hdr.job-no  EQ iopcJobID
        NO-ERROR.    
    IF NOT AVAILABLE job-hdr THEN DO:
        /* Assigns 0 to JobID & JobID2 if JobID is invalid for registered tags */
        IF lRegTag THEN
            ASSIGN
                iopcJobID  = "0"                     
                ipcJobID2  = "0"
                .
        /* Throws error if JobID is invalid for un-registered tags */
        ELSE DO: 
            ASSIGN
                opcMessage = "Invalid JobID entered for Tag  (" + ipcInventoryStockID + ")"                    
                oplSuccess = NO
                .
                
            RETURN.
        END.
    END.
    /* Validate JobID2 if JobID is valid */
    IF AVAILABLE job-hdr THEN DO:
        FOR EACH  job-hdr
            WHERE job-hdr.company EQ ipcCompany
              AND job-hdr.job-no  EQ iopcJobID
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
                  AND job.job-no  EQ iopcJobID
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
                opcMessage = "Invalid JobID2 entered for Tag  (" + ipcInventoryStockID + ")"                    
                oplSuccess = NO
                .
                
            RETURN.
        END. 
    END.
    
    IF AVAILABLE job-hdr THEN
        ASSIGN
            cPrimaryID = IF NOT lRegTag THEN
                             job-hdr.i-no
                         ELSE
                             cPrimaryID
            dStdCost   = job-hdr.std-mat-cost +
                         job-hdr.std-lab-cost +
                         job-hdr.std-fix-cost +
                         job-hdr.std-var-cost
            .
END. 

/* Validates loc & bin if they are non-blank */
IF ipcWarehouseID NE "" AND ipcLocationID NE "" THEN DO:
    
    /* Validate warehouse */        
    RUN ValidateLoc IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  ipcWareHouseID,
        OUTPUT lValidLoc
        ).
    
    IF NOT lValidLoc THEN DO:
        ASSIGN 
            opcMessage = "Invalid WareHouseID entered for Tag  (" + ipcInventoryStockID + ")"                     
            oplSuccess = NO
            .
            
        RETURN.
    END.

   /* Validate location */
    RUN ValidateBin IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  ipcWareHouseID,
        INPUT  ipcLocationID,
        OUTPUT lValidBin
        ).
    
    IF ipcLocationID EQ "" OR NOT lValidBin THEN DO:
        ASSIGN 
            opcMessage = "Invalid LocationID entered for Tag  (" + ipcInventoryStockID + ")"
            oplSuccess = NO 
            .
            
        RETURN.
    END.
END.

/* Gets loc & bin from item if they are blank */
IF ipcWarehouseID EQ "" OR ipcLocationID EQ "" THEN DO:
    IF NOT lItemType THEN DO:
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ ipcCompany
               AND itemfg.i-no    EQ cPrimaryID
             NO-ERROR.
        IF AVAILABLE itemfg THEN    
            ASSIGN
                ipcWarehouseID             = itemfg.def-loc
                ipcLocationID              = itemfg.def-loc-bin
                ipiQuantitySubUnitsPerUnit = IF ipiQuantitySubUnitsPerUnit EQ 0 THEN
                                                itemfg.case-count
                                             ELSE
                                                ipiQuantitySubUnitsPerUnit 
                .
    END.
    ELSE DO:
        FIND FIRST item NO-LOCK
             WHERE item.company EQ ipcCompany
               AND item.i-no    EQ cPrimaryID
             NO-ERROR.
        IF AVAILABLE item THEN
            ASSIGN
                ipcWarehouseID = item.loc
                ipcLocationID  = item.loc-bin
                .
    END.
END.

/* FG process */
IF NOT lItemType THEN DO: 
    /* Postive quantity */
    IF ipdQuantity GT 0 THEN DO:
        /* Checks fg-rctd record for the input tag and quantity */     
        FIND FIRST fg-rctd NO-LOCK  
             WHERE fg-rctd.company EQ ipcCompany
               AND fg-rctd.tag     EQ ipcInventoryStockID
               AND fg-rctd.qty     GT 0
             NO-ERROR.
        IF AVAILABLE fg-rctd THEN DO:
            ASSIGN
                opcMessage = "Tag# (" + ipcInventoryStockID + ") has already been used, please enter a negative quantity."
                oplSuccess = NO
                .
       
            RETURN.
        END.
    END.
     /* Negative quantity */
    ELSE DO:
        /* Checks fg-rctd record for the input tag and quantity */     
        FIND FIRST fg-rctd NO-LOCK  
             WHERE fg-rctd.company EQ ipcCompany
               AND fg-rctd.tag     EQ ipcInventoryStockID
             NO-ERROR.
        IF NOT AVAILABLE fg-rctd THEN DO:
            ASSIGN
                opcMessage = "Receipt does not exists for the Tag# (" + ipcInventoryStockID + ")"
                oplSuccess = NO
                .
       
            RETURN.
        END.
        ELSE DO:
            FOR EACH  fg-rctd NO-LOCK  
                WHERE fg-rctd.company EQ ipcCompany
                  AND fg-rctd.tag     EQ ipcInventoryStockID:
                opdFinalQuantity = opdFinalQuantity + fg-rctd.qty. /* calculates sum of all receipts on the tag */ 
            END.
            opdFinalQuantity = opdFinalQuantity + ipdQuantity. /* calculates final qunatity */
            /* Checks whether final quantity is negative */
            IF opdFinalQuantity LT 0 THEN DO:
                ASSIGN
                    opcMessage = "Tag# (" + ipcInventoryStockID + ") has already been used, and negative quantity is more than on-hand quantity"
                    oplSuccess = NO.
                    .
        
                RETURN.
            END.
        END.
    END.
    
    /* Creates receipts  */ 
    RUN pFGRecordCreation (
        INPUT        ipcCompany,
        INPUT        ipcInventoryStockID,
        INPUT        ipdQuantity,
        INPUT        ipcQuantityUOM,
        INPUT-OUTPUT iopiPONo,
        INPUT        ipiPOLine,
        INPUT-OUTPUT iopcJobID,
        INPUT        ipcJobID2,
        INPUT        ipiQuantityPerSubUnit,
        INPUT        ipiQuantitySubUnitsPerUnit,
        INPUT        ipcWarehouseID,
        INPUT        ipcLocationID,
        OUTPUT       opriRctd
        ) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            opcMessage = "Error while creating FG receipt for the Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1)
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Checks sys-ctrl */
    {sys/inc/sspostfg.i}

    opdFinalQuantity = IF ipdQuantity LT 0 THEN
                           opdFinalQuantity
                       ELSE
                           ipdQuantity
                       .
    IF (SSPostFG-log OR ipcSSPostFG EQ "yes") AND (ipcSSPostFG NE "no") THEN DO:	
        /* Posts Receipts */
        RUN PostFinishedGoodsForUser IN hdInventoryProcs(
            INPUT        ipcCompany,
            INPUT        cReceipt,        /* Receipt */
            INPUT        ipcUsername,
            INPUT        lPromptForClose, /* Executes API closing orders logic */
            INPUT-OUTPUT oplSuccess,
            INPUT-OUTPUT opcMessage
            ) NO-ERROR.

        IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
            ASSIGN
                opcMessage = "Error while posting FG receipt for the Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
                oplSuccess = NO
                .
    				
            RETURN.
        END.
    END.
END.
/* RM process */
ELSE DO:
    IF ipdQuantity LT 0 THEN DO:
        ASSIGN
            opcMessage = "Enter positive quantity for Tag (" + ipcInventoryStockID + ")"
            oplSuccess = NO
            .
				
        RETURN.
    END.
    
    /* Verifies whether auto issue of receipts is enabled for sys-ctrl configuration "AUTOISSU" */
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, /* Company Code */ 
        INPUT "AUTOISSU", /* sys-ctrl name */
        INPUT "L",        /* Output return value */
        INPUT NO,         /* Use ship-to */
        INPUT NO,         /* ship-to vendor */
        INPUT "",         /* ship-to vendor value */
        INPUT "",         /* shi-id value */
        OUTPUT lAutoIssue, 
        OUTPUT lRecFound
        ).

    /* Checks rm-rctd record for the input tag */     
    FIND FIRST rm-rdtlh NO-LOCK  
         WHERE rm-rdtlh.company EQ ipcCompany
           AND rm-rdtlh.tag     EQ ipcInventoryStockID
           AND rm-rdtlh.r-no    NE 0
         NO-ERROR.
    
    /* Checks whether Tag is already used */
    IF AVAILABLE rm-rdtlh THEN DO:
        ASSIGN
            opcMessage = "Tag (" + ipcInventoryStockID + ") is already used to create RM receipt"
            oplSuccess = NO
            .
            
        RETURN.
    END. 
    
    /* Creates receipts  */ 
    RUN pRMRecordCreation (
        INPUT        ipcCompany,
        INPUT        ipcInventoryStockID,
        INPUT        ipdQuantity,
        INPUT        ipcQuantityUOM,
        INPUT-OUTPUT iopiPONo,
        INPUT        ipiPOLine,
        INPUT-OUTPUT iopcJobID,
        INPUT        ipcJobID2,
        INPUT        ipiQuantityPerSubUnit,
        INPUT        ipiQuantitySubUnitsPerUnit,
        INPUT        ipcWarehouseID,
        INPUT        ipcLocationID,
        OUTPUT       opriRctd
        ) NO-ERROR.
        
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            opcMessage = "Error while creating RM receipt for the Tag (" + ipcInventoryStockID + ")" + ERROR-STATUS:GET-MESSAGE(1)
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    FOR EACH  rm-rctd NO-LOCK
        WHERE rm-rctd.company   EQ ipcCompany
          AND (rm-rctd.user-id  EQ "" OR
               rm-rctd.user-id  EQ ipcUserName)
          AND rm-rctd.rita-code EQ "R" /* Receipts */:

        CREATE ttRctd.
        BUFFER-COPY rm-rctd TO ttRctd
        ASSIGN
            ttRctd.rmrctdRowID  = ROWID(rm-rctd)
            ttRctd.ttRctdHasRec = YES
            ttRctd.SeqNo        = 1
            .
        IF lAutoIssue THEN DO:
            /* creates RMRctd records for receipts */
            RUN InventoryReceipt_RMIssueCreation IN hdInventoryReceipt (
                INPUT-OUTPUT TABLE ttRctd BY-REFERENCE,  /* Just need to pass handle */
                INPUT        rm-rctd.company,
                INPUT        rm-rctd.tag,
                INPUT-OUTPUT oplSuccess,
                OUTPUT       opcMessage
                ) NO-ERROR.

            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
                ASSIGN
                    opcMessage = "Error while creating RM issue for Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
                    oplSuccess = NO
                    .  
                                      
                RETURN.
            END. 
        END.
    END.
    
    opdFinalQuantity = ipdQuantity.
    
    /* Posts RM goods  */
    RUN InventoryReceipt_PostRMItems IN hdInventoryReceipt (
        INPUT-OUTPUT TABLE ttRctd BY-REFERENCE, /* Just need to pass handle */
        INPUT        ipcCompany,
        INPUT        iopiPONo,
        INPUT-OUTPUT oplSuccess,
        OUTPUT       opcMessage
        ) NO-ERROR.
        
    IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
        ASSIGN
            opcMessage = "Error while Posting RM receipt for Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
            oplSuccess = NO
            .
            
        RETURN.
    END.
END.

PROCEDURE pFGRecordCreation PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new fg-rctd record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcInventoryStockID        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipdQuantity                AS DECIMAL   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPONo                   AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPOLine                  AS INTEGER   NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopcJobID                  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobID2                  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantityPerSubUnit      AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcWarehouseID             AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcLocationID              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT       PARAMETER opriFGRctd                 AS ROWID     NO-UNDO.
    
    DEFINE VARIABLE lAverageCost AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
    
    FIND FIRST fg-ctrl NO-LOCK 
         WHERE fg-ctrl.company EQ ipcCompany
         NO-ERROR.
    
    lAverageCost = AVAILABLE fg-ctrl AND fg-ctrl.inv-meth EQ "A" /* add comment */. 
    
    /* Retrieving last record of fg-rctd table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/  
    FIND LAST bf-fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
    IF AVAILABLE bf-fg-rctd AND bf-fg-rctd.r-no GT iRNo THEN
        iRNo = bf-fg-rctd.r-no.
    
    /* Retrieving last record of fg-rcpth table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no 
       In some cases r-no from fg-rcpth is greater than r-no from fg-rctd - so this additional find and the logic is required */    
    FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.        
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iRNo THEN 
        iRNo = fg-rcpth.r-no.
    
    CREATE bf-fg-rctd.
    ASSIGN
        bf-fg-rctd.r-no       = iRNo + 1
        bf-fg-rctd.rct-date   = TODAY
        bf-fg-rctd.trans-time = TIME
        bf-fg-rctd.company    = ipcCompany
        bf-fg-rctd.rita-code  = "R" /* Receipt */
        bf-fg-rctd.tag        = ipcInventoryStockID
        bf-fg-rctd.qty        = ipdQuantity
        bf-fg-rctd.loc        = ipcWarehouseID
        bf-fg-rctd.loc-bin    = ipcLocationID
        bf-fg-rctd.created-by = ipcUserName
        bf-fg-rctd.updated-by = ipcUserName
        bf-fg-rctd.qty-case   = ipiQuantityPerSubUnit
        bf-fg-rctd.cases-unit = ipiQuantitySubUnitsPerUnit
        bf-fg-rctd.cases      = IF ipiQuantityPerSubUnit EQ 0 THEN
                                    0
                                ELSE
                                    TRUNC((ipdQuantity / ipiQuantityPerSubUnit),0)
        bf-fg-rctd.partial    = IF ipiQuantityPerSubUnit EQ 0 THEN
                                    0
                                ELSE IF ipdQuantity GT 0 THEN
                                    ipdQuantity MODULO ipiQuantityPerSubUnit
                                ELSE 
                                    -1 * ((-1 * ipdQuantity) MODULO ipiQuantityPerSubUnit)
        bf-fg-rctd.pur-uom   = ipcQuantityUOM
        bf-fg-rctd.std-cost  = dStdCost
        bf-fg-rctd.cost-uom  = cCostUOM
        bf-fg-rctd.ext-cost  = dExtCost
        bf-fg-rctd.frt-cost  = dFrtCost
        bf-fg-rctd.i-no      = cPrimaryID   
        bf-fg-rctd.enteredBy = ipcUserName
        bf-fg-rctd.enteredDT = NOW    
        NO-ERROR.
        
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            opcMessage = "FG receipt for the tag  (" + ipcInventoryStockID + ") is not created " + ERROR-STATUS:GET-MESSAGE(1)
            oplSuccess = NO
            .
  
        RETURN.
    END.
    
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ ipcCompany
           AND itemfg.i-no    EQ cPrimaryID
        NO-ERROR.
        
    IF AVAILABLE itemfg THEN DO:
        bf-fg-rctd.i-name = itemfg.i-name.
        
        IF bf-fg-rctd.std-cost EQ 0 THEN
            bf-fg-rctd.std-cost = IF lAverageCost THEN
                                      itemfg.avg-cost
                                  ELSE
                                      itemfg.last-cost.
    END.

    /* Validates whether Tag is registered in loadtag or not */
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.item-type EQ NO
           AND loadtag.tag-no    EQ ipcInventoryStockID
         NO-ERROR.
    IF NOT AVAILABLE loadtag THEN DO:
        
        /* Validates whether Tag is registered in bin or not */
        FIND FIRST fg-bin NO-LOCK
             WHERE fg-bin.company EQ ipcCompany
               AND fg-bin.tag     EQ ipcInventoryStockID
             NO-ERROR. 
        IF AVAILABLE fg-bin THEN DO:
            ASSIGN
                ipcJobID2             = STRING(fg-bin.job-no2)
                iopcJobID             = fg-bin.job-no
                iopiPONo              = INT(fg-bin.po-no)
                .
                
        END.
    END.
    ELSE DO:
        ASSIGN
            bf-fg-rctd.stack-code = loadtag.misc-char[2]
            ipcJobID2             = IF ipcJobID2 EQ "" THEN
                                        STRING(loadtag.job-no2)
                                    ELSE
                                        ipcJobID2
            iopcJobID             = IF iopcJobID EQ "" THEN
                                        loadtag.job-no
                                    ELSE
                                        iopcJobID
            iopiPONo              = IF iopiPONo EQ 0 THEN
                                        loadtag.po-no
                                    ELSE
                                        iopiPONo
            ipiPOLine             = IF ipiPOLine EQ 0 THEN
                                        loadtag.line
                                     ELSE
                                        ipiPOLine
            .
    END.
    ASSIGN
        bf-fg-rctd.job-no  = iopcJobID
        bf-fg-rctd.job-no2 = INT(ipcJobID2)
        bf-fg-rctd.po-no   = STRING(iopiPONo)
        bf-fg-rctd.po-line = ipiPOLine
        opriFGRctd         = ROWID(bf-fg-rctd)
        .
        
    RELEASE bf-fg-rctd.
END PROCEDURE.

PROCEDURE pRMRecordCreation PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new rm-rctd record
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcInventoryStockID        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipdQuantity                AS DECIMAL   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcQuantityUOM             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiPONo                   AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiPOLine                  AS INTEGER   NO-UNDO. 
    DEFINE INPUT-OUTPUT PARAMETER iopcJobID                  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcJobID2                  AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantityPerSubUnit      AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcWarehouseID             AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcLocationID              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT       PARAMETER opriRMRctd                 AS ROWID     NO-UNDO.
    
    DEFINE VARIABLE dCost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dQty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iFrm  AS DECIMAL NO-UNDO.
    
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    
    RUN sys/ref/asiseq.p (
        INPUT ipcCompany, 
        INPUT "rm_rcpt_seq", 
        OUTPUT iRNo
        ) NO-ERROR.
        
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            oplSuccess = NO
            opcMessage = "Could not obtain next sequence # to create RM receipts for the Tag (" + ipcInventoryStockID + ")"
            .
            
        RETURN.
    END.
	
    CREATE bf-rm-rctd.
    ASSIGN
        bf-rm-rctd.r-no       = iRNo + 1
        bf-rm-rctd.rct-date   = TODAY
        bf-rm-rctd.trans-time = TIME
        bf-rm-rctd.company    = ipcCompany
        bf-rm-rctd.rita-code  = "R" /* Receipt */
        bf-rm-rctd.tag        = ipcInventoryStockID
        bf-rm-rctd.loc        = ipcWarehouseID
        bf-rm-rctd.loc-bin    = ipcLocationID
        bf-rm-rctd.user-id    = ipcUserName
        bf-rm-rctd.pur-uom    = ipcQuantityUOM
        bf-rm-rctd.i-no       = cPrimaryID
        NO-ERROR .
         
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            opcMessage = "RM receipt for the tag (" + ipcInventoryStockID + ") is not created " + ERROR-STATUS:GET-MESSAGE(1)
            oplSuccess = NO
            .
            
        RETURN.
    END. 

    /* Validates whether Tag is registered or not */
    FIND FIRST loadtag NO-LOCK
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.item-type EQ YES
           AND loadtag.tag-no    EQ ipcInventoryStockID
         NO-ERROR.
    
    /* If input PO number is null then retrive PO number and line from loadtag */ 
    IF AVAILABLE loadtag AND (iopiPONo EQ 0 AND ipiPOLine EQ 0) THEN
        ASSIGN
            iopiPONo     = IF iopiPONo EQ 0 THEN 
                              loadtag.po-no
                          ELSE
                              iopiPONo
            ipiPOline   = IF ipiPOLine EQ 0 THEN
                              loadtag.line
                          ELSE
                              ipiPOLine
            .

    RUN pGetQtyFrm (
        INPUT  iopiPONo,
        INPUT  ipiPOline,
        INPUT  ipdQuantity,
        OUTPUT dQty,
        OUTPUT iFrm 
        ) NO-ERROR.
        
    IF ERROR-STATUS:ERROR THEN DO:
        ASSIGN
            opcMessage = "Error while getting RM quantity for Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1)
            oplSuccess = NO
            .
                                
        RETURN.
    END.
    ASSIGN
         bf-rm-rctd.po-no   = STRING(iopiPONo)
         bf-rm-rctd.po-line = ipiPOLine
         bf-rm-rctd.s-num   = iFrm
         bf-rm-rctd.qty     = dQty
         .
         
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company   EQ ipcCompany
           AND po-ordl.po-no     EQ iopiPONo
           AND po-ordl.line      EQ ipiPOLine
           AND po-ordl.item-type EQ YES
         NO-ERROR.
    
    IF AVAILABLE po-ordl THEN DO:
        dCost = po-ordl.cost.
        
        /* Gets cost */
        RUN rm/getpocst.p (
            BUFFER po-ordl,
            INPUT  po-ordl.pr-uom,
            INPUT-OUTPUT dCost
            ). 
        
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                opcMessage = "Error while getting RM cost for Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1)
                oplSuccess = NO
                . 
                                   
            RETURN.
        END. 
          
        RUN InventoryReceipt_ConvertVendCompCurr IN hdInventoryReceipt (
            INPUT        ipcCompany,
            INPUT-OUTPUT dCost
            ) NO-ERROR.
        
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                opcMessage = "Error while converting RM cost for Tag (" + ipcInventoryStockID + ") " + ERROR-STATUS:GET-MESSAGE(1)
                oplSuccess = NO
                .   
                                 
            RETURN.
        END.
        
        ASSIGN
            bf-rm-rctd.cost     = dCost
            bf-rm-rctd.b-num    = (po-ordl.b-num)
            bf-rm-rctd.pur-uom  = po-ordl.pr-qty-uom
            bf-rm-rctd.cost-uom = po-ordl.pr-uom
            .
    END.
    
    IF NOT AVAILABLE loadtag THEN DO:
        
        IF AVAILABLE po-ordl THEN
            ASSIGN
                bf-rm-rctd.i-name  = po-ordl.i-name
                bf-rm-rctd.job-no  = po-ordl.job-no
                bf-rm-rctd.job-no2 = po-ordl.job-no2
                .
                
        RELEASE bf-rm-rctd.  
        RETURN.             
    END.
      
    ASSIGN
        bf-rm-rctd.i-name    = loadtag.i-name
        bf-rm-rctd.job-no    = IF iopcJobID NE "" THEN
                                   iopcJobID
                               ELSE
                                   loadtag.job-no
        bf-rm-rctd.job-no2   = IF ipcJobID2 NE "" THEN
                                   INT(ipcJobID2)
                               ELSE
                                   loadtag.job-no2
        opriRMRctd           = ROWID(bf-rm-rctd)
        .

    RELEASE bf-rm-rctd.

END PROCEDURE.

PROCEDURE pGetQtyFrm PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Gets quantity and frm 
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT  PARAMETER iopiPONo     AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ipiPOLine   AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ipdQuantity AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER opdQty      AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER opiFrm      AS INTEGER NO-UNDO.
   
   DEFINE VARIABLE dQty AS DECIMAL NO-UNDO.

   FIND FIRST po-ordl NO-LOCK
        WHERE po-ordl.company   EQ ipcCompany
          AND po-ordl.po-no     EQ iopiPONo
          AND po-ordl.line      EQ ipiPOLine
          AND po-ordl.item-type EQ YES
        NO-ERROR.
 
   IF po-ordl.job-no NE "" AND po-ordl.s-num EQ ? THEN
        FIND FIRST job NO-LOCK
             WHERE job.company EQ ipcCompany
               AND job.job-no  EQ po-ordl.job-no
               AND job.job-no2 EQ po-ordl.job-no2
             NO-ERROR.
        
    IF AVAILABLE job THEN DO:
        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2
              AND job-mat.rm-i-no EQ po-ordl.i-no:
              
            ASSIGN
                opiFrm  = job-mat.frm
                opdQty  = job-mat.qty
                dQty    = dQty + job-mat.qty
                opdQty  = IF dQty NE 0 THEN
                              ipdQuantity * (opdQty / dQty)
                          ELSE
                              0.
                
                .  
        END.
    END.  
    ELSE DO: /* if job is not available then get frm from po-ordl */
        ASSIGN
            opiFrm = po-ordl.s-num
            opdQty = ipdQuantity
            .
    END.

END PROCEDURE.

DELETE PROCEDURE hdInventoryReceipt.
DELETE PROCEDURE hdSession.
DELETE PROCEDURE hdTags.
DELETE PROCEDURE hdInventoryProcs.


    
    


