/*------------------------------------------------------------------------
    File        : api\inbound\CreateTagForPO.p
    Purpose     : creates Tagforpo

    Syntax      :

    Description : creates Tagforpo

    Author(s)   : Vishnu Vellanki
    Created     : Tue Oct 11 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
  
/* existing includes */
{inventory/ttinventory.i "NEW SHARED"}.
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}

DEFINE INPUT  PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiPONo               AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPOLine             AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType           AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER ipdQuantity           AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcStockIDAlias       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcWareHouseID        AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLocationID         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcCreateReceipt      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcInventoryStockID   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcLoadtagFormat      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiTagCopies          AS INTEGER   NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess            AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage            AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE    NO-UNDO.
DEFINE VARIABLE iTagNo           AS INTEGER   NO-UNDO.
DEFINE VARIABLE iRNo             AS INTEGER   NO-UNDO.
DEFINE VARIABLE lValidLoc        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValidBin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cTransfer        AS CHARACTER NO-UNDO INITIAL "R".
DEFINE VARIABLE cItemFG          AS CHARACTER NO-UNDO INITIAL "FG".
DEFINE VARIABLE cItemRM          AS CHARACTER NO-UNDO INITIAL "RM".
DEFINE VARIABLE cCustNo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cCostUOM         AS CHARACTER NO-UNDO.
DEFINE VARIABLE dStdCost         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dExtCost         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dFrtCost         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lAverageCost     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hdCostProcs      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lFGPOFrt         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturn          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lItemType        AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE ttBoardToWIP NO-UNDO
    FIELD rmrdtlhRowID AS ROWID
    FIELD rmrcpthRowID AS ROWID
    FIELD rmbinRowID   AS ROWID
    FIELD jobmatRowID  AS ROWID
    FIELD itemRowID    AS ROWID
    .

DEFINE TEMP-TABLE ttRctd NO-UNDO LIKE rm-rctd 
    FIELD ttRctdRowID  AS ROWID
    FIELD rmrctdRowID  AS ROWID
    FIELD ttRctdHasRec AS LOGICAL INIT NO
    FIELD SeqNo        AS INTEGER
    FIELD DBSeqNo      AS INTEGER 
    FIELD vend-tag     AS CHARACTER 
    INDEX SeqNo SeqNo i-no
    .
        
DEFINE TEMP-TABLE ttMat NO-UNDO 
    FIELD frm AS INTEGER
    FIELD qty AS DECIMAL
    INDEX frm frm
    . 
                                    
/* This will eventually move to setsession - START >>>*/
&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
  
DEFINE VARIABLE hdSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hdTags    AS HANDLE NO-UNDO.
 
RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.
RUN system/session.p  PERSISTENT SET hdSession.
SESSION:ADD-SUPER-PROCEDURE (hdSession).
RUN system/TagProcs.p PERSISTENT SET hdTags.
SESSION:ADD-SUPER-PROCEDURE (hdTags).
{sys/inc/var.i "new shared"}
ASSIGN
    g_company = ipcCompany
    cocode    = g_company
    .
/* END <<<*/

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 
RUN system\CostProcs.p PERSISTENT SET hdCostProcs. 

RUN sys/ref/nk1look.p (
    INPUT ipcCompany, /* Company Code */
    INPUT "FGPOFRT",  /* sys-ctrl name */
    INPUT "L",        /* Output return value */
    INPUT NO,         /* Use ship-to */
    INPUT NO,         /* ship-to vendor */
    INPUT "",         /* ship-to vendor value */
    INPUT "",         /* shi-id value */
    OUTPUT cReturn, 
    OUTPUT lRecFound
    ).
    
ASSIGN
    oplSuccess    = YES
    lValidLoc     = (ipcWareHouseID NE "")
    lValidBin     = (ipcLocationID  NE "")
    ipcItemType   = IF ipcItemType NE cItemFG AND ipcItemType NE cItemRM  THEN
                        cItemFG
                    ELSE
                        ipcItemType
    lFGPOFrt      = lRecFound AND cReturn EQ "YES"
    lItemType     = (ipcItemType NE cItemFG)
    .

/* Validate company */
IF NOT CAN-FIND(FIRST company NO-LOCK
                WHERE company.company EQ ipcCompany) THEN DO:
    ASSIGN 
        opcMessage = "Invalid Company (" + ipcCompany + ") entered for item (" + ipcPrimaryID + ")"
        oplSuccess = NO
        .
    RETURN.
END.

/* Validates PO Number */
FIND FIRST po-ord NO-LOCK
     WHERE po-ord.company EQ ipcCompany
       AND po-ord.po-no   EQ ipiPoNo
     NO-ERROR.
IF NOT AVAILABLE po-ord THEN DO:
    ASSIGN 
        opcMessage = "Invalid PO Number (" + STRING(ipiPONo) + ") entered for item (" + ipcPrimaryID + ")"                 
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validates whether PO Number is in Hold / not */
IF po-ord.stat EQ "H" THEN DO:
    ASSIGN 
        opcMessage = "PO# (" + STRING(ipiPoNo) + ") is on hold and cannot print load tags for a PO on hold"                
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validate PO Line */
IF NOT CAN-FIND(FIRST po-ordl NO-LOCK
                WHERE po-ordl.company EQ ipcCompany
                  AND po-ordl.po-no   EQ ipiPONo
                  AND po-ordl.line    EQ ipiPOLine) THEN DO:
    ASSIGN 
        opcMessage = "Invalid PO Line (" + STRING(ipiPOLine) + ") entered for item (" + ipcPrimaryID + ")"                  
        oplSuccess = NO
        .
        
    RETURN.
END.

/* Validates item */
IF ipcItemType EQ cItemFG THEN DO:
    FIND FIRST itemfg NO-LOCK
         WHERE itemfg.company EQ ipcCompany
           AND itemfg.i-no    EQ ipcPrimaryID
         NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN 
            opcMessage = "Invalid Item (" + ipcPrimaryID + ")"                  
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    IF NOT lValidLoc OR NOT lValidBin THEN
        ASSIGN
            ipcWarehouseID = itemfg.def-loc
            ipcLocationID  = itemfg.def-loc-bin
            .
END.
ELSE DO:
    FIND FIRST item NO-LOCK
         WHERE item.company EQ ipcCompany
           AND item.i-no    EQ ipcPrimaryID
         NO-ERROR.
    IF NOT AVAILABLE item THEN DO:
        ASSIGN 
            opcMessage = "Invalid Item (" + ipcPrimaryID + ")"                   
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    IF NOT lValidLoc OR NOT lValidBin THEN
        ASSIGN
            ipcWarehouseID = item.loc
            ipcLocationID  = item.loc-bin
            .
END.

/* Validates loc and bin for non-blank values */   
IF lValidLoc AND lValidBin THEN DO:

    /* Validate warehouse */        
    RUN ValidateLoc IN hdInventoryProcs (
        ipcCompany,
        ipcWareHouseID,
        OUTPUT lValidLoc
        ).
    IF NOT lValidLoc THEN DO:
        ASSIGN 
            opcMessage = "Invalid WareHouseID (" + ipcWarehouseID + ") entered for item (" + ipcPrimaryID + ")"                    
            oplSuccess = NO
            .
            
        RETURN.
    END.
    
    /* Validate bin location */
    RUN ValidateBin IN hdInventoryProcs (
        ipcCompany,
        ipcWareHouseID,
        ipcLocationID,
        OUTPUT lValidBin
        ).
    IF NOT lValidBin THEN DO:
        ASSIGN 
            opcMessage = "Invalid LocationID (" + ipcLocationID + ") entered for item (" + ipcPrimaryID + ")"
            oplSuccess = NO 
            .
            
        RETURN.
    END.
END.

/* Validate CreateReceipt */
IF ipcCreateReceipt EQ "" OR (ipcCreateReceipt NE "YES" AND ipcCreateReceipt NE "NO") THEN DO:
    ASSIGN 
        opcMessage = "Please enter valid option (yes / no) to create receipt for item (" + ipcPrimaryID + ")"
        oplSuccess = NO
        .
        
    RETURN.
END.

FIND FIRST loadtag NO-LOCK 
     WHERE loadtag.company     EQ ipcCompany
       AND loadtag.item-type   EQ lItemType
       AND loadtag.is-case-tag EQ NO
     NO-ERROR.
IF NOT AVAILABLE loadtag THEN DO:
    ASSIGN
        opcMessage = "company " + ipcCompany + "is not available to create loadtag for item (" + ipcPrimaryID + ")"
        oplSuccess = NO
        .
    RETURN. 
END.

FIND FIRST po-ordl NO-LOCK 
     WHERE po-ordl.company   EQ po-ord.company
       AND po-ordl.po-no     EQ po-ord.po-no
       AND po-ordl.line      EQ ipiPOLine
       AND po-ordl.i-no      EQ ipcPrimaryID 
       AND po-ordl.item-type EQ lItemType  
     NO-ERROR.
IF NOT AVAILABLE po-ordl THEN DO:
    ASSIGN 
        opcMessage = "Primary ID ("+ ipcPrimaryID + ") is not available for the given PO Number (" + STRING(ipiPONo) + ") and PO Line (" + STRING(ipiPOLine) + ")" 
        oplSuccess = NO
        .
        
    RETURN.
END.
 
IF po-ordl.cust-no EQ "" THEN
    FIND FIRST oe-ordl NO-LOCK
         WHERE oe-ordl.company EQ ipcCompany
           AND oe-ordl.ord-no  EQ po-ordl.ord-no
         NO-ERROR.
    
cCustNo = IF po-ordl.cust-no NE "" THEN
              po-ordl.cust-no
          ELSE IF AVAILABLE oe-ordl AND oe-ordl.cust-no NE "" THEN
              oe-ordl.cust-no
          ELSE
              "".
                          
FIND FIRST cust NO-LOCK
     WHERE cust.company EQ ipcCompany
       AND cust.cust-no EQ cCustNo
     NO-ERROR.
opiTagCopies = IF AVAILABLE cust THEN
                   cust.int-field[1]
               ELSE
                   0.
                   
IF opiTagCopies EQ 0 THEN
    RUN sys\ref\nk1look.p(
        INPUT ipcCompany, /* Company Code */
        INPUT "LOADTAG",  /* sys-ctrl name */
        INPUT "I",        /* Output return value */
        INPUT YES,        /* Use ship-to */
        INPUT YES,        /* ship-to vendor */
        INPUT cCustNo,    /* ship-to vendor value */
        INPUT "",         /* shi-id value */
        OUTPUT opiTagCopies,
        OUTPUT lRecFound
        ).
                        
IF opiTagCopies EQ 0 THEN
    opiTagCopies = 1.
                  
FIND FIRST cust-part NO-LOCK
     WHERE cust-part.company EQ ipcCompany
       AND cust-part.cust-no EQ cCustNo
       AND cust-part.i-no    EQ ipcPrimaryID
     NO-ERROR.
opcLoadtagFormat = IF AVAILABLE cust-part THEN
                       cust-part.labelPallet
                   ELSE
                       "".
           
IF opcLoadtagFormat EQ "" THEN               
    RUN sys\ref\nk1look.p(
        INPUT ipcCompany, /* Company Code */
        INPUT "BARDIR",   /* sys-ctrl name */
        INPUT "C",        /* Output return value */
        INPUT YES,        /* Use ship-to */
        INPUT YES,        /* ship-to vendor */
        INPUT cCustNo,    /* ship-to vendor value */
        INPUT "",         /* shi-id value */
        OUTPUT opcLoadtagFormat,  
        OUTPUT lRecFound
        ).

DO TRANSACTION ON ERROR UNDO,LEAVE:

    /* FG Process */
    IF ipcItemType EQ cItemFG THEN DO: 

        /* Creates a new tag number for FG items */
        RUN pGetNextLoadtagNumberForFGItem(
            INPUT  ipcCompany, 
            INPUT  ipcPrimaryID, 
            OUTPUT iTagNo
            ).
         
        /* Creates a new loadtag table record for FG items */
        RUN pCreateLoadTagForFGItem (
            INPUT  ipcCompany, 
            INPUT  ipiPONo, 
            INPUT  ipiPOLine, 
            INPUT  ipcPrimaryID, 
            INPUT  ipcItemType, 
            INPUT  ipdQuantity, 
            INPUT  ipdQuantityPerSubUnit, 
            INPUT  ipcStockIDAlias, 
            INPUT  iTagNo,
            INPUT  ROWID(po-ordl), 
            OUTPUT opcInventoryStockID
            )NO-ERROR.
        
        /* Retrieving last record of fg-rctd table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/ 
        FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
        IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iRNo THEN 
            iRNo = fg-rctd.r-no.
        
        /* Retrieving last record of fg-rcpth table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/ 
        /* In some cases r-no from fg-rcpth is greater than r-no from fg-rctd - so this additional find and the logic is required*/
        FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iRNo THEN 
            iRNo = fg-rcpth.r-no.
            
        IF ipcCreateReceipt EQ "yes" THEN DO:
        
            /* Creates a new FGRctd table record  */
            RUN pFGReceiptCreation NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN
                    opcMessage = "Unable to Create receipt for item (" + ipcPrimaryID + ") " + ERROR-STATUS:GET-MESSAGE(1)
                    oplSuccess = NO
                    .
                UNDO, LEAVE.
            END.

            /* Posts Receipts for FG items */
            RUN PostFinishedGoodsForUser IN hdInventoryProcs(
                INPUT        ipcCompany,
                INPUT        cTransfer,
                INPUT        ipcUsername,
                INPUT-OUTPUT oplSuccess,
                INPUT-OUTPUT opcMessage
                ) NO-ERROR.
                    
            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
                ASSIGN
                    opcMessage = "Unable to Post receipt for item (" + ipcPrimaryID + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
                    oplSuccess = NO
                    .
                UNDO, LEAVE.
            END.
         END.
    END.
    
    /* RM Process */    
    ELSE DO:
        DEFINE VARIABLE cTagNo     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lAutoIssue AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE iNo        AS INTEGER   NO-UNDO INITIAL 1.
        
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

        /* creates new tag number for RM items - existing logic, taken from program (rmloadtg4.w) */   
        DO WHILE TRUE:
            cTagNo = STRING(po-ordl.po-no,'9999999') + STRING(po-ordl.line,'999') + STRING(iNo,'9999999').
            IF NOT CAN-FIND(FIRST loadtag
                            WHERE loadtag.company   EQ ipcCompany
                              AND loadtag.item-type EQ YES
                              AND loadtag.tag-no    EQ cTagNo) THEN LEAVE.
            iNo = iNo + 1.
        END. /* do while */
        
        
        /* Creates a new loadtag table record for RM items */
        RUN pCreateLoadTagForRMItem (
            INPUT ipcWarehouseID, 
            INPUT ipcLocationID, 
            INPUT cTagNo,
            INPUT ROWID(po-ordl)
            )NO-ERROR.
            
        opcInventoryStockID = cTagNo.        
        
        IF ipcCreateReceipt EQ "yes" THEN DO:
        
            /* Creates a new RMRctd table record  */
            RUN pRMReceiptCreation NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                ASSIGN
                    opcMessage = "Unable to Create receipt for item (" + ipcPrimaryID + ") " + ERROR-STATUS:GET-MESSAGE(1)
                    oplSuccess = NO
                    .
                UNDO, LEAVE.
            END.
            
            FOR EACH  rm-rctd NO-LOCK
                WHERE rm-rctd.company   EQ ipcCompany
                  AND rm-rctd.po-no     EQ STRING(ipiPONo)
                  AND rm-rctd.rita-code EQ "R" /* Receipts */:
    
                CREATE ttRctd.
                BUFFER-COPY rm-rctd TO ttRctd
                ASSIGN
                    ttRctd.rmrctdRowID  = ROWID(rm-rctd)
                    ttRctd.ttRctdHasRec = YES
                    ttRctd.SeqNo        = 1
                    .
                IF lAutoIssue THEN
                    /* creates RMRctd records for receipts */
                    RUN pRMIssueCreation. 
            END.
            
            /* Posts RM goods  */
            RUN PostRMItems (
                INPUT-OUTPUT oplSuccess,
                OUTPUT       opcMessage
                ) NO-ERROR.
  
            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
                ASSIGN
                    opcMessage = "Unable to Post receipt for item (" + ipcPrimaryID + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
                    oplSuccess = NO
                    .                    
                UNDO, LEAVE.
            END.            
        END.
    END.
END.

PROCEDURE pGetNextLoadtagNumberForFGItem PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new TAG number for FG Process
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiNextTag   AS INTEGER   NO-UNDO.

    DEFINE VARIABLE iLastFGTag AS INTEGER.
    DEFINE VARIABLE iLastRMTag AS INTEGER.
  
    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company             EQ     ipcCompany
            AND loadtag.item-type           EQ     NO
            AND loadtag.is-case-tag         EQ     NO
            AND loadtag.tag-no              BEGINS ipcPrimaryID
            AND SUBSTR(loadtag.tag-no,1,15) EQ     ipcPrimaryID
          USE-INDEX tag NO-ERROR.

    iLastFGTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.

    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company             EQ     ipcCompany
            AND loadtag.item-type           EQ     YES
            AND loadtag.is-case-tag         EQ     NO
            AND loadtag.tag-no              BEGINS ipcPrimaryID
            AND SUBSTR(loadtag.tag-no,1,15) EQ     ipcPrimaryID
          USE-INDEX tag NO-ERROR.

    iLastRMTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
  
    opiNextTag = MAX(iLastFGTag, iLastRMTag).

END PROCEDURE.


PROCEDURE pCreateLoadTagForFGItem PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:Creates new record in loadtag table for FG items
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPONo               AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine             AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPrimaryID          AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemType           AS CHARACTER NO-UNDO. 
    DEFINE INPUT  PARAMETER ipdQuantity           AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcStockIDAlias       AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiTagNo              AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipriPOOrdl            AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInventoryStockID   AS CHARACTER NO-UNDO.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE ROWID(po-ordl) EQ ipriPOOrdl
         NO-ERROR.
         
    FIND FIRST itemfg NO-LOCK 
         WHERE itemfg.company EQ ipcCompany
           AND itemfg.i-no    EQ po-ordl.i-no 
         NO-ERROR.
        
    FIND FIRST fg-bin NO-LOCK 
         WHERE fg-bin.company EQ ipcCompany
           AND fg-bin.i-no    EQ ipcPrimaryID
           AND fg-bin.job-no  EQ loadtag.job-no
           AND fg-bin.tag     EQ loadtag.tag-no  
         NO-ERROR.
                    
    CREATE loadtag.
    ASSIGN
        loadtag.company       = ipcCompany
        loadtag.tag-no        = STRING(CAPS(ipcPrimaryID),"x(15)") + STRING(ipiTagNo,"99999") 
        loadtag.item-type     = NO 
        loadtag.ord-no        = IF can-find(FIRST cust 
                                            WHERE cust.company EQ ipcCompany
                                              AND cust.cust-no EQ itemfg.cust-no
                                              AND cust.active  EQ "X") THEN 
                                    0 
                                ELSE 
                                    loadtag.ord-no
        loadtag.i-no          = ipcPrimaryID
        loadtag.i-name        = po-ordl.i-name
        loadtag.qty           = ipdQuantity
        loadtag.partial       = IF AVAILABLE fg-bin THEN fg-bin.partial-count ELSE 0
        loadtag.sts           = "Printed"  
        loadtag.tag-date      = TODAY
        loadtag.tag-time      = TIME
        loadtag.misc-char[1]  = ipcStockIDAlias
        loadtag.po-no         = INT(ipiPONo)
        loadtag.line          = ipiPOLine
        loadtag.loc           = ipcWarehouseID
        loadtag.loc-bin       = ipcLocationID
        loadtag.qty-case      = ipdQuantityPerSubUnit
        loadtag.case-bundle   = IF AVAILABLE itemfg AND itemfg.case-pall NE 0 THEN itemfg.case-pall  ELSE 1
        loadtag.pallet-no     = IF AVAILABLE itemfg THEN itemfg.trno ELSE ""
        loadtag.tot-cases     = (loadtag.pallet-count - loadtag.partial) / loadtag.case-bundle
        loadtag.pallet-count  = (loadtag.qty-case * loadtag.case-bundle)
        opcInventoryStockID   = loadtag.tag-no 
        .
END PROCEDURE.

PROCEDURE pFGReceiptCreation PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new fg-rctd record
 Notes:
------------------------------------------------------------------------------*/
    CREATE fg-rctd.
    ASSIGN
        fg-rctd.r-no       = iRNo + 1
        fg-rctd.rct-date   = TODAY
        fg-rctd.trans-time = TIME
        fg-rctd.company    = loadtag.company
        fg-rctd.rita-code  = "R"
        fg-rctd.qty        = loadtag.qty
        fg-rctd.i-name     = loadtag.i-name
        fg-rctd.i-no       = loadtag.i-no
        fg-rctd.job-no     = loadtag.job-no
        fg-rctd.job-no2    = loadtag.job-no2
        fg-rctd.t-qty      = loadtag.pallet-count
        fg-rctd.pur-uom    = itemfg.prod-uom
        fg-rctd.cost-uom   = itemfg.prod-uom
        fg-rctd.std-cost   = IF AVAILABLE fg-bin THEN fg-bin.std-tot-cost ELSE itemfg.std-tot-cost    
        fg-rctd.ext-cost   = (fg-rctd.t-qty / 1000) * fg-rctd.std-cost
        fg-rctd.qty-case   = loadtag.qty-case
        fg-rctd.partial    = loadtag.partial
        fg-rctd.cases      = IF loadtag.qty-case NE 0 THEN TRUNC(fg-rctd.t-qty / loadtag.qty-case,0) ELSE 0
        fg-rctd.cases-unit = loadtag.case-bundle
        fg-rctd.loc        = loadtag.loc
        fg-rctd.loc-bin    = loadtag.loc-bin
        fg-rctd.tag        = loadtag.tag-no
        fg-rctd.stack-code = loadtag.misc-char[2]
        fg-rctd.tot-wt     = loadtag.misc-dec[1] 
        fg-rctd.created-by = ipcUsername
        fg-rctd.updated-by = ipcUsername
        fg-rctd.po-no      = TRIM(STRING(loadtag.po-no,">>>>>>>>>>")).
        fg-rctd.po-line    = loadtag.line 
        .
    /* Gets costs */    
    RUN pGetCostsFromPO (
        INPUT  ipcCompany, 
        INPUT  ipiPONo,
        INPUT  ipiPOLine,
        INPUT  ipcPrimaryID,
        INPUT  ipdQuantity,
        OUTPUT dStdCost, 
        OUTPUT cCostUOM, 
        OUTPUT dExtCost, 
        OUTPUT dFrtCost
        ). 
        
    ASSIGN
        fg-rctd.std-cost  = dStdCost
        fg-rctd.cost-uom  = cCostUOM
        fg-rctd.ext-cost  = dExtCost
        fg-rctd.frt-cost  = dFrtCost
        .
           
    IF fg-rctd.cases EQ 0 AND fg-rctd.partial NE 0 THEN
        ASSIGN 
            fg-rctd.cases    = (IF fg-rctd.partial LT 0 THEN -1 ELSE 1)
            fg-rctd.qty-case = (IF fg-rctd.partial LT 0 THEN - fg-rctd.partial ELSE fg-rctd.partial)
            fg-rctd.partial  = 0
            .
END PROCEDURE.

PROCEDURE pGetCostsFromPO PRIVATE:
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

PROCEDURE pCreateLoadTagForRMItem PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:Creates new record in loadtag table for RM items
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcWarehouseID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcLocationID  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcTagNo       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipriPOOrdl     AS ROWID     NO-UNDO.

    DEFINE VARIABLE dQty AS DECIMAL NO-UNDO.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE ROWID(po-ordl) EQ ipriPOOrdl
         NO-ERROR.
   
    CREATE loadtag.
    ASSIGN
        loadtag.company      = po-ordl.company
        loadtag.tag-no       = ipctagNo
        loadtag.item-type    = po-ordl.item-type
        loadtag.po-no        = po-ordl.po-no
        loadtag.line         = po-ordl.line
        loadtag.job-no       = po-ordl.job-no
        loadtag.job-no2      = po-ordl.job-no2
        loadtag.form-no      = po-ordl.s-num
        loadtag.blank-no     = po-ordl.b-num
        loadtag.ord-no       = po-ordl.ord-no
        loadtag.i-no         = CAPS(po-ordl.i-no)
        loadtag.i-name       = po-ordl.i-name
        loadtag.qty          = ipdQuantity
        loadtag.qty-case     = ipdQuantityPerSubUnit
        loadtag.case-bundle  = 1
        loadtag.pallet-count = ipdQuantityPerSubUnit
        loadtag.loc          = ipcWarehouseID
        loadtag.loc-bin      = ipcLocationID
        loadtag.tot-cases    = 0
        loadtag.sts          = "Printed"
        loadtag.tag-date     = TODAY
        loadtag.tag-time     = TIME
        .

    EMPTY TEMP-TABLE ttMat.
    
    IF po-ordl.job-no NE "" AND po-ordl.s-num EQ ? THEN
        FIND FIRST job NO-LOCK
             WHERE job.company EQ ipcCompany
               AND job.job-no  EQ po-ordl.job-no
               AND job.job-no2 EQ po-ordl.job-no2
             NO-ERROR.
        
    IF AVAILABLE job THEN DO:
        FOR EACH job-mat
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2
              AND job-mat.rm-i-no EQ po-ordl.i-no
            NO-LOCK:
            
            CREATE ttMat.
            ASSIGN
                ttMat.frm = job-mat.frm
                ttMat.qty = job-mat.qty
                dQty      = dQty + job-mat.qty
                .  
        END.
  
        /* Calculate qty */
        FOR EACH ttMat:
            ttMat.qty = IF dQty NE 0 THEN
                            ipdQuantity * (ttMat.qty / dQty)
                        ELSE
                            0.
        END.
    END.  
    ELSE DO: /* if job is not available then get frm from po-ordl */
       CREATE ttMat.
       ASSIGN
           ttMat.frm = po-ordl.s-num
           ttMat.qty = ipdQuantity
           .
    END.
END PROCEDURE.

PROCEDURE pRMReceiptCreation PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates a new rm-rctd record for receipts
 Notes:
------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-rm-rctd FOR rm-rctd.
    
    DEFINE VARIABLE iSeqNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE dCost  AS DECIMAL NO-UNDO.
    
    /* Gets row number of the last rmrctd record */
    RUN sys/ref/asiseq.p (
        INPUT ipcCompany, 
        INPUT "rm_rcpt_seq", 
        OUTPUT iSeqNo 
        ).
        
    dCost = po-ordl.cost.
    
    /* Gets costs */
    RUN rm/getpocst.p (
        BUFFER po-ordl,
        INPUT  po-ordl.pr-uom,
        INPUT-OUTPUT dCost
        ).
        
    RUN pConvertVendCompCurr(
        INPUT-OUTPUT dCost
        ).
        
    CREATE bf-rm-rctd.
    ASSIGN
        bf-rm-rctd.r-no        = iSeqNo
        bf-rm-rctd.rct-date    = TODAY
        bf-rm-rctd.company     = ipcCompany
        bf-rm-rctd.rita-code   = "R"
        bf-rm-rctd.i-name      = ipcPrimaryID
        bf-rm-rctd.i-no        = loadtag.i-no
        bf-rm-rctd.job-no      = loadtag.job-no
        bf-rm-rctd.job-no2     = loadtag.job-no2
        bf-rm-rctd.po-no       = STRING(loadtag.po-no)
        bf-rm-rctd.po-line     = loadtag.line
        bf-rm-rctd.s-num       = ttMat.frm
        bf-rm-rctd.b-num       = po-ordl.b-num
        bf-rm-rctd.qty         = ttMat.qty
        bf-rm-rctd.pur-uom     = po-ordl.cons-uom
        bf-rm-rctd.cost        = dCost
        bf-rm-rctd.cost-uom    = po-ordl.pr-uom
        bf-rm-rctd.loc         = loadtag.loc
        bf-rm-rctd.loc-bin     = loadtag.loc-bin
        bf-rm-rctd.tag         = loadtag.tag-no
        bf-rm-rctd.user-id     = ipcUserName
        bf-rm-rctd.upd-date    = TODAY
        bf-rm-rctd.upd-time    = TIME
        .
        
    RELEASE bf-rm-rctd.
END PROCEDURE.

PROCEDURE pRMIssueCreation PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Creates new rm-rctd record for issues
 Notes:
------------------------------------------------------------------------------*/
    DEFINE VARIABLE dQty1    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dQty2    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cUOMEach AS CHARACTER NO-UNDO INITIAL "EA".
    
    DEFINE BUFFER bf-ttRctd FOR ttRctd.
    
    EMPTY TEMP-TABLE ttMat.
    
    FIND FIRST ttRctd NO-LOCK
         WHERE ttRctd.company EQ rm-rctd.company
           AND ttRctd.tag     EQ rm-rctd.tag
         NO-ERROR.
         
    IF ttRctd.job-no NE "" AND ttRctd.s-num EQ ? THEN
        FIND FIRST job NO-LOCK
             WHERE job.company EQ ttRctd.company
               AND job.job-no  EQ ttRctd.job-no
               AND job.job-no2 EQ ttRctd.job-no2
        NO-ERROR.
        
    IF AVAILABLE job THEN DO:
        FOR EACH  job-mat NO-LOCK
            WHERE job-mat.company EQ job.company
              AND job-mat.job     EQ job.job
              AND job-mat.job-no  EQ job.job-no
              AND job-mat.job-no2 EQ job.job-no2
              AND job-mat.rm-i-no EQ ttRctd.i-no
            BY job-mat.frm:

            CREATE ttMat.
            ASSIGN
                ttMat.frm = job-mat.frm
                ttMat.qty = job-mat.qty
                dQty1     = dQty1 + job-mat.qty
                .
        END.
        
        FOR EACH ttMat:
            ttMat.qty = IF dQty1 NE 0 THEN
                            ttRctd.qty * (ttMat.qty / dQty1)
                        ELSE
                            0.
 
            IF ttRctd.pur-uom EQ cUOMEach THEN DO:
                IF (ttMat.qty - INT(ttMat.qty)) > 0 THEN 
                    ttMat.qty = INT(ttMat.qty) + 1.
                ELSE 
                    ttMat.qty = INT(ttMat.qty).
            END.
            
            dQty2 = dQty2 + ttMat.qty.
        END.

        IF dQty2 NE ttRctd.qty THEN
            FOR FIRST ttMat:
                ttMat.qty = ttMat.qty + (ttRctd.qty - dQty2).
            END.
      END.
      ELSE DO:
          CREATE ttMat.
          ASSIGN
              ttMat.frm = ttRctd.s-num
              ttMat.qty = ttRctd.qty
              .
      END.
      
      FOR EACH ttMat:
          CREATE bf-ttRctd.
          BUFFER-COPY ttRctd EXCEPT rec_key TO bf-ttRctd
          ASSIGN
              bf-ttRctd.rita-code   = "I"
              bf-ttRctd.ttRctdRowID = ROWID(ttRctd)
              bf-ttRctd.SeqNo       = 2
              bf-ttRctd.s-num       = ttMat.frm
              bf-ttRctd.qty         = ttMat.qty 
              .
          FIND FIRST po-ord NO-LOCK 
               WHERE po-ord.company EQ ttRctd.company 
                 AND po-ord.po-no   EQ INT(ttRctd.po-no)
               NO-ERROR.
            
          IF AVAILABLE po-ord AND po-ord.TYPE <> "S" THEN
              bf-ttRctd.po-no = ""  .
          
          DELETE ttMat.
      END.
      
    RELEASE bf-ttRctd. 
END PROCEDURE.

PROCEDURE PostRMItems:
/*------------------------------------------------------------------------------
 Purpose: Posts RM items
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER ioplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage  AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-rm-rctd  FOR rm-rctd.
    DEFINE BUFFER bf-rm-bin   FOR rm-bin.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.

    DEFINE VARIABLE dCvtQty        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iTrNum         AS INTEGER NO-UNDO.
    DEFINE VARIABLE dBinQty        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dCost          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE iIndex         AS INTEGER NO-UNDO.
    DEFINE VARIABLE dReceiptQty    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dOutQty        AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dBwt           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dLen           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dWid           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dDep           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE riRecid        AS RECID   NO-UNDO.
    DEFINE VARIABLE riPOOrdl       AS ROWID   NO-UNDO.
    
    EMPTY TEMP-TABLE ttBoardToWIP.

    transblok:
    FOR EACH ttRctd
        WHERE CAN-FIND(FIRST item 
                       WHERE item.company EQ ipcCompany
                         AND item.i-no    EQ ttRctd.i-no)
        BREAK BY ttRctd.seqNo
              BY ttRctd.i-no
              BY ttRctd.r-no
              BY RECID(ttRctd):
        
        FIND FIRST rm-rctd EXCLUSIVE-LOCK
             WHERE ROWID(rm-rctd) EQ ttRctd.rmrctdRowID
             NO-ERROR NO-WAIT.
        IF NOT AVAILABLE rm-rctd THEN DO:
            ioplSuccess = NO.
            
            IF LOCKED(rm-rctd) THEN
                opcMessage  = "Receipt Record is in use.Can Not Update for item (" + ipcPrimaryID + ")".
            ELSE
                opcMessage  = "Receipt record is not available to post for item (" + ipcPrimaryID + ")".
  
            RETURN.
        END.
 
        FIND FIRST item EXCLUSIVE-LOCK
             WHERE item.company EQ rm-rctd.company
               AND item.i-no    EQ rm-rctd.i-no
             NO-ERROR NO-WAIT.
  
        IF NOT AVAILABLE item THEN DO:
            ioplSuccess = NO.
            
            IF LOCKED(item) THEN
                opcMessage  = "Item Record is in use.Can Not Update for item (" + ipcPrimaryID + ")".
            ELSE
                opcMessage  = "Item record is not available to post for item (" + ipcPrimaryID + ")".
  
            RETURN.
        END.

        IF rm-rctd.rita-code EQ "I" AND INT(rm-rctd.po-no) NE 0 THEN
            FOR EACH bf-rm-rctd NO-LOCK
                WHERE bf-rm-rctd.company   EQ ipcCompany
                  AND bf-rm-rctd.i-no      EQ rm-rctd.i-no
                  AND bf-rm-rctd.rita-code EQ "R" /* Receipts */
                  AND bf-rm-rctd.po-no     EQ rm-rctd.po-no
                  AND bf-rm-rctd.r-no      LT rm-rctd.r-no:
  
                UNDO transblok, NEXT transblok.
            END.
  
        FIND FIRST job NO-LOCK
             WHERE job.company EQ rm-rctd.company
               AND job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(rm-rctd.job-no))) + TRIM(rm-rctd.job-no)
               AND job.job-no2 EQ rm-rctd.job-no2
             NO-ERROR.
  
        /** Find Bin & if not avail then create it **/
        FIND FIRST rm-bin
             WHERE rm-bin.company EQ rm-rctd.company
               AND rm-bin.loc     EQ rm-rctd.loc
               AND rm-bin.i-no    EQ rm-rctd.i-no
               AND rm-bin.loc-bin EQ rm-rctd.loc-bin
               AND rm-bin.tag     EQ rm-rctd.tag
             NO-ERROR.
        IF NOT AVAILABLE rm-bin THEN DO:
            CREATE rm-bin.
            ASSIGN
                rm-bin.company = rm-rctd.company
                rm-bin.loc     = rm-rctd.loc
                rm-bin.loc-bin = rm-rctd.loc-bin
                rm-bin.tag     = rm-rctd.tag
                rm-bin.i-no    = rm-rctd.i-no
                .
        END.
  
        dCvtQty= rm-rctd.qty.
  
        IF NOT rm-rctd.rita-code  EQ "T"  AND
           NOT (rm-rctd.rita-code EQ "A" AND rm-rctd.qty LT 0) THEN
            rm-bin.po-no = ipiPONo.
  
        IF rm-rctd.pur-uom NE item.cons-uom AND item.cons-uom NE "" THEN
            RUN sys/ref/convquom.p (
                INPUT  rm-rctd.pur-uom,
                INPUT  item.cons-uom,
                INPUT  item.basis-w,
                INPUT  (IF item.r-wid EQ 0 THEN item.s-len ELSE 12),
                INPUT  (IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid),
                INPUT  item.s-dep,
                INPUT  dCvtQty,
                OUTPUT dCvtQty
                ).
  
        IF rm-rctd.rita-code EQ "R" THEN DO: /** RECEIPTS **/
            RUN pRMPost (
                INPUT        rm-bin.qty,
                INPUT-OUTPUT rm-bin.cost,
                INPUT        rm-rctd.qty,
                INPUT-OUTPUT rm-rctd.cost
                ).
  
            /* If total quantity was zero, rm-bin.cost will be ? */
            IF rm-bin.cost EQ ? THEN 
	         rm-bin.cost = 0.
				
            ASSIGN
                rm-bin.qty     = rm-bin.qty + dCvtQty
                item.last-cost = rm-rctd.cost
                item.q-onh     = item.q-onh + dCvtQty
                .
  
            IF INT(rm-rctd.po-no) NE 0 THEN
            update-po2: 
            DO:
                /* Check to see if po entered through the purchasing module. If
                so, verify all charaters are number type because po number
                in purchasing is a integer field. */
                DO i = 1 TO LENGTH(rm-rctd.po-no):
                    IF ASC(SUBSTRING(rm-rctd.po-no,i,1)) LT 48 OR
                       ASC(SUBSTRING(rm-rctd.po-no,i,1)) GT 57 THEN
                        LEAVE update-po2.
                END.
      
                FIND FIRST po-ord EXCLUSIVE-LOCK
                     WHERE po-ord.company EQ item.company
                       AND po-ord.po-no   EQ INT(rm-rctd.po-no)
                     NO-WAIT NO-ERROR.
                IF NOT AVAILABLE po-ord AND LOCKED po-ord THEN DO:
                    ASSIGN
                        ioplSuccess = NO
                        opcMessage  = "Purchase Order Record is in use.Can Not Update for item (" + ipcPrimaryID + ")"
                        .
      
                    RETURN.
                END.
      
                FOR EACH po-ordl NO-LOCK
                    WHERE po-ordl.company   EQ item.company
                      AND po-ordl.i-no      EQ rm-rctd.i-no
                      AND po-ordl.po-no     EQ int(rm-rctd.po-no)
                      AND po-ordl.deleted   EQ NO
                      AND po-ordl.item-type EQ YES
                      AND po-ordl.job-no    EQ rm-rctd.job-no
                      AND po-ordl.job-no2   EQ rm-rctd.job-no2
                    BREAK BY po-ordl.s-num DESCENDING:
      
                    riPOOrdl = ROWID(po-ordl).
      
                    IF LAST(po-ordl.s-num) OR po-ordl.s-num EQ rm-rctd.s-num THEN
                        LEAVE.
                END.
                
                FIND FIRST po-ordl EXCLUSIVE-LOCK
                     WHERE ROWID(po-ordl) EQ riPOOrdl
                     NO-WAIT NO-ERROR.
                IF AVAILABLE po-ordl THEN DO:
                    dCvtQty = rm-rctd.qty.
                    
                    IF rm-rctd.pur-uom NE po-ordl.cons-uom THEN
                        RUN sys/ref/convquom.p (
                            INPUT rm-rctd.pur-uom,
                            INPUT po-ordl.cons-uom,
                            INPUT item.basis-w,
                            INPUT po-ordl.s-len,
                            INPUT po-ordl.s-wid,
                            INPUT item.s-dep,
                            INPUT dCvtQty,
                            OUTPUT dCvtQty
                            ).
                    ASSIGN
                        po-ord.received   = YES
                        po-ordl.t-rec-qty = po-ordl.t-rec-qty + dCvtQty
                        .
                        
                    RUN rm/polclose.p (
                        INPUT ROWID(po-ordl),
                        INPUT rm-rctd.qty,
                        INPUT rm-rctd.pur-uom
                        ).
                END.
                ELSE IF NOT AVAILABLE po-ordl THEN DO:
                    ioplSuccess = NO.

                    IF LOCKED po-ordl THEN
                        opcMessage  = "Purchase Order Line Record is in use.  Can Not Update for item (" + ipcPrimaryID + ")".
                    ELSE
                        opcMessage  = "Purchase Order Line Record not found.  Can Not Update for item (" + ipcPrimaryID + ")".
      
                    RETURN.
                END.
            END.
            item.q-avail = item.q-onh + item.q-ono - item.q-comm.
        END. /* R */
        ELSE IF rm-rctd.rita-code EQ "I" THEN DO:  /** ISSUES **/
  
            IF rm-rctd.tag NE "" THEN
                FOR EACH  bf-rm-rdtlh FIELDS(r-no rita-code tag2) NO-LOCK
                    WHERE bf-rm-rdtlh.company   EQ ipcCompany
                      AND bf-rm-rdtlh.tag       EQ rm-rctd.tag
                      AND bf-rm-rdtlh.loc       EQ rm-rctd.loc
                      AND bf-rm-rdtlh.loc-bin   EQ rm-rctd.loc-bin
                      AND bf-rm-rdtlh.rita-code EQ "R"  /* Receipts */
                      AND bf-rm-rdtlh.tag2      NE "",
                    FIRST bf-rm-rcpth NO-LOCK
                    WHERE bf-rm-rcpth.r-no      EQ bf-rm-rdtlh.r-no
                      AND bf-rm-rcpth.rita-code EQ bf-rm-rdtlh.rita-code
                      AND bf-rm-rcpth.i-no      EQ rm-rctd.i-no:
                    rm-rctd.tag2 = bf-rm-rdtlh.tag2.
                END.
  
            IF AVAILABLE job AND job.job-no NE "" THEN DO:
                RUN rm/mkjobmat.p (
                    INPUT RECID(rm-rctd),
                    INPUT rm-rctd.company,
                    OUTPUT riRecid
                    ).
  
                FIND FIRST job-mat
                     WHERE RECID(job-mat) EQ riRecid NO-ERROR.
                IF NOT AVAILABLE job-mat THEN DO:
                    ASSIGN
                        ioplSuccess = NO
                        opcMessage  = "Job Mat Record not found for " + STRING(job.job-no) + "-" + STRING(job.job-no2,"99") + "  " + rm-rctd.i-no
                        .
  
                    RETURN.
                END.
  
                ASSIGN
                    dBwt = job-mat.basis-w
                    dLen = job-mat.len
                    dWid = job-mat.wid
                    dDep = item.s-dep
                    .
  
                IF dLen EQ 0 THEN
                    dLen = item.s-len.
  
                IF dWid EQ 0 THEN
                    dWid = IF item.r-wid NE 0 THEN
                               item.r-wid
                           ELSE
                               item.s-wid.
  
                IF dBwt EQ 0 THEN
                    dBwt = item.basis-w.
  
                IF INDEX("RL",job.stat) NE 0 THEN /* Checks job status whether it is "R" or "L" */
                    job.stat = "W".
  
                CREATE mat-act.
                ASSIGN
                    mat-act.company   = ipcCompany
                    mat-act.mat-date  = TODAY
                    mat-act.job       = job.job
                    mat-act.job-no    = job-mat.job-no
                    mat-act.job-no2   = job-mat.job-no2
                    mat-act.s-num     = job-mat.frm
                    mat-act.b-num     = job-mat.blank-no
                    mat-act.i-no      = job-mat.i-no
                    mat-act.i-name    = rm-rctd.i-name
                    mat-act.rm-i-no   = job-mat.i-no
                    mat-act.rm-i-name = rm-rctd.i-name
                    mat-act.pass      = rm-rctd.pass
                    mat-act.tag       = rm-rctd.tag
                    mat-act.loc       = rm-rctd.loc
                    mat-act.loc-bin   = rm-rctd.loc-bin
                    mat-act.opn       = yes
                    mat-act.mat-time  = time
                    .
  
                dOutQty = rm-rctd.qty.
                
                IF rm-rctd.pur-uom NE job-mat.qty-uom AND rm-rctd.pur-uom NE "" THEN
                    RUN sys/ref/convquom.p(
                        INPUT rm-rctd.pur-uom,
                        INPUT job-mat.qty-uom,
                        INPUT dBwt,
                        INPUT dLen,
                        INPUT dWid,
                        INPUT dDep,
                        INPUT rm-rctd.qty,
                        OUTPUT dOutQty
                        ).
  
                dCost = rm-rctd.cost.
  
                IF rm-rctd.pur-uom NE job-mat.sc-uom AND rm-rctd.pur-uom NE "" THEN
                    RUN sys/ref/convcuom.p(
                        INPUT rm-rctd.pur-uom,
                        INPUT job-mat.sc-uom,
                        INPUT dBwt,
                        INPUT dLen,
                        INPUT dWid,
                        INPUT dDep,
                        INPUT rm-rctd.cost,
                        OUTPUT dCost
                        ).
  
                ASSIGN
                    mat-act.qty-uom = job-mat.qty-uom
                    mat-act.cost    = dCost
                    mat-act.qty     = mat-act.qty     + dOutQty
                    job-mat.qty-iss = job-mat.qty-iss + dOutQty
                    job-mat.qty-all = job-mat.qty-all - dOutQty
                    item.q-comm     = item.q-comm     - rm-rctd.qty
                    .
  
                RUN sys/ref/convquom.p(
                    INPUT rm-rctd.pur-uom,
                    INPUT job-mat.sc-uom,
                    INPUT dBwt,
                    INPUT dLen,
                    INPUT dWid,
                    INPUT dDep,
                    INPUT rm-rctd.qty,
                    OUTPUT dOutQty
                    ).
  
                mat-act.ext-cost = mat-act.ext-cost + (dCost * dOutQty).
  
                IF job-mat.qty-all LT 0 THEN DO:
                
                    RUN sys/ref/convquom.p(
                        INPUT job-mat.qty-uom,
                        INPUT rm-rctd.pur-uom,
                        INPUT dBwt,
                        INPUT dLen,
                        INPUT dWid,
                        INPUT dDep,
                        INPUT job-mat.qty-all,
                        OUTPUT dOutQty
                        ).
                        
                    ASSIGN
                        job-mat.qty-all = 0
                        item.q-comm     = item.q-comm - dOutQty
                        .
                END.
  
                IF item.q-comm LT 0 THEN
                    item.q-comm = 0.
                    
                IF item.mat-type EQ "B" THEN
                    RUN rm/rm-addcr.p (
                        INPUT ROWID(rm-rctd)
                        ).
            END.
  
            FIND FIRST rm-bin
                 WHERE rm-bin.company EQ rm-rctd.company
                   AND rm-bin.loc     EQ rm-rctd.loc
                   AND rm-bin.i-no    EQ rm-rctd.i-no
                   AND rm-bin.loc-bin EQ rm-rctd.loc-bin
                   AND rm-bin.tag     EQ rm-rctd.tag
                 NO-ERROR.
  
            ASSIGN
                rm-bin.qty     = rm-bin.qty - dCvtQty
                item.q-onh     = item.q-onh - dCvtQty
                item.qlast-iss = rm-rctd.qty
                item.dlast-iss = rm-rctd.rct-date
                item.q-ytd     = item.q-ytd + rm-rctd.qty
                item.q-ptd     = item.q-ptd + rm-rctd.qty
                item.u-ptd     = item.u-ptd + (rm-rctd.cost * rm-rctd.qty)
                item.u-ytd     = item.u-ytd + (rm-rctd.cost * rm-rctd.qty)
                item.q-avail   = item.q-onh + item.q-ono - item.q-comm
                .
  
            IF rm-bin.qty EQ 0 THEN
                DELETE rm-bin.
  
        END.  /* I */
        IF rm-rctd.rita-code EQ "A" THEN DO:  /** ADJUSTMENTS **/
            IF rm-rctd.cost NE 0 THEN DO:
                RUN pRMPost (
                    INPUT        rm-bin.qty,
                    INPUT-OUTPUT rm-bin.cost,
                    INPUT        rm-rctd.qty,
                    INPUT-OUTPUT rm-rctd.cost
                    ).
            END.
  
            ASSIGN
                rm-bin.qty     = rm-bin.qty + dCvtQty
                item.last-cost = IF rm-rctd.cost NE 0 THEN
                                     rm-rctd.cost
                                 ELSE
                                     item.last-cost
                item.q-onh     = item.q-onh + dCvtQty
                item.q-avail   = item.q-onh + item.q-ono - item.q-comm
                .
        END. /* A */
        ELSE IF rm-rctd.rita-code EQ "T" THEN DO:  /** TRANSFERS **/
            ASSIGN
                rm-bin.qty   = rm-bin.qty - rm-rctd.qty
                rm-rctd.cost = rm-bin.cost.
  
            /* This code is to handel the Transfer to quantity to increase the BIN
            using a buffer record so current rm-bin record is not updated. */
  
            FIND FIRST bf-rm-bin
                 WHERE bf-rm-bin.company EQ rm-rctd.company
                   AND bf-rm-bin.loc     EQ rm-rctd.loc2
                   AND bf-rm-bin.i-no    EQ rm-rctd.i-no
                   AND bf-rm-bin.loc-bin EQ rm-rctd.loc-bin2
                   AND bf-rm-bin.tag     EQ rm-rctd.tag2
                 NO-ERROR.
            IF NOT AVAILABLE bf-rm-bin THEN DO:
                CREATE bf-rm-bin.
                ASSIGN
                    bf-rm-bin.company = rm-rctd.company
                    bf-rm-bin.loc     = rm-rctd.loc2
                    bf-rm-bin.loc-bin = rm-rctd.loc-bin2
                    bf-rm-bin.tag     = rm-rctd.tag2
                    bf-rm-bin.i-no    = rm-rctd.i-no
                    .
            END.
            RUN pRMPost (
                INPUT        bf-rm-bin.qty,
                INPUT-OUTPUT bf-rm-bin.cost,
                INPUT        rm-rctd.qty,
                INPUT-OUTPUT rm-rctd.cost
                ).
            bf-rm-bin.qty = bf-rm-bin.qty + rm-rctd.qty.
        END. /* T */
  
        IF TRIM(rm-rctd.tag) NE "" THEN
            FIND FIRST loadtag EXCLUSIVE-LOCK
                 WHERE loadtag.company     EQ rm-rctd.company
                   AND loadtag.item-type   EQ YES
                   AND loadtag.tag-no      EQ rm-rctd.tag
                   AND loadtag.i-no        EQ rm-rctd.i-no
                   AND loadtag.is-case-tag EQ NO
                 NO-ERROR.
  
            IF AVAILABLE loadtag THEN DO:
                ASSIGN
                    loadtag.loc     = rm-rctd.loc
                    loadtag.loc-bin = rm-rctd.loc-bin
                    .
        
            iIndex = INDEX("RI",rm-rctd.rita-code). /* checking rita-code for "RECEIPTS" and "ISSUES" */
  
            IF iIndex EQ 1 AND (NOT AVAILABLE rm-bin OR rm-bin.qty LT 0) THEN
                iIndex = 3.
  
            IF iIndex GT 0 THEN
                loadtag.sts = ENTRY(iIndex,"Received,Issued,Deleted").
        END.
  
        IF LAST-OF(ttRctd.i-no) THEN             /* Calculate average cost */
            FOR EACH rm-bin NO-LOCK
                WHERE rm-bin.company EQ rm-rctd.company
                  AND rm-bin.i-no    EQ rm-rctd.i-no
                BREAK BY rm-bin.i-no:
  
                IF FIRST(rm-bin.i-no) THEN
                    ASSIGN
                        dReceiptQty = 0
                        dCost       = 0
                        dBinQty     = rm-bin.qty
                        .
  
                IF dBinQty LT 0 THEN
                    dBinQty = dBinQty * -1.
  
                ASSIGN
                    dReceiptQty = dReceiptQty + dBinQty
                    dCost       = dCost    + (dBinQty * rm-bin.cost)
                    .
  
                IF dCost EQ ? THEN
                    dCost = 0.
  
                IF LAST(rm-bin.i-no) AND dReceiptQty NE 0 AND dCost NE 0
                    AND dReceiptQty NE ? AND dCost NE ? THEN
                    item.avg-cost = dCost / dReceiptQty.
  
            END. 
  
        RUN pAssignPrepInfo.
        RUN pFinal.
        
        FIND CURRENT rm-rctd  NO-LOCK NO-ERROR.
        FIND CURRENT item     NO-LOCK NO-ERROR.
        FIND CURRENT rm-rcpth NO-LOCK NO-ERROR.
        FIND CURRENT rm-rdtlh NO-LOCK NO-ERROR.
        FIND CURRENT job-mat  NO-LOCK NO-ERROR.
        
        IF item.mat-type EQ "B" AND AVAILABLE rm-rctd THEN DO:
            CREATE ttBoardToWIP.
            ASSIGN
                ttBoardToWIP.rmrdtlhRowID = ROWID(rm-rdtlh)
                ttBoardToWIP.rmrcpthRowID = ROWID(rm-rcpth)
                ttBoardToWIP.rmbinRowID   = ROWID(rm-bin)
                ttBoardToWIP.jobmatRowID  = ROWID(job-mat)
                ttBoardToWIP.itemRowID    = ROWID(item)
                .
        END.
  
    END.
    
    FOR EACH rm-rctd EXCLUSIVE-LOCK
        WHERE rm-rctd.company   EQ ipcCompany
          AND rm-rctd.rita-code EQ "ADDER":
        rm-rctd.rita-code = "I".      
    END.     

    DO TRANSACTION:
        REPEAT:  
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                 WHERE gl-ctrl.company EQ ipcCompany 
                 NO-ERROR NO-WAIT.
            
            IF LOCKED(gl-ctrl) THEN DO:
                ASSIGN
                    opcMessage  = "glctrl is locked"
                    ioplSuccess = NO
                    .
                
                RETURN.
            END.
            IF AVAILABLE gl-ctrl THEN DO:
                ASSIGN
                    iTrNum        = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = iTrNum
                    .
  
                FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
  
                RUN pGLFromWork (
                    INPUT 1,
                    INPUT iTrNum
                    ).
                RUN pGLFromWork (
                    INPUT 2,
                    INPUT iTrNum
                    ).
                LEAVE.
            END.
        END.
    END.
  
    RUN pCreateWIPInventoryStock.
    
    RELEASE bf-rm-rctd.
    RELEASE bf-rm-rcpth.
    RELEASE bf-rm-rdtlh.
    RELEASE bf-rm-bin.
END PROCEDURE.

PROCEDURE pAssignPrepInfo PRIVATE:
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-ttRctd FOR ttRctd.

FOR EACH  bf-ttRctd   
    WHERE bf-ttRctd.SeqNo EQ ttRctd.SeqNo 
      AND bf-ttRctd.i-no  EQ ttRctd.i-no:

    FOR EACH  prep 
        WHERE prep.company EQ ipcCompany
          AND prep.CODE    EQ bf-ttRctd.i-no:
        ASSIGN
            prep.loc            = bf-ttRctd.loc
            prep.loc-bin        = bf-ttRctd.loc-bin
            prep.received-date  = bf-ttRctd.rct-date
            .
        IF bf-ttRctd.job-no NE "" THEN
            ASSIGN
                prep.last-job-no    = bf-ttRctd.job-no
                prep.last-job-no2   = bf-ttRctd.job-no2
                .
    END.
END.

RELEASE bf-ttRctd.
END PROCEDURE.

PROCEDURE pFinal PRIVATE :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE iSeqNo       AS INTEGER NO-UNDO.
    DEFINE VARIABLE dQtyReceived AS DECIMAL NO-UNDO.
    DEFINE VARIABLE daPostDate   AS DATE    NO-UNDO.
    DEFINE VARIABLE lRMTags      AS LOGICAL INITIAL YES NO-UNDO.
     
    DEFINE BUFFER bf-ttrctd   FOR ttRctd.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
   
    IF rm-rctd.rita-code EQ "I" AND TRIM(rm-rctd.tag) NE "" THEN
        FOR EACH bf-rm-rdtlh NO-LOCK
            WHERE bf-rm-rdtlh.company   EQ rm-rctd.company
              AND bf-rm-rdtlh.tag       EQ rm-rctd.tag
              AND bf-rm-rdtlh.rita-code EQ "R",
            FIRST bf-rm-rcpth
            WHERE bf-rm-rcpth.r-no      EQ bf-rm-rdtlh.r-no
              AND bf-rm-rdtlh.rita-code EQ bf-rm-rdtlh.rita-code
            NO-LOCK:
    
            IF rm-rctd.po-no EQ "" THEN 
                rm-rctd.po-no = bf-rm-rcpth.po-no.
            
            IF rm-rctd.job-no EQ "" THEN
                ASSIGN
                    rm-rctd.job-no  = bf-rm-rcpth.job-no
                    rm-rctd.job-no2 = bf-rm-rcpth.job-no2
                    .
            
            LEAVE.
        END.
       
    IF lRMTags AND TRIM(rm-rctd.tag) NE "" THEN DO:
        FOR EACH wiptag EXCLUSIVE-LOCK
            WHERE wiptag.company   EQ rm-rctd.company 
              AND wiptag.rm-tag-no EQ rm-rctd.tag:
              
            wiptag.sts = "On Hand" .
        END.
    END.
    
    /* Creates history records */
    RUN pHistory.
    
    DELETE rm-rctd.
    
    FOR EACH bf-ttrctd WHERE bf-ttrctd.ttRctdRowID EQ ROWID(ttRctd):
        iSeqNo = 0.
        RUN sys/ref/asiseq.p (
            INPUT ipcCompany, 
            INPUT "rm_rcpt_seq", 
            OUTPUT iSeqNo
            ) NO-ERROR.
            
        IF ERROR-STATUS:ERROR THEN DO:
            ASSIGN
                opcMessage  = "Could not obtain next sequence #, please contact ASI:for item (" + ipcPrimaryID + ")"
                oplSuccess = NO
                .
            RETURN. 
        END.
    
        CREATE rm-rctd.
        BUFFER-COPY bf-ttrctd TO rm-rctd
        ASSIGN
             rm-rctd.r-no           = iSeqNo
             bf-ttrctd.r-no         = rm-rctd.r-no
             bf-ttrctd.ttRctdHasRec = YES
             bf-ttrctd.rmrctdRowID  = ROWID(rm-rctd)
             .    
    
    END.
    
    DELETE  ttRctd.
    RELEASE bf-ttrctd.
    RELEASE bf-rm-rdtlh.
    RELEASE bf-rm-rcpth.
    
END PROCEDURE.

PROCEDURE pGLFromWork :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipiRun   AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiTRNum AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE dCredits   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dDebits    AS DECIMAL   NO-UNDO. 
    DEFINE VARIABLE daPostDate AS DATE      NO-UNDO.
    DEFINE VARIABLE cRMPost    AS CHARACTER NO-UNDO INITIAL "RMPOST".
    
    DEFINE BUFFER bf-work-gl FOR work-gl.
    DEFINE BUFFER bf-gltrans FOR gltrans.
    
    daPostDate = TODAY.
    
    FIND FIRST period
         WHERE period.company EQ ipcCompany
           AND period.pst     LE daPostDate 
           AND period.pend    GE daPostDate 
         NO-LOCK.
    
    FOR EACH bf-work-gl NO-LOCK
        WHERE (ipiRun EQ 1 AND bf-work-gl.job-no NE "")
           OR (ipiRun EQ 2 AND bf-work-gl.job-no EQ "")
        BREAK BY bf-work-gl.actnum:

        ASSIGN
            dDebits  = dDebits  + bf-work-gl.debits
            dCredits = dCredits + bf-work-gl.credits
            .

        IF LAST-OF(bf-work-gl.actnum) THEN DO:
            CREATE bf-gltrans.
            ASSIGN
                bf-gltrans.company = ipcCompany
                bf-gltrans.actnum  = bf-work-gl.actnum
                bf-gltrans.jrnl    = cRMPost
                bf-gltrans.period  = IF AVAILABLE period THEN
                                        period.pnum
                                     ELSE
                                        0 
                bf-gltrans.tr-amt  = dDebits - dCredits
                bf-gltrans.tr-date = daPostDate
                bf-gltrans.tr-dscr = IF bf-work-gl.job-no NE "" THEN 
                                        "RM Issue to Job"
                                     ELSE 
                                        "RM Receipt"
                bf-gltrans.trnum   = ipiTRNum
                dDebits            = 0
                dCredits           = 0
                .
        END.
     
    END.
    RELEASE bf-gltrans.
    RELEASE bf-gltrans.
END PROCEDURE.

PROCEDURE pCreateWIPInventoryStock :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE VARIABLE lSuccess           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecValue          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRecFound          AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cSysCtrlRMIssueWIP AS CHARACTER NO-UNDO INITIAL "RMIssueWIP".
    
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,               /* Company Code */
        INPUT  cSysCtrlRMIssueWIP,       /* sys-ctrl name */
        INPUT  "L",                      /* Output return value I - int-fld, L - log-flf, C - char-fld, D - dec-fld, DT - date-fld */
        INPUT  FALSE,                    /* Use ship-to */
        INPUT  FALSE,                    /* ship-to vendor */
        INPUT  "",                       /* ship-to vendor value */
        INPUT  "",                       /* shi-id value */
        OUTPUT cRecValue,
        OUTPUT lRecFound
        ).
    
    IF lRecFound EQ FALSE THEN
        RETURN.
        
    IF LOGICAL(cRecValue) EQ FALSE THEN
        RETURN.
            
    IF VALID-HANDLE(hdInventoryProcs) THEN DO:
        FOR EACH ttBoardToWIP:
            RUN Inventory_CreateWIPInventoryStockForIssuedRM IN hdInventoryProcs (
                INPUT  ttBoardToWIP.rmrdtlhRowID,
                INPUT  ttBoardToWIP.rmrcpthRowID,
                INPUT  ttBoardToWIP.rmbinRowID,
                INPUT  ttBoardToWIP.jobmatRowID,
                INPUT  ttBoardToWIP.itemRowID,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ) NO-ERROR.   
        END.
    END.
END PROCEDURE.

PROCEDURE pRMPost PRIVATE:
/* ---------------------------------------------------  */
/* Rm Posting - Calculate new average cost for the bin                        */
/* -------------------------------------------------------------------------- */
    DEFINE INPUT        PARAMETER ipdBinQty       AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdBinCost     AS DECIMAL NO-UNDO.
    DEFINE INPUT        PARAMETER ipdReceiptQty   AS DECIMAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopdReceiptCost AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE dBinQty      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dReceiptQty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dTotalQty    AS DECIMAL NO-UNDO.

    ASSIGN
        dBinQty     = ipdBinQty 
        dReceiptQty = ipdReceiptQty 
        .

    /* Takes first total cost + 2nd total cost / total qty to get avg. cost */    
    dTotalQty  = dBinQty + dReceiptQty.
   
    /* Case 1 - Transaction is positive and bin is positive */
    IF dReceiptQty GE 0 AND dBinQty GT 0 THEN 
      /* Takes first total cost + 2nd total cost / total qty to get avg. cost */
       iopdBinCost = ((dBinQty * iopdBinCost) + (dReceiptQty * iopdReceiptCost)) / dTotalQty.
    ELSE
    /* Case 2 - Transaction is positive, orig. bin qty is negative */
    /* Take the cost from the new transaction to avoid very large denominator */
    IF dReceiptQty GE 0 AND dBinQty LE 0 THEN 
        iopdBinCost = iopdReceiptCost.
    ELSE
    /* Case 3 - Transaction qty is negative, new bin quantity is positive */
    IF dReceiptQty LT 0 AND dReceiptQty + dBinQty GT 0 THEN 
        iopdBinCost = ((dBinQty * iopdBinCost) + (dReceiptQty * iopdReceiptCost)) / dTotalQty.
    ELSE
    /* Case 4 - Both transaction and bin quantities are negative */
    IF dReceiptQty LT 0 AND dReceiptQty + dBinQty LE 0 THEN 
      /* No change */
    IF iopdBinCost LT 0 THEN
        iopdBinCost  = iopdBinCost * -1.
    /* If total quantity was zero, dBinCost will be ? */
    IF iopdBinCost EQ ? THEN 
        iopdBinCost = 0.

END PROCEDURE.


PROCEDURE pHistory:
/*------------------------------------------------------------------------------
  Purpose:    Creates history records  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/  
    DEFINE VARIABLE daPostDate AS DATE NO-UNDO.
    
    DEFINE BUFFER bf-rm-rcpth FOR rm-rcpth.
    DEFINE BUFFER bf-rm-rdtlh FOR rm-rdtlh.
    
    daPostDate = TODAY.
    
    FIND FIRST bf-rm-rcpth NO-LOCK
         WHERE bf-rm-rcpth.r-no EQ rm-rctd.r-no 
         NO-ERROR.
    IF AVAILABLE bf-rm-rcpth THEN DO:
        RUN sys/ref/asiseq.p (
            INPUT bf-rm-rcpth.company, 
            INPUT "rm_rcpt_seq", 
            OUTPUT bf-rm-rcpth.r-no
            ) NO-ERROR.
    END.
  
    CREATE bf-rm-rcpth.
    BUFFER-COPY rm-rctd EXCEPT rec_key user-id upd-date upd-time TO bf-rm-rcpth
    ASSIGN
        bf-rm-rcpth.trans-date = rm-rctd.rct-date
        bf-rm-rcpth.post-date  = daPostDate
        .

    CREATE bf-rm-rdtlh.
    BUFFER-COPY rm-rctd EXCEPT rec_key user-id upd-date upd-time TO bf-rm-rdtlh.

    IF rm-rctd.rita-code EQ "T" THEN DO:
        bf-rm-rdtlh.qty = rm-rctd.qty * -1.
        
        IF bf-rm-rdtlh.tag EQ bf-rm-rdtlh.tag2 THEN
            bf-rm-rdtlh.tag2 = "".
    
        CREATE bf-rm-rdtlh.
        BUFFER-COPY rm-rctd EXCEPT rec_key user-id upd-date upd-time TO bf-rm-rdtlh
        ASSIGN
            bf-rm-rdtlh.loc     = rm-rctd.loc2
            bf-rm-rdtlh.loc-bin = rm-rctd.loc-bin2
            bf-rm-rdtlh.tag     = rm-rctd.tag2
            .
        
        IF bf-rm-rdtlh.tag EQ bf-rm-rdtlh.tag2 THEN
            bf-rm-rdtlh.tag2 = "".
    
    END.
    RELEASE bf-rm-rdtlh.
    RELEASE bf-rm-rcpth.
END PROCEDURE.

PROCEDURE pConvertVendCompCurr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER iopdCost AS DECIMAL DECIMALS 4 NO-UNDO.
    
    FIND FIRST vend NO-LOCK
         WHERE vend.company EQ po-ord.company 
           AND vend.vend-no EQ po-ord.vend-no
         NO-ERROR.

    IF AVAILABLE vend THEN DO:
        FIND FIRST company NO-LOCK 
             WHERE company.company EQ ipcCompany
             NO-ERROR.

        IF vend.curr-code NE company.curr-code THEN DO:
            FIND FIRST currency NO-LOCK
                 WHERE currency.company EQ po-ord.company 
                   AND currency.c-code  EQ vend.curr-code
                 NO-ERROR.

            IF AVAILABLE currency THEN DO:
                iopdCost = iopdCost * currency.ex-rate.

                RELEASE currency.
            END.
        END.

        RELEASE company.
        RELEASE vend.
    END.
END PROCEDURE.

DELETE PROCEDURE hdCostProcs.
DELETE PROCEDURE hdInventoryProcs.
DELETE PROCEDURE hdSession.
DELETE PROCEDURE hdTags.
    
    


