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
DEFINE VARIABLE cReceipt         AS CHARACTER NO-UNDO INITIAL "R".
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
DEFINE VARIABLE cVendNo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdReceipt        AS HANDLE    NO-UNDO.
DEFINE VARIABLE lPromptForClose  AS LOGICAL   NO-UNDO.

{api\inbound\ttMat.i}
{api\inbound\ttRctd.i} 
                         
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
RUN api\inbound\InventoryReceiptProcs.p PERSISTENT SET hdReceipt. 

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
    
ASSIGN
    cCustNo = IF po-ordl.cust-no NE "" THEN
                  po-ordl.cust-no
              ELSE IF AVAILABLE oe-ordl AND oe-ordl.cust-no NE "" THEN
                  oe-ordl.cust-no
              ELSE
                  ""
    cVendNo = po-ordl.vend-no
    .

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

/* Gets loadtagformat for FG */
IF ipcPrimaryID EQ cItemFG THEN DO:               
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
            INPUT YES,        /* ship-to customer */
            INPUT cCustNo,    /* ship-to customer value */
            INPUT "",         /* shi-id value */
            OUTPUT opcLoadtagFormat,  
            OUTPUT lRecFound
            ).
END.
/* Get loadtagformat for RM */
ELSE
    RUN sys\ref\nk1look.p(
        INPUT ipcCompany, /* Company Code */
        INPUT "RMBARDIR", /* sys-ctrl name */
        INPUT "C",        /* Output return value */
        INPUT YES,        /* Use ship-to */
        INPUT NO,         /* ship-to vendor */
        INPUT cVendNo,    /* ship-to vendor value */
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
                INPUT        cReceipt,        /* Receipt */
                INPUT        ipcUsername,
                INPUT        lPromptForClose, /* Executes API closing orders logic */
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
                 
                IF lAutoIssue THEN DO:
                    /* creates RMRctd records for receipts */
                    RUN InventoryReceipt_RMIssueCreation IN hdReceipt(
                        INPUT-OUTPUT TABLE ttRctd BY-REFERENCE, /* Just need to pass handle */
                        INPUT        rm-rctd.company,
                        INPUT        rm-rctd.tag,
                        INPUT-OUTPUT oplSuccess,
                        OUTPUT       opcMessage
                        ) NO-ERROR.

                    IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
                        ASSIGN
                            opcMessage = "Error while creating RM issue for item (" + ipcPrimaryID + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
                            oplSuccess = NO
                            .                    
                        
                        UNDO,LEAVE.
                    END.
                END.
            
            END.

            /* Posts RM goods  */
            RUN InventoryReceipt_PostRMItems IN hdReceipt (
                INPUT-OUTPUT TABLE ttRctd BY-REFERENCE, /* Just need to pass handle */
                INPUT        ipcCompany,
                INPUT        ipiPONo,
                INPUT-OUTPUT oplSuccess,
                OUTPUT       opcMessage
                ) NO-ERROR.
  
            IF ERROR-STATUS:ERROR OR NOT oplSuccess THEN DO:
                ASSIGN
                    opcMessage = "Error while Posting RM receipt for item (" + ipcPrimaryID + ") " + ERROR-STATUS:GET-MESSAGE(1) + " " + opcMessage
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
    RUN InventoryReceipt_GetCostsFromPO IN hdReceipt (
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
        
    RUN InventoryReceipt_ConvertVendCompCurr IN hdReceipt (
        INPUT        ipcCompany,
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

DELETE PROCEDURE hdReceipt.
DELETE PROCEDURE hdSession.
DELETE PROCEDURE hdTags.
DELETE PROCEDURE hdInventoryProcs.

    
    


