/*------------------------------------------------------------------------
    File        : api\inbound\CreateTagForPO.p
    Purpose     : creates Tagforpo

    Syntax      :

    Description : creates Tagforpo

    Author(s)   : Vishnu Vellanki
    Created     : Tue Oct 11 07:33:22 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/
{inventory/ttinventory.i "NEW SHARED"}.
{fg/invrecpt.i NEW}

DEFINE INPUT  PARAMETER ipcCompany            AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiPONo               AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipiPOLine             AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcPrimaryID          AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemType           AS CHARACTER NO-UNDO. 
DEFINE INPUT  PARAMETER ipdQuantity           AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipdQuantityPerSubUnit AS DECIMAL   NO-UNDO.
DEFINE INPUT  PARAMETER ipcStockIDAlias       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcUsername           AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcCreateReceipt      AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcInventoryStockID   AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oplSuccess            AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER opcMessage            AS CHARACTER NO-UNDO.

DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.
DEFINE VARIABLE lValidCompany    AS LOGICAL NO-UNDO.
DEFINE VARIABLE lValidPONo       AS LOGICAL NO-UNDO.
DEFINE VARIABLE lValidPOLine     AS LOGICAL NO-UNDO.
DEFINE VARIABLE iTagNo           AS INTEGER NO-UNDO.
DEFINE VARIABLE lFGRecpt         AS LOGICAL NO-UNDO.
DEFINE VARIABLE iRNo             AS INTEGER NO-UNDO.

RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 

ASSIGN
    oplSuccess    = YES
    lValidCompany = YES
    lValidPONo    = YES
    lValidPOLine  = YES
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

/* Validate PO Number */
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

/* Validate PO Line and if PO Line is not valid, then it dafults to 1*/
lValidPOLine = CAN-FIND(FIRST po-ordl NO-LOCK
                         WHERE po-ordl.company EQ ipcCompany
                           AND po-ordl.po-no   EQ ipiPONo
                           AND po-ordl.line    EQ ipiPOLine).
IF NOT lValidPOLine THEN
    ipiPOLine = 1.

/* Validate CreateReceipt */
IF ipcCreateReceipt EQ "" OR (ipcCreateReceipt NE "YES" AND ipcCreateReceipt NE "NO") THEN DO:
    ASSIGN 
        opcMessage = "Please enter valid option (yes / no) for creating receipt"
        oplSuccess = NO
        .
        RETURN.
END.
   
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

IF NOT oplSuccess THEN
    RETURN.
    
/* This block will be modified with extra logic to handle inventory stockalias for po creation */
IF ipcItemType EQ "FG" THEN DO: 
    FIND FIRST loadtag NO-LOCK WHERE loadtag.company     EQ ipcCompany
                                 AND loadtag.item-type   EQ NO
                                 AND loadtag.is-case-tag EQ NO
                               NO-ERROR.
    
    IF NOT AVAILABLE loadtag THEN DO:
        ASSIGN
            opcMessage = "company " + ipcCompany + "is not available to create loadtag"
            oplSuccess = NO
            .
        RETURN. 
    END.  
    
    FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ loadtag.company
                                AND po-ord.po-no   EQ ipiPONo 
                              NO-ERROR.
                      
    FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                                 AND po-ordl.po-no   EQ po-ord.po-no
                                 AND po-ordl.line    EQ ipiPOLine
                                 AND po-ordl.i-no    EQ ipcPrimaryID USE-INDEX po-no  
                               NO-ERROR.
    IF NOT AVAILABLE po-ordl THEN DO:
        ASSIGN 
            opcMessage = "Primary ID is not available for the given PO Number and PO Line" 
            oplSuccess = NO
            .
        RETURN.
    END.
            
    FIND FIRST cust NO-LOCK WHERE cust.company EQ ipcCompany
                              AND cust.cust-no EQ po-ord.cust-no 
                            NO-ERROR.
    
    FIND FIRST vend NO-LOCK WHERE vend.company EQ ipcCompany
                              AND vend.vend-no EQ po-ord.vend-no 
                            NO-ERROR.
    
    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ ipcCompany
                                AND itemfg.i-no    EQ po-ordl.i-no 
                              NO-ERROR.
    
    FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ ipcCompany
                                AND fg-bin.i-no    EQ ipcPrimaryID
                                AND fg-bin.job-no  EQ loadtag.job-no
                                AND fg-bin.tag     EQ loadtag.tag-no  
                              NO-ERROR.

 /* Creates a new TAG number */
    RUN GetNextLoadtagNumber (
        INPUT  ipcCompany, 
        INPUT  ipcPrimaryID, 
        OUTPUT iTagNo
        ).
     
     
    DO TRANSACTION ON ERROR UNDO,LEAVE:
      
        /* Creates a new loadtag table record*/
        RUN CreateLoadTag (
            INPUT  ipcCompany, 
            INPUT  ipiPONo, 
            INPUT  ipiPOLine, 
            INPUT  ipcPrimaryID, 
            INPUT  ipcItemType, 
            INPUT  ipdQuantity, 
            INPUT  ipdQuantityPerSubUnit, 
            INPUT  ipcStockIDAlias, 
            INPUT  iTagNo,
            OUTPUT opcInventoryStockID
            ).
      
        /* Checking sys-cntrl to create FG reciept for the given company*/     
        FIND FIRST sys-ctrl NO-LOCK
             WHERE sys-ctrl.company EQ ipcCompany
               AND sys-ctrl.name    EQ "FGRECPT"
             NO-ERROR.
        lFGRecpt = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".
    
        /* Retrieving last record of fg-rctd table*/ 
        FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
        IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iRNo THEN 
            iRNo = fg-rctd.r-no.
        
        /* Creates a new FGRctd table record  */
        IF ipcCreateReceipt EQ "yes" THEN	
            RUN FGReceiptCreation.
     
        /* Catches error if do transaction block fails*/
        CATCH eSysError AS Progress.Lang.SysError:
            ASSIGN
                opcMessage = "Unable to create tag#"
                oplSuccess = NO
                .
            RETURN.
        END.
    END.   
END.    


PROCEDURE GetNextLoadtagNumber PRIVATE :
/*------------------------------------------------------------------------------
 Purpose: Creates new TAG number
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER  ipcCompany  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER  ipcFGItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER  opiNextTag  AS INTEGER   NO-UNDO.

    DEFINE VARIABLE iLastFGTag AS INTEGER.
    DEFINE VARIABLE iLastRMTag AS INTEGER.
  
    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company             EQ ipcCompany
            AND loadtag.item-type           EQ NO
            AND loadtag.is-case-tag         EQ NO
            AND loadtag.tag-no              BEGINS ipcFGItemID 
            AND SUBSTR(loadtag.tag-no,1,15) EQ ipcFGItemID
          USE-INDEX tag NO-ERROR.

    iLastFGTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.

    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company             EQ ipcCompany
            AND loadtag.item-type           EQ YES
            AND loadtag.is-case-tag         EQ NO
            AND loadtag.tag-no              BEGINS ipcFGItemID 
            AND SUBSTR(loadtag.tag-no,1,15) EQ ipcFGItemID
          USE-INDEX tag NO-ERROR.

    iLastRMTag = (IF AVAILABLE loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
  
    opiNextTag = MAX(iLastFGTag, iLastRMTag).

END PROCEDURE.


PROCEDURE CreateLoadTag PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:Creates new record in loadtag table
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
    DEFINE INPUT  PARAMETER iTagNo                AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInventoryStockID   AS CHARACTER NO-UNDO.

    CREATE loadtag.
    ASSIGN
        loadtag.company       = ipcCompany
        loadtag.tag-no        = STRING(CAPS(ipcPrimaryID),"x(15)") + STRING(iTagNo,"99999") 
        loadtag.item-type     = NO 
        loadtag.ord-no        = IF can-find(FIRST cust WHERE cust.company EQ ipcCompany
                                                         AND cust.cust-no EQ itemfg.cust-no
                                                         AND cust.active  EQ "X")
                                THEN 0 ELSE loadtag.ord-no
        loadtag.i-no          = ipcPrimaryID
        loadtag.i-name        = po-ordl.i-name
        loadtag.qty           = ipdQuantity
        loadtag.partial       = IF AVAILABLE fg-bin THEN fg-bin.partial-count ELSE 0
        loadtag.sts           = "Printed"  
        loadtag.tag-date      = TODAY
        loadtag.tag-time      = TIME
        loadtag.misc-char[1]  = ipcStockIDAlias
        /*
        loadtag.misc-dec[1]   = w-ord.unit-wt  
        loadtag.misc-dec[2]   = w-ord.pallt-wt
        loadtag.misc-char[2]  = w-ord.lot
        loadtag.spare-char-1  = w-ord.SSCC
        */
        loadtag.po-no         = INT(ipiPONo)
        loadtag.line          = ipiPOLine
        loadtag.loc           = IF AVAILABLE fg-bin THEN fg-bin.loc ELSE ""
        loadtag.loc-bin       = IF AVAILABLE fg-bin THEN fg-bin.loc-bin ELSE ""
        loadtag.qty-case      = ipdQuantityPerSubUnit
        loadtag.case-bundle   = IF AVAILABLE itemfg AND itemfg.case-pall NE 0 THEN itemfg.case-pall  ELSE 1
        loadtag.pallet-no     = IF AVAILABLE itemfg THEN itemfg.trno ELSE ""
        loadtag.tot-cases     = (loadtag.pallet-count - loadtag.partial) / loadtag.case-bundle
        loadtag.pallet-count  = (loadtag.qty-case * loadtag.case-bundle)
        opcInventoryStockID   = loadtag.tag-no 
        .
END PROCEDURE.

PROCEDURE FGReceiptCreation PRIVATE :
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
    
    IF fg-rctd.cases EQ 0 AND fg-rctd.partial NE 0 THEN
        ASSIGN 
            fg-rctd.cases    = (IF fg-rctd.partial LT 0 THEN -1 ELSE 1)
            fg-rctd.qty-case = (IF fg-rctd.partial LT 0 THEN - fg-rctd.partial ELSE fg-rctd.partial)
            fg-rctd.partial  = 0
            .
    
    IF loadtag.po-no GT 0 THEN
        ASSIGN 
            fg-rctd.job-no = ""
            fg-rctd.job-no2 = 0
            .
END PROCEDURE.



DELETE PROCEDURE hdInventoryProcs.









    
    


