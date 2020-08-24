
/*------------------------------------------------------------------------
    File        : CreateInventoryAdjustment.p
    Purpose     : Create Inventory adjustment transactions 

    Syntax      :

    Description : Create Inventory adjustment transactions

    Author(s)   : Rahul Rawat
    Created     : Fri Jul 24 01:59:25 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    {Inventory/ttInventory.i "NEW SHARED"}
    {jc/jcgl-sh.i  NEW}
    
    DEFINE INPUT        PARAMETER ipcCompany                 AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcInventoryStockID        AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantity                AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantityPerSubUnit      AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipiQuantitySubUnitsPerUnit AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ipcWarehouseID             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER ipcLocationID              AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcReasonCode              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT       PARAMETER opisequenceID              AS INT64     NO-UNDO.
    DEFINE OUTPUT       PARAMETER oplSuccess                 AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcMessage                 AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cTransactionTypeAdj   AS CHARACTER NO-UNDO INITIAL "A".
    DEFINE VARIABLE cDBNameASI            AS CHARACTER NO-UNDO INITIAL "ASI".
    DEFINE VARIABLE lPromptForClose       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReasonCodes          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dStdTotCost           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cJobNo                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cItemID               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustNo               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPurUOM               AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdInventoryProcs      AS HANDLE    NO-UNDO.
    DEFINE VARIABLE hdConversionProcs     AS HANDLE    NO-UNDO.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN system/ConversionProcs.p   PERSISTENT SET hdConversionProcs.
      
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
    /* Input validation */
    RUN pValidateInputs (
        INPUT        ipcCompany,                
        INPUT        ipcInventoryStockID,  
        INPUT-OUTPUT ipiQuantity,     
        INPUT-OUTPUT ipiQuantityPerSubUnit,     
        INPUT        ipiQuantitySubUnitsPerUnit,
        INPUT-OUTPUT ipcWarehouseID,            
        INPUT-OUTPUT ipcLocationID,             
        INPUT-OUTPUT ipcReasonCode, 
        OUTPUT       cItemID,
        OUTPUT       cItemName,
        OUTPUT       cJobNo,
        OUTPUT       iJobNo2,
        OUTPUT       cPurUOM,
        OUTPUT       dStdTotCost,
        OUTPUT       cCustNo,            
        OUTPUT       oplSuccess,
        OUTPUT       opcMessage
        ) NO-ERROR.
        
    /* Creates fg-rctd of rita-code "A" */   
    IF oplSuccess AND NOT ERROR-STATUS:ERROR THEN 
        RUN pCreateInventoryAdjustment(
            INPUT  ipcCompany,                
            INPUT  ipcInventoryStockID,      
            INPUT  ipiQuantity,               
            INPUT  ipiQuantityPerSubUnit,     
            INPUT  ipiQuantitySubUnitsPerUnit,
            INPUT  ipcWarehouseID,            
            INPUT  ipcLocationID,             
            INPUT  ipcReasonCode,
            INPUT  cItemID,
            INPUT  cItemName,
            INPUT  cJobNo,
            INPUT  iJobNo2,
            INPUT  cPurUOM,
            INPUT  dStdTotCost,
            INPUT  cCustNo,                         
            OUTPUT oplSuccess,                
            OUTPUT opcMessage                
            ). 
            
    IF ERROR-STATUS:ERROR THEN
        opcMessage = ERROR-STATUS:GET-MESSAGE(1).
        
    IF VALID-HANDLE(hdInventoryProcs) THEN 
        DELETE PROCEDURE hdInventoryProcs.
        
    IF VALID-HANDLE(hdConversionProcs) THEN 
        DELETE PROCEDURE hdConversionProcs.
        
                                   
/* **********************  Internal Procedures  *********************** */

PROCEDURE pCreateInventoryAdjustment PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany                  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcInventoryStockID         AS CHARACTER NO-UNDO.    
    DEFINE INPUT  PARAMETER ipiQuantity                 AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityPerSubUnit       AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantitySubUnitsPerUnit  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID              AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcLocationID               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcReasonCode               AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID                   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemName                 AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobNO                    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobNo2                   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcPurUOM                   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdStdTotCost               AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustNo                   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess                  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage                  AS CHARACTER NO-UNDO.
   
    DEFINE VARIABLE iRNo       AS INT64   NO-UNDO.
    DEFINE VARIABLE rifgrctd   AS ROWID   NO-UNDO.
    DEFINE VARIABLE dOutputQty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lError     AS LOGICAL NO-UNDO.
        
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-bin   FOR fg-bin.
    
    oplSuccess = TRUE.
               
    /* Retrieving last record of fg-rctd table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
    FIND LAST bf-fg-rctd NO-LOCK
         USE-INDEX fg-rctd NO-ERROR.
    IF AVAILABLE bf-fg-rctd AND bf-fg-rctd.r-no GT iRNo THEN
        iRNo = bf-fg-rctd.r-no.
    
    /* Retrieving last record of fg-rcpth table - USE-INDEX fg-rctd is required to enforce the query to fetch latest r-no*/
    /* In some cases r-no from fg-rcpth is greater than r-no from fg-rctd - so this additional find and the logic is required*/
    FIND LAST bf-fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE bf-fg-rcpth AND bf-fg-rcpth.r-no GT iRNo THEN
        iRNo = bf-fg-rcpth.r-no.
    
    CREATE bf-fg-rctd.
    ASSIGN
        bf-fg-rctd.r-no           = iRNo + 1
        bf-fg-rctd.rct-date       = TODAY
        bf-fg-rctd.trans-time     = TIME
        bf-fg-rctd.company        = ipcCompany
        bf-fg-rctd.rita-code      = cTransactionTypeAdj
        bf-fg-rctd.qty            = ipiQuantity
        bf-fg-rctd.i-no           = ipcItemID
        bf-fg-rctd.i-name         = ipcItemName
        bf-fg-rctd.job-no         = ipcJobNo
        bf-fg-rctd.job-no2        = ipiJobNo2
        bf-fg-rctd.s-num          = 0  /* Assign sheet# to 0. Existing logic from b-fgadj.w */
        bf-fg-rctd.cases          = IF ipiQuantityPerSubUnit EQ 0  THEN 0 
                                    ELSE TRUNC((ipiQuantity / ipiQuantityPerSubUnit),0)
        bf-fg-rctd.qty-case       = ipiQuantityPerSubUnit
        bf-fg-rctd.partial        = IF ipiQuantityPerSubUnit EQ 0 THEN 0                                                     
                                    ELSE IF ipiQuantity GT 0 THEN ipiQuantity MODULO ipiQuantityPerSubUnit              
                                    ELSE -1 * ((-1 * ipiQuantity) MODULO ipiQuantityPerSubUnit)
        bf-fg-rctd.t-qty          = (bf-fg-rctd.cases * bf-fg-rctd.qty-case) + bf-fg-rctd.partial
        bf-fg-rctd.units-pallet   = 1
        bf-fg-rctd.cases-unit     = 1
        bf-fg-rctd.loc            = ipcWarehouseID
        bf-fg-rctd.loc-bin        = ipcLocationID
        bf-fg-rctd.tag            = ipcInventoryStockID
        bf-fg-rctd.pur-uom        = ipcPurUOM
        bf-fg-rctd.cost-uom       = ipcPurUOM
        bf-fg-rctd.ext-cost       = bf-fg-rctd.t-qty / (IF ipcPurUOM EQ "M" THEN 1000 ELSE 1) * ipdStdTotCost 
        bf-fg-rctd.cust-no        = cCustNo
        bf-fg-rctd.reject-code[1] = ipcReasonCode
        bf-fg-rctd.created-by     = USERID(cDBNameASI)
        bf-fg-rctd.updated-by     = USERID(cDBNameASI)
        opisequenceID             = bf-fg-rctd.r-no
        .

    FIND CURRENT bf-fg-rctd NO-LOCK NO-ERROR.
        
    /* Posts fg-rctd records */
    RUN PostFinishedGoodsForUser IN hdInventoryProcs(
        INPUT        ipcCompany,
        INPUT        cTransactionTypeAdj,       /* Adjustment */
        INPUT        bf-fg-rctd.created-by,
        INPUT        lPromptForClose, /* Executes API closing orders logic */
        INPUT-OUTPUT oplSuccess,
        INPUT-OUTPUT opcMessage
        ) NO-ERROR.
 
    IF ERROR-STATUS:ERROR THEN
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Error while posting Finished Good"
            . 
END PROCEDURE.

PROCEDURE pValidateInputs PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT          PARAMETER ipcCompany                  AS CHARACTER NO-UNDO.
    DEFINE INPUT          PARAMETER ipcInventoryStockID         AS CHARACTER NO-UNDO.    
    DEFINE INPUT-OUTPUT   PARAMETER iopiQuantity                AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT   PARAMETER iopiQuantityPerSubUnit      AS INTEGER   NO-UNDO.
    DEFINE INPUT          PARAMETER ipiQuantitySubUnitsPerUnit  AS INTEGER   NO-UNDO.
    DEFINE INPUT-OUTPUT   PARAMETER iopcWarehouseID             AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT   PARAMETER iopcLocationID              AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT   PARAMETER iopcReasonCode              AS CHARACTER NO-UNDO.
    DEFINE OUTPUT         PARAMETER opcItemID                   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT         PARAMETER opcItemName                 AS CHARACTER NO-UNDO.
    DEFINE OUTPUT         PARAMETER opcJobNO                    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT         PARAMETER opiJobNo2                   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT         PARAMETER opcPurUOM                   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT         PARAMETER opdStdTotCost               AS DECIMAL   NO-UNDO.
    DEFINE OUTPUT         PARAMETER opcCustNo                   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT         PARAMETER oplSuccess                  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT         PARAMETER opcMessage                  AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidBin  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValidLoc  AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lError     AS LOGICAL NO-UNDO.
    
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    /* Company validation */
    IF ipcCompany EQ "" THEN DO:
        ASSIGN
            opcMessage = "Empty Company"
            oplSuccess = NO
            .
        RETURN.
    END.
    IF NOT CAN-FIND(FIRST company NO-LOCK
                    WHERE company.company EQ ipcCompany) THEN DO:
        ASSIGN
            opcMessage = "Invalid Company"
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

    FIND FIRST loadtag NO-LOCK 
         WHERE loadtag.company   EQ ipcCompany
           AND loadtag.item-type EQ NO
           AND loadtag.tag-no    EQ ipcInventoryStockID
         NO-ERROR.   
    IF NOT AVAILABLE loadtag THEN DO:              
        ASSIGN 
            opcMessage = "Invalid Tag (" + ipcInventoryStockID + ")"
            oplSuccess = NO
            .
        RETURN.        
    END.
           
    FIND FIRST itemfg NO-LOCK 
         WHERE itemfg.company EQ ipcCompany 
           AND itemfg.i-no    EQ loadtag.i-no
         NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
        ASSIGN 
            opcMessage = "Invalid item on Tag (" +  ipcInventoryStockID + ")"
            oplSuccess = NO
            .   
        RETURN.        
    END.
    
    ASSIGN 
        opcItemId   = itemfg.i-no
        opcItemName = itemfg.i-name
        . 
    
    IF (iopcWarehouseID EQ "" AND  iopcLocationID NE "") OR 
       (iopcWarehouseID NE "" AND  iopcLocationID EQ "") THEN DO:
        ASSIGN 
            opcMessage = "Either warehouse or location is blank"
            oplSuccess = NO
            .
        RETURN.    
            
    END.       
        
                               
    IF iopiQuantity EQ 0 THEN DO:
        ASSIGN 
            opcMessage = "Quantity can not be zero for Tag (" + ipcInventoryStockID + ")"                   
            oplSuccess = NO
            .
                        
        RETURN.
    END. 
   
    /* Validates loc & bin if they are non-blank */
    IF iopcWarehouseID NE "" AND iopcLocationID NE "" THEN DO:    
        /* Validate warehouse */        
        RUN ValidateLoc IN hdInventoryProcs (
            INPUT  ipcCompany,
            INPUT  iopcWareHouseID,
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
            INPUT  iopcWareHouseID,
            INPUT  iopcLocationID,
            OUTPUT lValidBin
            ).
        
        IF NOT lValidBin THEN DO:
            ASSIGN 
                opcMessage = "Invalid LocationID entered for Tag (" + ipcInventoryStockID + ")"
                oplSuccess = NO 
                .
                
            RETURN.
        END.
    END.   
    IF iopcReasonCode EQ "" THEN DO:
        ASSIGN
            opcMessage = "Blank reason code for Tag (" + ipcInventoryStockID + ")"
            oplSuccess = NO
            .
        RETURN.
    END.
                                             
    FIND FIRST rejct-cd NO-LOCK 
         WHERE rejct-cd.type EQ "ADJ"
           AND rejct-cd.code EQ iopcReasonCode
         NO-ERROR.
         
    IF NOT AVAILABLE rejct-cd THEN DO: 
        ASSIGN 
            opcMessage = "Invalid reason code for tag (" +  ipcInventoryStockID + ")"
            oplSuccess = NO
            .
        RETURN.    
    END.       
    
    iopcReasonCode = rejct-cd.code + " - " + rejct-cd.dscr.   
    
    IF iopcWarehouseID EQ "" AND  iopcLocationID EQ "" THEN 
        FIND FIRST bf-fg-bin NO-LOCK 
             WHERE bf-fg-bin.company EQ ipcCompany
               AND bf-fg-bin.tag     EQ ipcInventoryStockID
             NO-ERROR.
    ELSE             
        FIND FIRST bf-fg-bin NO-LOCK  
             WHERE bf-fg-bin.company   EQ ipcCompany
               AND bf-fg-bin.tag       EQ ipcInventoryStockID
               AND bf-fg-bin.i-no      EQ opcItemID
               AND bf-fg-bin.loc       EQ iopcWarehouseID
               AND bf-fg-bin.loc-bin   EQ iopcLocationID
            NO-ERROR.
            
    IF NOT AVAILABLE bf-fg-bin THEN DO:
        ASSIGN
            opcMessage = "No bin record exists for tag (" +  ipcInventoryStockID + ")"
            oplSuccess = NO
            .
        RETURN.       
    END.    
    ELSE DO:
        ASSIGN 
            opcJobNo        = bf-fg-bin.job-no
            opiJobNo2       = bf-fg-bin.job-no2 
            opcPurUOM       = bf-fg-bin.pur-uom
            opcCustNo       = bf-fg-bin.cust-no
            opdStdTotCost   = bf-fg-bin.std-tot-cost
            iopcWarehouseID = bf-fg-bin.loc
            iopcLocationID  = bf-fg-bin.loc-bin
            .   
                             
        IF iopiQuantityPerSubUnit EQ 0 THEN
            iopiQuantityPerSubUnit = bf-fg-bin.case-count. /* Take from fg-bin if it is 0 */
                            
    END.                                                  
    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.
