
/*------------------------------------------------------------------------
    File        : ImportFGAdj.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for FG Adjustment 	

    Author(s)   : Sewa Singh
    Created     : Mon Sept 28 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportFGAdj
    FIELD Company                  AS CHARACTER 
    FIELD Location                 AS CHARACTER 
    FIELD cTagNo                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Tag" HELP "Required - Size:20"    
    FIELD iQuantity                AS INTEGER   FORMAT ">>>>>>>>9" COLUMN-LABEL "Quantity" HELP "Required - Integer"
    FIELD iQuantityPerSubUnit      AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Unit" HELP "Optional - Integer"
    FIELD iQuantitySubUnitsPerUnit AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Sun Unit" HELP "Optional - Integer"
    FIELD cWarehouseID             AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse" HELP "Optional - Size:5"
    FIELD cLocationID              AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Bin" HELP "Optional - Size:8"        
    FIELD cReasonCode              AS CHARACTER FORMAT "X(15)" COLUMN-LABEL "Adj Reason" HELP "Required - Size:5"
        
    .
DEFINE VARIABLE giIndexOffset     AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE hdInventoryProcs  AS HANDLE  NO-UNDO.
{Inventory/ttInventory.i "NEW SHARED"}
{jc/jcgl-sh.i  NEW}

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
/*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportFGAdj"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGAdj FOR ttImportFGAdj.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iRNo                AS int64     NO-UNDO.        
    DEFINE VARIABLE lError              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cItemID             AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTransactionTypeAdj AS CHARACTER NO-UNDO INITIAL "A".
            
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-bin   FOR fg-bin.       
            
    FIND FIRST loadtag NO-LOCK 
        WHERE loadtag.company   EQ ipbf-ttImportFGAdj.Company
        AND loadtag.item-type EQ NO
        AND loadtag.tag-no    EQ ipbf-ttImportFGAdj.cTagNo
        NO-ERROR.  
         
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ ipbf-ttImportFGAdj.Company 
        AND itemfg.i-no    EQ loadtag.i-no
        NO-ERROR.     
    cItemID = itemfg.i-no .
    IF ipbf-ttImportFGAdj.cWarehouseID EQ "" AND  ipbf-ttImportFGAdj.cLocationID EQ "" THEN
        FIND FIRST bf-fg-bin NO-LOCK 
            WHERE bf-fg-bin.company EQ ipbf-ttImportFGAdj.Company
            AND bf-fg-bin.tag     EQ ipbf-ttImportFGAdj.cTagNo
            NO-ERROR.
    ELSE             
        FIND FIRST bf-fg-bin NO-LOCK  
            WHERE bf-fg-bin.company   EQ ipbf-ttImportFGAdj.Company
            AND bf-fg-bin.tag       EQ ipbf-ttImportFGAdj.cTagNo
            AND bf-fg-bin.i-no      EQ cItemID
            AND bf-fg-bin.loc       EQ ipbf-ttImportFGAdj.cWarehouseID
            AND bf-fg-bin.loc-bin   EQ ipbf-ttImportFGAdj.cLocationID
            NO-ERROR.

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
        bf-fg-rctd.company        = ipbf-ttImportFGAdj.Company
        bf-fg-rctd.rita-code      = cTransactionTypeAdj
        bf-fg-rctd.qty            = ipbf-ttImportFGAdj.iQuantity
        bf-fg-rctd.i-no           = IF AVAILABLE itemfg THEN itemfg.i-no ELSE ""
        bf-fg-rctd.i-name         = IF AVAILABLE itemfg THEN itemfg.i-name ELSE ""
        bf-fg-rctd.job-no         = bf-fg-bin.job-no
        bf-fg-rctd.job-no2        = bf-fg-bin.job-no2
        bf-fg-rctd.s-num          = 0  /* Assign sheet# to 0. Existing logic from b-fgadj.w */
        bf-fg-rctd.cases          = IF ipbf-ttImportFGAdj.iQuantityPerSubUnit EQ 0  THEN 0 
                                    ELSE TRUNC((ipbf-ttImportFGAdj.iQuantity / ipbf-ttImportFGAdj.iQuantityPerSubUnit),0)
        bf-fg-rctd.qty-case       = ipbf-ttImportFGAdj.iQuantityPerSubUnit
        bf-fg-rctd.partial        = IF ipbf-ttImportFGAdj.iQuantityPerSubUnit EQ 0 THEN 0                                                     
                                    ELSE IF ipbf-ttImportFGAdj.iQuantity GT 0 THEN ipbf-ttImportFGAdj.iQuantity MODULO ipbf-ttImportFGAdj.iQuantityPerSubUnit              
                                    ELSE -1 * ((-1 * ipbf-ttImportFGAdj.iQuantity) MODULO ipbf-ttImportFGAdj.iQuantityPerSubUnit)
        bf-fg-rctd.t-qty          = (bf-fg-rctd.cases * bf-fg-rctd.qty-case) + bf-fg-rctd.partial
        bf-fg-rctd.units-pallet   = 1
        bf-fg-rctd.cases-unit     = 1
        bf-fg-rctd.loc            = ipbf-ttImportFGAdj.cWarehouseID
        bf-fg-rctd.loc-bin        = ipbf-ttImportFGAdj.cLocationID
        bf-fg-rctd.tag            = ipbf-ttImportFGAdj.cTagNo
        bf-fg-rctd.pur-uom        = bf-fg-bin.pur-uom
        bf-fg-rctd.cost-uom       = bf-fg-bin.pur-uom
        bf-fg-rctd.ext-cost       = bf-fg-rctd.t-qty / (IF bf-fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) * bf-fg-bin.std-tot-cost 
        bf-fg-rctd.cust-no        = bf-fg-bin.cust-no
        bf-fg-rctd.reject-code[1] = ipbf-ttImportFGAdj.cReasonCode
        bf-fg-rctd.created-by     = USERID(LDBNAME(1))
        bf-fg-rctd.updated-by     = USERID(LDBNAME(1))         
        .

    FIND CURRENT bf-fg-rctd NO-LOCK NO-ERROR.         
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGAdj FOR ttImportFGAdj.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemID    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidBin  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidLoc  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMsg       AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.    
    
    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGAdj.cTagNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Tag is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF (ipbf-ttImportFGAdj.cWarehouseID EQ "" AND  ipbf-ttImportFGAdj.cLocationID NE "") OR 
            (ipbf-ttImportFGAdj.cWarehouseID NE "" AND  ipbf-ttImportFGAdj.cLocationID EQ "") THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Either warehouse or location is blank".
    END.     
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGAdj.iQuantity EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Quantity is Zero".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGAdj.cReasonCode EQ ""  THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Reason Code is blank".
    END.
        
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:      
    
        FIND FIRST rejct-cd NO-LOCK 
            WHERE rejct-cd.type EQ "ADJ"
            AND rejct-cd.code EQ ipbf-ttImportFGAdj.cReasonCode
            NO-ERROR.
         
        IF NOT AVAILABLE rejct-cd THEN 
        DO: 
            ASSIGN 
                opcNote  = "Invalid reason code for tag (" +  ipbf-ttImportFGAdj.cTagNo + ")"
                oplValid = NO
                .           
        END.       
    
        /* Validates loc & bin if they are non-blank */
        IF oplValid AND ipbf-ttImportFGAdj.cWarehouseID NE "" AND ipbf-ttImportFGAdj.cLocationID NE "" THEN 
        DO:    
            /* Validate warehouse */        
            RUN ValidateLoc IN hdInventoryProcs (
                INPUT  ipbf-ttImportFGAdj.Company,
                INPUT  ipbf-ttImportFGAdj.cWareHouseID,
                OUTPUT lValidLoc
                ).
                    
            IF NOT lValidLoc THEN 
            DO:
                ASSIGN 
                    opcNote  = "Invalid WareHouseID entered for Tag  "                     
                    oplValid = NO
                    .           
            END.  
    
            /* Validate location */
            RUN ValidateBin IN hdInventoryProcs (
                INPUT  ipbf-ttImportFGAdj.Company,
                INPUT  ipbf-ttImportFGAdj.cWareHouseID,
                INPUT  ipbf-ttImportFGAdj.cLocationID,
                OUTPUT lValidBin
                ).
        
            IF oplValid AND NOT lValidBin THEN 
            DO:
                ASSIGN 
                    opcNote  = "Invalid LocationID entered for Tag "
                    oplValid = NO 
                    .              
            END.   
        END.   
       
    END.
    
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        FIND FIRST loadtag NO-LOCK 
            WHERE loadtag.company   EQ ipbf-ttImportFGAdj.Company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ ipbf-ttImportFGAdj.cTagNo
            NO-ERROR.   
        IF NOT AVAILABLE loadtag THEN 
        DO:              
            ASSIGN 
                cMsg     = "Invalid Tag (" + ipbf-ttImportFGAdj.cTagNo + ")"
                oplValid = NO
                .
             
        END.
        
        IF AVAILABLE loadtag THEN
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ ipbf-ttImportFGAdj.Company 
            AND itemfg.i-no    EQ loadtag.i-no
            NO-ERROR.
        IF oplValid AND NOT AVAILABLE itemfg THEN 
        DO:
            ASSIGN 
                cMsg     = "Invalid item on Tag (" +  ipbf-ttImportFGAdj.cTagNo + ")"
                oplValid = NO
                .                 
        END.
        IF AVAILABLE itemfg THEN
            ASSIGN 
                cItemId = itemfg.i-no          
                . 
    
        IF oplValid THEN    
        DO:
            IF ipbf-ttImportFGAdj.cWarehouseID EQ "" AND  ipbf-ttImportFGAdj.cLocationID EQ "" THEN
                FIND FIRST bf-fg-bin NO-LOCK 
                    WHERE bf-fg-bin.company EQ ipbf-ttImportFGAdj.Company
                    AND bf-fg-bin.tag     EQ ipbf-ttImportFGAdj.cTagNo
                    NO-ERROR.
            ELSE             
                FIND FIRST bf-fg-bin NO-LOCK  
                    WHERE bf-fg-bin.company   EQ ipbf-ttImportFGAdj.Company
                    AND bf-fg-bin.tag       EQ ipbf-ttImportFGAdj.cTagNo
                    AND bf-fg-bin.i-no      EQ cItemID
                    AND bf-fg-bin.loc       EQ ipbf-ttImportFGAdj.cWarehouseID
                    AND bf-fg-bin.loc-bin   EQ ipbf-ttImportFGAdj.cLocationID
                    NO-ERROR.
            
            IF NOT AVAILABLE bf-fg-bin THEN 
            DO:
                ASSIGN
                    cMsg     = "No bin record exists for tag (" +  ipbf-ttImportFGAdj.cTagNo + ")"
                    oplValid = NO
                    .          
            END.  
            IF AVAILABLE bf-fg-bin AND ipbf-ttImportFGAdj.cWarehouseID EQ "" THEN
                ASSIGN
                    ipbf-ttImportFGAdj.cWarehouseID = bf-fg-bin.loc
                    ipbf-ttImportFGAdj.cLocationID  = bf-fg-bin.loc-bin .
            IF  AVAILABLE bf-fg-bin AND  ipbf-ttImportFGAdj.iQuantityPerSubUnit EQ 0 THEN        
                ipbf-ttImportFGAdj.iQuantityPerSubUnit = bf-fg-bin.case-count. /* Take from fg-bin if it is 0 */
    
        END.                     
        
        IF cMsg NE "" THEN 
        DO:
            ASSIGN
                oplValid = NO
                opcNote  = cMsg .                 
        END.
        ELSE
            ASSIGN 
                oplValid = YES
                opcNote  = "Add record"
                .           
    END.       
    
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
    IF VALID-HANDLE(hdInventoryProcs) THEN 
        DELETE PROCEDURE hdInventoryProcs.        
  
END PROCEDURE.




 
