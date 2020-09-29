
/*------------------------------------------------------------------------
    File        : ImportFGCount.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for FG Adjustment 	

    Author(s)   : Sewa Singh
    Created     : Tue Sept 29 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportFGCount
    FIELD Company             AS CHARACTER 
    FIELD Location            AS CHARACTER
    FIELD cWarehouseID        AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "Warehouse" HELP "Requird - Size:5"
    FIELD cLocationID         AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Bin" HELP "Requird - Size:8"        
    FIELD cCustID             AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Customer" HELP "Optional - Size:8"
    FIELD iPOID               AS INTEGER   FORMAT ">>>>>>>>9" COLUMN-LABEL "PO#" HELP "Optional - integer"        
    FIELD iPOLine             AS INTEGER   FORMAT ">>>9" COLUMN-LABEL "PO Line" HELP "Optional - integer"     
    FIELD cJobID              AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Job #" HELP "Optional - Size:8"
    FIELD iJobID2             AS INTEGER   FORMAT ">>>9" COLUMN-LABEL "Job 2" HELP "Optional - Integer"        
    FIELD cItemID             AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item" HELP "Requird - Size:15" 
    FIELD cTagNo              AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Tag#" HELP "Required - Size:20"    
    FIELD iQuantity           AS INTEGER   FORMAT ">>>>>>>>9" COLUMN-LABEL "Quantity" HELP "Required - Integer"
    FIELD iQuantityPerSubUnit AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Unit" HELP "Optional - Integer"
    FIELD iQuantityPartial    AS INTEGER   FORMAT ">>>>>>9" COLUMN-LABEL "Partial" HELP "Optional - Integer"     
    FIELD lPost               AS LOGICAL   FORMAT "Yes/No" COLUMN-LABEL "Auto Post" HELP "optional - Yes or No (Blank - No)"
    FIELD lZeroOutCount       AS LOGICAL   FORMAT "Yes/No" COLUMN-LABEL "Zero Count" HELP "optional - Yes or No (Blank - No)"
        
    .
DEFINE VARIABLE giIndexOffset    AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.
{Inventory/ttInventory.i "NEW SHARED"}
{sys/inc/VAR.i NEW SHARED}
{oe/invwork.i NEW}
{fg/fullset.i NEW}

DEFINE TEMP-TABLE w-fg-rctd NO-UNDO
    LIKE fg-rctd.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
/*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportFGCount"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGCount FOR ttImportFGCount.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
            
    DEFINE VARIABLE cTransactionTypeCount AS CHARACTER NO-UNDO INITIAL "C".    
    DEFINE VARIABLE cJobID                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE isequenceID           AS int64     NO-UNDO.         
    DEFINE VARIABLE iRNo                  AS int64     NO-UNDO.
    DEFINE VARIABLE rifgrctd              AS ROWID     NO-UNDO.
    DEFINE VARIABLE lSuccess              AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage              AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-fg-bin   FOR fg-bin.
        
    cocode = ipbf-ttImportFGCount.company .
    
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
        bf-fg-rctd.r-no         = iRNo + 1
        bf-fg-rctd.rct-date     = TODAY
        bf-fg-rctd.trans-time   = TIME
        bf-fg-rctd.company      = ipbf-ttImportFGCount.Company
        bf-fg-rctd.rita-code    = cTransactionTypeCount
        bf-fg-rctd.qty          = ipbf-ttImportFGCount.iQuantity
        bf-fg-rctd.i-no         = ipbf-ttImportFGCount.cItemID
        bf-fg-rctd.job-no       = ipbf-ttImportFGCount.cJobID
        bf-fg-rctd.job-no2      = ipbf-ttImportFGCount.iJobID2
        bf-fg-rctd.s-num        = 0                      /* Assign sheet# to 0. Existing logic from b-phys.w */
        bf-fg-rctd.cases        = ipbf-ttImportFGCount.iQuantity
        bf-fg-rctd.qty-case     = ipbf-ttImportFGCount.iQuantityPerSubUnit
        bf-fg-rctd.partial      = ipbf-ttImportFGCount.iQuantityPartial
        bf-fg-rctd.t-qty        = (ipbf-ttImportFGCount.iQuantity * ipbf-ttImportFGCount.iQuantityPerSubUnit) + ipbf-ttImportFGCount.iQuantityPartial
        bf-fg-rctd.units-pallet = 1
        bf-fg-rctd.cases-unit   = 1
        bf-fg-rctd.loc          = ipbf-ttImportFGCount.cWarehouseID
        bf-fg-rctd.loc-bin      = ipbf-ttImportFGCount.cLocationID
        bf-fg-rctd.tag          = ipbf-ttImportFGCount.cTagNo
        bf-fg-rctd.cust-no      = ipbf-ttImportFGCount.cCustID
        bf-fg-rctd.created-by   = USERID(LDBNAME(1))
        bf-fg-rctd.updated-by   = USERID(LDBNAME(1))
        bf-fg-rctd.po-no        = TRIM(STRING(ipbf-ttImportFGCount.iPOID,">>>>>>>>>>"))
        bf-fg-rctd.po-line      = ipbf-ttImportFGCount.iPOLine
        isequenceID             = bf-fg-rctd.r-no
        .
        
    rifgrctd = ROWID(bf-fg-rctd).

    FIND FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company = ipbf-ttImportFGCount.Company
        AND bf-itemfg.i-no    = ipbf-ttImportFGCount.cItemID
        NO-ERROR.
    IF AVAILABLE bf-itemfg THEN
        ASSIGN
            bf-fg-rctd.i-name   = bf-itemfg.i-name
            bf-fg-rctd.pur-uom  = bf-itemfg.prod-uom
            bf-fg-rctd.cost-uom = bf-itemfg.prod-uom
            .

    FIND FIRST bf-fg-bin
        WHERE bf-fg-bin.company EQ bf-fg-rctd.company
        AND bf-fg-bin.i-no    EQ bf-fg-rctd.i-no
        AND bf-fg-bin.job-no  EQ bf-fg-rctd.job-no
        AND bf-fg-bin.job-no2 EQ bf-fg-rctd.job-no2
        AND bf-fg-bin.loc     EQ bf-fg-rctd.loc
        AND bf-fg-bin.loc-bin EQ bf-fg-rctd.loc-bin
        AND bf-fg-bin.tag     EQ bf-fg-rctd.tag
        AND bf-fg-bin.cust-no EQ bf-fg-rctd.cust-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE bf-fg-bin THEN
        ASSIGN
            bf-fg-rctd.ext-cost = bf-fg-rctd.t-qty / (IF bf-fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) * bf-fg-bin.std-tot-cost
            bf-fg-rctd.cost     = bf-fg-rctd.ext-cost / bf-fg-rctd.t-qty
            bf-fg-rctd.cost-uom = bf-fg-bin.pur-uom.
    ELSE
        RUN fg/fgrctdcst.p (
            INPUT  ROWID(bf-fg-rctd),
            INPUT  bf-fg-rctd.t-qty,
            OUTPUT bf-fg-rctd.cost,
            OUTPUT bf-fg-rctd.cost-uom,
            OUTPUT bf-fg-rctd.ext-cost
            ).

    /* Pur-uom blank was causing miscalculation during cost recalc ( Existing comment from b-phys.w) */
    IF bf-fg-rctd.pur-uom = "" THEN
        bf-fg-rctd.pur-uom = bf-fg-rctd.cost-uom.

    IF bf-fg-rctd.ext-cost EQ ? THEN
        bf-fg-rctd.ext-cost = 0.

    IF bf-fg-rctd.cost EQ ? THEN
        bf-fg-rctd.cost = 0.

    RELEASE bf-fg-rctd.
    RELEASE bf-fg-rcpth.
    RELEASE bf-itemfg.
    RELEASE bf-fg-bin.
        
    IF ipbf-ttImportFGCount.lZeroOutCount THEN
        RUN pZeroOutCount (
            INPUT  rifgrctd,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ) NO-ERROR.         
        
                            
    IF ipbf-ttImportFGCount.lPost THEN
        RUN pPostCount (
            INPUT  ipbf-ttImportFGCount.Company,
            INPUT  iSequenceID,
            OUTPUT lSuccess,
            OUTPUT cMessage
            ) NO-ERROR.      
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGCount FOR ttImportFGCount.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lActive     AS LOGICAL   NO-UNDO.    
    DEFINE VARIABLE cItemTypeFG AS CHARACTER NO-UNDO INITIAL "FG".
    DEFINE VARIABLE lValidLoc   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidBin   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidTag   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMsg        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cValidNote  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hdJobProcs       AS HANDLE NO-UNDO.
    
    RUN inventory/InventoryProcs.p PERSISTENT SET hdInventoryProcs.    
    RUN jc/JobProcs.p PERSISTENT SET hdJobProcs.
    
    /* Formats Job number */
    ipbf-ttImportFGCount.cJobID = DYNAMIC-FUNCTION (
               "fAddSpacesToString" IN hdJobProcs , ipbf-ttImportFGCount.cJobID , 6 , TRUE
               ).
    
    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGCount.cWarehouseID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "WareHouseID is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGCount.cLocationID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Bin is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGCount.cItemID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "FG Item is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGCount.cTagNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Tag is Blank".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGCount.iQuantity LT 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Quantity cannot be less than zero".
    END.
    
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGCount.iQuantityPerSubUnit LT 0  THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "QuantityPerSubUnit cannot be less than zero".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGCount.iQuantityPartial LT 0  THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "QuantityPartial cannot be less than zero".
    END.
        
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:     
        /* Validates loc & bin if they are non-blank */
        IF oplValid AND ipbf-ttImportFGCount.cWarehouseID NE "" AND ipbf-ttImportFGCount.cLocationID NE "" THEN 
        DO:    
            /* Validate warehouse */        
            RUN ValidateLoc IN hdInventoryProcs (
                INPUT  ipbf-ttImportFGCount.Company,
                INPUT  ipbf-ttImportFGCount.cWareHouseID,
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
                INPUT  ipbf-ttImportFGCount.Company,
                INPUT  ipbf-ttImportFGCount.cWareHouseID,
                INPUT  ipbf-ttImportFGCount.cLocationID,
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
        RUN ValidateLoadTag IN hdInventoryProcs (
            INPUT  ipbf-ttImportFGCount.Company,
            INPUT  "FG",
            INPUT  ipbf-ttImportFGCount.cTagNo,
            OUTPUT lValidTag,
            OUTPUT cMessage
            ) NO-ERROR.
                                       
        IF NOT lValidTag THEN 
        DO:              
            ASSIGN 
                cMsg     = "Invalid Tag (" + ipbf-ttImportFGCount.cTagNo + ")"
                oplValid = NO
                .              
        END.
        
        IF oplValid AND ipbf-ttImportFGCount.cJobID NE "" THEN 
        DO:
            RUN pValidateJob (
                INPUT  ipbf-ttImportFGCount.Company,
                INPUT  ipbf-ttImportFGCount.cJobID,
                INPUT  ipbf-ttImportFGCount.iJobID2,
                INPUT  ipbf-ttImportFGCount.cItemID,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ) NO-ERROR.
          
            IF NOT lSuccess THEN 
            DO:              
                ASSIGN 
                    cMsg     = cMessage 
                    oplValid = NO
                    .              
            END.
        END.
        
        IF oplValid AND  ipbf-ttImportFGCount.iPOID NE 0 THEN 
        DO:
            RUN pValidatePOAndPOLine (
                INPUT  ipbf-ttImportFGCount.Company,
                INPUT  ipbf-ttImportFGCount.iPOID,
                INPUT  ipbf-ttImportFGCount.iPOLine,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ) NO-ERROR.
                       
            IF NOT lSuccess THEN 
            DO:              
                ASSIGN 
                    cMsg     = cMessage  
                    oplValid = NO
                    .              
            END.
        END.
        
        IF oplValid AND ipbf-ttImportFGCount.cCustID NE "" THEN 
        DO:
            RUN ValidateCust IN hdInventoryProcs (
                INPUT  ipbf-ttImportFGCount.Company,
                INPUT  ipbf-ttImportFGCount.cCustID,
                OUTPUT lSuccess,
                OUTPUT cMessage
                ) NO-ERROR.

            IF NOT lSuccess THEN 
            DO:              
                ASSIGN 
                    cMsg     = cMessage 
                    oplValid = NO
                    .              
            END.
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
    
    IF oplValid THEN 
    DO:
        IF oplValid AND ipbf-ttImportFGCount.cItemID NE "" THEN 
            RUN pIsValidFGITemID (ipbf-ttImportFGCount.cItemID, NO, ipbf-ttImportFGCount.Company, OUTPUT oplValid, OUTPUT cValidNote).
            
    END.         
    
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
    IF VALID-HANDLE(hdInventoryProcs) THEN 
        DELETE PROCEDURE hdInventoryProcs.  
    IF VALID-HANDLE(hdJobProcs) THEN 
        DELETE PROCEDURE hdJobProcs.    
  
END PROCEDURE.


PROCEDURE pValidateJob PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate job
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobID   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobID2  AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job-hdr FOR job-hdr.

    FIND FIRST bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        AND bf-job-hdr.job-no  EQ ipcJobID
        AND bf-job-hdr.job-no2 EQ ipiJobID2
        AND bf-job-hdr.i-no    EQ ipcItemID
        NO-ERROR.
    IF NOT AVAILABLE bf-job-hdr THEN 
    DO:
        FIND FIRST bf-job-hdr NO-LOCK
            WHERE bf-job-hdr.company EQ ipcCompany
            AND bf-job-hdr.job-no  EQ ipcJobID
            NO-ERROR.
        IF AVAILABLE bf-job-hdr THEN
            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "jc/jc-calc.p"
                AND reftable.company  EQ ipcCompany
                AND reftable.loc      EQ ""
                AND reftable.code     EQ STRING(bf-job-hdr.job,"999999999")
                AND reftable.code2    EQ ipcItemID
                NO-ERROR.
        IF NOT AVAILABLE reftable THEN 
        DO:
            ASSIGN
                oplSuccess = FALSE
                opcMessage = "Item# - " + ipcItemID + " is not on Job# - "
                               + ipcJobID + "-" + STRING(ipiJobID2)
                .
            RETURN.
        END.
    END.

    IF NOT AVAILABLE bf-job-hdr THEN 
    DO:
        ASSIGN
            oplSuccess = FALSE
            opcMessage = "Invalid Job#"
            .
        RETURN.
    END.

    RELEASE bf-job-hdr.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.

PROCEDURE pValidatePOAndPOLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to validate PO and PO Line
     Notes:
    ------------------------------------------------------------------------------*/     
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOID    AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPOLine  AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    RUN ValidatePO IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  ipiPOID,
        OUTPUT oplSuccess
        ) NO-ERROR.
    IF NOT oplSuccess THEN do:
     opcMessage = "Invalid PO#".
        RETURN.
    END.

    RUN ValidatePOLine IN hdInventoryProcs (
        INPUT  ipcCompany,
        INPUT  ipiPOID,
        INPUT  ipiPOLine,
        OUTPUT oplSuccess
        ) NO-ERROR.
    IF NOT oplSuccess THEN do:
     opcMessage = "Invalid PO Line".
        RETURN.
    END.    

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.


PROCEDURE pPostCount PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Procedure to Post Count transaction
     Notes:
    ------------------------------------------------------------------------------*/   
    DEFINE INPUT  PARAMETER ipcCompany    AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSequenceID AS int64     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess    AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lCreatGlAccount AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iRNo            AS int64     NO-UNDO.
    DEFINE VARIABLE iAdjQty         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iPeriod         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTrnNum         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.

    /* The below variable are required to be defined with the below names,
       as these variables are being used in existing posting program fg-cpost.i */
    DEFINE VARIABLE begin_userid    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE end_userid      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-q-adj-ytd     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty-onh       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-post-date     AS DATE      NO-UNDO INITIAL TODAY.

    /* The below buffers are required to be defined with below names,
       as theser buffers are being used in existing posting program fg-cpost.i */
    DEFINE BUFFER b-fg-rctd   FOR fg-rctd.
    DEFINE BUFFER b-itemfg    FOR itemfg.
    DEFINE BUFFER b-fg-bin    FOR fg-bin.
    DEFINE BUFFER b2-fg-bin   FOR fg-bin.

    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-itemfg   FOR itemfg.
    DEFINE BUFFER bf-period   FOR period.

    EMPTY TEMP-TABLE w-fg-rctd.

    /* The below variables need to be assigned with the current userid,
       as these are validated while posting in fg-cpost.i */
    ASSIGN
        begin_userid = USERID(LDBNAME(1))
        end_userid   = USERID(LDBNAME(1))
        .

    FIND FIRST fg-rctd NO-LOCK
        WHERE fg-rctd.r-no EQ ipiSequenceID
        NO-ERROR.
       
    FIND FIRST bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ ipcCompany
        AND bf-itemfg.i-no    EQ fg-rctd.i-no
        AND bf-itemfg.isaset
        AND bf-itemfg.alloc
        NO-ERROR.
    IF AVAILABLE bf-itemfg THEN 
    DO:
        /* Code to create temp-table tt-fg-set records for items of set type */
        RUN fg/fullset.p (
            INPUT ROWID(bf-itemfg)
            ) NO-ERROR.
           
            
        /* For each of the tt-fg-set records, create temp-table w-fg-rctd record,
           which in turn will create fg-rctd records */
        FOR EACH tt-fg-set,
            FIRST b-itemfg
            WHERE b-itemfg.company EQ ipcCompany
            AND b-itemfg.i-no    EQ tt-fg-set.part-no
            NO-LOCK:

            iRNo = 1.

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

            CREATE w-fg-rctd.
            BUFFER-COPY fg-rctd TO w-fg-rctd
                ASSIGN
                w-fg-rctd.i-no   = b-itemfg.i-no
                w-fg-rctd.i-name = b-itemfg.i-name
                w-fg-rctd.r-no   = iRNo
                .

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ ipcCompany
                AND fg-bin.i-no    EQ fg-rctd.i-no
                AND fg-bin.loc     EQ fg-rctd.loc
                AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                AND fg-bin.tag     EQ fg-rctd.tag
                AND fg-bin.job-no  EQ fg-rctd.job-no
                AND fg-bin.job-no2 EQ fg-rctd.job-no2
                AND fg-bin.cust-no EQ fg-rctd.cust-no
                USE-INDEX co-ino NO-ERROR.
            iAdjQty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) * tt-fg-set.part-qty-dec.

            FIND FIRST fg-bin
                WHERE fg-bin.company EQ ipcCompany
                AND fg-bin.i-no    EQ b-itemfg.i-no
                AND fg-bin.loc     EQ fg-rctd.loc
                AND fg-bin.loc-bin EQ fg-rctd.loc-bin
                AND fg-bin.tag     EQ fg-rctd.tag
                AND fg-bin.job-no  EQ fg-rctd.job-no
                AND fg-bin.job-no2 EQ fg-rctd.job-no2
                AND fg-bin.cust-no EQ fg-rctd.cust-no
                USE-INDEX co-ino NO-ERROR.
            iAdjQty = (IF AVAILABLE fg-bin THEN fg-bin.qty ELSE 0) - iAdjQty.

            IF iAdjQty LT 0 THEN
                iAdjQty = 0.

            w-fg-rctd.t-qty = (fg-rctd.t-qty * tt-fg-set.part-qty-dec) + iAdjQty.
        END.
    END.
        
    /* Call to create and post fg-rctd records created for set type items */
    {fg/fg-cpost.i w-}

    /* Call to post fg-rctd record created */
    {fg/fg-cpost.i}

    /* Code to fetch sys-ctrl configuration "AdjustGL" log field */
    RUN sys/ref/nk1look.p (
        INPUT  ipcCompany,     /* Company       */
        INPUT  "AdjustGL", /* Sys-Ctrl Name */
        INPUT  "L",        /* Logical       */
        INPUT  NO,         /* Check by cust */
        INPUT  YES,        /* Use Cust      */
        INPUT  "",         /* Customer      */
        INPUT  "",         /* Ship-to       */
        OUTPUT cRtnChar,
        OUTPUT lRecFound
        ).
    IF lRecFound THEN
        lCreatGlAccount = LOGICAL(cRtnChar) NO-ERROR.

    /* Code to fetch the period */
    FIND FIRST bf-period NO-LOCK
        WHERE bf-period.company EQ ipcCompany
        AND bf-period.pst     LE TODAY
        AND bf-period.pend    GE TODAY
        NO-ERROR.
      
    iPeriod = bf-period.pnum.

    /* Creates GL records */
    IF lCreatGlAccount AND iPeriod GT 0 THEN 
    DO:
        REPEAT:
            FIND FIRST gl-ctrl EXCLUSIVE-LOCK
                WHERE gl-ctrl.company EQ ipcCompany
                NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN 
            DO:
                ASSIGN
                    iTrnNum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = iTrnNum
                    .
                FIND CURRENT gl-ctrl NO-LOCK NO-ERROR.
                LEAVE.
            END.
        END.

        FOR EACH work-job
            BREAK BY work-job.actnum:
            CREATE gltrans.
            ASSIGN
                gltrans.company = ipcCompany
                gltrans.actnum  = work-job.actnum
                gltrans.jrnl    = "OEINV"
                gltrans.tr-date = TODAY
                gltrans.period  = iPeriod
                gltrans.trnum   = iTrnNum
                .

            IF work-job.fg THEN
                ASSIGN
                    gltrans.tr-amt  = - work-job.amt
                    gltrans.tr-dscr = "ORDER ENTRY INVOICE FG"
                    .
            ELSE
                ASSIGN
                    gltrans.tr-amt  = work-job.amt
                    gltrans.tr-dscr = "ORDER ENTRY INVOICE COGS"
                    .
        END.
    END.

    RELEASE bf-fg-rctd.
    RELEASE bf-fg-rcpth.
    RELEASE bf-itemfg.
    RELEASE bf-period.

    RELEASE b-fg-rctd.
    RELEASE b-itemfg.
    RELEASE b-fg-bin.
    RELEASE b2-fg-bin.

    ASSIGN
        oplSuccess = TRUE
        opcMessage = "Success"
        .
END PROCEDURE.    
 

PROCEDURE pZeroOutCount PRIVATE :
    /*------------------------------------------------------------------------------
      Purpose:   Count becomes zero if current loc/bin/custno is different 
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprifgrctd AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
     
    DEFINE VARIABLE iRNo AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    
    oplSuccess = YES.
    
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

    FIND FIRST fg-rctd NO-LOCK
        WHERE ROWID(fg-rctd) EQ iprifgrctd
        NO-ERROR.
    
    FOR EACH bf-fg-rctd 
        WHERE bf-fg-rctd.company  EQ fg-rctd.company           
        AND bf-fg-rctd.i-no     EQ fg-rctd.i-no
        AND bf-fg-rctd.tag      EQ fg-rctd.tag
        AND ROWID(bf-fg-rctd)   NE ROWID(fg-rctd): 
        DELETE bf-fg-rctd.
    END.

    FOR EACH fg-bin NO-LOCK
        WHERE fg-bin.company EQ fg-rctd.company  
        AND fg-bin.i-no    EQ fg-rctd.i-no
        AND fg-bin.job-no  EQ fg-rctd.job-no
        AND fg-bin.job-no2 EQ fg-rctd.job-no2 
        AND fg-bin.tag     EQ fg-rctd.tag
        AND fg-bin.qty     NE 0:

        IF fg-bin.loc     NE fg-rctd.loc     OR
            fg-bin.loc-bin NE fg-rctd.loc-bin OR
            fg-bin.cust-no NE fg-rctd.cust-no THEN 
        DO:
            CREATE bf-fg-rctd.
            BUFFER-COPY fg-rctd EXCEPT fg-rctd.r-no fg-rctd.loc fg-rctd.loc-bin fg-rctd.cust-no TO bf-fg-rctd.
            ASSIGN 
                bf-fg-rctd.r-no       = iRNo
                bf-fg-rctd.loc        = fg-bin.loc
                bf-fg-rctd.loc-bin    = fg-bin.loc-bin
                bf-fg-rctd.cust-no    = fg-bin.cust-no
                bf-fg-rctd.cases      = 0
                bf-fg-rctd.qty-case   = 0
                bf-fg-rctd.cases-unit = 0
                bf-fg-rctd.partial    = 0
                bf-fg-rctd.t-qty      = 0
                iRNo                  = iRNo + 1
                .
        END.
    END.  /* for each fg-bin*/
    
    RELEASE bf-fg-rctd.
    RELEASE bf-fg-rcpth.
END PROCEDURE.
