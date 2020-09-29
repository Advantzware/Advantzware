
/*------------------------------------------------------------------------
    File        : ImportFGRctd.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for FG Receipt 	

    Author(s)   : Sewa Singh
    Created     : Thu Sept 24 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportFGRctd
    FIELD Company                  AS CHARACTER 
    FIELD Location                 AS CHARACTER        
    FIELD cTagNo                   AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Tag" HELP "Required - Size:20"
    FIELD dQuantity                AS DECIMAL   FORMAT ">>>,>>>,>>9.99" COLUMN-LABEL "Qty" HELP "Optional - Decimal" 
    FIELD cQuantityUOM             AS CHARACTER FORMAT "x(3)" COLUMN-LABEL "Qty Uom" HELP "Optional - Size:3"
    FIELD iPONo                    AS INTEGER   FORMAT ">>>>>>" COLUMN-LABEL "PO#" HELP "Optional - Integer"
    FIELD iPOLine                  AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Po Line#" HELP "Optional - Integer"
    FIELD cJobID                   AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Job#" HELP "Optional - Size:6"
    FIELD iJobID2                  AS INTEGER   FORMAT ">>9" COLUMN-LABEL "Job No2" HELP "Optional - Size:Integer"
    FIELD iQuantityPerSubUnit      AS INTEGER   FORMAT ">>>>>>>9" COLUMN-LABEL "Unit" HELP "Optional - Integer"
    FIELD iQuantitySubUnitsPerUnit AS INTEGER   FORMAT ">>>>>>>9" COLUMN-LABEL "Unit Count" HELP "Optional - Integer"     
    FIELD cWarehouseID             AS CHARACTER FORMAT "X(5)" COLUMN-LABEL "Warehouse" HELP "Required - Size:5"
    FIELD cLocationID              AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "Bin" HELP "Optional - Size:8"
    FIELD lSSPostFG                AS LOGICAL   FORMAT "Yes/No" COLUMN-LABEL "Auto Post" HELP "Optional - Yes Or No (Blank - No)"    
    
    .      
    
DEFINE VARIABLE giIndexOffset      AS INTEGER   NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE hdInventoryProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE hdInventoryReceipt AS HANDLE    NO-UNDO.
DEFINE VARIABLE dFinalQuantity     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE iRNo               AS INTEGER   NO-UNDO.
DEFINE VARIABLE cPrimaryID         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRegTag            AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cCostUOM           AS CHARACTER NO-UNDO.
DEFINE VARIABLE dStdCost           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dExtCost           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dFrtCost           AS DECIMAL   NO-UNDO.

{sys/inc/var.i "new shared"}
{inventory/ttinventory.i "NEW SHARED"}.
{jc/jcgl-sh.i  NEW}

/* ********************  Preprocessor Definitions  ******************** */


/* ************************  Function Prototypes ********************** */
FUNCTION fGetValueInteger RETURNS INTEGER PRIVATE
    ( ipcCompany AS CHARACTER,
    ipcControlValue AS CHARACTER) FORWARD.
    
FUNCTION fGetValueLogical RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER,
    ipcControlValue AS CHARACTER) FORWARD.    


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
/*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportFGRctd"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGRctd FOR ttImportFGRctd.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.       
    
    DEFINE VARIABLE cReceipt        AS CHARACTER NO-UNDO INITIAL "R".           
    DEFINE VARIABLE lPromptForClose AS LOGICAL   NO-UNDO.    
    DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    RUN api\inbound\InventoryReceiptProcs.p PERSISTENT SET hdInventoryReceipt.
    
    ASSIGN
        dStdCost = 0
        cCostUOM = "" 
        dExtCost = 0
        dFrtCost = 0
        lSuccess = YES.
    
    ipbf-ttImportFGRctd.cQuantityUOM = IF ipbf-ttImportFGRctd.cQuantityUOM NE "EA" AND ipbf-ttImportFGRctd.cQuantityUOM NE "M" THEN
        "EA" /* Each */  ELSE
        ipbf-ttImportFGRctd.cQuantityUOM .
    cocode         = ipbf-ttImportFGRctd.Company .  
    
    /* Validates PO Number and Quantity when tag is not registered */
    FIND FIRST loadtag NO-LOCK
        WHERE loadtag.company   EQ ipbf-ttImportFGRctd.company
        AND loadtag.item-type EQ NO
        AND loadtag.tag-no    EQ ipbf-ttImportFGRctd.cTagNo
        NO-ERROR.
    IF NOT AVAILABLE loadtag THEN 
    DO:
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company   EQ ipbf-ttImportFGRctd.company
            AND loadtag.item-type EQ YES
            AND loadtag.tag-no    EQ ipbf-ttImportFGRctd.cTagNo
            NO-ERROR.
    END.

    IF NOT AVAILABLE loadtag THEN 
    DO:
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ ipbf-ttImportFGRctd.company
            AND fg-bin.tag     EQ ipbf-ttImportFGRctd.cTagNo
            NO-ERROR.
    END.
    
    ASSIGN
        lRegTag    = AVAILABLE loadtag OR AVAILABLE fg-bin
        cPrimaryID = IF AVAILABLE loadtag THEN 
                                   loadtag.i-no
                                 ELSE IF AVAILABLE fg-bin THEN
                                   fg-bin.i-no
                                 ELSE
                                   "" .                                    
 
    IF ipbf-ttImportFGRctd.iPONo NE 0 THEN 
    DO:                                     
        IF lRegTag THEN
            FIND FIRST po-ordl NO-LOCK
                WHERE po-ordl.company EQ ipbf-ttImportFGRctd.company
                AND po-ordl.po-no   EQ ipbf-ttImportFGRctd.iPONo
                AND po-ordl.line    EQ ipbf-ttImportFGRctd.iPOLine
                AND po-ordl.i-no    EQ cPrimaryID 
                NO-ERROR.
        ELSE
            FIND FIRST po-ordl NO-LOCK
                WHERE po-ordl.company EQ ipbf-ttImportFGRctd.company
                AND po-ordl.po-no   EQ ipbf-ttImportFGRctd.iPONo
                AND po-ordl.line    EQ ipbf-ttImportFGRctd.iPOLine
                NO-ERROR.              
             
        IF NOT lRegTag THEN 
            ASSIGN    
                cPrimaryID = po-ordl.i-no               
                .   
            
        RUN InventoryReceipt_GetCostsFromPO IN hdInventoryReceipt (
            INPUT  ipbf-ttImportFGRctd.company, 
            INPUT  ipbf-ttImportFGRctd.iPONo,
            INPUT  ipbf-ttImportFGRctd.iPOLine,
            INPUT  cPrimaryID,
            INPUT  ipbf-ttImportFGRctd.dQuantity,
            OUTPUT dStdCost, 
            OUTPUT cCostUOM, 
            OUTPUT dExtCost, 
            OUTPUT dFrtCost
            ) NO-ERROR.          
    END.
    
    
    IF ipbf-ttImportFGRctd.cJobID NE "" THEN 
    DO:
        FIND FIRST job-hdr NO-LOCK 
            WHERE job-hdr.company EQ ipbf-ttImportFGRctd.Company
            AND job-hdr.job-no  EQ ipbf-ttImportFGRctd.cJobID
            NO-ERROR.    
        IF NOT AVAILABLE job-hdr THEN 
        DO:
            /* Assigns 0 to JobID & JobID2 if JobID is invalid for registered tags */
            IF lRegTag THEN
                ASSIGN
                    ipbf-ttImportFGRctd.cJobID  = "0"                     
                    ipbf-ttImportFGRctd.iJobID2 = 0
                    .           
        END.
        /* Validate JobID2 if JobID is valid */
        IF AVAILABLE job-hdr THEN 
        DO:
            FOR EACH  job-hdr
                WHERE job-hdr.company EQ ipbf-ttImportFGRctd.Company
                AND job-hdr.job-no  EQ ipbf-ttImportFGRctd.cJobID
                AND job-hdr.job-no2 EQ INT(ipbf-ttImportFGRctd.iJobID2)
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
                    WHERE job.company EQ ipbf-ttImportFGRctd.Company
                    AND job.job-no  EQ ipbf-ttImportFGRctd.cJobID
                    AND job.job-no2 EQ INT(ipbf-ttImportFGRctd.iJobID2)
                    NO-LOCK,
                    FIRST job-hdr
                    WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    NO-LOCK:
                    LEAVE.
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
                cCostUOM   = IF dStdCost GT 0 THEN "M" ELSE ""              
                .
    END. 


    /* Gets loc & bin from item if they are blank */
    IF ipbf-ttImportFGRctd.cWarehouseID EQ "" OR ipbf-ttImportFGRctd.cLocationID EQ "" THEN 
    DO:
   
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipbf-ttImportFGRctd.Company
            AND itemfg.i-no    EQ cPrimaryID
            NO-ERROR.
        IF AVAILABLE itemfg THEN    
            ASSIGN
                ipbf-ttImportFGRctd.cWarehouseID             = itemfg.def-loc
                ipbf-ttImportFGRctd.cLocationID              = itemfg.def-loc-bin
                ipbf-ttImportFGRctd.iQuantitySubUnitsPerUnit = IF ipbf-ttImportFGRctd.iQuantitySubUnitsPerUnit EQ 0 THEN
                                                itemfg.case-count
                                             ELSE
                                                ipbf-ttImportFGRctd.iQuantitySubUnitsPerUnit 
                .               
    END.

             
    /* Creates receipts  */ 
    RUN pFGRecordCreation (
        INPUT        ipbf-ttImportFGRctd.Company,
        INPUT        ipbf-ttImportFGRctd.cTagNo,
        INPUT        ipbf-ttImportFGRctd.dQuantity,
        INPUT        ipbf-ttImportFGRctd.cQuantityUOM,
        INPUT-OUTPUT ipbf-ttImportFGRctd.iPONo,
        INPUT        ipbf-ttImportFGRctd.iPOLine,
        INPUT-OUTPUT ipbf-ttImportFGRctd.cJobID,
        INPUT        STRING(ipbf-ttImportFGRctd.iJobID2),
        INPUT        ipbf-ttImportFGRctd.iQuantityPerSubUnit,
        INPUT        ipbf-ttImportFGRctd.iQuantitySubUnitsPerUnit,
        INPUT        ipbf-ttImportFGRctd.cWarehouseID,
        INPUT        ipbf-ttImportFGRctd.cLocationID
        ) NO-ERROR.
        
        
    /* Checks sys-ctrl */
    {sys/inc/sspostfg.i}

    dFinalQuantity = IF ipbf-ttImportFGRctd.dQuantity LT 0 THEN
        dFinalQuantity
        ELSE
        ipbf-ttImportFGRctd.dQuantity
        .   
                       
    IF (SSPostFG-log OR ipbf-ttImportFGRctd.lSSPostFG EQ YES) AND (ipbf-ttImportFGRctd.lSSPostFG NE NO) THEN 
    DO:	   
        /* Posts Receipts */
        RUN PostFinishedGoodsForUser IN hdInventoryProcs(
            INPUT        ipbf-ttImportFGRctd.Company,
            INPUT        cReceipt,        /* Receipt */
            INPUT        USERID(LDBNAME(1)),
            INPUT        lPromptForClose, /* Executes API closing orders logic */
            INPUT-OUTPUT lSuccess,
            INPUT-OUTPUT cMessage
            ) NO-ERROR.               
           
    END.                     
    
   DELETE PROCEDURE hdInventoryReceipt. 
   DELETE PROCEDURE hdInventoryProcs.
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGRctd FOR ttImportFGRctd.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote      AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE lv-msg          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReceipt        AS CHARACTER NO-UNDO INITIAL "R".           
    DEFINE VARIABLE lPromptForClose AS LOGICAL   NO-UNDO.     
    DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.         
    DEFINE VARIABLE lValidBin       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidLoc       AS LOGICAL   NO-UNDO.
    RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs.
    
    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGRctd.cTagNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Tag can not be empty".
    END.       
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGRctd.iPONo NE 0 AND ipbf-ttImportFGRctd.cJobID NE "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Enter either PO Number or Job Number for Tag".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:  
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company   EQ ipbf-ttImportFGRctd.Company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ ipbf-ttImportFGRctd.cTagNo
            NO-ERROR.            

        IF NOT AVAILABLE loadtag THEN 
        DO:
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-ttImportFGRctd.Company
                AND fg-bin.tag     EQ ipbf-ttImportFGRctd.cTagNo
                NO-ERROR.
        END.

        IF NOT AVAILABLE fg-bin AND NOT AVAILABLE loadtag THEN 
        DO:
            IF oplValid AND ipbf-ttImportFGRctd.iPONo EQ 0 AND ipbf-ttImportFGRctd.cJobID EQ "" THEN 
            DO:
                ASSIGN 
                    opcNote  = "Tag  is not registered. Please provide valid PO Number / Job Number"                    
                    oplValid = NO
                    .                    
            END.
                
            IF oplValid AND ipbf-ttImportFGRctd.dQuantity EQ 0 THEN 
            DO:
                ASSIGN 
                    opcNote  = "Quantity can not be zero for Tag "                   
                    oplValid = NO
                    .                      
            END.
                
            IF oplValid AND ipbf-ttImportFGRctd.iQuantityPerSubUnit LE 0 THEN 
            DO:
                ASSIGN 
                    opcNote  = "Units must be greater than zero for Tag "                   
                    oplValid = NO
                    .                    
            END.
                
        END.
            
            
        ASSIGN
            lRegTag                   = AVAILABLE loadtag OR AVAILABLE fg-bin
            cPrimaryID                = IF AVAILABLE loadtag THEN 
                                           loadtag.i-no
                                         ELSE IF AVAILABLE fg-bin THEN
                                           fg-bin.i-no
                                         ELSE
                                           ""
            
            ipbf-ttImportFGRctd.dQuantity  = IF ipbf-ttImportFGRctd.dQuantity NE 0 THEN
                                             ipbf-ttImportFGRctd.dQuantity
                                         ELSE IF AVAILABLE loadtag THEN
                                             loadtag.qty
                                         ELSE IF AVAILABLE fg-bin THEN
                                             fg-bin.qty
                                         ELSE
                                             0
            ipbf-ttImportFGRctd.iQuantityPerSubUnit      = IF ipbf-ttImportFGRctd.iQuantityPerSubUnit NE 0 THEN
                                             ipbf-ttImportFGRctd.iQuantityPerSubUnit
                                         ELSE IF AVAILABLE loadtag THEN
                                             loadtag.qty-case
                                         ELSE IF AVAILABLE fg-bin THEN
                                             fg-bin.case-count
                                         ELSE
                                             0
            ipbf-ttImportFGRctd.iQuantitySubUnitsPerUnit = IF ipbf-ttImportFGRctd.iQuantitySubUnitsPerUnit NE 0 THEN
                                            ipbf-ttImportFGRctd.iQuantitySubUnitsPerUnit 
                                         ELSE IF AVAILABLE loadtag THEN
                                            loadtag.case-bundle
                                         ELSE IF AVAILABLE fg-bin THEN
                                            fg-bin.cases-unit
                                         ELSE
                                            0
            . 
           
        /* Validate PO Number */
        IF oplValid AND ipbf-ttImportFGRctd.iPONo NE 0 THEN 
        DO:
          
            IF oplValid AND ipbf-ttImportFGRctd.iPONo NE 0 THEN
                RUN pIsValidPoNo (ipbf-ttImportFGRctd.iPONo, NO, ipbf-ttImportFGRctd.Company, OUTPUT oplValid, OUTPUT opcNote).             

            /* Validate PO Line */
            IF lRegTag THEN
                FIND FIRST po-ordl NO-LOCK
                    WHERE po-ordl.company EQ ipbf-ttImportFGRctd.Company
                    AND po-ordl.po-no   EQ ipbf-ttImportFGRctd.iPONo
                    AND po-ordl.line    EQ ipbf-ttImportFGRctd.iPOLine
                    AND po-ordl.i-no    EQ cPrimaryID 
                    NO-ERROR.
            ELSE
                FIND FIRST po-ordl NO-LOCK
                    WHERE po-ordl.company EQ ipbf-ttImportFGRctd.Company
                    AND po-ordl.po-no   EQ ipbf-ttImportFGRctd.iPONo
                    AND po-ordl.line    EQ ipbf-ttImportFGRctd.iPOLine
                    NO-ERROR.
                     
            IF oplValid AND  NOT AVAILABLE po-ordl THEN 
            DO:
                ASSIGN 
                    opcNote  = "Invalid POLine entered for Tag  "                  
                    oplValid = NO
                    .        
            END.                
        END.  /* iopiPONo NE 0*/       
      
        /* Validate non-blank JobID & JobID2 */
        IF oplValid AND ipbf-ttImportFGRctd.cJobID NE "" THEN 
        DO:
            FIND FIRST job-hdr NO-LOCK 
                WHERE job-hdr.company EQ ipbf-ttImportFGRctd.Company
                AND job-hdr.job-no  EQ ipbf-ttImportFGRctd.cJobID
                NO-ERROR.     
            IF NOT AVAILABLE job-hdr THEN 
            DO:                   
                IF lRegTag THEN
                    ASSIGN
                        ipbf-ttImportFGRctd.cJobID  = ""                     
                        ipbf-ttImportFGRctd.iJobID2 = 0
                        .                   
                ELSE 
                DO: 
                    ASSIGN
                        opcNote  = "Invalid JobID "                    
                        oplValid = NO
                        .              
                END.
            END.
            /* Validate JobID2 if JobID is valid */
            IF oplValid AND AVAILABLE job-hdr THEN 
            DO:
                FOR EACH  job-hdr
                    WHERE job-hdr.company EQ ipbf-ttImportFGRctd.Company
                    AND job-hdr.job-no  EQ ipbf-ttImportFGRctd.cJobID
                    AND job-hdr.job-no2 EQ INT(ipbf-ttImportFGRctd.iJobID2)
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
                        WHERE job.company EQ ipbf-ttImportFGRctd.Company
                        AND job.job-no  EQ ipbf-ttImportFGRctd.cJobID
                        AND job.job-no2 EQ INT(ipbf-ttImportFGRctd.iJobID2)
                        NO-LOCK,
                        FIRST job-hdr
                        WHERE job-hdr.company EQ job.company
                        AND job-hdr.job     EQ job.job
                        AND job-hdr.job-no  EQ job.job-no
                        AND job-hdr.job-no2 EQ job.job-no2
                        NO-LOCK:
                        LEAVE.
                    END.

                IF NOT AVAILABLE job-hdr THEN 
                DO:
                    ASSIGN
                        opcNote  = "Invalid JobID2 "                    
                        oplValid = NO
                        .             
                END. 
            END.       
    
        END.  /* ipbf-ttImportFGRctd.cJobID NE ""*/    
      
    END.
           
    IF oplValid AND ipbf-ttImportFGRctd.cWarehouseID NE "" AND ipbf-ttImportFGRctd.cLocationID NE "" THEN 
    DO:
    
        RUN ValidateLoc IN hdInventoryProcs (
            INPUT  ipbf-ttImportFGRctd.Company,
            INPUT  ipbf-ttImportFGRctd.cWareHouseID,
            OUTPUT lValidLoc
            ).
    
        IF oplValid AND NOT lValidLoc THEN 
        DO:
            ASSIGN 
                opcNote  = "Invalid WareHouseID entered for Tag "                     
                oplValid = NO
                .       
        END.   

        /* Validate location */
        RUN ValidateBin IN hdInventoryProcs (
            INPUT  ipbf-ttImportFGRctd.Company,
            INPUT  ipbf-ttImportFGRctd.cWareHouseID,
            INPUT  ipbf-ttImportFGRctd.cLocationID,
            OUTPUT lValidBin
            ).
    
        IF oplValid AND( ipbf-ttImportFGRctd.cLocationID EQ "" OR NOT lValidBin) THEN 
        DO:
            ASSIGN 
                opcNote  = "Invalid Location Bin"
                oplValid = NO 
                .          
        END.  
    
    END.
                        
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGRctd.dQuantity GT 0 THEN 
        DO:
            /* Checks fg-rctd record for the input tag and quantity */     
            FIND FIRST fg-rctd NO-LOCK  
                WHERE fg-rctd.company EQ ipbf-ttImportFGRctd.Company
                AND fg-rctd.tag     EQ ipbf-ttImportFGRctd.cTagNo
                AND fg-rctd.qty     GT 0
                NO-ERROR.
            IF AVAILABLE fg-rctd THEN 
            DO:
                ASSIGN
                    lv-msg   = "Tag# has already been used, please enter a negative quantity."
                    oplValid = NO
                    .             
            END.
        END.
        /* Negative quantity */
        ELSE 
        DO:
            /* Checks fg-rctd record for the input tag and quantity */     
            FIND FIRST fg-rctd NO-LOCK  
                WHERE fg-rctd.company EQ ipbf-ttImportFGRctd.Company
                AND fg-rctd.tag     EQ ipbf-ttImportFGRctd.cTagNo
                NO-ERROR.
            IF NOT AVAILABLE fg-rctd THEN 
            DO:
                ASSIGN
                    lv-msg   = "Receipt does not exists for the Tag#" 
                    oplValid = NO
                    .            
            END.
            ELSE 
            DO:
                FOR EACH  fg-rctd NO-LOCK  
                    WHERE fg-rctd.company EQ ipbf-ttImportFGRctd.Company
                    AND fg-rctd.tag     EQ ipbf-ttImportFGRctd.cTagNo:
                    dFinalQuantity = dFinalQuantity + fg-rctd.qty. /* calculates sum of all receipts on the tag */ 
                END.
                dFinalQuantity = dFinalQuantity + ipbf-ttImportFGRctd.dQuantity. /* calculates final qunatity */
                /* Checks whether final quantity is negative */
                IF dFinalQuantity LT 0 THEN 
                DO:
                    ASSIGN
                        lv-msg   = "Tag# has already been used, and negative quantity is more than on-hand quantity"
                        oplValid = NO.
                    .             
                
                END.
            END.
        END.
                
        
        IF lv-msg NE "" THEN 
        DO:
            ASSIGN
                oplValid = NO
                opcNote  = lv-msg .                 
        END.
        ELSE
            ASSIGN 
                oplValid = YES
                opcNote  = "Add record"
                .           
    END.  
      
    
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
        
    DELETE PROCEDURE hdInventoryProcs.
    
END PROCEDURE.


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
        bf-fg-rctd.created-by = USERID(LDBNAME(1))
        bf-fg-rctd.updated-by = USERID(LDBNAME(1))
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
        bf-fg-rctd.pur-uom    = ipcQuantityUOM
        bf-fg-rctd.std-cost   = dStdCost
        bf-fg-rctd.cost-uom   = cCostUOM
        bf-fg-rctd.ext-cost   = dExtCost
        bf-fg-rctd.frt-cost   = dFrtCost
        bf-fg-rctd.i-no       = cPrimaryID       
        NO-ERROR.
        
        
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipcCompany
        AND itemfg.i-no    EQ cPrimaryID
        NO-ERROR.
        
    IF AVAILABLE itemfg THEN 
    DO:
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
    IF NOT AVAILABLE loadtag THEN 
    DO:
        
        /* Validates whether Tag is registered in bin or not */
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ ipcCompany
            AND fg-bin.tag     EQ ipcInventoryStockID
            NO-ERROR. 
        IF AVAILABLE fg-bin THEN 
        DO:
            ASSIGN
                ipcJobID2 = STRING(fg-bin.job-no2)
                iopcJobID = fg-bin.job-no
                iopiPONo  = INT(fg-bin.po-no)
                .
                
        END.
    END.
    ELSE 
    DO:
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
        .
        
    RELEASE bf-fg-rctd.
END PROCEDURE.

 
FUNCTION fGetValueInteger RETURNS INTEGER PRIVATE
    ( ipcCompany AS CHARACTER,
    ipcControlValue AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: returns the next xNO for ar-inv creation
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE iReturnValue AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cReturn      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.      
    
    RUN sys/ref/nk1look.p (ipcCompany, ipcControlValue , "I", NO, NO, "","", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN iReturnValue = INTEGER(cReturn) NO-ERROR. 
    
    RETURN iReturnValue .
		
END FUNCTION.

FUNCTION fGetValueLogical RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER,
    ipcControlValue AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: returns the next xNO for ar-inv creation
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE lReturnValue AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturn      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.        
    
    RUN sys/ref/nk1look.p (ipcCompany, ipcControlValue , "L", NO, NO, "","", OUTPUT cReturn, OUTPUT lFound).
    IF lFound THEN lReturnValue = LOGICAL(cReturn) NO-ERROR. 
    
    RETURN lReturnValue .
		
END FUNCTION.



