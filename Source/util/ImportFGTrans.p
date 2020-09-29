
/*------------------------------------------------------------------------
    File        : ImportFGTrans.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for FG Receipt 	

    Author(s)   : Sewa Singh
    Created     : Thu Sept 28 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util\ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportFGTrans
    FIELD Company      AS CHARACTER 
    FIELD Location     AS CHARACTER 
    FIELD cWareHouseID AS CHARACTER FORMAT "x(5)" COLUMN-LABEL "To Warehouse" HELP "Required - Size:5"    
    FIELD cLocationID  AS CHARACTER FORMAT "x(8)" COLUMN-LABEL "To Bin" HELP "Required - Size:8"
    FIELD cTagNo       AS CHARACTER FORMAT "x(20)" COLUMN-LABEL "Tag" HELP "Required - Size:20"
    FIELD cPrimaryID   AS CHARACTER FORMAT "x(15)" COLUMN-LABEL "FG Item" HELP "Required - Size:15"     
    .
    
DEFINE VARIABLE giIndexOffset    AS INTEGER NO-UNDO INIT 2. /*Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data*/
DEFINE VARIABLE hdInventoryProcs AS HANDLE  NO-UNDO.
{inventory/ttinventory.i "NEW SHARED"}.
{jc/jcgl-sh.i  NEW}   

/* ********************  Preprocessor Definitions  ******************** */



/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */
/*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportFGTrans"}


PROCEDURE pProcessRecord PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Processes an import record, incrementing the "opiAdded" variable
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGTrans FOR ttImportFGTrans.
    DEFINE INPUT PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE iRNo            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cTransfer       AS CHARACTER NO-UNDO INITIAL "T".
    DEFINE VARIABLE lPromptForClose AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lSuccess        AS LOGICAL   NO-UNDO INIT YES.
    DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
    DEFINE BUFFER b-fg-rctd  FOR fg-rctd.
    
    RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 
    
    FIND FIRST loadtag WHERE loadtag.company   = ipbf-ttImportFGTrans.Company
        AND loadtag.ITEM-type = NO
        AND loadtag.tag-no    = ipbf-ttImportFGTrans.cTagNo NO-LOCK NO-ERROR.
    
    FIND FIRST fg-bin WHERE fg-bin.company = ipbf-ttImportFGTrans.Company
        AND fg-bin.i-no    = ipbf-ttImportFGTrans.cPrimaryID
        AND fg-bin.tag     = loadtag.tag-no
        AND fg-bin.qty     > 0
        NO-LOCK NO-ERROR.
                         
                         
    FIND FIRST itemfg WHERE itemfg.company = ipbf-ttImportFGTrans.Company
        AND itemfg.i-no = fg-bin.i-no NO-LOCK NO-ERROR.

    /* Retrieving last record of fg-rctd table */ 
    FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
    IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iRNo THEN 
        iRNo = fg-rctd.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iRNo THEN 
        iRNo = fg-rcpth.r-no.
        
    /* Creates fg-rctd record for new bin & location */     
    CREATE bf-fg-rctd.
    
    DO WHILE TRUE:
        iRNo = iRNo + 1.
        FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ iRNo USE-INDEX r-no NO-LOCK NO-ERROR.
        IF AVAILABLE fg-rcpth THEN NEXT.
        FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ iRNo USE-INDEX fg-rctd NO-LOCK NO-ERROR.
        IF AVAILABLE b-fg-rctd THEN NEXT.
        LEAVE.
    END.      
    
    ASSIGN
        bf-fg-rctd.r-no         = iRNo 
        bf-fg-rctd.rct-date     = TODAY
        bf-fg-rctd.trans-time   = TIME
        bf-fg-rctd.s-num        = 0
        bf-fg-rctd.units-pallet = fg-bin.units-pallet
        bf-fg-rctd.cases-unit   = fg-bin.cases-unit
        bf-fg-rctd.company      = fg-bin.company
        bf-fg-rctd.tag          = fg-bin.tag
        bf-fg-rctd.i-no         = fg-bin.i-no
        bf-fg-rctd.i-name       = itemfg.i-name 
        bf-fg-rctd.po-no        = fg-bin.po-no 
        bf-fg-rctd.qty-case     = IF AVAILABLE fg-bin THEN fg-bin.case-count
                               ELSE loadtag.qty-case
        bf-fg-rctd.job-no       = fg-bin.job-no
        bf-fg-rctd.job-no2      = fg-bin.job-no2
        bf-fg-rctd.loc          = fg-bin.loc
        bf-fg-rctd.loc-bin      = fg-bin.loc-bin
        bf-fg-rctd.tag2         = fg-bin.tag
        bf-fg-rctd.loc2         = ipbf-ttImportFGTrans.cWareHouseID
        bf-fg-rctd.loc-bin2     = ipbf-ttImportFGTrans.cLocationID
        bf-fg-rctd.cust-no      = CAPS(fg-bin.cust-no)
        bf-fg-rctd.rita-code    = cTransfer
        bf-fg-rctd.cases        = IF AVAILABLE fg-bin THEN TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                               ELSE loadtag.tot-cases
        bf-fg-rctd.partial      = IF AVAILABLE fg-bin THEN fg-bin.partial-count
                               ELSE loadtag.partial 
        bf-fg-rctd.t-qty        = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial 
        bf-fg-rctd.created-by   = USERID(LDBNAME(1))
        bf-fg-rctd.updated-by   = USERID(LDBNAME(1))
        bf-fg-rctd.pur-uom      = fg-bin.pur-uom
        bf-fg-rctd.cost-uom     = fg-bin.pur-uom
        bf-fg-rctd.std-cost     = fg-bin.std-tot-cost
        bf-fg-rctd.ext-cost     = fg-rctd.t-qty * fg-rctd.std-cost
        bf-fg-rctd.enteredBy    = USERID(LDBNAME(1))
        bf-fg-rctd.enteredDT    = DATETIME(TODAY, MTIME)
        .
          
    /* Posts fg-rctd records */
    RUN PostFinishedGoodsForUser IN hdInventoryProcs(
        INPUT        ipbf-ttImportFGTrans.Company,
        INPUT        cTransfer,       /* Transfer */
        INPUT        USERID(LDBNAME(1)),
        INPUT        lPromptForClose, /* Executes API closing orders logic */
        INPUT-OUTPUT lSuccess,
        INPUT-OUTPUT cMessage
        ) NO-ERROR .                 
               
    DELETE PROCEDURE hdInventoryProcs.            
   
   
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Validates a given Import Record for key fields
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportFGTrans FOR ttImportFGTrans.
    DEFINE INPUT PARAMETER iplUpdateDuplicates AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER iplFieldValidation AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lValidBin AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidLoc AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValidTag AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cMsg      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMessage  AS CHARACTER NO-UNDO.

    RUN Inventory\InventoryProcs.p PERSISTENT SET hdInventoryProcs. 
    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGTrans.cTagNo EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Tag is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportFGTrans.cPrimaryID EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "FG Item is Blank".
    END.    
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:   
        /* Validate warehouse */        
        RUN ValidateLoc IN hdInventoryProcs (
            ipbf-ttImportFGTrans.Company,
            ipbf-ttImportFGTrans.cWareHouseID,
            OUTPUT lValidLoc
            ).

        IF NOT lValidLoc THEN 
        DO:
            ASSIGN 
                opcNote  = "Invalid WareHouseID " + ipbf-ttImportFGTrans.cWareHouseID                 
                oplValid = NO
                .             
        END.

        /* Validate location */
        RUN ValidateBin IN hdInventoryProcs (
            ipbf-ttImportFGTrans.Company,
            ipbf-ttImportFGTrans.cWareHouseID,
            ipbf-ttImportFGTrans.cLocationID,
            OUTPUT lValidBin
            ).

        IF oplValid AND ( ipbf-ttImportFGTrans.cLocationID EQ "" OR NOT lValidBin) THEN 
        DO:
            ASSIGN 
                opcNote  = "Invalid Bin " + ipbf-ttImportFGTrans.cLocationID
                oplValid = NO 
                .             
        END.       
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        RUN ValidateLoadTag IN hdInventoryProcs (
            INPUT  ipbf-ttImportFGTrans.Company,
            INPUT  "FG",
            INPUT  ipbf-ttImportFGTrans.cTagNo,
            OUTPUT lValidTag,
            OUTPUT cMessage
            ) NO-ERROR.
            
        IF NOT lValidTag THEN 
        DO:
            ASSIGN
                cMsg     = "Invalid Loadtag# " + ipbf-ttImportFGTrans.cTagNo 
                oplValid = NO
                .            
        END.

        /* Checks whether inventory is valid or not */        
        FIND FIRST fg-bin WHERE fg-bin.company EQ ipbf-ttImportFGTrans.Company
            AND fg-bin.i-no    EQ ipbf-ttImportFGTrans.cPrimaryID
            AND fg-bin.tag     EQ ipbf-ttImportFGTrans.cTagNo
            AND fg-bin.qty     GT 0
            NO-LOCK NO-ERROR.
        IF oplValid AND NOT AVAILABLE fg-bin THEN 
        DO:
            ASSIGN
                cMsg     = "Invalid Inventory for the tag " + ipbf-ttImportFGTrans.cTagNo
                oplValid = NO
                .              
        END.
     
        /* Checks whether To Whse/Bin and From Whse/Bin are same or not */
        IF oplValid AND fg-bin.loc     EQ ipbf-ttImportFGTrans.cWareHouseID AND
            fg-bin.loc-bin EQ ipbf-ttImportFGTrans.cLocationID  THEN 
        DO:
            ASSIGN
                cMsg     = "To Whse/Bin may not be the same as From Whse/Bin for item " + ipbf-ttImportFGTrans.cPrimaryID
                oplValid = NO
                .             
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
        
    DELETE PROCEDURE hdInventoryProcs.
END PROCEDURE.



