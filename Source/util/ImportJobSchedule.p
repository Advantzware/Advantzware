/*------------------------------------------------------------------------
    File        : ImportJobSchedule.p
    Purpose     : 

    Syntax      :

    Description : Import Program (Persistent) for Configuring and Processing the Import for Prep and Die	

    Author(s)   : Sewa Singh
    Created     : Wed Sept 11
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{util/ttImport.i SHARED}

DEFINE TEMP-TABLE ttImportJobSchedule NO-UNDO
    FIELD company         AS CHARACTER 
    FIELD location        AS CHARACTER 
    FIELD machineCode     AS CHARACTER FORMAT "x(10)"       COLUMN-LABEL "Machine"          HELP "Required - Size:10"
    FIELD line            AS INTEGER   FORMAT ">>9"         COLUMN-LABEL "Line"             HELP "Required - Integer"
    FIELD jobNumber       AS CHARACTER FORMAT "x(6)"        COLUMN-LABEL "Job No"           HELP "Required - Size:6" 
    FIELD jobNumber2      AS INTEGER   FORMAT ">>9"         COLUMN-LABEL "Run#"             HELP "Required - Integer"
    FIELD form-no         AS INTEGER   FORMAT ">9"          COLUMN-LABEL "Form"             HELP "Required - Integer"
    FIELD blank-no        AS INTEGER   FORMAT ">9"          COLUMN-LABEL "Blank"            HELP "Required - Integer"
    FIELD pass            AS INTEGER   FORMAT ">>9"         COLUMN-LABEL "Pass"             HELP "Required - Integer" 
    FIELD quantity        AS INTEGER   FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Job Quantity"     HELP "Required - Integer"
    FIELD itemNumber      AS CHARACTER FORMAT "x(15)"       COLUMN-LABEL "FG Item"          HELP "Required - Size:15"
    FIELD itemDescription AS CHARACTER FORMAT "x(30)"       COLUMN-LABEL "Item Description" HELP "Optional - Size:30"
    FIELD customerNumber  AS CHARACTER FORMAT "x(8)"        COLUMN-LABEL "Customer"         HELP "Required - Size:8"      
    FIELD dueDate         AS DATE      FORMAT "99/99/9999"  COLUMN-LABEL "Due Date"         HELP "Required - Date"
    FIELD startDate       AS DATE      FORMAT "99/99/9999"  COLUMN-LABEL "Start Date"       HELP "Optional - Date"
    FIELD orderType       AS CHARACTER FORMAT "x(10)"       COLUMN-LABEL "Order Type"       HELP "Optional - Original, Repeat, Change"
    FIELD industry        AS CHARACTER FORMAT "x"           COLUMN-LABEL "Industry"         HELP "Required - 1 if Folding - 2 if Corrugated"
    FIELD runQuantity     AS INTEGER   FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Run Quantity"     HELP "Required - Integer"
    FIELD mrHours         AS DECIMAL   FORMAT ">,>>9.99"    COLUMN-LABEL "MR Hours"         HELP "Required - Decimal"
    FIELD runHours        AS DECIMAL   FORMAT ">,>>9.99"    COLUMN-LABEL "Run Hours"        HELP "Required - Decimal"
    .
/* Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data */
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2.

DEFINE TEMP-TABLE ttUniqueJob NO-UNDO
    FIELD company    AS CHARACTER 
    FIELD jobNumber  AS CHARACTER 
    FIELD jobNumber2 AS INTEGER
    FIELD job        AS INTEGER
        INDEX ttUniqueJob IS PRIMARY
            company
            jobNumber
            jobNumber2
            .  

FUNCTION fUniqueJNo RETURNS INTEGER ():
    DEFINE BUFFER job-hdr FOR job-hdr.
    
    FIND LAST job-hdr NO-LOCK NO-ERROR.
    RETURN IF AVAILABLE job-hdr THEN job-hdr.j-no + 1 ELSE 1.

END.

/* ********************  Preprocessor Definitions  ******************** */

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */
 /*This Includes Procedures with the expected parameters.  Includes pInitialize, pAddRecord, pProcessImport*/
{util/ImportProcs.i &ImportTempTable = "ttImportJobSchedule"}

PROCEDURE pProcessRecord PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:  Processes an import record, incrementing the "opiAdded" variable
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportJobSchedule FOR ttImportJobSchedule.

    DEFINE INPUT        PARAMETER iplIgnoreBlanks AS LOGICAL NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiAdded       AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE riNote AS ROWID   NO-UNDO.
    DEFINE VARIABLE iJob   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iJNo   AS INTEGER NO-UNDO.
    
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-job-mch FOR job-mch.      

    FIND FIRST ttUniqueJob
         WHERE ttUniqueJob.company    EQ ipbf-ttImportJobSchedule.company
           AND ttUniqueJob.jobNumber  EQ ipbf-ttImportJobSchedule.jobNumber
           AND ttUniqueJob.jobNumber2 EQ ipbf-ttImportJobSchedule.jobNumber2
         NO-ERROR.
    IF NOT AVAILABLE ttUniqueJob THEN DO:
        RUN pGetInternalJob (
            INPUT  ipbf-ttImportJobSchedule.company,
            OUTPUT iJob
            ). 
        CREATE ttUniqueJob.
        ASSIGN
            ttUniqueJob.company    = ipbf-ttImportJobSchedule.company
            ttUniqueJob.jobNumber  = ipbf-ttImportJobSchedule.jobNumber
            ttUniqueJob.jobNumber2 = ipbf-ttImportJobSchedule.jobNumber2
            ttUniqueJob.job        = iJob
            .
        CREATE bf-job.
        ASSIGN 
            bf-job.company    = ipbf-ttImportJobSchedule.company
            bf-job.loc        = ipbf-ttImportJobSchedule.location
            bf-job.job        = ttUniqueJob.job
            bf-job.job-no     = ipbf-ttImportJobSchedule.jobNumber
            bf-job.job-no2    = ipbf-ttImportJobSchedule.jobNumber2
            bf-job.stat       = "P"
            bf-job.opened     = TRUE 
            bf-job.industry   = ipbf-ttImportJobSchedule.industry
            bf-job.orderType  = ipbf-ttImportJobSchedule.orderType
            bf-job.due-date   = ipbf-ttImportJobSchedule.dueDate
            bf-job.start-Date = ipbf-ttImportJobSchedule.startDate
            .                
    END. /* if not avail */ 

    iopiAdded = iopiAdded + 1.
                
    IF NOT CAN-FIND(FIRST bf-job-hdr
                    WHERE bf-job-hdr.company  EQ ipbf-ttImportJobSchedule.company
                      AND bf-job-hdr.job-no   EQ ipbf-ttImportJobSchedule.jobNumber
                      AND bf-job-hdr.job-no2  EQ ipbf-ttImportJobSchedule.jobNumber2
                      AND bf-job-hdr.frm      EQ ipbf-ttImportJobSchedule.form-no
                      AND bf-job-hdr.blank-no EQ ipbf-ttImportJobSchedule.blank-no) THEN DO:
        iJNo = fUniqueJNo().
        CREATE bf-job-hdr.
        ASSIGN 
            bf-job-hdr.company    = ipbf-ttImportJobSchedule.company
            bf-job-hdr.loc        = ipbf-ttImportJobSchedule.location
            bf-job-hdr.job        = ttUniqueJob.job
            bf-job-hdr.job-no     = ipbf-ttImportJobSchedule.jobNumber
            bf-job-hdr.job-no2    = ipbf-ttImportJobSchedule.jobNumber2
            bf-job-hdr.frm        = ipbf-ttImportJobSchedule.form-no
            bf-job-hdr.blank-no   = ipbf-ttImportJobSchedule.blank-no
            bf-job-hdr.i-no       = ipbf-ttImportJobSchedule.itemNumber
            bf-job-hdr.cust-no    = ipbf-ttImportJobSchedule.customerNumber
            bf-job-hdr.opened     = TRUE
            bf-job-hdr.due-date   = ipbf-ttImportJobSchedule.dueDate
            bf-job-hdr.start-Date = ipbf-ttImportJobSchedule.startDate
            bf-job-hdr.qty        = ipbf-ttImportJobSchedule.quantity
            bf-job-hdr.j-no       = iJNo
            .     
    END.

    CREATE bf-job-mch.
    ASSIGN
        bf-job-mch.company  = ipbf-ttImportJobSchedule.company
        bf-job-mch.line     = ipbf-ttImportJobSchedule.line
        bf-job-mch.m-code   = ipbf-ttImportJobSchedule.machineCode
        bf-job-mch.job      = ttUniqueJob.job
        bf-job-mch.job-no   = ipbf-ttImportJobSchedule.jobNumber
        bf-job-mch.job-no2  = ipbf-ttImportJobSchedule.jobNumber2
        bf-job-mch.frm      = ipbf-ttImportJobSchedule.form-no
        bf-job-mch.blank-no = ipbf-ttImportJobSchedule.blank-no
        bf-job-mch.pass     = ipbf-ttImportJobSchedule.pass
        bf-job-mch.i-no     = ipbf-ttImportJobSchedule.itemNumber
        bf-job-mch.i-name   = ipbf-ttImportJobSchedule.itemDescription 
        bf-job-mch.run-qty  = ipbf-ttImportJobSchedule.runQuantity
        bf-job-mch.mr-hr    = ipbf-ttImportJobSchedule.mrHours
        bf-job-mch.run-hr   = ipbf-ttImportJobSchedule.runHours
        bf-job-mch.speed    = bf-job-mch.run-qty / bf-job-mch.run-hr
        .                    
   {sys/inc/roundup.i bf-job-mch.speed}
   RELEASE bf-job.
   RELEASE bf-job-hdr.
   RELEASE bf-job-mch.                                                                                                                                 
                                                                                                                               
END PROCEDURE.                                                                                                                 
                                                                                                                               
PROCEDURE pValidate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Validates a given Import Record for key fields
 Notes:
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-ttImportJobSchedule FOR ttImportJobSchedule.
    DEFINE INPUT  PARAMETER iplUpdateDuplicates AS LOGICAL   NO-UNDO.
    DEFINE INPUT  PARAMETER iplFieldValidation  AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid            AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcNote             AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cValidNote AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-ttImportJobSchedule FOR ttImportJobSchedule.

    oplValid = YES.
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.jobNumber EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Job No is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.form-no LT 1 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Form must be greater than zero".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.itemNumber EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "FG Item is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.customerNumber EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Customer is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.industry EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Industry is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.quantity EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Quantity is Blank or Zero".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.machineCode EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Machine Code is Blank ".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.pass EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Machine Pass is Blank or Zero ".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.runQuantity EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Machine Run Qty is Blank or Zero ".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
    IF oplValid THEN 
    DO:
        IF CAN-FIND(FIRST bf-ttImportJobSchedule
                    WHERE bf-ttImportJobSchedule.company    EQ ipbf-ttImportJobSchedule.company
                      AND bf-ttImportJobSchedule.jobNumber  EQ ipbf-ttImportJobSchedule.jobNumber
                      AND bf-ttImportJobSchedule.jobNumber2 EQ ipbf-ttImportJobSchedule.jobNumber2
                      AND ROWID(bf-ttImportJobSchedule)     NE ROWID(ipbf-ttImportJobSchedule)) THEN 
        ASSIGN 
            oplValid = NO 
            opcNote  = "Duplicate Record in Import File"
            .
    END.
    /*Determine if Add or Update*/
    IF oplValid THEN 
    DO:
        IF CAN-FIND(FIRST job
                    WHERE job.company EQ ipbf-ttImportJobSchedule.company
                      AND job.job-no  EQ ipbf-ttImportJobSchedule.jobNumber
                      AND job.job-no2 EQ ipbf-ttImportJobSchedule.jobNumber2) THEN
        ASSIGN 
            oplValid = NO
            opcNote  = "Duplicate record exists"
            .            
        ELSE 
        ASSIGN 
            oplValid = YES
            opcNote = "Add record"
            .         
    END.
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:         
        IF oplValid AND ipbf-ttImportJobSchedule.itemNumber NE "" THEN 
            RUN pIsValidFGITemID (ipbf-ttImportJobSchedule.itemNumber, NO, ipbf-ttImportJobSchedule.company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportJobSchedule.customerNumber NE "" THEN 
            RUN pIsValidCustomerID (ipbf-ttImportJobSchedule.customerNumber, NO, ipbf-ttImportJobSchedule.company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportJobSchedule.machineCode NE "" THEN 
            RUN pIsValidMachCode (ipbf-ttImportJobSchedule.machineCode, NO, ipbf-ttImportJobSchedule.company, OUTPUT oplValid, OUTPUT cValidNote).               
            
        IF oplValid AND ipbf-ttImportJobSchedule.orderType NE "" THEN   
            RUN pIsValidFromList ("Order Type", ipbf-ttImportJobSchedule.orderType, "Original,Repeat,Change", OUTPUT oplValid, OUTPUT cValidNote).     
            
        IF oplValid AND ipbf-ttImportJobSchedule.industry NE "" THEN   
            RUN pIsValidFromList ("Industry", ipbf-ttImportJobSchedule.industry, "1,2", OUTPUT oplValid, OUTPUT cValidNote).                 
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    IF ipbf-ttImportJobSchedule.orderType EQ "Original" THEN
        ASSIGN ipbf-ttImportJobSchedule.orderType = "O".
    ELSE IF ipbf-ttImportJobSchedule.orderType EQ "Repeat" THEN
        ASSIGN ipbf-ttImportJobSchedule.orderType = "R".
    ELSE IF ipbf-ttImportJobSchedule.orderType EQ "Change" THEN
        ASSIGN ipbf-ttImportJobSchedule.orderType = "C".
    ELSE ASSIGN ipbf-ttImportJobSchedule.orderType = "O".    
    
END PROCEDURE.

PROCEDURE pGetInternalJob PRIVATE :
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiJob     AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-job     FOR job.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    
    opiJob = 1.
        
    FIND LAST bf-job NO-LOCK
         WHERE bf-job.company EQ ipcCompany 
         USE-INDEX job NO-ERROR.
    
    FIND LAST bf-job-hdr NO-LOCK
         WHERE bf-job-hdr.company EQ ipcCompany
         USE-INDEX job NO-ERROR.
    
    IF bf-job-hdr.job GT bf-job.job THEN 
        opiJob = bf-job-hdr.job + 1.
    
    IF bf-job.job GE bf-job-hdr.job THEN
        opiJob = bf-job.job + 1.

END PROCEDURE.
