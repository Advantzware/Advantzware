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
    FIELD company    AS CHARACTER 
    FIELD location   AS CHARACTER 
    FIELD line       AS INTEGER   FORMAT ">>9"         COLUMN-LABEL "Order"            HELP "Required - Integer"
    FIELD m-code     AS CHARACTER FORMAT "x(6)"        COLUMN-LABEL "Machine"          HELP "Required - Size:6"
    FIELD job-no     AS CHARACTER FORMAT "x(6)"        COLUMN-LABEL "Job No"           HELP "Required - Size:6" 
    FIELD job-no2    AS INTEGER   FORMAT ">>9"         COLUMN-LABEL "Run#"             HELP "Required - Integer"
    FIELD frm        AS INTEGER   FORMAT ">9"          COLUMN-LABEL "Form"             HELP "Required - Integer"
    FIELD blank-no   AS INTEGER   FORMAT ">9"          COLUMN-LABEL "Blank"            HELP "Required - Integer"
    FIELD pass       AS INTEGER   FORMAT ">>9"         COLUMN-LABEL "Pass"             HELP "Required - Integer" 
    FIELD qty        AS INTEGER   FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Job Quantity"     HELP "Required - Integer"
    FIELD i-no       AS CHARACTER FORMAT "x(15)"       COLUMN-LABEL "FG Item"          HELP "Required - Size:15"
    FIELD i-name     AS CHARACTER FORMAT "x(30)"       COLUMN-LABEL "Item Description" HELP "Optional - Size:30"
    FIELD cust-no    AS CHARACTER FORMAT "x(8)"        COLUMN-LABEL "Customer"         HELP "Required - Size:8"      
    FIELD due-date   AS DATE      FORMAT "99/99/9999"  COLUMN-LABEL "Due Date"         HELP "Required - Date"
    FIELD run-qty    AS INTEGER   FORMAT ">>>,>>>,>>9" COLUMN-LABEL "Run Quantity"     HELP "Required - Integer"
    FIELD mr-hr      AS DECIMAL   FORMAT ">,>>9.99"    COLUMN-LABEL "MR Hours"         HELP "Required - Decimal"
    FIELD run-hr     AS DECIMAL   FORMAT ">,>>9.99"    COLUMN-LABEL "Run Hours"        HELP "Required/Optional - Decimal"
    FIELD speed      AS INTEGER   FORMAT ">>>,>>9"     COLUMN-LABEL "Speed"            HELP "Optional/Required - Integer"
    .
/* Set to 2 to skip Company and Location field in temp-table since this will not be part of the import data */
DEFINE VARIABLE giIndexOffset AS INTEGER NO-UNDO INIT 2.

DEFINE TEMP-TABLE ttUniqueJob NO-UNDO
    FIELD company    AS CHARACTER 
    FIELD job-no  AS CHARACTER 
    FIELD job-no2 AS INTEGER
    FIELD job        AS INTEGER
        INDEX ttUniqueJob IS PRIMARY
            company
            job-no
            job-no2
            .  

FUNCTION fIndustry RETURNS CHARACTER (ipcCompany AS CHARACTER):
    DEFINE VARIABLE cCEMenu AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCEMenu AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (
        ipcCompany,"CEMENU","C",NO,NO,"","",
        OUTPUT cCEMenu,OUTPUT lCEMenu
        ).
    IF lCEMenu AND cCEMenu NE "" THEN
    RETURN IF cCEMenu EQ "Foldware" THEN "1"
      ELSE IF cCEMenu EQ "Corrware" THEN "2"
      ELSE "".
END FUNCTION.

FUNCTION fUniqueJNo RETURNS INTEGER ():
    DEFINE BUFFER job-hdr FOR job-hdr.
    
    FIND LAST job-hdr NO-LOCK NO-ERROR.
    RETURN IF AVAILABLE job-hdr THEN job-hdr.j-no + 1 ELSE 1.
END FUNCTION.

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

    FIND FIRST bf-job EXCLUSIVE-LOCK
         WHERE bf-job.company EQ ipbf-ttImportJobSchedule.company
           AND bf-job.loc     EQ ipbf-ttImportJobSchedule.location
           AND bf-job.job-no  EQ ipbf-ttImportJobSchedule.job-no
           AND bf-job.job-no2 EQ ipbf-ttImportJobSchedule.job-no2
         NO-ERROR.
    IF NOT AVAILABLE bf-job THEN DO:
        FIND FIRST ttUniqueJob
             WHERE ttUniqueJob.company EQ ipbf-ttImportJobSchedule.company
               AND ttUniqueJob.job-no  EQ ipbf-ttImportJobSchedule.job-no
               AND ttUniqueJob.job-no2 EQ ipbf-ttImportJobSchedule.job-no2
             NO-ERROR.
        IF NOT AVAILABLE ttUniqueJob THEN DO:
            RUN pGetInternalJob (
                ipbf-ttImportJobSchedule.company,
                OUTPUT iJob
                ). 
            CREATE ttUniqueJob.
            ASSIGN
                ttUniqueJob.company = ipbf-ttImportJobSchedule.company
                ttUniqueJob.job-no  = ipbf-ttImportJobSchedule.job-no
                ttUniqueJob.job-no2 = ipbf-ttImportJobSchedule.job-no2
                ttUniqueJob.job     = iJob
                .
            CREATE bf-job.
            ASSIGN 
                bf-job.company   = ipbf-ttImportJobSchedule.company
                bf-job.loc       = ipbf-ttImportJobSchedule.location
                bf-job.job       = ttUniqueJob.job
                bf-job.job-no    = ipbf-ttImportJobSchedule.job-no
                bf-job.job-no2   = ipbf-ttImportJobSchedule.job-no2
                bf-job.stat      = "P"
                bf-job.opened    = TRUE 
                bf-job.orderType = "O"
                bf-job.industry  = fIndustry(ipbf-ttImportJobSchedule.company)
                .                
        END. /* if not avail */ 
    END. /* if not avail */
    bf-job.due-date   = ipbf-ttImportJobSchedule.due-date.                

    FIND FIRST bf-job-hdr EXCLUSIVE-LOCK
         WHERE bf-job-hdr.company  EQ ipbf-ttImportJobSchedule.company
           AND bf-job-hdr.loc      EQ ipbf-ttImportJobSchedule.location
           AND bf-job-hdr.job-no   EQ ipbf-ttImportJobSchedule.job-no
           AND bf-job-hdr.job-no2  EQ ipbf-ttImportJobSchedule.job-no2
           AND bf-job-hdr.frm      EQ ipbf-ttImportJobSchedule.frm
         NO-ERROR.
    IF NOT AVAILABLE bf-job-hdr THEN DO:
        iJNo = fUniqueJNo().
        CREATE bf-job-hdr.
        ASSIGN 
            bf-job-hdr.company  = ipbf-ttImportJobSchedule.company
            bf-job-hdr.loc      = ipbf-ttImportJobSchedule.location
            bf-job-hdr.job-no   = ipbf-ttImportJobSchedule.job-no
            bf-job-hdr.job-no2  = ipbf-ttImportJobSchedule.job-no2
            bf-job-hdr.frm      = ipbf-ttImportJobSchedule.frm
            bf-job-hdr.opened   = TRUE
            bf-job-hdr.job      = ttUniqueJob.job
            bf-job-hdr.j-no     = iJNo
            .
    END. /* if not avail */
    ASSIGN
        bf-job-hdr.blank-no   = ipbf-ttImportJobSchedule.blank-no
        bf-job-hdr.i-no       = ipbf-ttImportJobSchedule.i-no
        bf-job-hdr.cust-no    = ipbf-ttImportJobSchedule.cust-no
        bf-job-hdr.due-date   = ipbf-ttImportJobSchedule.due-date
        bf-job-hdr.qty        = ipbf-ttImportJobSchedule.qty
        .     

    FIND FIRST bf-job-mch EXCLUSIVE-LOCK
         WHERE bf-job-mch.company  EQ ipbf-ttImportJobSchedule.company
           AND bf-job-mch.line     EQ ipbf-ttImportJobSchedule.line
           AND bf-job-mch.m-code   EQ ipbf-ttImportJobSchedule.m-code
           AND bf-job-mch.job-no   EQ ipbf-ttImportJobSchedule.job-no
           AND bf-job-mch.job-no2  EQ ipbf-ttImportJobSchedule.job-no2
           AND bf-job-mch.frm      EQ ipbf-ttImportJobSchedule.frm
           AND bf-job-mch.blank-no EQ ipbf-ttImportJobSchedule.blank-no
           AND bf-job-mch.pass     EQ ipbf-ttImportJobSchedule.pass
         NO-ERROR.
    IF NOT AVAILABLE bf-job-mch THEN DO:
        CREATE bf-job-mch.
        ASSIGN
            bf-job-mch.company  = ipbf-ttImportJobSchedule.company
            bf-job-mch.line     = ipbf-ttImportJobSchedule.line
            bf-job-mch.m-code   = ipbf-ttImportJobSchedule.m-code
            bf-job-mch.job-no   = ipbf-ttImportJobSchedule.job-no
            bf-job-mch.job-no2  = ipbf-ttImportJobSchedule.job-no2
            bf-job-mch.frm      = ipbf-ttImportJobSchedule.frm
            bf-job-mch.blank-no = ipbf-ttImportJobSchedule.blank-no
            bf-job-mch.pass     = ipbf-ttImportJobSchedule.pass
            bf-job-mch.job      = ttUniqueJob.job
            iopiAdded           = iopiAdded + 1
            .                    
    END. /* if not avail */
    ASSIGN
        bf-job-mch.i-no    = ipbf-ttImportJobSchedule.i-no
        bf-job-mch.i-name  = ipbf-ttImportJobSchedule.i-name 
        bf-job-mch.run-qty = ipbf-ttImportJobSchedule.run-qty
        bf-job-mch.mr-hr   = ipbf-ttImportJobSchedule.mr-hr
        bf-job-mch.run-hr  = ipbf-ttImportJobSchedule.run-hr
        bf-job-mch.speed   = ipbf-ttImportJobSchedule.speed
        .
    IF bf-job-mch.speed EQ 0 AND bf-job-mch.run-hr NE 0 THEN
    bf-job-mch.speed = bf-job-mch.run-qty / bf-job-mch.run-hr.
    ELSE
    IF bf-job-mch.speed NE 0 AND bf-job-mch.run-hr EQ 0 THEN
    bf-job-mch.run-hr = bf-job-mch.run-qty / bf-job-mch.speed.
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

    ASSIGN 
        oplValid = YES
        opcNote = "Add record"
        .         
/*    MESSAGE                                                                  */
/*    "ipbf-ttImportJobSchedule.run-qty:" ipbf-ttImportJobSchedule.run-qty SKIP*/
/*    "ipbf-ttImportJobSchedule.speed:" ipbf-ttImportJobSchedule.speed         */
/*    VIEW-AS ALERT-BOX.                                                       */
    
    /*Check for Key Field(s) to be not blank*/
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.job-no EQ '' THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Job No is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.frm LT 1 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Form must be greater than zero".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.i-no EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "FG Item is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.cust-no EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Customer is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.qty EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Quantity is Blank or Zero".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.m-code EQ "" THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Machine Code is Blank".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.pass EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Machine Pass is Blank or Zero".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.run-qty EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Machine Run Qty is Blank or Zero".
    END.
    IF oplValid THEN 
    DO:
        IF ipbf-ttImportJobSchedule.run-hr EQ 0 AND
           ipbf-ttImportJobSchedule.speed  EQ 0 THEN 
            ASSIGN 
                oplValid = NO
                opcNote  = "Machine Run Hours and Speed are both Blank or Zero".
    END.
    
    /*Check for Duplicate Import Record and Ignore It*/ 
/*    IF oplValid THEN                                                                              */
/*    DO:                                                                                           */
/*        IF CAN-FIND(FIRST bf-ttImportJobSchedule                                                  */
/*                    WHERE bf-ttImportJobSchedule.company  EQ ipbf-ttImportJobSchedule.company     */
/*                      AND bf-ttImportJobSchedule.m-code   EQ ipbf-ttImportJobSchedule.m-code      */
/*                      AND bf-ttImportJobSchedule.line     EQ ipbf-ttImportJobSchedule.line        */
/*                      AND bf-ttImportJobSchedule.job-no   EQ ipbf-ttImportJobSchedule.job-no      */
/*                      AND bf-ttImportJobSchedule.job-no2  EQ ipbf-ttImportJobSchedule.job-no2     */
/*                      AND bf-ttImportJobSchedule.frm      EQ ipbf-ttImportJobSchedule.frm         */
/*                      AND bf-ttImportJobSchedule.blank-no EQ ipbf-ttImportJobSchedule.blank-no    */
/*                      AND bf-ttImportJobSchedule.pass     EQ ipbf-ttImportJobSchedule.pass        */
/*                      AND ROWID(bf-ttImportJobSchedule)   NE ROWID(ipbf-ttImportJobSchedule)) THEN*/
/*        ASSIGN                                                                                    */
/*            oplValid = NO                                                                         */
/*            opcNote  = "Duplicate Record in Import File"                                          */
/*            .                                                                                     */
/*    END.                                                                                          */
/*    /*Determine if Add or Update*/                                                                */
/*    IF oplValid THEN                                                                              */
/*    DO:                                                                                           */
/*        IF CAN-FIND(FIRST job                                                                     */
/*                    WHERE job.company EQ ipbf-ttImportJobSchedule.company                         */
/*                      AND job.job-no  EQ ipbf-ttImportJobSchedule.job-no                          */
/*                      AND job.job-no2 EQ ipbf-ttImportJobSchedule.job-no2) THEN                   */
/*        ASSIGN                                                                                    */
/*            oplValid = NO                                                                         */
/*            opcNote  = "Duplicate record exists"                                                  */
/*            .                                                                                     */
/*        ELSE                                                                                      */
/*    ASSIGN                    */
/*        oplValid = YES        */
/*        opcNote = "Add record"*/
/*        .                     */
/*    END.*/
    
    /*Field Level Validation*/
    IF oplValid AND iplFieldValidation THEN 
    DO:         
        IF oplValid AND ipbf-ttImportJobSchedule.i-no NE "" THEN 
            RUN pIsValidFGITemID (ipbf-ttImportJobSchedule.i-no, NO, ipbf-ttImportJobSchedule.company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportJobSchedule.cust-no NE "" THEN 
            RUN pIsValidCustomerID (ipbf-ttImportJobSchedule.cust-no, NO, ipbf-ttImportJobSchedule.company, OUTPUT oplValid, OUTPUT cValidNote).

        IF oplValid AND ipbf-ttImportJobSchedule.m-code NE "" THEN 
            RUN pIsValidMachCode (ipbf-ttImportJobSchedule.m-code, NO, ipbf-ttImportJobSchedule.company, OUTPUT oplValid, OUTPUT cValidNote).               
    END.
    IF NOT oplValid AND cValidNote NE "" THEN opcNote = cValidNote.
    
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
