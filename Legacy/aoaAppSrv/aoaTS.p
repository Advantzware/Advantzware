&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : aoaAppSrv/aoaTS.p
    Purpose     : AppServer Functions and Procedures

    Syntax      : 

    Description : AppServer Functions and Procedures

    Author(s)   : Ron Stark
    Created     : 3.23.2016
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Machine Transactions.rpa */
DEFINE TEMP-TABLE ttMachineTransactions NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD machine      LIKE machtran.machine       LABEL "Machine"
    FIELD custPartNo   AS CHARACTER                LABEL "Cust Part" FORMAT "x(15)"
    FIELD jobNumber    LIKE machtran.job_number    LABEL "Job"
    FIELD jobSub       LIKE machtran.job_sub       LABEL "Sub"
    FIELD formNumber   LIKE machtran.form_number   LABEL "Form"
    FIELD blankNumber  LIKE machtran.blank_number  LABEL "Blank"
    FIELD passSequence LIKE machtran.pass_sequence LABEL "Pass"
    FIELD chargeCode   LIKE machtran.charge_code
    FIELD startDate    LIKE machtran.start_date
    FIELD startTime    AS CHARACTER                LABEL "Log In"    FORMAT "hh:mm am"
    FIELD endDate      LIKE machtran.end_date
    FIELD endTime      AS CHARACTER                LABEL "Log Out"   FORMAT "hh:mm am"
    FIELD shift        LIKE machtran.shift
    FIELD totalTime    AS CHARACTER                LABEL "Total"     FORMAT "hh:mm am"
    FIELD runQty       LIKE machtran.run_qty
    FIELD wasteQty     LIKE machtran.waste_qty
    FIELD xxRecKey     LIKE machtran.rec_key
    FIELD xxSort       AS CHARACTER                LABEL "Sort"      FORMAT "x(100)"
    FIELD xxTotalTime  AS INTEGER                  LABEL "TotTime"   FORMAT "99999"
    .
DEFINE TEMP-TABLE ttMachineEmployeeTransactions NO-UNDO
    FIELD employee      LIKE machemp.employee
    FIELD firstName     LIKE emptrack.employee.first_name
    FIELD lastName      LIKE emptrack.employee.last_name
    FIELD startDate     LIKE machemp.start_date
    FIELD startTime     AS CHARACTER LABEL "Start Time" FORMAT "hh:mm am"
    FIELD endDate       LIKE machemp.end_date
    FIELD endTime       AS CHARACTER LABEL "End Time"   FORMAT "hh:mm am"
    FIELD totalTime     AS CHARACTER LABEL "Total Time" FORMAT "hh:mm am"
    FIELD shift         LIKE machemp.shift
    FIELD rateUsage     LIKE machemp.rate_usage
    FIELD rateType      LIKE machemp.ratetype
    FIELD rate          LIKE machemp.rate
    FIELD xxTableRecKey LIKE machemp.table_rec_key
    .
/* Machine Transactions.rpa */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-fGetSubTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetSubTableHandle Procedure 
FUNCTION fGetSubTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fMachineTransactions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fMachineTransactions Procedure 
FUNCTION fMachineTransactions RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-pMachineTransactions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMachineTransactions Procedure 
PROCEDURE pMachineTransactions :
/*------------------------------------------------------------------------------
  Purpose:     Machine Transactions.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pMachineTransactions.i}
    
    /* local variables */
    DEFINE VARIABLE cCustPartNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO.
    
    /* subject business logic */
    ASSIGN
        iStartTime = 0
        iEndTime   = 86400
        .
    FIND FIRST shift NO-LOCK
         WHERE shift.company EQ ipcCompany
           AND shift.shift   EQ iStartShift
         NO-ERROR.
    IF AVAILABLE shift THEN
    iStartTime = shift.start-time.
    
    IF iStartShift NE iEndShift THEN DO:
        FIND FIRST shift NO-LOCK
             WHERE shift.company EQ ipcCompany
               AND shift.shift   EQ iEndShift
             NO-ERROR.
        IF AVAILABLE shift THEN
        iEndTime = shift.end-time.
    END. /* different shifts */
    
    FOR EACH machtran NO-LOCK
        WHERE machtran.company      EQ ipcCompany
          AND machtran.machine      GE cStartMachine
          AND machtran.machine      LE cEndMachine
          AND machtran.start_date   GE dtStartMachTranDate
          AND machtran.start_date   LE dtEndMachTranDate
          AND machtran.shift        GE STRING(iStartShift)
          AND machtran.shift        LE STRING(iEndShift)
          AND DATETIME(machtran.start_date,machtran.start_time)
           GE DATETIME(dtStartMachTranDate,iStartTime)
          AND DATETIME(machtran.start_date,machtran.start_time)
           LE DATETIME(dtEndMachTranDate,iEndTime)
        BREAK BY machtran.machine
              BY machtran.job_number
              BY machtran.job_sub
              BY machtran.form_number
              BY machtran.blank_number
        :
        IF FIRST-OF(machtran.blank_number) THEN DO:
            cCustPartNo = "".
            FIND FIRST job-mch NO-LOCK
                 WHERE job-mch.company  EQ machtran.company
                   AND job-mch.m-code   EQ machtran.machine
                   AND job-mch.job-no   EQ machtran.job_number
                   AND job-mch.job-no2  EQ machtran.job_sub
                   AND job-mch.frm      EQ machtran.form_number
                   AND job-mch.blank-no EQ machtran.blank_number
                   AND job-mch.pass     EQ machtran.pass_sequence
                 NO-ERROR.
            IF AVAILABLE job-mch AND job-mch.i-no NE "" THEN DO:
                FIND FIRST itemfg NO-LOCK
                     WHERE itemfg.company EQ machtran.company
                       AND itemfg.i-no    EQ job-mch.i-no
                     NO-ERROR.
                IF AVAILABLE itemfg THEN
                cCustPartNo = itemfg.part-no.
            END. /* avail job-mch */
            IF cCustPartNo EQ "" THEN DO:
                FIND FIRST job-hdr NO-LOCK
                     WHERE job-hdr.company  EQ machtran.company
                       AND job-hdr.job-no   EQ machtran.job_number
                       AND job-hdr.job-no2  EQ machtran.job_sub
                       AND job-hdr.frm      EQ machtran.form_number
                       AND job-hdr.blank-no EQ machtran.blank_number
                     NO-ERROR.
                IF NOT AVAILABLE job-hdr THEN
                FIND FIRST job-hdr NO-LOCK
                     WHERE job-hdr.company  EQ machtran.company
                       AND job-hdr.job-no   EQ machtran.job_number
                       AND job-hdr.job-no2  EQ machtran.job_sub
                       AND job-hdr.frm      EQ machtran.form_number
                     NO-ERROR.
                IF NOT AVAILABLE job-hdr THEN
                FIND FIRST job-hdr NO-LOCK
                     WHERE job-hdr.company  EQ machtran.company
                       AND job-hdr.job-no   EQ machtran.job_number
                       AND job-hdr.job-no2  EQ machtran.job_sub
                     NO-ERROR.
                IF AVAILABLE job-hdr THEN DO:
                    FIND FIRST itemfg NO-LOCK
                         WHERE itemfg.company EQ machtran.company
                           AND itemfg.i-no    EQ job-hdr.i-no
                         NO-ERROR.
                    IF AVAILABLE itemfg THEN
                    cCustPartNo = itemfg.part-no.
                END. /* avail job-mch */
            END. /* else avail job-mch */
        END. /* if first-of */
        /*
        IF FIRST-OF(machtran.blank_number) THEN DO:
            cCustPartNo = "".
            FIND FIRST job-hdr NO-LOCK
                 WHERE job-hdr.company EQ machtran.company
                   AND job-hdr.job-no  EQ machtran.job_number
                 NO-ERROR.
            IF AVAILABLE job-hdr THEN DO:
                FIND FIRST job NO-LOCK
                     WHERE job.company EQ job-hdr.company
                       AND job.job     EQ job-hdr.job
                       AND job.job-no  EQ job-hdr.job-no
                       AND job.job-no2 EQ job-hdr.job-no2
                       AND job.stat    NE "H"
                     NO-ERROR.
                IF AVAILABLE job THEN DO:
                    FIND FIRST ef NO-LOCK
                         WHERE ef.form-no   EQ job-hdr.frm
                            OR est.est-type NE 8
                         NO-ERROR.
                    IF AVAILABLE ef THEN DO:
                        FIND FIRST eb NO-LOCK
                             WHERE eb.company   EQ job-hdr.company
                               AND eb.est-no    EQ job-hdr.est-no
                               AND eb.form-no   EQ ef.form-no
                               AND (eb.blank-no EQ job-hdr.blank-no
                                OR est.est-type NE 8)
                             NO-ERROR.
                   IF AVAILABLE eb THEN
                   cCustPartNo = eb.part-no.
                    END. /* avail ef */
                END. /* avail job */
            END. /* avail job-hdr */
        END. /* first-of blank */
        */
        CREATE ttMachineTransactions.
        ASSIGN
            ttMachineTransactions.machine      = machtran.machine
            ttMachineTransactions.custPartNo   = cCustPartNo
            ttMachineTransactions.jobNumber    = machtran.job_number
            ttMachineTransactions.jobSub       = machtran.job_sub
            ttMachineTransactions.formNumber   = machtran.form_number
            ttMachineTransactions.blankNumber  = machtran.blank_number
            ttMachineTransactions.passsequence = machtran.pass_sequence
            ttMachineTransactions.chargeCode   = machtran.charge_code
            ttMachineTransactions.startDate    = machtran.start_date
            ttMachineTransactions.startTime    = STRING(machtran.start_time,"hh:mm am")
            ttMachineTransactions.endDate      = machtran.end_date
            ttMachineTransactions.endTime      = STRING(machtran.end_time,"hh:mm am")
            ttMachineTransactions.shift        = machtran.shift
            ttMachineTransactions.totalTime    = STRING(machtran.total_time,"hh:mm")
            ttMachineTransactions.xxTotalTime  = machtran.total_time
            ttMachineTransactions.runQty       = machtran.run_qty
            ttMachineTransactions.wasteQty     = machtran.waste_qty
            ttMachineTransactions.xxRecKey     = machtran.rec_key
            ttMachineTransactions.xxSort       = IF cSort EQ "Start Date / Time" THEN
                                                 STRING(machtran.start_date)
                                               + STRING(machtran.start_time)
                                               + machtran.machine
                                            ELSE IF cSort EQ "Start Date / Job#" THEN
                                                 STRING(machtran.start_date)
                                               + machtran.job_number
                                               + machtran.machine
                                               + STRING(machtran.start_time)
                                            ELSE machtran.machine
                                               + STRING(machtran.start_date)
                                               + STRING(machtran.start_time)              
            .
        IF lSubRpt_EmployeeTransactions THEN
        FOR EACH machemp NO-LOCK
            WHERE machemp.table_rec_key EQ machtran.rec_key
            :
            CREATE ttMachineEmployeeTransactions.
            ASSIGN
                ttMachineEmployeeTransactions.employee  = machemp.employee
                ttMachineEmployeeTransactions.startDate = machemp.start_date
                ttMachineEmployeeTransactions.startTime = STRING(machemp.start_time,"hh:mm am")
                ttMachineEmployeeTransactions.endDate   = machemp.end_date
                ttMachineEmployeeTransactions.endTime   = STRING(machemp.end_time,"hh:mm am")
                ttMachineEmployeeTransactions.totalTime = STRING(machemp.total_time,"hh:mm")
                ttMachineEmployeeTransactions.shift     = machemp.shift
                ttMachineEmployeeTransactions.rateUsage = machemp.rate_usage
                ttMachineEmployeeTransactions.rateType  = machemp.ratetype
                ttMachineEmployeeTransactions.rate      = machemp.rate
                .
            FIND FIRST employee NO-LOCK
                 WHERE employee.company EQ machtran.company
                   AND employee.employee EQ machemp.employee
                 NO-ERROR.
            IF AVAILABLE employee THEN
            ASSIGN
                ttMachineEmployeeTransactions.firstName = employee.first_name
                ttMachineEmployeeTransactions.lastName  = employee.last_name
                .
        END. /* each machemp */
    END. /* each machtran */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-fGetSubTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetSubTableHandle Procedure 
FUNCTION fGetSubTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Machine Transactions.rpa */
        WHEN "r-mchtrn." THEN
        RETURN TEMP-TABLE ttMachineEmployeeTransactions:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fGetTableHandle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGetTableHandle Procedure 
FUNCTION fGetTableHandle RETURNS HANDLE
  ( ipcProgramID AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    CASE ipcProgramID:
        /* Machine Transactions.rpa */
        WHEN "r-mchtrn." THEN
        RETURN TEMP-TABLE ttMachineTransactions:HANDLE.
    END CASE.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-fMachineTransactions) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fMachineTransactions Procedure 
FUNCTION fMachineTransactions RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Machine Transactions.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttMachineTransactions.

    RUN pMachineTransactions (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttMachineTransactions:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

