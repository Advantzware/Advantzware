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
    FIELD custName     AS CHARACTER                LABEL "Customer"  FORMAT "x(30)"
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
        INDEX sortBy IS PRIMARY rowType xxSort
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

/* Machine Transaction Summary.rpa */
DEFINE TEMP-TABLE ttMachineTransactionSummary NO-UNDO
    {aoaAppSrv/ttFields.i}
    FIELD machine          AS CHARACTER LABEL "Machine"       FORMAT "x(6)"
    FIELD jobNumber        AS CHARACTER LABEL "Job"           FORMAT "x(10)"
    FIELD mrHours          AS DECIMAL   LABEL "MR Hrs"        FORMAT ">>>9.99"
    FIELD runHours         AS DECIMAL   LABEL "Run Hrs"       FORMAT ">>>9.99"
    FIELD dtHours          AS DECIMAL   LABEL "DT Hrs"        FORMAT ">>>9.99"
    FIELD totalHours       AS DECIMAL   LABEL "Tot Hrs"       FORMAT ">>>9.99"
    FIELD laborHours       AS DECIMAL   LABEL "Lab Hrs"       FORMAT ">>>9.99"
    FIELD netPieces        AS INTEGER   LABEL "Net Pieces"    FORMAT ">>>9"
    FIELD piecesPerHour    AS INTEGER   LABEL "Pieces Hr"     FORMAT ">>>9"
    FIELD numberOn         AS INTEGER   LABEL "Number On"     FORMAT ">>>9"
    FIELD kicksPerHour     AS INTEGER   LABEL "Kicks Hr"      FORMAT ">>>9"
    FIELD piecesPerManHour AS INTEGER   LABEL "Pieces Man Hr" FORMAT ">>>9"
    FIELD mrWaste          AS INTEGER   LABEL "MR Waste"      FORMAT ">>>9"
    FIELD runWaste         AS INTEGER   LABEL "Run Waste"     FORMAT ">>>9"
    FIELD totalWaste       AS INTEGER   LABEL "Tot Waste"     FORMAT ">>>9"
    FIELD wastePct         AS DECIMAL   LABEL "Waste Pct"     FORMAT ">>>9.99"
        INDEX sortBy IS PRIMARY rowType machine jobNumber
        .
/* Machine Transaction Summary.rpa */

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

&IF DEFINED(EXCLUDE-fMachineTransactionSummary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fMachineTransactionSummary Procedure 
FUNCTION fMachineTransactionSummary RETURNS HANDLE
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
    FIND FIRST shifts NO-LOCK
         WHERE shifts.company EQ ipcCompany
           AND shifts.shift   EQ STRING(iStartShift)
         NO-ERROR.
    IF AVAILABLE shifts THEN
    ASSIGN
        iStartTime = shifts.start_time
        iEndTime = shifts.end_time
        .
    
    IF iStartShift NE iEndShift THEN DO:
        FIND FIRST shifts NO-LOCK
             WHERE shifts.company EQ ipcCompany
               AND shifts.shift   EQ STRING(iEndShift)
             NO-ERROR.
        IF AVAILABLE shifts THEN
        iEndTime = shifts.end_time.
    END. /* different shifts */

    IF dtStartMachTranDate EQ dtEndMachTranDate AND
       iEndTime LT iStartTime THEN
    iEndTime = 86400.
    
    FOR EACH machtran NO-LOCK
        WHERE machtran.company    EQ ipcCompany
          AND machtran.machine    GE cStartMachine
          AND machtran.machine    LE cEndMachine
          AND machtran.start_date GE dtStartMachTranDate
          AND machtran.start_date LE dtEndMachTranDate
          AND machtran.shift      GE STRING(iStartShift)
          AND machtran.shift      LE STRING(iEndShift)
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
            RELEASE cust.
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
                FIND FIRST cust NO-LOCK
                     WHERE cust.company EQ job-hdr.company
                       AND cust.cust-no EQ job-hdr.cust-no
                     NO-ERROR.
                IF cCustPartNo EQ "" THEN DO:
                    FIND FIRST itemfg NO-LOCK
                         WHERE itemfg.company EQ machtran.company
                           AND itemfg.i-no    EQ job-hdr.i-no
                         NO-ERROR.
                    IF AVAILABLE itemfg THEN
                    cCustPartNo = itemfg.part-no.
                END. /* if cust part blank */
            END. /* avail job-mch */
        END. /* if first-of */
        CREATE ttMachineTransactions.
        ASSIGN
            ttMachineTransactions.machine      = machtran.machine
            ttMachineTransactions.custPartNo   = cCustPartNo
            ttMachineTransactions.custName     = IF AVAILABLE cust THEN cust.name ELSE ""
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
                                                 STRING(machtran.start_date,"99/99/9999")
                                               + STRING(machtran.start_time,"99999")
                                               + machtran.machine
                                            ELSE IF cSort EQ "Start Date / Job#" THEN
                                                 STRING(machtran.start_date,"99/99/9999")
                                               + machtran.job_number
                                               + machtran.machine
                                               + STRING(machtran.start_time,"99999")
                                            ELSE STRING(machtran.machine,"x(6)")
                                               + STRING(machtran.start_date,"99/99/9999")
                                               + STRING(machtran.start_time,"99999")
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

&IF DEFINED(EXCLUDE-pMachineTransactionSummary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMachineTransactionSummary Procedure 
PROCEDURE pMachineTransactionSummary :
/*------------------------------------------------------------------------------
  Purpose:     Machine Transaction Summary.rpa
  Parameters:  Company, Batch Seq, User ID
  Notes:       
------------------------------------------------------------------------------*/
    {aoaAppSrv/pMachineTransactionSummary.i}
    
    /* local variables */
    DEFINE VARIABLE cJobNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartTime AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime   AS INTEGER   NO-UNDO.
    
    /* subject business logic */
    ASSIGN
        iStartTime = 0
        iEndTime   = 86400
        .
    FIND FIRST shifts NO-LOCK
         WHERE shifts.company EQ ipcCompany
           AND shifts.shift   EQ STRING(iStartShift)
         NO-ERROR.
    IF AVAILABLE shifts THEN
    iStartTime = shifts.start_time.
    
    IF iStartShift NE iEndShift THEN DO:
        FIND FIRST shifts NO-LOCK
             WHERE shifts.company EQ ipcCompany
               AND shifts.shift   EQ STRING(iEndShift)
             NO-ERROR.
        IF AVAILABLE shifts THEN
        iEndTime = shifts.end_time.
    END. /* different shifts */
    
    FOR EACH machtran NO-LOCK
        WHERE machtran.company    EQ ipcCompany
          AND machtran.machine    GE cStartMachine
          AND machtran.machine    LE cEndMachine
          AND machtran.start_date GE dtStartMachTranDate
          AND machtran.start_date LE dtEndMachTranDate
          AND machtran.shift      GE STRING(iStartShift)
          AND machtran.shift      LE STRING(iEndShift)
          AND DATETIME(machtran.start_date,machtran.start_time)
           GE DATETIME(dtStartMachTranDate,iStartTime)
          AND DATETIME(machtran.start_date,machtran.start_time)
           LE DATETIME(dtEndMachTranDate,iEndTime)
        :
        cJobNo = machtran.job_number + "-" + STRING(machtran.job_sub,"99").
        FIND FIRST ttMachineTransactionSummary
             WHERE ttMachineTransactionSummary.machine   EQ machtran.machine
               AND ttMachineTransactionSummary.jobNumber EQ cJobNo
             NO-ERROR.
        IF NOT AVAILABLE ttMachineTransactionSummary THEN DO:
            CREATE ttMachineTransactionSummary.
            ASSIGN
                ttMachineTransactionSummary.machine   = machtran.machine
                ttMachineTransactionSummary.jobNumber = cJobNo
                .
        END. /* not avail */
        ASSIGN
            ttMachineTransactionSummary.mrHours          = ttMachineTransactionSummary.mrHours
                                                         + 0
            ttMachineTransactionSummary.runHours         = ttMachineTransactionSummary.runHours
                                                         + 0
            ttMachineTransactionSummary.dtHours          = ttMachineTransactionSummary.dtHours
                                                         + 0
            ttMachineTransactionSummary.totalHours       = ttMachineTransactionSummary.totalHours
                                                         + 0
            ttMachineTransactionSummary.laborHours       = ttMachineTransactionSummary.laborHours
                                                         + 0
            ttMachineTransactionSummary.netPieces        = ttMachineTransactionSummary.netPieces
                                                         + 0
            ttMachineTransactionSummary.piecesPerHour    = ttMachineTransactionSummary.piecesPerHour
                                                         + 0
            ttMachineTransactionSummary.numberOn         = ttMachineTransactionSummary.numberOn
                                                         + 0
            ttMachineTransactionSummary.kicksPerHour     = ttMachineTransactionSummary.kicksPerHour
                                                         + 0
            ttMachineTransactionSummary.piecesPerManHour = ttMachineTransactionSummary.piecesPerManHour
                                                         + 0
            ttMachineTransactionSummary.mrWaste          = ttMachineTransactionSummary.mrWaste
                                                         + 0
            ttMachineTransactionSummary.runWaste         = ttMachineTransactionSummary.runWaste
                                                         + 0
            ttMachineTransactionSummary.totalWaste       = ttMachineTransactionSummary.totalWaste
                                                         + 0
            ttMachineTransactionSummary.wastePct         = ttMachineTransactionSummary.wastePct
                                                         + 0
            .
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
        /* Machine Transaction Summary.rpa */
        WHEN "mtransum." THEN
        RETURN TEMP-TABLE ttMachineTransactionSummary:HANDLE.
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

&IF DEFINED(EXCLUDE-fMachineTransactionSummary) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fMachineTransactionSummary Procedure 
FUNCTION fMachineTransactionSummary RETURNS HANDLE
  ( ipcCompany AS CHARACTER,
    ipiBatch   AS INTEGER,
    ipcUserID  AS CHARACTER ) :
/*------------------------------------------------------------------------------
  Purpose:  Machine Transaction Summary.rpa
    Notes:  
------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE ttMachineTransactionSummary.

    RUN pMachineTransactionSummary (ipcCompany, ipiBatch, ipcUserID).

    RETURN TEMP-TABLE ttMachineTransactionSummary:HANDLE .

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

