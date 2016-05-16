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
    FIELD machine      LIKE machtran.machine
    FIELD jobNumber    LIKE machtran.job_number
    FIELD jobSub       LIKE machtran.job_sub
    FIELD formNumber   LIKE machtran.form_number
    FIELD blankNumber  LIKE machtran.blank_number
    FIELD passSequence LIKE machtran.pass_sequence
    FIELD chargeCode   LIKE machtran.charge_code
    FIELD startDate    LIKE machtran.start_date
    FIELD startTime    AS CHARACTER LABEL "Log In"  FORMAT "hh:mm am"
    FIELD endDate      LIKE machtran.end_date
    FIELD endTime      AS CHARACTER LABEL "Log Out" FORMAT "hh:mm am"
    FIELD shift        LIKE machtran.shift
    FIELD totalTime    AS CHARACTER LABEL "Total"   FORMAT "hh:mm am"
    FIELD runQty       LIKE machtran.run_qty
    FIELD wasteQty     LIKE machtran.waste_qty
    FIELD xxRecKey     LIKE machtran.rec_key
    FIELD xxSort       AS CHARACTER LABEL "Sort"    FORMAT "x(100)"
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

    /* subject business logic */
    FOR EACH machtran NO-LOCK
        WHERE machtran.company EQ ipcCompany
          AND machtran.machine GE cStartMachine
          AND machtran.machine LE cEndMachine
          AND machtran.start_date GE dtStartMachTranDate
          AND machtran.start_date LE dtEndMachTranDate
        :
        CREATE ttMachineTransactions.
        ASSIGN
            ttMachineTransactions.machine      = machtran.machine
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
            ttMachineTransactions.runQty       = machtran.run_qty
            ttMachineTransactions.wasteQty     = machtran.waste_qty
            ttMachineTransactions.xxRecKey     = machtran.rec_key
            ttMachineTransactions.xxSort       = IF cSort EQ "Start Date / Time" THEN
                                                 STRING(machtran.start_date)
                                               + STRING(machtran.start_time)
                                               + machtran.machine
                                               + machtran.job_number
                                               + STRING(machtran.end_date)
                                               + STRING(machtran.end_time)
                                            ELSE IF cSort EQ "Start Date / Job#" THEN
                                                 STRING(machtran.start_date)
                                               + machtran.job_number
                                               + machtran.machine
                                               + STRING(machtran.start_time)
                                               + STRING(machtran.end_date)
                                               + STRING(machtran.end_time)
                                            ELSE machtran.machine
                                               + STRING(machtran.start_date)
                                               + STRING(machtran.start_time)              
                                               + machtran.job_number
                                               + STRING(machtran.end_date)
                                               + STRING(machtran.end_time)
            .
        IF lShowEmployeeTrans THEN
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

