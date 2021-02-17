/*------------------------------------------------------------------------
  File:         r-mchtrn.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 4.10.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttMachineTransactions
{AOA/tempTable/ttMachineTransactions.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 8
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cCustPartNo     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustName       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemFG         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dMSF            AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iShiftStartTime AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iShiftEndTime   AS INTEGER   NO-UNDO INITIAL 86400.
    
    DEFINE QUERY qMachTran FOR machtran, mach, job, est, ef, eb SCROLLING.
    DEFINE QUERY qMachEmp  FOR machemp, employee.
    
    RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
    SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).
    
    RUN calcShiftStartTime (
        cCompany,
        lUseTimes,
        cStartShift,
        cStartTime,
        OUTPUT iShiftStartTime
        ).
    RUN calcShiftEndTime (
        cCompany,
        lUseTimes,
        cStartShift,
        cEndShift,
        cEndTime,
        OUTPUT iShiftStartTime
        ).
    IF dtStartTransDate EQ dtEndTransDate  AND
       iShiftEndTime    LT iShiftStartTime THEN
    iShiftEndTime = 86400.
    
    OPEN QUERY qMachTran
    FOR EACH machtran NO-LOCK
        WHERE machtran.company    EQ cCompany
          AND machtran.machine    GE cStartMachine
          AND machtran.machine    LE cEndMachine
          AND machtran.start_date GE dtStartTransDate
          AND machtran.start_date LE dtEndTransDate
          AND machtran.shift      GE cStartShift
          AND machtran.shift      LE cEndShift
          AND DATETIME(machtran.start_date,machtran.start_time * 1000) GE DATETIME(dtStartTransDate,iShiftStartTime)
          AND DATETIME(machtran.end_date,  machtran.end_time   * 1000) LE DATETIME(dtEndTransDate,  iShiftEndTime),
        FIRST mach NO-LOCK
        WHERE mach.company EQ machtran.company
          AND mach.m-code  EQ machtran.machine,
        FIRST job NO-LOCK
        WHERE job.company EQ machtran.company
          AND job.job-no  EQ machtran.job_number
          AND job.job-no2 EQ machtran.job_sub
          AND job.est-no  NE "",
        FIRST est NO-LOCK OUTER-JOIN 
        WHERE est.company EQ job.company
          AND est.est-no  EQ job.est-no,
        FIRST ef NO-LOCK OUTER-JOIN
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ machtran.form_number,
        FIRST eb NO-LOCK OUTER-JOIN
        WHERE eb.company   EQ ef.company
          AND eb.est-no    EQ ef.est-no
          AND eb.form-no   EQ ef.form-no
          AND (eb.blank-no EQ machtran.blank_number
           OR  machtran.blank_number EQ 0)
        BREAK BY machtran.machine
              BY machtran.job_number
              BY machtran.job_sub
              BY machtran.form_number
              BY machtran.blank_number
        .
    GET FIRST qMachTran.
    DO WHILE AVAILABLE machtran:
        /* level 5 = machtran.blank_number */
        IF QUERY qMachTran:FIRST-OF(5) THEN
        RUN pGetCustInfo (BUFFER machtran, OUTPUT cCustPartNo, OUTPUT cCustName, OUTPUT cItemFG).
        
        dMSF = 0.
        IF AVAILABLE eb THEN 
        CASE mach.p-type:
            WHEN "B" THEN 
                IF AVAILABLE eb THEN 
                dMSF = machtran.run_qty * eb.t-wid * eb.t-len     / 1000 / 144.
            WHEN "R" THEN
                IF AVAILABLE ef THEN 
                dMSF = machtran.run_qty * ef.gsh-wid * ef.gsh-len / 1000 / 144.
            WHEN "S" THEN
                IF AVAILABLE ef THEN
                dMSF = machtran.run_qty * ef.nsh-wid * ef.nsh-len / 1000 / 144.
        END. /* case */
    
        CREATE ttMachineTransactions.
        ASSIGN
            ttMachineTransactions.machine        = machtran.machine
            ttMachineTransactions.custPartNo     = cCustPartNo
            ttMachineTransactions.custName       = cCustName
            ttMachineTransactions.jobNumber      = machtran.job_number
            ttMachineTransactions.jobSub         = machtran.job_sub
            ttMachineTransactions.formNumber     = machtran.form_number
            ttMachineTransactions.blankNumber    = machtran.blank_number
            ttMachineTransactions.passsequence   = machtran.pass_sequence
            ttMachineTransactions.chargeCode     = machtran.charge_code
            ttMachineTransactions.startDate      = machtran.start_date
            ttMachineTransactions.startTime      = STRING(machtran.start_time,"hh:mm am")
            ttMachineTransactions.endDate        = machtran.end_date
            ttMachineTransactions.endTime        = STRING(machtran.end_time,"hh:mm am")
            ttMachineTransactions.shift          = machtran.shift
            ttMachineTransactions.msf            = dMSF                    
            ttMachineTransactions.totalTime      = machtran.total_time / 3600
            ttMachineTransactions.zzTotalTime    = machtran.total_time
            ttMachineTransactions.runQty         = machtran.run_qty
            ttMachineTransactions.wasteQty       = machtran.waste_qty
            ttMachineTransactions.runComplete    = machtran.completed
            ttMachineTransactions.xxRecKey       = machtran.rec_key
            ttMachineTransactions.loginDateTime  = TRIM(STRING(machtran.start_date) + " " + STRING(machtran.start_time,"hh:mm am"))
            ttMachineTransactions.logoutDateTime = TRIM(STRING(machtran.end_date) + " " + STRING(machtran.end_time,"hh:mm am"))                                
            iCount = iCount + 1
            .
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
        GET NEXT qMachTran.
    END. /* do while */
    DELETE PROCEDURE hDynCalcField.
END PROCEDURE.

{aoa/BL/pGetCustInfo.i}
