/*------------------------------------------------------------------------
  File: r-mchtrn.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Machine Transactions.rpa */
{aoa/tempTable/ttMachineTransactions.i}
{aoa/tempTable/ttMachineEmployeeTransactions.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttMachineTransactions.
DEFINE OUTPUT PARAMETER TABLE FOR ttMachineEmployeeTransactions.
{aoa/includes/pMachineTransactions.i}

/* local variables */
DEFINE VARIABLE cCustPartNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemFG     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO INITIAL 86400.

/* subject business logic */
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
    IF FIRST-OF(machtran.blank_number) THEN
    RUN pGetCustInfo (BUFFER machtran, OUTPUT cCustPartNo, OUTPUT cCustName, OUTPUT cItemFG).
    
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
        ttMachineTransactions.totalTime      = machtran.total_time / 3600
        ttMachineTransactions.xxTotalTime    = machtran.total_time
        ttMachineTransactions.runQty         = machtran.run_qty
        ttMachineTransactions.wasteQty       = machtran.waste_qty
        ttMachineTransactions.runComplete    = machtran.completed
        ttMachineTransactions.xxRecKey       = machtran.rec_key
        ttMachineTransactions.xxSort         = IF cSort EQ "Start Date / Time" THEN
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
        ttMachineTransactions.loginDateTime  = TRIM(STRING(machtran.start_date) + " " + STRING(machtran.start_time,"hh:mm am"))
        ttMachineTransactions.logoutDateTime = TRIM(STRING(machtran.end_date) + " " + STRING(machtran.end_time,"hh:mm am"))                                
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
            ttMachineEmployeeTransactions.totalTime = machemp.total_time / 3600
            ttMachineEmployeeTransactions.shift     = machemp.shift
            ttMachineEmployeeTransactions.rateUsage = machemp.rate_usage
            ttMachineEmployeeTransactions.rateType  = machemp.ratetype
            ttMachineEmployeeTransactions.rate      = machemp.rate
            ttMachineEmployeeTransactions.loginDateTime  = TRIM(STRING(machemp.start_date) + " " + STRING(machemp.start_time,"hh:mm am"))
            ttMachineEmployeeTransactions.logoutDateTime = TRIM(STRING(machemp.end_date) + " " + STRING(machemp.end_time,"hh:mm am"))   
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

{aoa/BL/pGetCustInfo.i}
