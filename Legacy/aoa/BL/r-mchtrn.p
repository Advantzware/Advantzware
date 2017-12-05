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
DEFINE VARIABLE dMSF        AS DECIMAL   NO-UNDO.

/* subject business logic */
{aoa/includes/shiftStartEndTime.i}

IF dtStartMachTranDate EQ dtEndMachTranDate AND
   iShiftEndTime LT iShiftStartTime THEN
iShiftEndTime = 86400.

FOR EACH machtran NO-LOCK
    WHERE machtran.company    EQ ipcCompany
      AND machtran.machine    GE cStartMachine
      AND machtran.machine    LE cEndMachine
      AND machtran.start_date GE dtStartMachTranDate
      AND machtran.start_date LE dtEndMachTranDate
      AND machtran.shift      GE STRING(iStartShift)
      AND machtran.shift      LE STRING(iEndShift)
      AND DATETIME(machtran.start_date,machtran.start_time * 1000) GE DATETIME(dtStartMachTranDate,iShiftStartTime)
      AND DATETIME(machtran.start_date,machtran.start_time * 1000) LE DATETIME(dtEndMachTranDate,iShiftEndTime)
    BREAK BY machtran.machine
          BY machtran.job_number
          BY machtran.job_sub
          BY machtran.form_number
          BY machtran.blank_number
    :
    IF FIRST-OF(machtran.blank_number) THEN
    RUN pGetCustInfo (BUFFER machtran, OUTPUT cCustPartNo, OUTPUT cCustName, OUTPUT cItemFG).
    
    dMSF = 0.
    FIND FIRST mach NO-LOCK 
         WHERE mach.company EQ machtran.company
           AND mach.m-code  EQ machtran.machine
         NO-ERROR.
    IF AVAILABLE mach THEN DO:
        FIND FIRST job NO-LOCK
             WHERE job.company EQ machtran.company
               AND job.job-no  EQ machtran.job_number
               AND job.job-no2 EQ machtran.job_sub
             NO-ERROR.
        IF AVAILABLE job AND TRIM (job.est-no) NE "" THEN DO:
            FIND FIRST est NO-LOCK
                 WHERE est.company EQ job.company
                   AND est.est-no  EQ job.est-no
                 NO-ERROR.
            IF AVAILABLE est THEN DO:
                FIND FIRST ef NO-LOCK
                     WHERE ef.company EQ est.company
                       AND ef.est-no  EQ est.est-no
                       AND ef.form-no EQ machtran.form_number
                     NO-ERROR.
                IF AVAILABLE ef THEN DO:
                    FIND FIRST eb NO-LOCK
                         WHERE eb.company   EQ ef.company
                           AND eb.est-no    EQ ef.est-no
                           AND eb.form-no   EQ ef.form-no
                           AND (eb.blank-no EQ machtran.blank_number
                            OR  machtran.blank_number EQ 0)
                         NO-ERROR.
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
                END. /* avail ef */
            END. /* avail est */
        END. /* avail job */
    END. /* avail mach */  

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
