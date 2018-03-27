/*------------------------------------------------------------------------
  File: mtransum.p
  Description: Business Logic
*/

/* ***************************  Definitions  ***************************/

/* Machine Transaction Summary.rpa */
{aoa/tempTable/ttMachineTransactionSummary.i}

/* Parameters Definitions ---                                           */
DEFINE OUTPUT PARAMETER TABLE FOR ttMachineTransactionSummary.
{aoa/includes/pMachineTransactionSummary.i}

/* local variables */
DEFINE VARIABLE cCustPartNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustName   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cItemFG     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJobNo      AS CHARACTER NO-UNDO.

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
    :
    cJobNo = machtran.job_number + "-" + STRING(machtran.job_sub,"99").
    FIND FIRST ttMachineTransactionSummary
         WHERE ttMachineTransactionSummary.machine   EQ machtran.machine
           AND ttMachineTransactionSummary.jobNumber EQ cJobNo
         NO-ERROR.
    IF NOT AVAILABLE ttMachineTransactionSummary THEN DO:
        RUN pGetCustInfo (BUFFER machtran, OUTPUT cCustPartNo, OUTPUT cCustName, OUTPUT cItemFG).
        CREATE ttMachineTransactionSummary.
        ASSIGN
            ttMachineTransactionSummary.machine    = machtran.machine
            ttMachineTransactionSummary.custPartNo = cCustPartNo
            ttMachineTransactionSummary.custName   = cCustName
            ttMachineTransactionSummary.jobNumber  = cJobNo
            .
    END. /* not avail */
    FIND FIRST job-code NO-LOCK
         WHERE job-code.code EQ machtran.charge_code
         NO-ERROR.
    IF NOT AVAILABLE job-code THEN NEXT.

    CASE job-code.cat:
        WHEN "MR" THEN
        ASSIGN
            ttMachineTransactionSummary.mrHours = ttMachineTransactionSummary.mrHours
                                                + machtran.total_time / 3600
            ttMachineTransactionSummary.mrWaste = ttMachineTransactionSummary.mrWaste
                                                + machtran.waste_qty
            .
        WHEN "RUN" THEN DO:
            ASSIGN
                ttMachineTransactionSummary.runHours = ttMachineTransactionSummary.runHours
                                                     + machtran.total_time / 3600
                ttMachineTransactionSummary.runWaste = ttMachineTransactionSummary.runWaste
                                                     + machtran.waste_qty
                .
            FIND FIRST job NO-LOCK
                 WHERE job.company EQ ipcCompany
                   AND job.job-no  EQ SUBSTRING(cJobNo,1,6)
                   AND job.job-no2 EQ INTEGER(SUBSTRING(cJobNo,8))
                 NO-ERROR.
            IF AVAILABLE job THEN DO:
                FIND FIRST eb NO-LOCK
                     WHERE eb.company  EQ ipcCompany
                       AND eb.est-no   EQ job.est-no
                       AND eb.stock-no EQ cItemFG
                     NO-ERROR.
                IF AVAILABLE eb THEN
                ttMachineTransactionSummary.numberOn = ttMachineTransactionSummary.numberOn + eb.num-up.
            END. /* avail job */

        END. /* run */
        OTHERWISE
        ASSIGN
            ttMachineTransactionSummary.dtHours = ttMachineTransactionSummary.dtHours
                                                + machtran.total_time / 3600
            ttMachineTransactionSummary.mrWaste = ttMachineTransactionSummary.mrWaste
                                                + machtran.waste_qty
            .
    END CASE.

    FOR EACH machemp NO-LOCK
        WHERE machemp.table_rec_key EQ machtran.rec_key
        :
        ttMachineTransactionSummary.laborHours = ttMachineTransactionSummary.laborHours
                                               + machemp.total_time / 3600.
    END. /* each machemp */

    ASSIGN
        ttMachineTransactionSummary.totalHours       = ttMachineTransactionSummary.mrHours
                                                     + ttMachineTransactionSummary.runHours
                                                     + ttMachineTransactionSummary.dtHours
        ttMachineTransactionSummary.netPieces        = ttMachineTransactionSummary.netPieces
                                                     + machtran.run_qty
        ttMachineTransactionSummary.piecesPerHour    = ttMachineTransactionSummary.netPieces
                                                     / ttMachineTransactionSummary.totalHours
        ttMachineTransactionSummary.kicks            = ttMachineTransactionSummary.netPieces
                                                     / ttMachineTransactionSummary.numberOn
        ttMachineTransactionSummary.piecesPerManHour = ttMachineTransactionSummary.netPieces
                                                     / ttMachineTransactionSummary.laborHours
        ttMachineTransactionSummary.totalWaste       = ttMachineTransactionSummary.mrWaste
                                                     + ttMachineTransactionSummary.runWaste
        ttMachineTransactionSummary.wastePct         = (ttMachineTransactionSummary.totalWaste
                                                     / (ttMachineTransactionSummary.totalWaste
                                                     +  ttMachineTransactionSummary.netPieces))
                                                     * 100
        .  
    IF ttMachineTransactionSummary.kicks            EQ ? THEN ttMachineTransactionSummary.kicks            = 0.
    IF ttMachineTransactionSummary.piecesPerHour    EQ ? THEN ttMachineTransactionSummary.piecesPerHour    = 0.
    IF ttMachineTransactionSummary.piecesPerManHour EQ ? THEN ttMachineTransactionSummary.piecesPerManHour = 0.
    IF ttMachineTransactionSummary.wastePct         EQ ? THEN ttMachineTransactionSummary.wastePct         = 0.
END. /* each machtran */

{aoa/BL/pGetCustInfo.i}
