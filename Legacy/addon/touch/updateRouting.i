/* updateRouting.i */

  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipFrm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipBlankNo AS INTEGER NO-UNDO.

  DEFINE BUFFER bMachTran FOR machtran.
  DEFINE BUFFER bJobMch FOR job-mch.
  DEFINE BUFFER bJob FOR job.

  FIND FIRST bJobMch EXCLUSIVE-LOCK 
       WHERE bJobMch.company EQ ipCompany
         AND bJobMch.m-code EQ ipMCode
         AND bJobMch.job-no EQ ipJobNo
         AND bJobMch.job-no2 EQ ipJobNo2
         AND bJobMch.frm EQ ipFrm
         AND bJobMch.blank-no EQ ipBlankNo NO-ERROR.
  IF NOT AVAILABLE bJobMch THEN
  RETURN.

  FOR EACH bMachTran NO-LOCK
      WHERE bMachTran.company EQ ipCompany
        AND bMachTran.machine EQ ipMCode
        AND bMachTran.job_number EQ ipJobNo
        AND bMachTran.job_sub EQ ipJobNo2
        AND bMachTran.form_number EQ ipFrm
        AND bMachTran.blank_number EQ ipBlankNo
        AND (bMachTran.charge_code EQ 'MR'
         OR  bMachTran.charge_code EQ 'RUN')
      BREAK BY bMachTran.charge_code:
    IF FIRST-OF(bMachTran.charge_code) THEN
    CASE bMachTran.charge_code:
      WHEN 'MR' THEN
      DO:
        FIND bJob OF bJobMch EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN
          bJobMch.start-date-su = bMachTran.start_date
          bJobMch.start-time-su = bMachTran.start_time
          bJobMch.end-date-su = ?
          bJobMch.end-time-su = 0
          bJobMch.mr-complete = NO
          bJob.start-date = bMachTran.start_date.
        FIND CURRENT bJob NO-LOCK.
      END.
      WHEN 'RUN' THEN
      ASSIGN
        bJobMch.start-date = bMachTran.start_date
        bJobMch.start-time = bMachTran.start_time
        bJobMch.end-date = ?
        bJobMch.end-time = 0
        bJobMch.run-complete = NO.
    END CASE.
    IF LAST-OF(bMachTran.charge_code) THEN
    CASE bMachTran.charge_code:
      WHEN 'MR' THEN
      ASSIGN
        bJobMch.end-date-su = bMachTran.end_date
        bJobMch.end-time-su = bMachTran.end_time
        bJobMch.mr-complete = bMachTran.completed.
      WHEN 'RUN' THEN
      ASSIGN
        bJobMch.end-date = bMachTran.end_date
        bJobMch.end-time = bMachTran.end_time
        bJobMch.run-complete = bMachTran.completed.
    END CASE.
  END. /* each bMachTran */
  FIND CURRENT bJobMch NO-LOCK.
