/* machtran.i */

IF CAN-FIND(FIRST machemp WHERE machemp.table_rec_key = machtran.rec_key) THEN
FOR EACH machemp WHERE machemp.table_rec_key = machtran.rec_key EXCLUSIVE-LOCK:
  DELETE machemp.
END.

DEFINE BUFFER bJobMch FOR job-mch.

FIND FIRST bJobMch EXCLUSIVE-LOCK 
     WHERE bJobMch.company EQ machtran.company
       AND bJobMch.m-code EQ machtran.machine
       AND bJobMch.job-no EQ machtran.job_number
       AND bJobMch.job-no2 EQ machtran.job_sub
       AND bJobMch.frm EQ machtran.form_number
       AND bJobMch.blank-no EQ machtran.blank_number
       AND bJobMch.pass EQ machtran.pass_sequence NO-ERROR.
IF AVAILABLE bJobMch THEN DO:
  IF machtran.charge_code EQ 'MR' THEN bJobMch.mr-complete = NO.
  IF machtran.charge_code EQ 'Run' THEN bJobMch.run-complete = NO.
END. /* if avail */
