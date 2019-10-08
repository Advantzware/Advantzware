/* asiDC.p */

DISABLE TRIGGERS FOR LOAD OF machtran.

DEFINE INPUT PARAMETER ipRowIDs AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipJobRowID AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipTable AS CHARACTER NO-UNDO.

DEFINE OUTPUT PARAMETER opStartDate AS DATE NO-UNDO.
DEFINE OUTPUT PARAMETER opStartTime AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER opEndDate AS DATE NO-UNDO INITIAL ?.
DEFINE OUTPUT PARAMETER opEndTime AS INTEGER NO-UNDO INITIAL ?.
DEFINE OUTPUT PARAMETER opJobLocked AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opJobCompleted AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opMRCompleted AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opRunCompleted AS LOGICAL NO-UNDO.

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}

DEFINE VARIABLE chargeCodes AS CHARACTER NO-UNDO.
DEFINE VARIABLE downtimeID AS INTEGER NO-UNDO.
DEFINE VARIABLE jobMchRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvResource AS CHARACTER NO-UNDO.
DEFINE VARIABLE transDate AS DATE NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE j AS INTEGER NO-UNDO.

jobMchRowID = TO-ROWID(ENTRY(2,ipRowIDs)).
FIND job-mch NO-LOCK WHERE ROWID(job-mch) EQ jobMchRowID NO-ERROR.
IF NOT AVAILABLE job-mch THEN RETURN.

IF ipTable EQ 'ttblJob' THEN
FIND ttblJob NO-LOCK WHERE ROWID(ttblJob) EQ ipJobRowID NO-ERROR.

IF ipTable EQ 'pendingJob' THEN
FIND pendingJob NO-LOCK WHERE ROWID(pendingJob) EQ ipJobRowID NO-ERROR.

lvResource = IF AVAILABLE ttblJob THEN ttblJob.resource
        ELSE IF AVAILABLE pendingJob THEN pendingJob.resource
        ELSE job-mch.m-code.

/* need to make two passes, client might post trans to virtual or actual machine */
j = IF lvResource EQ job-mch.m-code THEN 1 ELSE 2.
DO i = 1 TO j:
  IF i EQ 2 THEN lvResource = job-mch.m-code.
  FOR EACH machtran NO-LOCK
      WHERE machtran.company EQ job-mch.company
        AND machtran.machine EQ lvResource /* job-mch.m-code */
        AND machtran.job_number EQ job-mch.job-no
        AND machtran.job_sub EQ job-mch.job-no2
        AND machtran.form_number EQ job-mch.frm
        AND machtran.blank_number EQ job-mch.blank-no
        AND machtran.pass_sequence EQ job-mch.pass:
    IF CAN-FIND(FIRST job-code WHERE job-code.code EQ machtran.charge_code
                                 AND job-code.cat EQ 'MR') OR
       CAN-FIND(FIRST job-code WHERE job-code.code EQ machtran.charge_code
                                 AND job-code.cat EQ 'Run') THEN DO:
      IF opJobLocked EQ NO THEN
      ASSIGN
        opStartDate = machtran.start_date
        opStartTime = machtran.start_time
        opJobLocked = YES
        .
    END.
    ELSE DO:
      ASSIGN
        lvStartDate = machtran.start_date
        lvEndDate = IF machtran.end_date NE ? THEN machtran.end_date ELSE machtran.start_date
        .
      DO transDate = lvStartDate TO lvEndDate:
        ASSIGN
          downtimeID = 13 + WEEKDAY(DATE(MONTH(transDate),1,YEAR(transDate))) + DAY(transDate)
          lvStartTime = IF transDate NE machtran.start_date THEN 0 ELSE machtran.start_time
          lvEndTime = IF transDate NE machtran.end_date OR machtran.end_date EQ ? THEN 86400
                      ELSE machtran.end_time
          .
        FIND FIRST ttblDowntime EXCLUSIVE-LOCK
             WHERE ttblDowntime.resource EQ lvResource /* job-mch.m-code */
               AND ttblDowntime.startDate EQ transDate
               AND ttblDowntime.dayID EQ downtimeID
               AND ttblDowntime.startTime EQ lvStartTime
             NO-ERROR.
        IF NOT AVAILABLE ttblDowntime THEN DO:
          RUN copyDowntime (transDate,machtran.machine,downtimeID).
          CREATE ttblDowntime.
          ASSIGN
            ttblDowntime.resource = machtran.machine
            ttblDowntime.startDate = transDate
            ttblDowntime.dayID = downtimeID
            ttblDowntime.startTime = lvStartTime
            .
        END. /* if not avail */
        ttblDowntime.endTime = lvEndTime.
      END. /* do transdate */
    END. /* if can-find else */
  END. /* each machtran */

  FOR EACH job-code NO-LOCK WHERE job-code.cat EQ 'MR':
    FIND LAST machtran NO-LOCK
         WHERE machtran.company EQ job-mch.company
           AND machtran.machine EQ lvResource /* job-mch.m-code */
           AND machtran.job_number EQ job-mch.job-no
           AND machtran.job_sub EQ job-mch.job-no2
           AND machtran.form_number EQ job-mch.frm
           AND machtran.blank_number EQ job-mch.blank-no
           AND machtran.pass_sequence EQ job-mch.pass
           AND machtran.charge_code EQ job-code.code
         NO-ERROR.
    IF AVAILABLE machtran THEN DO:
      opMRCompleted = machtran.completed.
      LEAVE.
    END. /* avail machtran */
  END. /* each job-code */

  ASSIGN
    opEndDate = ?
    opEndTime = ?.
  FOR EACH job-code NO-LOCK WHERE job-code.cat EQ 'Run':
    FIND LAST machtran NO-LOCK
         WHERE machtran.company EQ job-mch.company
           AND machtran.machine EQ lvResource /* job-mch.m-code */
           AND machtran.job_number EQ job-mch.job-no
           AND machtran.job_sub EQ job-mch.job-no2
           AND machtran.form_number EQ job-mch.frm
           AND machtran.blank_number EQ job-mch.blank-no
           AND machtran.pass_sequence EQ job-mch.pass
           AND machtran.charge_code EQ job-code.code
         NO-ERROR.
    IF AVAILABLE machtran AND machtran.completed THEN DO:
      ASSIGN
        opEndDate = machtran.end_date
        opEndTime = machtran.end_time
        opMRCompleted = YES
        opRunCompleted = YES
        .
      LEAVE.
    END. /* avail machtran */
  END. /* each job-code */
END. /* do i */

opJobCompleted = opMRCompleted AND opRunCompleted.

/* this block corrects entries placed against a virual machine (schedule machine)
   vs. the actual machine found in the job routing */
IF job-mch.m-code NE lvResource THEN
FOR EACH machtran EXCLUSIVE-LOCK
    WHERE machtran.company EQ job-mch.company
      AND machtran.machine EQ lvResource /* job-mch.m-code */
      AND machtran.job_number EQ job-mch.job-no
      AND machtran.job_sub EQ job-mch.job-no2
      AND machtran.form_number EQ job-mch.frm
      AND machtran.blank_number EQ job-mch.blank-no
      AND machtran.pass_sequence EQ job-mch.pass
      AND machtran.posted EQ NO
      AND machtran.end_date NE ?
    :
  machtran.machine = job-mch.m-code.
END. /* each machtran */

PROCEDURE copyDowntime:
  DEFINE INPUT PARAMETER ipDate AS DATE NO-UNDO.
  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipDayID AS INTEGER NO-UNDO.

  DEFINE VARIABLE foundDowntime AS LOGICAL NO-UNDO.
  
  IF CAN-FIND(FIRST ttblDowntime
              WHERE ttblDowntime.resource EQ ipResource
                AND ttblDowntime.startDate EQ ipDate
                AND ttblDowntime.dayID EQ ipDayID) THEN RETURN.
  FOR EACH ttblDowntime NO-LOCK
      WHERE ttblDowntime.resource EQ ipResource
        AND ttblDowntime.startDate EQ ?
        AND ttblDowntime.dayID EQ WEEKDAY(ipDate) + 7
      :
    CREATE buffDowntime.
    ASSIGN
      buffDowntime.dayID = ipDayID
      buffDowntime.resource = ipResource
      buffDowntime.startDate = ipDate
      buffDowntime.startTime = ttblDowntime.startTime
      buffDowntime.endTime = ttblDowntime.endTime
      foundDowntime = YES
      .
  END. /* each ttbldowntime */
  IF NOT foundDowntime THEN
  FOR EACH ttblDowntime NO-LOCK
      WHERE ttblDowntime.resource EQ '<Calendar>'
        AND ttblDowntime.startDate EQ ?
        AND ttblDowntime.dayID EQ WEEKDAY(ipDate):
    CREATE buffDowntime.
    ASSIGN
      buffDowntime.dayID = ipDayID
      buffDowntime.resource = ipResource
      buffDowntime.startDate = ipDate
      buffDowntime.startTime = ttblDowntime.startTime
      buffDowntime.endTime = ttblDowntime.endTime
      .
  END. /* each ttbldowntime */
END PROCEDURE.
