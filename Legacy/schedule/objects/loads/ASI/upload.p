/* upload.p - ASI upload process */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE jobMchRowID AS ROWID NO-UNDO.
DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
DEFINE VARIABLE lvResSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE lvJob AS CHARACTER NO-UNDO.
DEFINE VARIABLE statusStr AS CHARACTER NO-UNDO.

FUNCTION numericDateTime RETURNS DECIMAL (ipDate AS DATE,ipTime AS INTEGER):
  {{&includes}/numericDateTime.i}
END FUNCTION.

DISABLE TRIGGERS FOR LOAD OF job.
DISABLE TRIGGERS FOR LOAD OF job-mch.
DISABLE TRIGGERS FOR LOAD OF job-hdr.
DISABLE TRIGGERS FOR LOAD OF reftable.

OUTPUT TO 'schedule/load.log' APPEND.
PUT UNFORMATTED 'Start Save: ' STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss') ' for ' ID ' by ' USERID('nosweat') SKIP.

FOR EACH pendingJob NO-LOCK:
  RELEASE job-mch.
  jobMchRowID = TO-ROWID(ENTRY(2,pendingJob.rowIDs)).
  IF ENTRY(3,pendingJob.rowIDs) NE "None" THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK 
       WHERE job-mch.est-op_rec_key EQ ENTRY(3,pendingJob.rowIDs)
         AND job-mch.company EQ ENTRY(1,pendingJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,pendingJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,pendingJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,pendingJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,pendingJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,pendingJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,pendingJob.keyValue))
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN 
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,pendingJob.keyValue)
         AND job-mch.m-code EQ ENTRY(3,pendingJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,pendingJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,pendingJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,pendingJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,pendingJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,pendingJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,pendingJob.keyValue))
         AND ROWID(job-mch) EQ jobMchRowID
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,pendingJob.keyValue)
         AND job-mch.m-code EQ pendingJob.resource
         AND job-mch.job EQ INTEGER(ENTRY(4,pendingJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,pendingJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,pendingJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,pendingJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,pendingJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,pendingJob.keyValue))
         AND ROWID(job-mch) EQ jobMchRowID
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,pendingJob.keyValue)
         AND job-mch.m-code EQ pendingJob.resource
         AND job-mch.job EQ INTEGER(ENTRY(4,pendingJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,pendingJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,pendingJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,pendingJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,pendingJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,pendingJob.keyValue))
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,pendingJob.keyValue)
         AND job-mch.m-code EQ ENTRY(3,pendingJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,pendingJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,pendingJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,pendingJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,pendingJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,pendingJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,pendingJob.keyValue))
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN 
  FIND FIRST job-mch EXCLUSIVE-LOCK 
       WHERE job-mch.company EQ ENTRY(1,pendingJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,pendingJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,pendingJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,pendingJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,pendingJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,pendingJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,pendingJob.keyValue))
         AND ROWID(job-mch) EQ jobMchRowID
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN NEXT.
  ASSIGN
    statusStr = ''
    job-mch.end-date = ?
    job-mch.end-date-su = ?
    job-mch.end-time = 0
    job-mch.end-time-su = 0
    job-mch.start-date = ?
    job-mch.start-date-su = ?
    job-mch.start-time = 0
    job-mch.start-time-su = 0
    .
  /* only change if not already run-complete */
  IF job-mch.run-complete EQ NO THEN
  job-mch.run-complete = pendingJob.jobCompleted.
  RUN updateJob (job-mch.company,job-mch.job,job-mch.start-date-su).
  DO i = 2 TO NUM-ENTRIES(customValueList):
    IF NOT pendingJob.jobStatus[i - 1] THEN
    statusStr = statusStr + ',' + ENTRY(i,customValueList).
  END.
  RUN setLiveUpdate (job-mch.company,job-mch.job-no,job-mch.job-no2,
                     job-mch.frm,pendingJob.resource,pendingJob.liveUpdate).
  RUN statusNote (jobMchRowID,job-mch.company,pendingJob.resource,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,statusStr).
END. /* each pendingJob */

FOR EACH ttblJob NO-LOCK BREAK BY ttblJob.jobSort BY ttblJob.resourceSequence:
  RELEASE job-mch.
  jobMchRowID = TO-ROWID(ENTRY(2,ttblJob.rowIDs)).
  IF ENTRY(3,ttblJob.rowIDs) NE "None" THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK 
       WHERE job-mch.est-op_rec_key EQ ENTRY(3,ttblJob.rowIDs)
         AND job-mch.company EQ ENTRY(1,ttblJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,ttblJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,ttblJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,ttblJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,ttblJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,ttblJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,ttblJob.keyValue))
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN 
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,ttblJob.keyValue)
         AND job-mch.m-code EQ ENTRY(3,ttblJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,ttblJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,ttblJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,ttblJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,ttblJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,ttblJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,ttblJob.keyValue))
         AND ROWID(job-mch) EQ jobMchRowID
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,ttblJob.keyValue)
         AND job-mch.m-code EQ ttblJob.resource
         AND job-mch.job EQ INTEGER(ENTRY(4,ttblJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,ttblJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,ttblJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,ttblJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,ttblJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,ttblJob.keyValue))
         AND ROWID(job-mch) EQ jobMchRowID
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,ttblJob.keyValue)
         AND job-mch.m-code EQ ttblJob.resource
         AND job-mch.job EQ INTEGER(ENTRY(4,ttblJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,ttblJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,ttblJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,ttblJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,ttblJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,ttblJob.keyValue))
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN
  FIND FIRST job-mch EXCLUSIVE-LOCK
       WHERE job-mch.company EQ ENTRY(1,ttblJob.keyValue)
         AND job-mch.m-code EQ ENTRY(3,ttblJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,ttblJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,ttblJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,ttblJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,ttblJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,ttblJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,ttblJob.keyValue))
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN 
  FIND FIRST job-mch EXCLUSIVE-LOCK 
       WHERE job-mch.company EQ ENTRY(1,ttblJob.keyValue)
         AND job-mch.job EQ INTEGER(ENTRY(4,ttblJob.keyValue))
         AND job-mch.job-no EQ ENTRY(5,ttblJob.keyValue)
         AND job-mch.job-no2 EQ INTEGER(ENTRY(6,ttblJob.keyValue))
         AND job-mch.frm EQ INTEGER(ENTRY(7,ttblJob.keyValue))
         AND job-mch.blank-no EQ INTEGER(ENTRY(8,ttblJob.keyValue))
         AND job-mch.pass EQ INTEGER(ENTRY(9,ttblJob.keyValue))
         AND ROWID(job-mch) EQ jobMchRowID
       NO-ERROR.
  IF NOT AVAILABLE job-mch THEN DO:
      PUT UNFORMATTED 'ttblJob:' AT 5
          ' Resource: ' ttblJob.resource
          ' AltResource: ' ttblJob.altResource
          ' RowIDs: ' ttblJob.rowIDs 
          ' KeyValue: ' ttblJob.keyValue
          ' SB Run?: ' ttblJob.jobCompleted
          ' ** Not Found **' SKIP.
      NEXT.
  END.
  IF STRING(ROWID(job-mch)) NE ENTRY(2,ttblJob.rowIDs) THEN DO:
      PUT UNFORMATTED 'ttblJob: ** RowID Error **' AT 5
          ' Resource: ' ttblJob.resource
          ' RowIDs: ' ttblJob.rowIDs ' - RowID: ' STRING(ROWID(job-mch))
          ' KeyValue: ' ttblJob.keyValue
          ' RecKey: ' job-mch.rec_key
          ' EstOPRecKey: ' job-mch.est-op_rec_key
          ' Run?: ' job-mch.run-complete
          ' SB Run?: ' ttblJob.jobCompleted
          ' Current: ' job-mch.m-code
          ' New: ' ttblJob.altResource SKIP.
  END.
  
  IF CAN-FIND(FIRST jobNotes WHERE jobNotes.jobRowID EQ jobMchRowID) THEN
  RUN check4Notes (jobMchRowID,job-mch.company,ttblJob.resource,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm).
  statusStr = ''.
  DO i = 2 TO NUM-ENTRIES(customValueList):
    IF NOT ttblJob.jobStatus[i - 1] THEN
    statusStr = statusStr + ',' + ENTRY(i,customValueList).
  END.
  RUN statusNote (jobMchRowID,job-mch.company,ttblJob.resource,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,statusStr).
  RUN downtimeSpan (ttblJob.resource,INTEGER(job-mch.mr-hr * 3600),
                    ttblJob.startDate,ttblJob.startTime,
                    OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
  IF SUBSTR(ttblJob.jobSort,1,8) NE lvJob THEN
  ASSIGN
    lvJob = SUBSTR(ttblJob.jobSort,1,8)
    lvResSeq = 0
    .
  ASSIGN
    lvResSeq = lvResSeq + 1
    job-mch.seq-no = ttblJob.jobSequence
    job-mch.start-date-su = ttblJob.startDate
    job-mch.start-time-su = ttblJob.startTime
    job-mch.end-date-su = lvEndDate
    job-mch.end-time-su = lvEndTime
    job-mch.start-date = lvEndDate
    job-mch.start-time = lvEndTime
    job-mch.end-date = ttblJob.endDate
    job-mch.end-time = ttblJob.endTime
    job-mch.anchored = ttblJob.jobLocked
    job-mch.lag-time = ttblJob.lagTime
    .
  /* only change if not already run-complete */
  IF job-mch.run-complete EQ NO THEN DO:
      IF ttblJob.jobCompleted THEN
      PUT UNFORMATTED 'ttblJob:' AT 5
          ' RowIDs: ' ttblJob.rowIDs ' - RowID: ' STRING(ROWID(job-mch))
          ' KeyValue: ' ttblJob.keyValue
          ' RecKey: ' job-mch.rec_key
          ' Run?: ' job-mch.run-complete
          ' SB Run?: ' ttblJob.jobCompleted SKIP.
      job-mch.run-complete = ttblJob.jobCompleted.
  END.
  IF job-mch.m-code NE ttblJob.altResource THEN DO:
      PUT UNFORMATTED 'ttblJob:' AT 5
          ' RowIDs: ' ttblJob.rowIDs ' - RowID: ' STRING(ROWID(job-mch))
          ' KeyValue: ' ttblJob.keyValue
          ' RecKey: ' job-mch.rec_key
          ' Current: ' job-mch.m-code
          ' New: ' ttblJob.altResource SKIP.
      job-mch.m-code = ttblJob.altResource.
  END.
  RUN setLiveUpdate (job-mch.company,job-mch.job-no,job-mch.job-no2,
                     job-mch.frm,ttblJob.resource,ttblJob.liveUpdate).
  /* set job-hdr start date based on earliest job-mch start date */
  IF FIRST-OF(ttblJob.jobSort) THEN DO:
    FIND FIRST job-hdr OF job-mch EXCLUSIVE-LOCK NO-ERROR.
    IF AVAILABLE job-hdr THEN
    FOR EACH job-mch NO-LOCK
        WHERE job-mch.company EQ job-hdr.company
          AND job-mch.j-no EQ job-hdr.j-no BY job-mch.start-date:
      IF job-mch.start-date-su NE ? THEN DO:
        job-hdr.start-date = job-mch.start-date-su.
        RUN updateJob (job-mch.company,job-mch.job,job-mch.start-date-su).
        LEAVE.
      END. /* start-date-su ne ? */
    END. /* each job-mch */
    RELEASE job-hdr.
  END. /* first-of */
END. /* each ttbljob */

PUT UNFORMATTED '  End Save: ' STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss') ' for ' ID ' by ' USERID('nosweat') SKIP(1).
OUTPUT CLOSE.

MESSAGE 'Schedule Board Save (upload) Complete.' VIEW-AS ALERT-BOX.

PROCEDURE setLiveUpdate:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipForm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipLiveUpdate AS LOGICAL NO-UNDO.

  DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.

  lvCode = ipJobNo + ',' + STRING(ipJobNo2) + ',' + STRING(ipForm) + ',' + ipMCode.
  FIND FIRST reftable EXCLUSIVE-LOCK
       WHERE reftable.reftable EQ 'sbLiveUpdate'
         AND reftable.company EQ ipCompany
         AND reftable.loc EQ ''
         AND reftable.code EQ lvCode NO-ERROR.
  IF NOT AVAILABLE reftable THEN
  DO:
    CREATE reftable.
    ASSIGN
      reftable.reftable = 'sbLiveUpdate'
      reftable.company = ipCompany
      reftable.code = lvCode.
  END.
  reftable.code2 = TRIM(STRING(ipLiveUpdate,'Yes/No')).
END PROCEDURE.

PROCEDURE statusNote:
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipForm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStatusStr AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.

  lvCode = ipMCode + ',' + STRING(ipJob) + ',' + ipJobNo + ',' + STRING(ipJobNo2) + ',' + STRING(ipForm).

  FIND FIRST reftable EXCLUSIVE-LOCK
       WHERE reftable.reftable EQ 'SB: Status'
         AND reftable.company EQ ipCompany
         AND reftable.loc EQ ''
         AND reftable.code EQ lvCode NO-ERROR.
  IF NOT AVAILABLE reftable AND ipStatusStr EQ customValueList THEN RETURN.
  IF NOT AVAILABLE reftable AND ipStatusStr NE customValueList THEN
  DO:
    CREATE reftable.
    ASSIGN
      reftable.reftable = 'SB: Status'
      reftable.company = ipCompany
      reftable.loc = ''
      reftable.code = lvCode
      reftable.code2 = STRING(TODAY) + ',' + STRING(TIME).
  END.
  IF ipStatusStr EQ customValueList THEN
  DELETE reftable.
  ELSE
  reftable.dscr = ipStatusStr.
END PROCEDURE.

PROCEDURE check4Notes:
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipForm AS INTEGER NO-UNDO.

  DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lvCode2 AS CHARACTER NO-UNDO.
  
  FOR EACH jobNotes EXCLUSIVE-LOCK WHERE jobNotes.jobRowID EQ ipRowID:
    IF jobNotes.noteText NE '' THEN DO:
      ASSIGN
        lvCode = ipMCode + ',' + STRING(ipJob) + ',' + ipJobNo + ',' + STRING(ipJobNo2) + ',' + STRING(ipForm)
        lvCode2 = STRING(jobNotes.noteDate) + ',' + STRING(jobNotes.noteTime).
      FIND FIRST reftable EXCLUSIVE-LOCK
           WHERE reftable.reftable EQ 'SB: Note'
             AND reftable.company EQ ipCompany
             AND reftable.loc EQ ''
             AND reftable.code EQ lvCode
             AND reftable.code2 EQ lvCode2 NO-ERROR.
      IF (jobNotes.noteKey EQ '' AND NOT jobNotes.deleteNote) OR NOT AVAILABLE reftable THEN
      DO:
        CREATE reftable.
        ASSIGN
          reftable.reftable = 'SB: Note'
          reftable.company = ipCompany
          reftable.loc = ''
          reftable.code = lvCode
          reftable.code2 = lvCode2
          jobNotes.noteKey = 'Added'.
      END.
    END. /* noteText ne '' */
    IF AVAILABLE reftable THEN
    DO:
      IF jobNotes.deleteNote THEN
      DELETE reftable.
      ELSE
      reftable.dscr = jobNotes.noteText.
    END.
    RELEASE reftable.
  END. /* each jobNotes */
END PROCEDURE.

PROCEDURE downtimeSpan:
  {{&includes}/{&Board}/downtimeSpan.i}
END PROCEDURE.

PROCEDURE newEnd:
  {{&includes}/{&Board}/newEnd.i}
END PROCEDURE.

PROCEDURE updateJob:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate AS DATE NO-UNDO.

  FIND FIRST job EXCLUSIVE-LOCK
       WHERE job.company EQ ipCompany
         AND job.job EQ ipJob NO-ERROR.
  IF AVAILABLE job THEN
  job.start-date = ipStartDate.
END PROCEDURE.
