/* upload.p - ASI upload process */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}

/* configuration vars */
{{&includes}/configVars.i}
/* configuration version procedures */
{{&includes}/configVersion.i}

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

RUN getConfiguration.

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
    job-mch.sbLiveUpdate = pendingJob.liveUpdate
    .
  /* only change if not already run-complete */
  IF job-mch.run-complete EQ NO THEN
  job-mch.run-complete = pendingJob.jobCompleted.
  RUN updateJobStartDate (job-mch.company,job-mch.job,job-mch.start-date-su).
/*  DO i = 2 TO NUM-ENTRIES(customValueList):                */
/*    IF NOT pendingJob.jobStatus[i - 1] THEN                */
/*    statusStr = statusStr + ',' + ENTRY(i,customValueList).*/
/*  END.                                                     */
  RUN setLiveUpdate (job-mch.company,job-mch.job-no,job-mch.job-no2,
                     job-mch.frm,pendingJob.resource,pendingJob.liveUpdate).
  RUN statusNote (jobMchRowID,job-mch.company,pendingJob.resource,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,
                  pendingJob.jobStatus[1],
                  pendingJob.jobStatus[2],
                  pendingJob.jobStatus[3],
                  pendingJob.jobStatus[4],
                  pendingJob.jobStatus[5],
                  pendingJob.jobStatus[6],
                  pendingJob.jobStatus[7],
                  pendingJob.jobStatus[8],
                  pendingJob.jobStatus[9],
                  pendingJob.jobStatus[10],
                  pendingJob.jobStatus[11],
                  pendingJob.jobStatus[12],
                  pendingJob.jobStatus[13],
                  pendingJob.jobStatus[14]
                 ).
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
/*  DO i = 2 TO NUM-ENTRIES(customValueList):                */
/*    IF NOT ttblJob.jobStatus[i - 1] THEN                   */
/*    statusStr = statusStr + ',' + ENTRY(i,customValueList).*/
/*  END.                                                     */
  RUN statusNote (jobMchRowID,job-mch.company,ttblJob.resource,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,
                  ttblJob.jobStatus[1],
                  ttblJob.jobStatus[2],
                  ttblJob.jobStatus[3],
                  ttblJob.jobStatus[4],
                  ttblJob.jobStatus[5],
                  ttblJob.jobStatus[6],
                  ttblJob.jobStatus[7],
                  ttblJob.jobStatus[8],
                  ttblJob.jobStatus[9],
                  ttblJob.jobStatus[10],
                  ttblJob.jobStatus[11],
                  ttblJob.jobStatus[12],
                  ttblJob.jobStatus[13],
                  ttblJob.jobStatus[14]
                 ).
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
    job-mch.sbLiveUpdate = ttblJob.liveUpdate
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
        RUN updateJobStartDate (job-mch.company,job-mch.job,job-mch.start-date-su).
        LEAVE.
      END. /* start-date-su ne ? */
    END. /* each job-mch */
    RELEASE job-hdr.
  END. /* first-of */
END. /* each ttbljob */

IF htmlPageLocation NE "" THEN
RUN pHTMLPages.

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
  IF AVAILABLE reftable THEN
  DELETE reftable.
/*  IF NOT AVAILABLE reftable THEN DO:                   */
/*    CREATE reftable.                                   */
/*    ASSIGN                                             */
/*      reftable.reftable = 'sbLiveUpdate'               */
/*      reftable.company = ipCompany                     */
/*      reftable.code = lvCode.                          */
/*  END.                                                 */
/*  reftable.code2 = TRIM(STRING(ipLiveUpdate,'Yes/No')).*/
END PROCEDURE.

PROCEDURE statusNote:
  DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipMCode AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJobNo2 AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipForm AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus01 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus02 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus03 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus04 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus05 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus06 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus07 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus08 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus09 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus10 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus11 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus12 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus13 AS LOGICAL NO-UNDO.
  DEFINE INPUT PARAMETER ipStatus14 AS LOGICAL NO-UNDO.
/*  DEFINE INPUT PARAMETER ipStatusStr AS CHARACTER NO-UNDO.*/
  
  DEFINE VARIABLE lvCode AS CHARACTER NO-UNDO.

  FIND FIRST sbStatus EXCLUSIVE-LOCK 
       WHERE sbStatus.company EQ ipCompany
         AND sbStatus.m-code  EQ ipMCode
         AND sbStatus.job-no  EQ ipJobNo
         AND sbStatus.job-no2 EQ ipJobNo2
         AND sbStatus.frm     EQ ipForm
       NO-ERROR.
  IF NOT AVAILABLE sbStatus THEN DO:
    CREATE sbStatus.
    ASSIGN
      sbStatus.company = ipCompany
      sbStatus.m-code  = ipMCode
      sbStatus.job-no  = ipJobNo
      sbStatus.job-no2 = ipJobNo2
      sbStatus.frm     = ipForm
      .
  END. /* if not avail */
  ASSIGN
    sbStatus.sbStatus[1]  = ipStatus01
    sbStatus.sbStatus[2]  = ipStatus02
    sbStatus.sbStatus[3]  = ipStatus03
    sbStatus.sbStatus[4]  = ipStatus04
    sbStatus.sbStatus[5]  = ipStatus05
    sbStatus.sbStatus[6]  = ipStatus06
    sbStatus.sbStatus[7]  = ipStatus07
    sbStatus.sbStatus[8]  = ipStatus08
    sbStatus.sbStatus[9]  = ipStatus09
    sbStatus.sbStatus[10] = ipStatus10
    sbStatus.sbStatus[11] = ipStatus11
    sbStatus.sbStatus[12] = ipStatus12
    sbStatus.sbStatus[13] = ipStatus13
    sbStatus.sbStatus[14] = ipStatus14
    .

  lvCode = ipMCode + ',' + STRING(ipJob) + ',' + ipJobNo + ',' + STRING(ipJobNo2) + ',' + STRING(ipForm).
  FIND FIRST reftable EXCLUSIVE-LOCK
       WHERE reftable.reftable EQ 'SB: Status'
         AND reftable.company EQ ipCompany
         AND reftable.loc EQ ''
         AND reftable.code EQ lvCode
       NO-ERROR.
  IF AVAILABLE reftable THEN
  DELETE reftable.
/*  IF NOT AVAILABLE reftable AND ipStatusStr EQ customValueList THEN RETURN.*/
/*  IF NOT AVAILABLE reftable AND ipStatusStr NE customValueList THEN DO:    */
/*    CREATE reftable.                                                       */
/*    ASSIGN                                                                 */
/*      reftable.reftable = 'SB: Status'                                     */
/*      reftable.company = ipCompany                                         */
/*      reftable.loc = ''                                                    */
/*      reftable.code = lvCode                                               */
/*      reftable.code2 = STRING(TODAY) + ',' + STRING(TIME).                 */
/*  END.                                                                     */
/*  IF ipStatusStr EQ customValueList THEN                                   */
/*  DELETE reftable.                                                         */
/*  ELSE                                                                     */
/*  reftable.dscr = ipStatusStr.                                             */
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
      FIND FIRST sbNote EXCLUSIVE-LOCK 
           WHERE sbNote.company EQ ipCompany
             AND sbNote.m-code  EQ ipMCode
             AND sbNote.job-no  EQ ipJobNo
             AND sbNote.job-no2 EQ ipJobNo2
             AND sbNote.frm     EQ ipForm
           NO-ERROR.
      IF NOT AVAILABLE sbNote OR (jobnotes.noteKey EQ "" AND jobnotes.deleteNote EQ NO) THEN DO:
        CREATE sbNote.
        ASSIGN
          sbNote.company  = ipCompany
          sbNote.m-code   = ipMCode
          sbNote.job-no   = ipJobNo
          sbNote.job-no2  = ipJobNo2
          sbNote.frm      = ipForm
          sbNote.noteDate = jobNotes.noteDate
          sbNote.noteTime = jobNotes.noteTime
          .
      END. /* if not avail */
      IF AVAILABLE sbNote THEN DO:
        IF jobNotes.deleteNote THEN
        DELETE sbNote.
        ELSE
        sbNote.jobNote = jobNotes.noteText.
      END.
      RELEASE sbNote.
    
      ASSIGN
        lvCode = ipMCode + ',' + STRING(ipJob) + ',' + ipJobNo + ',' + STRING(ipJobNo2) + ',' + STRING(ipForm)
        lvCode2 = STRING(jobNotes.noteDate) + ',' + STRING(jobNotes.noteTime).
      FIND FIRST reftable EXCLUSIVE-LOCK
           WHERE reftable.reftable EQ 'SB: Note'
             AND reftable.company EQ ipCompany
             AND reftable.loc EQ ''
             AND reftable.code EQ lvCode
             AND reftable.code2 EQ lvCode2
           NO-ERROR.
      IF AVAILABLE reftable THEN 
      DELETE reftable.
/*      IF (jobNotes.noteKey EQ '' AND NOT jobNotes.deleteNote) OR NOT AVAILABLE reftable THEN DO:*/
/*        CREATE reftable.                                                                        */
/*        ASSIGN                                                                                  */
/*          reftable.reftable = 'SB: Note'                                                        */
/*          reftable.company = ipCompany                                                          */
/*          reftable.loc = ''                                                                     */
/*          reftable.code = lvCode                                                                */
/*          reftable.code2 = lvCode2                                                              */
/*          jobNotes.noteKey = 'Added'                                                            */
/*          .                                                                                     */
/*      END.                                                                                      */
    END. /* noteText ne '' */
/*    IF AVAILABLE reftable THEN DO:                                                              */
/*      IF jobNotes.deleteNote THEN                                                               */
/*      DELETE reftable.                                                                          */
/*      ELSE                                                                                      */
/*      reftable.dscr = jobNotes.noteText.                                                        */
/*    END.                                                                                        */
/*    RELEASE reftable.                                                                           */
  END. /* each jobNotes */
END PROCEDURE.

PROCEDURE downtimeSpan:
  {{&includes}/{&Board}/downtimeSpan.i}
END PROCEDURE.

PROCEDURE newEnd:
  {{&includes}/{&Board}/newEnd.i}
END PROCEDURE.

PROCEDURE updateJobStartDate:
  DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
  DEFINE INPUT PARAMETER ipJob AS INTEGER NO-UNDO.
  DEFINE INPUT PARAMETER ipStartDate AS DATE NO-UNDO.

  FIND FIRST job EXCLUSIVE-LOCK
       WHERE job.company EQ ipCompany
         AND job.job EQ ipJob NO-ERROR.
  IF AVAILABLE job THEN
  job.start-date = ipStartDate.
END PROCEDURE.

PROCEDURE getConfiguration:
  {{&includes}/{&Board}/getConfiguration.i}
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
END PROCEDURE.

PROCEDURE pHTMLPages:
    DEFINE VARIABLE cResource AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAltLine  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cBGColor  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCompany  AS CHARACTER NO-UNDO.
    
    cCompany = ENTRY(1,ttblJob.keyValue).
    INPUT FROM VALUE (SEARCH ('{&data}/' + ID + '/resourceList.dat')) NO-ECHO.
    IMPORT ^.
    REPEAT:
        IMPORT cResource.
        FIND FIRST mach NO-LOCK 
             WHERE mach.company EQ cCompany
               AND mach.m-code  EQ cResource
             NO-ERROR.
        OUTPUT TO VALUE(htmlPageLocation + '\' + cResource + '.htm').
        PUT UNFORMATTED
            '<html>' SKIP
            '<head>' SKIP
            '<title>Operation: ' cResource (IF AVAILABLE mach THEN ' - ' + mach.m-dscr ELSE '') '</title>' SKIP
            '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
            '<meta http-equiv="Refresh" content="120">' SKIP
            '</head>' SKIP
            '<a name="Top">' SKIP
            '<form>' SKIP
            '<fieldset>' SKIP
            '<legend>Operation: <b><font color="DC143C">' cResource (IF AVAILABLE mach THEN ' - ' + mach.m-dscr ELSE '')
            '</font></b> (updated ' STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')'
            '~&nbsp;</legend>' SKIP
            '<table border="1" cellspacing="0" cellpadding="1" width="100%">' SKIP
            '  <tr>' SKIP
            '    <td bgcolor="#CCCCCC" align="center" nowrap><b>Seq</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" nowrap><b>Job</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" nowrap><b>Customer</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" nowrap><b>Item</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" align="right" nowrap><b>Run Qty</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" align="center" nowrap><b>Due Date</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" align="center" nowrap><b>Start Date</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" align="right" nowrap><b>Time</b></td>' SKIP
            '    <td bgcolor="#CCCCCC" align="center" nowrap><b>End Date</b></td>' SKIP 
            '    <td bgcolor="#CCCCCC" align="right" nowrap><b>Time</b></td>' SKIP 
            '  </tr>' SKIP
            .
        lAltLine = YES.
        FOR EACH ttblJob NO-LOCK
            WHERE ttblJob.resource EQ cResource
            BREAK BY ttblJob.jobSequence
                  BY ttblJob.jobSort
            :
            ASSIGN 
                lAltLine = NOT lAltLine
                cBGColor = IF lAltLine THEN ' bgcolor="D1FCC5"' ELSE ''
                .
            PUT UNFORMATTED
                '  <tr>' SKIP
                '    <td' cBGColor ' align="center" nowrap>' ttblJob.jobSequence '</td>' SKIP
                '    <td' cBGColor ' nowrap><b>' ttblJob.jobSort '</b></td>' SKIP
                '    <td' cBGColor ' nowrap>' ttblJob.userField02 '</td>' SKIP
                '    <td' cBGColor ' nowrap>' ttblJob.userField09 '</td>' SKIP
                '    <td' cBGColor ' align="right" nowrap>' ttblJob.userField15 '</td>' SKIP
                '    <td' cBGColor ' align="center" nowrap>' STRING(ttblJob.dueDate,'99.99.9999') '</td>' SKIP
                '    <td' cBGColor ' align="center" nowrap>' STRING(ttblJob.startDate,'99.99.9999') '</td>' SKIP
                '    <td' cBGColor ' align="right" nowrap>' STRING(ttblJob.startTime,"hh:mm:ss am") '</td>' SKIP
                '    <td' cBGColor ' align="center" nowrap>' STRING(ttblJob.endDate,'99.99.9999') '</td>' SKIP 
                '    <td' cBGColor ' align="right" nowrap>' STRING(ttblJob.endTime,"hh:mm:ss am") '</td>' SKIP 
                '  </tr>' SKIP
                .
        END. /* each ttbljob */
        PUT UNFORMATTED
            '</table>' SKIP
            '<div align="left"><a href="#Top">Top</a>' SKIP
            '<div align="right">~&copy; Advantzware, Inc., All Rights Reserved</div>' SKIP
            '</fieldset>' SKIP
            '</form>' SKIP
            '</html>' SKIP
            .
        OUTPUT CLOSE.
    END. /* repeat */
    INPUT CLOSE.
END PROCEDURE.