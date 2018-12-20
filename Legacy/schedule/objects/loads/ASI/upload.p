/* upload.p - ASI upload process */

{schedule/scopDir.i}
{{&includes}/defBoard.i}
{{&includes}/sharedVars.i}
{{&includes}/ttblJob.i}
{{&includes}/specialTime.i}

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
DEFINE VARIABLE hTtblJob AS HANDLE NO-UNDO.
DEFINE VARIABLE hPendingJob AS HANDLE NO-UNDO.
DEFINE VARIABLE iAuditID AS INTEGER NO-UNDO.
DEFINE VARIABLE iLogEntry AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttHTMLFields NO-UNDO
    FIELD fieldType   AS CHARACTER 
    FIELD fieldLabel  AS CHARACTER
    FIELD fieldName   AS CHARACTER
    FIELD fieldFormat AS CHARACTER
        INDEX ttHTMLFields IS PRIMARY fieldType
        .

FUNCTION fHTMLFieldValue RETURNS CHARACTER
    (ipcTable       AS CHARACTER,
     ipcFieldLabel  AS CHARACTER,
     ipcFieldName   AS CHARACTER,
     ipcFieldFormat AS CHARACTER
     ):
        
    DEFINE VARIABLE cFieldValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE hTable      AS HANDLE    NO-UNDO.
    
    CASE ipcTable:
        WHEN "Job" THEN hTable = hTtblJob.
        WHEN "Pending" THEN hTable = hPendingJob.
    END CASE.

    /* special case fields */
    CASE ipcFieldLabel:
        WHEN 'Downtime' THEN
        cFieldValue = STRING(specialTime(hTable:BUFFER-FIELD("downtimeSpan"):BUFFER-VALUE()),ipcFieldFormat).
        WHEN 'Due Time' THEN
        cFieldValue = STRING(hTable:BUFFER-FIELD("dueTime"):BUFFER-VALUE(),ipcFieldFormat).
        WHEN 'End Time' THEN
        cFieldValue = STRING(hTable:BUFFER-FIELD("endTime"):BUFFER-VALUE(),ipcFieldFormat).
        WHEN 'Job Time' THEN
        cFieldValue = STRING(specialTime(hTable:BUFFER-FIELD("timeSpan"):BUFFER-VALUE()),ipcFieldFormat).
        WHEN 'Start Time' THEN
        cFieldValue = STRING(hTable:BUFFER-FIELD("startTime"):BUFFER-VALUE(),ipcFieldFormat).
        WHEN 'Total Time' THEN
        cFieldValue = STRING(specialTime(INTEGER(hTable:BUFFER-FIELD("timeSpan"):BUFFER-VALUE())
                    + INTEGER(hTable:BUFFER-FIELD("downtimeSpan"):BUFFER-VALUE())),ipcFieldFormat).
        OTHERWISE
        cFieldValue = STRING(hTable:BUFFER-FIELD(ipcFieldName):BUFFER-VALUE(),ipcFieldFormat).
    END CASE.

    RETURN cFieldValue.
END FUNCTION.

FUNCTION numericDateTime RETURNS DECIMAL (ipDate AS DATE,ipTime AS INTEGER):
  {{&includes}/numericDateTime.i}
END FUNCTION.

DISABLE TRIGGERS FOR LOAD OF job.
DISABLE TRIGGERS FOR LOAD OF job-mch.
DISABLE TRIGGERS FOR LOAD OF job-hdr.
DISABLE TRIGGERS FOR LOAD OF reftable.

RUN spCreateAuditHdr (
    "LOG",     /* type  */
    "ASI",     /* db    */
    "sbPro.", /* table */
    ID,        /* key   */
    OUTPUT iAuditID
    ).
RUN pLogEntry ("SaveBegin", STRING(TODAY,"99.99.9999") + " @ " + STRING(TIME,"hh:mm:ss")).

ASSIGN 
    hTtblJob    = BUFFER ttblJob:HANDLE
    hPendingJob = BUFFER pendingJob:HANDLE
    .
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
  /* change machine only if not already done by DC or TS */
  IF NOT job-mch.est-op_rec_key BEGINS 'DC' AND
     NOT job-mch.est-op_rec_key BEGINS 'TS' AND
     job-mch.m-code NE pendingJob.altResource THEN
  job-mch.m-code = pendingJob.altResource.
  RUN updateJobStartDate (job-mch.company,job-mch.job,job-mch.start-date-su).
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
      RUN pLogEntry ("ttblJob", "** Not Found **").
      RUN pLogEntry ("Resource", ttblJob.resource).
      RUN pLogEntry ("AltResource", ttblJob.altResource).
      RUN pLogEntry ("RowIDs", ttblJob.rowIDs). 
      RUN pLogEntry ("KeyValue", ttblJob.keyValue).
      RUN pLogEntry ("SB Run?", ttblJob.jobCompleted).
      NEXT.
  END.
  IF STRING(ROWID(job-mch)) NE ENTRY(2,ttblJob.rowIDs) THEN DO:
      RUN pLogEntry ("ttblJob", "** RowID Error **").
      RUN pLogEntry ("Resource", ttblJob.resource).
      RUN pLogEntry ("RowIDs", ttblJob.rowIDs).
      RUN pLogEntry ("RowID", STRING(ROWID(job-mch))).
      RUN pLogEntry ("KeyValue", ttblJob.keyValue).
      RUN pLogEntry ("RecKey", job-mch.rec_key).
      RUN pLogEntry ("EstOPRecKey", job-mch.est-op_rec_key).
      RUN pLogEntry ("Run?", job-mch.run-complete).
      RUN pLogEntry ("SB Run?", ttblJob.jobCompleted).
      RUN pLogEntry ("Current", job-mch.m-code).
      RUN pLogEntry ("New", ttblJob.altResource).
  END. /* if */
  
  IF CAN-FIND(FIRST jobNotes WHERE jobNotes.jobRowID EQ jobMchRowID) THEN
  RUN check4Notes (jobMchRowID,job-mch.company,ttblJob.resource,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm).
  statusStr = ''.
  RUN statusNote (
    jobMchRowID,job-mch.company,ttblJob.resource,job-mch.job,job-mch.job-no,job-mch.job-no2,job-mch.frm,
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
      IF ttblJob.jobCompleted THEN DO:
          RUN pLogEntry ("RowIDs", ttblJob.rowIDs).
          RUN pLogEntry ("RowID", STRING(ROWID(job-mch))).
          RUN pLogEntry ("KeyValue", ttblJob.keyValue).
          RUN pLogEntry ("RecKey", job-mch.rec_key).
          RUN pLogEntry ("Run?", job-mch.run-complete).
          RUN pLogEntry ("SB Run?", ttblJob.jobCompleted).
      END. /* if completed */
      job-mch.run-complete = ttblJob.jobCompleted.
  END.
  /* change machine only if not already done by DC or TS */
  IF NOT job-mch.est-op_rec_key BEGINS 'DC' AND
     NOT job-mch.est-op_rec_key BEGINS 'TS' AND
     job-mch.m-code NE ttblJob.altResource THEN DO:
      RUN pLogEntry ("RowIDs", ttblJob.rowIDs).
      RUN pLogEntry ("RowID", STRING(ROWID(job-mch))).
      RUN pLogEntry ("KeyValue", ttblJob.keyValue).
      RUN pLogEntry ("RecKey", job-mch.rec_key).
      RUN pLogEntry ("Current", job-mch.m-code).
      RUN pLogEntry ("New", ttblJob.altResource).
      job-mch.m-code = ttblJob.altResource.
  END.
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

FIND FIRST module NO-LOCK
     WHERE module.db-name EQ "ASI"
       AND module.module  EQ "sbHTML"
     NO-ERROR.
IF AVAILABLE module AND
   module.is-used EQ YES AND
   module.expire-date GE TODAY AND 
   htmlPageLocation NE "" THEN
RUN pHTMLPages.

RUN pLogEntry ("SaveEnd", STRING(TODAY,"99.99.9999") + " @ " + STRING(TIME,"hh:mm:ss")).

/* **********************  Internal Procedures  *********************** */


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
    END. /* noteText ne '' */
  END. /* each jobNotes */
END PROCEDURE.

PROCEDURE downtimeSpan:
  {{&includes}/{&Board}/downtimeSpan.i}
END PROCEDURE.

PROCEDURE getConfiguration:
  {{&includes}/{&Board}/getConfiguration.i}
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/config.dat')) NO-ECHO.
  IMPORT version.
  INPUT CLOSE.
  RUN VALUE('get' + version).
END PROCEDURE.

PROCEDURE newEnd:
  {{&includes}/{&Board}/newEnd.i}
END PROCEDURE.

PROCEDURE pHTMLBranding:
    PUT UNFORMATTED
        '  <img src="' SEARCH("Graphics/asiicon.ico")
        '" align="middle">~&nbsp;<b><a href="http://www.advantzware.com" target="_blank">'
        '<font face="{&fontFace}">Advantzware, Inc.</a>~&nbsp;~&copy;</b></font>' SKIP
        . 
END PROCEDURE.

PROCEDURE pHTMLData:
    DEFINE INPUT PARAMETER ipcTable   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcBGColor AS CHARACTER NO-UNDO.
    
    PUT UNFORMATTED '    <tr>' SKIP.
    FOR EACH ttHTMLFields
        WHERE ttHTMLFields.fieldType EQ ipcTable
        :
        PUT UNFORMATTED
            '      <td' ipcBGColor ' align="center" nowrap><font face="{&fontFace}">'
            fHTMLFieldValue(ipcTable,ttHTMLFields.fieldLabel,ttHTMLFields.fieldName,ttHTMLFields.fieldFormat)
            '</font></td>' SKIP
            .
    END. /* each tthtmlfields */
    PUT UNFORMATTED '    </tr>' SKIP.
END PROCEDURE. 

PROCEDURE pHTMLFooter:
    PUT UNFORMATTED
        '  </table>' SKIP
        '  <div align="left"><font face="{&fontFace}"><a href="#Top">Top</a></font>' SKIP
        '  <div align="right"><font face="{&fontFace}">~&copy; Advantzware, Inc., All Rights Reserved</font></div>' SKIP
        '</fieldset>' SKIP
        '</form>' SKIP
        '</html>' SKIP
        .
END PROCEDURE.

PROCEDURE pHTMLHeader:
    DEFINE INPUT PARAMETER ipcHTMLTitle  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcHTMLLegend AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplScript     AS LOGICAL   NO-UNDO.
    
    PUT UNFORMATTED
        '<html>' SKIP
        '<head>' SKIP
        '<title>' ipcHTMLTitle '</title>' SKIP
        '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">' SKIP
        '<meta http-equiv="Refresh" content="120">' SKIP
        .
    IF iplScript THEN 
    PUT UNFORMATTED
        '<script type="text/javascript">' SKIP
        '// Popup window code' SKIP
        'function newPopup(url) ~{' SKIP
        '  popupWindow = window.open(' SKIP
        '    url,"popUpWindow","height=' htmlPopupHeight
        ',width=' htmlPopupWidth ',left=10,top=10,resizable=yes,'
        'scrollbars=yes,toolbar=yes,menubar=no,location=no,directories=no,status=yes")' SKIP
        '}' SKIP
        '</script>' SKIP
        .
    PUT UNFORMATTED
        '</head>' SKIP
        '<a name="Top"></a>' SKIP
        '<form>' SKIP
        '<fieldset>' SKIP
        '  <legend><font face="{&fontFace}">' ipcHTMLLegend ' (updated '
        STRING(TODAY,'99.99.9999') ' @ ' STRING(TIME,'hh:mm:ss am') ')</font>'
        '~&nbsp;</legend>' SKIP
        .
END PROCEDURE.

PROCEDURE pHTMLLabel:
    DEFINE INPUT PARAMETER ipcTable   AS CHARACTER NO-UNDO.
    
    PUT UNFORMATTED '    <tr>' SKIP.
    FOR EACH ttHTMLFields
        WHERE ttHTMLFields.fieldType EQ ipcTable
        :
        PUT UNFORMATTED 
            '      <td bgcolor="#C5EBD8" align="center" nowrap><font face="{&fontFace}"><b>'
            ttHTMLFields.fieldLabel
            '</b></font></td>' SKIP
            .
    END. /* each tthtmlfields */
    PUT UNFORMATTED '    </tr>' SKIP.
END PROCEDURE.

PROCEDURE pHTMLPages:
    &Scoped-define fontFace Arial, Helvetica, sans-serif
    
    DEFINE VARIABLE lAltLine       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cBGColor       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobs          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dtDate         AS DATE      NO-UNDO.
    DEFINE VARIABLE lUsed          AS LOGICAL   NO-UNDO EXTENT 1440.
    DEFINE VARIABLE cBookedTime    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBookedTime    AS INTEGER   NO-UNDO EXTENT 999.
    DEFINE VARIABLE cUsedTime      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iUsedTime      AS INTEGER   NO-UNDO EXTENT 999.
    DEFINE VARIABLE cAvailTime     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iAvailTime     AS INTEGER   NO-UNDO EXTENT 999.
    DEFINE VARIABLE iStartTime     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cDays          AS CHARACTER NO-UNDO INITIAL "Sun,Mon,Tue,Wed,Thu,Fri,Sat".
    DEFINE VARIABLE dStartDateTime AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dEndDateTime   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE idx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE jdx            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cHTMLTitle     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLLegend    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lScript        AS LOGICAL   NO-UNDO.
    
    DEFINE BUFFER bTtblJob FOR ttblJob.
    
    RUN pttHTMLFields.
    
    ASSIGN 
        cHTMLTitle  = "Schedule Board Resources"
        cHTMLLegend = "Schedule Board Resource Capacity View"
        lScript     = YES
        .
    OUTPUT TO VALUE(htmlPageLocation + '\sbResources.htm').
    RUN pHTMLHeader (cHTMLTitle,cHTMLLegend,lScript).
    RUN pHTMLBranding.
    PUT UNFORMATTED
        '  <table align="right" cellspacing="2" cellpadding="8">' SKIP
        '    <tr>' SKIP 
        '      <td><font face="{&fontFace}">Legend:</font></td>' SKIP 
        '      <td bgcolor="#C0BEBE"><font face="{&fontFace}"><b>Downtime</b></font></td>' SKIP 
        '      <td bgcolor="#AAD5B9"><font face="{&fontFace}"><b>Booked (#Jobs)</b></font></td>' SKIP 
        '      <td bgcolor="#F1FE98"><font face="{&fontFace}"><b>Available</b></font></td>' SKIP 
        '    </tr>' SKIP  
        '  </table>' SKIP 
        '  <table border="1" cellspacing="0" cellpadding="12" width="100%">' SKIP
        '    <tr>' SKIP
        '      <td bgcolor="#F4F4F4" align="center" nowrap><font face="{&fontFace}"><b>Resource</b></font></td>' SKIP
        '      <td bgcolor="#F4F4F4" align="center" nowrap><font face="{&fontFace}"><b>Name</b></font></td>' SKIP
        '      <td bgcolor="#F4F4F4" align="center" nowrap><font face="{&fontFace}"><b>Pending</b></font></td>' SKIP
        .
    DO dtDate = TODAY TO TODAY + htmlCapacityDays - 1:
        cBGColor = IF WEEKDAY(dtDate) EQ 1 OR WEEKDAY(dtDate) EQ 7 THEN 'bgcolor="#DBDEF2"' ELSE 'bgcolor="#F4F4F4"'.
        PUT UNFORMATTED
            '      <td ' cBGColor ' align="center" nowrap><font face="{&fontFace}"><b>'
            ENTRY(WEEKDAY(dtDate),cDays) ' ' MONTH(dtDate) '/' DAY(dtDate) '</b></font></td>' SKIP
            .
    END. /* do dtdate */ 
    PUT UNFORMATTED
        '    </tr>' SKIP
        .
    lAltLine = YES.
    FOR EACH ttblJob NO-LOCK
        BREAK BY ttblJob.resource
              BY ttblJob.jobSequence
              BY ttblJob.jobSort
        :
        IF FIRST-OF(ttblJob.resource) THEN DO:
            FIND FIRST mach NO-LOCK 
                 WHERE mach.company EQ ENTRY(1,ttblJob.keyValue)
                   AND mach.m-code  EQ ttblJob.resource
                 NO-ERROR.
            ASSIGN 
                iJobs = 0
                iTime = 0
                .
            FOR EACH pendingJob
                WHERE pendingJob.resource EQ ttblJob.resource
                :
                ASSIGN 
                    iJobs = iJobs + 1
                    iTime = iTime + pendingJob.timeSpan
                    .
            END. /* each pendingjob */
            ASSIGN 
                lAltLine = NOT lAltLine
                cBGColor = IF lAltLine THEN ' bgcolor="D1FCC5"' ELSE ''
                .
            PUT UNFORMATTED
                '    <tr>' SKIP
                '      <td' cBGColor ' align="center" nowrap rowspan="3"><font face="{&fontFace}">'
                '<img src="'
                (IF SEARCH("Graphics/48x48/" + ttblJob.resource + ".png") NE ? THEN
                    SEARCH("Graphics/48x48/" + ttblJob.resource + ".png") ELSE
                    SEARCH("Graphics/48x48/gearwheels.png"))
                '" width="48" height="48" align="center">~&nbsp~&nbsp~&nbsp~&nbsp<b>'
                ttblJob.resource '</a></b></font></td>' SKIP
                '      <td' cBGColor ' align="center" nowrap rowspan="3"><font face="{&fontFace}"><b>'
                '<a href="' htmlPageLocation + '\' + REPLACE(ttblJob.resource,"/","") + '.htm" target="_blank">'
                (IF AVAILABLE mach THEN mach.m-dscr ELSE ttblJob.resource) '</a></b></font></td>' SKIP
                '      <td' cBGColor ' align="center" nowrap rowspan="3" align="center"><font face="{&fontFace}">'
                .
            IF iJobs NE 0 THEN
            PUT UNFORMATTED
                '<a href="JavaScript:newPopup(~''
                REPLACE(htmlPageLocation + '\' + ttblJob.resource,"\","/")
                'Pending.htm~');">Jobs: <b>' iJobs '</a><br>' specialTime(iTime) '</b>'
                .
            ELSE PUT UNFORMATTED "~&nbsp".
            PUT UNFORMATTED
                '</font></td>' SKIP
                .
            DO dtDate = TODAY TO TODAY + htmlCapacityDays - 1:
                ASSIGN 
                    idx            = dtDate - TODAY + 1
                    iUsedTime[idx] = 0
                    .
                FOR EACH boardDowntime
                    WHERE boardDowntime.resource  EQ ttblJob.resource
                      AND boardDowntime.startDate EQ dtDate
                    :
                    iUsedTime[idx] = iUsedTime[idx] + boardDowntime.downtimeSpan.
                END. /* each boarddowntime */
                cUsedTime = IF iUsedTime[idx] EQ 86400 THEN "24:00"
                       ELSE IF iUsedTime[idx] EQ 0 THEN ""
                       ELSE STRING(iUsedTime[idx],"hh:mm").
                PUT UNFORMATTED 
                    '      <td bgcolor="#C0BEBE" align="center" nowrap><font face="{&fontFace}"><b>'
                    cUsedTime '</b></font></td>' SKIP 
                    .
            END. /* do dtdate */
            PUT UNFORMATTED 
                '    </tr>' SKIP
                '    <tr>' SKIP
                .
            DO dtDate = TODAY TO TODAY + htmlCapacityDays - 1:
                ASSIGN
                    lUsed            = NO
                    idx              = dtDate - TODAY + 1
                    iBookedTime[idx] = 0
                    dStartDateTime   = numericDateTime(dtDate,0)
                    dEndDateTime     = numericDateTime(dtDate,86400)
                    iJobs            = 0
                    .
                FOR EACH bTtblJob
                    WHERE bTtblJob.resource      EQ ttblJob.resource
                      AND bTtblJob.startDateTime LE dEndDateTime
                      AND bTtblJob.endDateTime   GE dStartDateTime
                    :
                    ASSIGN
                        iStartTime = IF bTtblJob.startDateTime LT dStartDateTime THEN 0
                                     ELSE INTEGER(bTtblJob.startTime / 60)
                        iEndTime   = IF bTtblJob.endDateTime   GT dEndDateTime   THEN 1440
                                     ELSE INTEGER(bTtblJob.endTime   / 60)
                        iJobs      = iJobs + 1
                        .
                    IF iStartTime EQ 0 THEN iStartTime = 1.
                    DO jdx = iStartTime TO iEndTime:
                        IF jdx LE EXTENT(lUsed) THEN
                        lUsed[jdx] = YES.
                    END. /* do jdx */
                END. /* each bTtblJob */
                DO jdx = 1 TO EXTENT(lUsed):
                  IF lUsed[jdx] THEN iBookedTime[idx] = iBookedTime[idx] + 1.
                END. /* do jdx */
                ASSIGN 
                    iBookedTime[idx] = iBookedTime[idx] * 60
                    iBookedTime[idx] = iBookedTime[idx] - IF iBookedTime[idx] NE 0 THEN iUsedTime[idx] ELSE 0
                    cBookedTime = IF iBookedTime[idx] EQ 86400 THEN "24:00"
                             ELSE IF iBookedTime[idx] EQ 0 THEN ""
                             ELSE STRING(iBookedTime[idx],"hh:mm")
                    .
                IF cBookedTime NE "" THEN
                cBookedTime = cBookedTime + " (" + STRING(iJobs) + ")".
                PUT UNFORMATTED 
                    '      <td bgcolor="#AAD5B9" align="center" nowrap><font face="{&fontFace}"><b>'
                    cBookedTime '</b></font></td>' SKIP
                    .
            END. /* do dtdate */
            PUT UNFORMATTED 
                '    </tr>' SKIP
                '    <tr>' SKIP
                .
            DO dtDate = TODAY TO TODAY + htmlCapacityDays - 1:
                ASSIGN
                    idx = dtDate - TODAY + 1
                    iAvailTime[idx] = 86400 - iUsedTime[idx] - iBookedTime[idx]
                    .
                cAvailTime = IF iAvailTime[idx] EQ 86400 THEN "24:00"
                        ELSE IF iAvailTime[idx] EQ 0 THEN ""
                        ELSE STRING(iAvailTime[idx],"hh:mm").
                PUT UNFORMATTED 
                    '      <td bgcolor="#F1FE98" align="center" nowrap><font face="{&fontFace}"><b>'
                    cAvailTime '</b></font></td>' SKIP
                    .
            END. /* do dtdate */
            PUT UNFORMATTED 
                '    </tr>' SKIP
                '    <tr>' SKIP
                '      <td colspan="34" bgcolor="#F4F4F4"></td>' SKIP 
                '    </tr>' SKIP
                .
        END. /* if first-of */
    END. /* each ttbljob */
    RUN pHTMLFooter.
    OUTPUT CLOSE.

    FOR EACH ttblJob NO-LOCK
        BREAK BY ttblJob.resource
              BY ttblJob.jobSequence
              BY ttblJob.jobSort
        :
        IF FIRST-OF(ttblJob.resource) THEN DO:
            FIND FIRST mach NO-LOCK 
                 WHERE mach.company EQ ENTRY(1,ttblJob.keyValue)
                   AND mach.m-code  EQ ttblJob.resource
                 NO-ERROR.
            ASSIGN 
                cHTMLTitle  = "Operation: " + ttblJob.resource
                            + (IF AVAILABLE mach THEN " - " + mach.m-dscr ELSE "")
                cHTMLLegend = "Operation: <b><font color=~"DC143C~">" + ttblJob.resource
                            + (IF AVAILABLE mach THEN " - " + mach.m-dscr ELSE "")
                            + "</font></b>"
                lScript     = NO
                .
            OUTPUT TO VALUE(htmlPageLocation + '\' + REPLACE(ttblJob.resource,"/","") + '.htm').
            RUN pHTMLHeader (cHTMLTitle,cHTMLLegend,lScript).
            RUN pHTMLBranding.
            PUT UNFORMATTED
                '  <table border="1" cellspacing="0" cellpadding="14" width="100%">' SKIP
                .
            RUN pHTMLLabel ("Job").
            lAltLine = YES.
        END. /* if first-of */
        ASSIGN 
            lAltLine = NOT lAltLine
            cBGColor = IF lAltLine THEN ' bgcolor="EEDFD2"' ELSE ''
            .
        RUN pHTMLData ("Job",cBGColor).
        IF LAST-OF(ttblJob.resource) THEN DO:
            RUN pHTMLFooter.
            OUTPUT CLOSE.
        END. /* if last-of */
    END. /* each ttbljob */
    
    FOR EACH pendingJob
        BREAK BY pendingJob.resource
              BY pendingJob.jobSort
        :
        IF FIRST-OF(pendingJob.resource) THEN DO: 
            FIND FIRST mach NO-LOCK 
                 WHERE mach.company EQ ENTRY(1,ttblJob.keyValue)
                   AND mach.m-code  EQ pendingJob.resource
                 NO-ERROR.
            ASSIGN 
                cHTMLTitle  = "Pending Operation: " + pendingJob.resource
                            + (IF AVAILABLE mach THEN " - " + mach.m-dscr ELSE "")
                cHTMLLegend = "Pending Operation: <b><font color=~"DC143C~">" + pendingJob.resource
                            + (IF AVAILABLE mach THEN " - " + mach.m-dscr ELSE "")
                            + "</font></b>"
                lScript     = NO
                .
            OUTPUT TO VALUE(htmlPageLocation + '\' + pendingJob.resource + 'Pending.htm').
            RUN pHTMLHeader (cHTMLTitle,cHTMLLegend,lScript).
            RUN pHTMLBranding.
            PUT UNFORMATTED
                '  <table border="1" cellspacing="0" cellpadding="14" width="100%">' SKIP
                .
            RUN pHTMLLabel ("Pending").
            lAltLine = YES.
        END. /* if first-of */
        ASSIGN 
            lAltLine = NOT lAltLine
            cBGColor = IF lAltLine THEN ' bgcolor="EEDFD2"' ELSE ''
            .
        RUN pHTMLData ("Pending",cBGColor).
        IF LAST-OF(pendingJob.resource) THEN DO:
            RUN pHTMLFooter.
            OUTPUT CLOSE.
        END. /* if first-of */
    END.
END PROCEDURE.

PROCEDURE pLogEntry:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcField AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcValue AS CHARACTER NO-UNDO.

    iLogEntry = iLogEntry + 1.
    RUN spCreateAuditDtl (
        iAuditID, /* audit id     */
        STRING(iLogEntry,">>9") + " " + ipcField, /* field */
        0,        /* extent       */
        ipcValue, /* before value */
        "",       /* after value  */
        NO        /* index field  */
        ).
        
END PROCEDURE.

PROCEDURE pttHTMLFields:
    DEFINE VARIABLE cFieldType   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldLabel  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldName   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFieldFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHTMLFields  AS CHARACTER NO-UNDO.

    cHTMLFields = SEARCH("{&data}/" + ID + "/htmlFields.dat").
    IF cHTMLFields NE ? THEN DO:
        INPUT FROM VALUE(cHTMLFields) NO-ECHO.
        REPEAT:
            IMPORT cFieldType cFieldLabel cFieldName cFieldFormat.
            CREATE ttHTMLFields.
            ASSIGN
                ttHTMLFields.fieldType   = cFieldType
                ttHTMLFields.fieldLabel  = cFieldLabel
                ttHTMLFields.fieldName   = cFieldName
                ttHTMLFields.fieldFormat = cFieldFormat
                .
        END. /* repeat */
        INPUT CLOSE.
    END. /* if ne ? */
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
