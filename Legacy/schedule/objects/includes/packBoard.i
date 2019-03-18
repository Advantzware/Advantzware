/* packBoard.i */

/*------------------------------------------------------------------------------
  Purpose:     move and schedule jobs based on selection from container
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE ipValue AS CHARACTER NO-UNDO INITIAL 'All,,,,,'.
  DEFINE VARIABLE opContinue AS LOGICAL NO-UNDO.
  
  DEFINE VARIABLE condition AS LOGICAL NO-UNDO EXTENT 4.
  DEFINE VARIABLE endlessLoop AS LOGICAL NO-UNDO.
  DEFINE VARIABLE firstDate AS DATE NO-UNDO.
  DEFINE VARIABLE firstDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE firstTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE jobMoved AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lastDate AS DATE NO-UNDO.
  DEFINE VARIABLE lastDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lastTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvFirstDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvFirstTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvResource AS CHARACTER NO-UNDO.
  DEFINE VARIABLE resourceFirstDate AS DATE NO-UNDO.
  DEFINE VARIABLE resourceFirstTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE stackJob AS LOGICAL NO-UNDO.

  FIND FIRST ttblJob NO-LOCK USE-INDEX startDateTimeIdx NO-ERROR.
  IF NOT AVAILABLE ttblJob THEN RETURN.
  lvResource = ttblJob.resource.
  FIND FIRST ttblJob NO-LOCK WHERE ttblJob.resource EQ lvResource NO-ERROR.
  ASSIGN
    lvStartDate = IF AVAILABLE ttblJob THEN ttblJob.startDate ELSE TODAY
    lvStartTime = IF AVAILABLE ttblJob THEN ttblJob.startTime ELSE TIME.
  FIND FIRST ttblJob NO-LOCK
       WHERE ttblJob.startDateTime GE numericDateTime(intDate[1],intSTime[1]) NO-ERROR.
  ASSIGN
    lvFirstDate = IF AVAILABLE ttblJob THEN ttblJob.startDate ELSE intDate[1]
    lvFirstTime = IF AVAILABLE ttblJob THEN ttblJob.startTime ELSE intSTime[1].
  RUN {&prompts}/packOptions.w ('Board',
                                INPUT-OUTPUT packOption,packOptionPrompt,
                                lvStartDate,lvStartTime,
                                lvFirstDate,lvFirstTime,
                                intDate[1],intSTime[1],
                                intDate[24],intETime[24],
                                OUTPUT firstDate,OUTPUT firstTime,
                                OUTPUT lastDate,OUTPUT lastTime).
  IF lvStartDate EQ ? THEN RETURN.
  ASSIGN
    firstDateTime = numericDateTime(firstDate,firstTime)
    lastDateTime = numericDateTime(lastDate,lastTime).
  
  APPLY 'ENTRY' TO intervals IN FRAME {&FRAME-NAME}.
  RUN setScreenStatus.
  RUN msgFrame ('Auto Schedule: ' + ipValue).

  &SCOPED-DEFINE ttblPhrase1 ttblJob.endDateTime GE firstDateTime ~
  AND ttblJob.startDateTime LE lastDateTime

  &SCOPED-DEFINE ttblPhrase2 {&ttblPhrase1} AND ttblJob.sortSequence EQ 0
  
  &SCOPED-DEFINE buffPhrase buffJob.endDateTime GE firstDateTime ~
  AND buffJob.startDateTime LE lastDateTime
  
  FOR EACH ttblJob EXCLUSIVE-LOCK WHERE {&ttblPhrase1}:
    ASSIGN
      ttblJob.sortSequence = 0
      ttblJob.anchored = NO.
  END. /* each ttbljob */
  DO WHILE CAN-FIND(FIRST ttblJob WHERE {&ttblPhrase2}):
    endlessLoop = YES.
    FOR EACH ttblJob EXCLUSIVE-LOCK WHERE {&ttblPhrase2} USE-INDEX resourceSequence
                     BY ttblJob.resourceSequence BY ttblJob.jobSequence:
      ASSIGN
        condition[1] = NOT CAN-FIND(buffJob
                       WHERE {&buffPhrase}
                         AND buffJob.job EQ ttblJob.job
                         AND buffJob.resourceSequence EQ ttblJob.resourceSequence - 1)
        condition[2] = NOT CAN-FIND(buffJob
                       WHERE {&buffPhrase}
                         AND buffJob.resource EQ ttblJob.resource
                         AND buffJob.jobSequence EQ ttblJob.jobSequence - 1)
        condition[3] = CAN-FIND(buffJob
                       WHERE {&buffPhrase}
                         AND buffJob.job EQ ttblJob.job
                         AND buffJob.resourceSequence EQ ttblJob.resourceSequence - 1
                         AND buffJob.sortSequence NE 0)
        condition[4] = CAN-FIND(buffJob
                       WHERE {&buffPhrase}
                         AND buffJob.resource EQ ttblJob.resource
                         AND buffJob.jobSequence EQ ttblJob.jobSequence - 1
                         AND buffJob.sortSequence NE 0).
      IF condition[1] AND condition[2] OR
         condition[1] AND condition[4] OR
         condition[2] AND condition[3] OR
         condition[3] AND condition[4] THEN
      RUN setSortSequence (ipValue).
      IF ttblJob.sortSequence NE 0 THEN endlessLoop = NO.
    END. /* each ttbljob */
    IF endlessLoop THEN LEAVE.
  END. /* do while */
  IF endLessLoop THEN
  DO:
    RUN LockWindowUpdate (0,OUTPUT i).
    RUN msgFrame (?).
    HIDE FRAME msgFrame NO-PAUSE.
    MESSAGE 'An Endless Loop Condition Exists!!!' SKIP(1)
      'Run the Error Exception Report for' SKIP
      'assistance to resolve Error Conditions' VIEW-AS ALERT-BOX ERROR.
    RETURN.
  END. /* if endlessloop */
  
  opContinue = YES.
  FOR EACH ttblJob NO-LOCK USE-INDEX packIdx
      WHERE ttblJob.endDateTime GE firstDateTime
        AND ttblJob.startDateTime LE lastDateTime:
    ASSIGN
      lvStartDate = ttblJob.startDate
      lvStartTime = ttblJob.startTime
      lvEndDate = ttblJob.endDate
      lvEndTime = ttblJob.endTime
      jobMoved = NO
      i = i + 1.
    IF NOT ttblJob.jobLocked AND
       NOT ttblJob.anchored AND
       NOT ttblJob.jobCompleted THEN
    DO:
      ASSIGN
        resourceFirstDate = firstDate
        resourceFirstTime = firstTime.
      IF packOption EQ 1 OR packOption EQ 5 THEN
      DO:
        FIND FIRST buffJob NO-LOCK
             WHERE buffJob.resource EQ ttblJob.resource
             USE-INDEX startDateTime NO-ERROR.
        IF AVAILABLE buffJob THEN
        ASSIGN
          resourceFirstDate = buffJob.startDate
          resourceFirstTime = buffJob.startTime.
      END.
      ASSIGN
        lvStartDate = resourceFirstDate
        lvStartTime = resourceFirstTime
        lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
        jobMoved = YES.
      DO WHILE TRUE:
        FIND FIRST boardDowntime NO-LOCK
             WHERE boardDowntime.resource EQ ttblJob.resource
               AND boardDowntime.startDateTime LE lvStartDateTime
               AND boardDowntime.endDateTime GT lvStartDateTime NO-ERROR.
        IF AVAILABLE boardDowntime THEN
        DO:
          ASSIGN
            lvStartDate = boardDowntime.endDate
            lvStartTime = boardDowntime.endTime.
          IF boardDowntime.endTime EQ 86400 THEN
          ASSIGN
            lvStartDate = lvStartDate + 1
            lvStartTime = 0.
          RUN newEnd (ttblJob.timeSpan,lvStartDate,lvStartTime,
                      OUTPUT lvEndDate,OUTPUT lvEndTime).
          ASSIGN
            lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
            lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
        END. /* if avail */
        ELSE LEAVE.
      END. /* while true */
      IF checkDowntimeConflict(ttblJob.resource,lvStartDateTime,lvEndDateTime) THEN
      RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,firstDate,0,
                        OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
      RUN jobStacker ('Job',ttblJob.job,ttblJob.resource,ttblJob.jobSequence,
                      ttblJob.timeSpan,resourceFirstDate,resourceFirstTime,
                      INPUT-OUTPUT jobMoved,INPUT-OUTPUT lvDowntimeSpan,
                      INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                      INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
      RUN jobStacker ('Resource',ttblJob.job,ttblJob.resource,ttblJob.jobSequence,
                      ttblJob.timeSpan,resourceFirstDate,resourceFirstTime,
                      INPUT-OUTPUT jobMoved,INPUT-OUTPUT lvDowntimeSpan,
                      INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                      INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
    END. /* if not, not, not */
    RUN jobStackerCreate (i,ttblJob.resource,ttblJob.jobSequence,ttblJob.job,lvDowntimeSpan,
                        lvStartDate,lvStartTime,lvEndDate,lvEndTime,ttblJob.lagTime,
                        jobMoved,ROWID(ttblJob)).
  END. /* each ttblJob */
  RUN msgFrame ('Auto Schedule: Update Jobs from Stack').
  FOR EACH jobStacker NO-LOCK:
    IF NOT jobStacker.jobChanged THEN NEXT.
    FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ jobStacker.ttblRowID.
    /* want to remove this functionality
    RUN jobMoveHistory (ROWID(ttblJob),jobStacker.startDate,jobStacker.startTime,
                        jobStacker.endDate,jobStacker.endTime,ttblJob.jobLocked,
                        jobStacker.downtimeSpan). */
    ASSIGN
      ttblJob.startDate = jobStacker.startDate
      ttblJob.startTime = jobStacker.startTime
      ttblJob.endDate = jobStacker.endDate
      ttblJob.endTime = jobStacker.endTime
      ttblJob.downtimeSpan = jobStacker.downtimeSpan
      schdChanged = YES. /* new */
    ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
    ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
    ASSIGN
      ttblJob.jobBGColor = jobBGColor()
      ttblJob.jobFGColor = jobFGColor()
      ttblJob.statusLabel = jobStatus()
      .
  END. /* each jobStacker */
  EMPTY TEMP-TABLE jobStacker.
  RUN msgFrame ('Setting Job Sequence Values').
  RUN setJobSequence.
  buildDowntime = NO.
  IF ENTRY(6,ipValue) EQ '' THEN
  APPLY 'VALUE-CHANGED' TO intervals IN FRAME {&FRAME-NAME}.
