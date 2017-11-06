/* packJob.i */

  DEFINE INPUT PARAMETER ipJob AS CHARACTER NO-UNDO.

  DEFINE VARIABLE endingDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvFirstDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvFirstTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvStartTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvEndDate AS DATE NO-UNDO.
  DEFINE VARIABLE lvEndTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvEndDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE lvTimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE lvDowntimeSpan AS INTEGER NO-UNDO.
  DEFINE VARIABLE priorDate AS DATE NO-UNDO EXTENT 2.
  DEFINE VARIABLE priorTime AS INTEGER NO-UNDO EXTENT 2.
  DEFINE VARIABLE priorDateTime AS DECIMAL NO-UNDO EXTENT 2.
  DEFINE VARIABLE lvResource AS CHARACTER NO-UNDO.
  
  FIND FIRST ttblJob NO-LOCK
       WHERE ttblJob.job EQ ipJob USE-INDEX resourceSequence NO-ERROR.
  IF NOT AVAILABLE ttblJob THEN RETURN.
  lvResource = ttblJob.resource.
  FIND FIRST ttblJob NO-LOCK WHERE ttblJob.resource EQ lvResource NO-ERROR.
  ASSIGN
    lvStartDate = IF AVAILABLE ttblJob THEN ttblJob.startDate ELSE TODAY
    lvStartTime = IF AVAILABLE ttblJob THEN ttblJob.startTime ELSE TIME.
  FIND FIRST ttblJob NO-LOCK WHERE ttblJob.resource EQ lvResource
       AND ttblJob.startDateTime GE numericDateTime(intDate[1],intSTime[1]) NO-ERROR.
  ASSIGN
    lvFirstDate = IF AVAILABLE ttblJob THEN ttblJob.startDate ELSE intDate[1]
    lvFirstTime = IF AVAILABLE ttblJob THEN ttblJob.startTime ELSE intSTime[1].
  RUN {&prompts}/packOptions.w ('Job: ' + ipJob,
                                INPUT-OUTPUT packOption,packOptionPrompt,
                                lvStartDate,lvStartTime,
                                lvFirstDate,lvFirstTime,
                                intDate[1],intSTime[1],
                                intDate[24],intETime[24],
                                OUTPUT lvStartDate,OUTPUT lvStartTime,
                                OUTPUT lvEndDate,OUTPUT lvEndTime).
  IF lvStartDate EQ ? THEN RETURN.
  RUN setScreenStatus.
  RUN msgFrame ('Packing Job: ' + ipJob).
  ASSIGN
    priorDate[1] = lvStartDate
    priorTime[1] = lvStartTime
    endingDateTime = numericDateTime(lvEndDate,lvEndTime)
    priorDateTime = numericDateTime(priorDate[1],priorTime[1]).
  FOR EACH ttblJob NO-LOCK WHERE ttblJob.job EQ ipJob
                             AND ttblJob.endDateTime GE priorDateTime[1]
                             AND ttblJob.startDateTime LE endingDateTime
      BY ttblJob.resourceSequence BY ttblJob.jobSequence:
    IF NOT ttblJob.jobLocked AND NOT ttblJob.jobCompleted THEN
    DO:
      RELEASE buffJob.
      IF cascadeJob THEN
      FIND LAST buffJob NO-LOCK USE-INDEX resourceSequence
           WHERE buffJob.job EQ ttblJob.job
             AND buffJob.resourceSequence LT ttblJob.resourceSequence
             AND buffJob.endDateTime GT priorDateTime[2] NO-ERROR.
      ELSE
      FIND LAST buffJob NO-LOCK
           WHERE buffJob.resource EQ ttblJob.resource
             AND buffJob.jobSequence LT ttblJob.jobSequence
             AND buffJob.endDateTime GT priorDateTime[2] NO-ERROR.
      IF AVAILABLE buffJob THEN DO:
        ASSIGN
          lvStartDate = buffJob.endDate
          lvStartTime = buffJob.endTime.
        IF buffJob.lagTime NE 0 THEN
        RUN addTime (buffJob.startDate,buffJob.startTime,buffJob.lagTime,
                     OUTPUT lvStartDate,OUTPUT lvStartTime).
      END. /* avail buffjob */
      ELSE
      ASSIGN
        lvStartDate = priorDate[1]
        lvStartTime = priorTime[1].
      RUN newEnd (ttblJob.timeSpan,lvStartDate,lvStartTime,
                  OUTPUT lvEndDate,OUTPUT lvEndTime).
      ASSIGN
        lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
        lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
      RUN firstAvailable (ttblJob.resource,ROWID(ttblJob),ttblJob.timeSpan,
                          INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                          INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                          INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
      RUN downtimeSpan (ttblJob.resource,ttblJob.timeSpan,lvStartDate,lvStartTime,
                        OUTPUT lvEndDate,OUTPUT lvEndTime,OUTPUT lvDowntimeSpan).
      ASSIGN
        ttblJob.startDate = lvStartDate
        ttblJob.startTime = lvStartTime
        ttblJob.endDate = lvEndDate
        ttblJob.endTime = lvEndTime
        ttblJob.downtimeSpan = lvDowntimeSpan
        .
      ttblJob.startDateTime = numericDateTime(lvStartDate,lvStartTime).
      ttblJob.endDateTime = numericDateTime(lvEndDate,lvEndTime).
      ASSIGN
        ttblJob.jobBGColor = jobBGColor()
        ttblJob.jobFGColor = jobFGColor()
        ttblJob.statusLabel = jobStatus()
        .
    END. /* if not joblocked & not jobcompleted */
    ELSE
    ASSIGN
      lvEndDate = ttblJob.endDate
      lvEndTime = ttblJob.endTime
      .
    IF cascadeJob THEN DO:
      IF ttblJob.lagTime NE 0 THEN
      RUN addTime (ttblJob.startDate,ttblJob.startTime,ttblJob.lagTime,
                   OUTPUT lvEndDate,OUTPUT lvEndTime).
      ASSIGN
        priorDate[1] = lvEndDate
        priorTime[1] = lvEndTime
        priorDateTime[2] = numericDateTime(priorDate[1],priorTime[1])
        .
    END. /* if cascadejob */
  END. /* each ttblJob */
  RUN msgFrame ('Setting Job Sequence Values').
  RUN setJobSequence.
  APPLY 'VALUE-CHANGED' TO intervals IN FRAME {&FRAME-NAME}.
