/* packResource.i */

  DEFINE INPUT PARAMETER ipResource AS CHARACTER NO-UNDO.

  DEFINE VARIABLE endingDate AS DATE NO-UNDO.
  DEFINE VARIABLE endingTime AS INTEGER NO-UNDO.
  DEFINE VARIABLE endingDateTime AS DECIMAL NO-UNDO.
  DEFINE VARIABLE boardEnd AS LOGICAL NO-UNDO.
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
  
  FIND FIRST ttblJob NO-LOCK WHERE ttblJob.resource EQ ipResource
                             USE-INDEX startDateTime NO-ERROR.
  ASSIGN
    lvStartDate = IF AVAILABLE ttblJob THEN ttblJob.startDate ELSE TODAY
    lvStartTime = IF AVAILABLE ttblJob THEN ttblJob.startTime ELSE TIME.
  FIND FIRST ttblJob NO-LOCK
       WHERE ttblJob.resource EQ ipResource
         AND ttblJob.startDateTime GE numericDateTime(intDate[1],intSTime[1])
       NO-ERROR.
  ASSIGN
    lvFirstDate = IF AVAILABLE ttblJob THEN ttblJob.startDate ELSE intDate[1]
    lvFirstTime = IF AVAILABLE ttblJob THEN ttblJob.startTime ELSE intSTime[1].
  RUN {&prompts}/packOptions.w ('Resource: ' + ipResource,
                                INPUT-OUTPUT packOption,packOptionPrompt,
                                lvStartDate,lvStartTime,
                                lvFirstDate,lvFirstTime,
                                intDate[1],intSTime[1],
                                intDate[24],intETime[24],
                                OUTPUT lvStartDate,OUTPUT lvStartTime,
                                OUTPUT lvEndDate,OUTPUT lvEndTime).
  IF lvStartDate EQ ? THEN RETURN.
  ASSIGN
    lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
    lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
    endingDateTime = lvEndDateTime
    priorDateTime[1] = lvStartDateTime.
  RUN afterDowntime (ipResource,1,
                     INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                     INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                     INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
  priorDateTime[2] = numericDateTime(lvStartDate,lvStartTime).
  IF priorDateTime[1] NE priorDateTime[2] THEN
  FOR EACH ttblJob EXCLUSIVE-LOCK WHERE ttblJob.resource EQ ipResource
                                    AND ttblJob.startDateTime GE priorDateTime[1]
                                    AND ttblJob.startDateTime LE priorDateTime[2]:
    IF ttblJob.jobLocked OR ttblJob.jobCompleted THEN NEXT.
    RUN newEnd (ttblJob.timeSpan,lvStartDate,lvStartTime,
                OUTPUT lvEndDate,OUTPUT lvEndTime).
    ASSIGN
      ttblJob.startDate = lvStartDate
      ttblJob.startTime = lvStartTime
      ttblJob.endDate = lvEndDate
      ttblJob.endTime = lvEndTime.
    ttblJob.startDateTime = numericDateTime(lvStartDate,lvStartTime).
    ttblJob.endDateTime = numericDateTime(lvEndDate,lvEndTime).
  END. /* each ttbljob */
  ASSIGN
    priorDate = lvStartDate
    priorTime = lvStartTime
    priorDateTime = numericDateTime(priorDate[1],priorTime[1]).
  FOR EACH ttblJob EXCLUSIVE-LOCK WHERE ttblJob.resource EQ ipResource
                                    AND ttblJob.endDateTime GE priorDateTime[1]
                                    AND ttblJob.startDateTime LE endingDateTime
      BY ttblJob.jobSequence:
    IF NOT ttblJob.jobLocked AND NOT ttblJob.jobCompleted THEN DO:
      RELEASE buffJob.
      IF cascadeJob THEN
      FIND LAST buffJob NO-LOCK
           WHERE buffJob.job EQ ttblJob.job
             AND buffJob.resourceSequence LT ttblJob.resourceSequence
             AND buffJob.endDateTime GT priorDateTime[2] NO-ERROR.
      IF AVAILABLE buffJob THEN DO:
        ASSIGN
          lvStartDate = buffJob.endDate
          lvStartTime = buffJob.endTime.
        IF buffJob.lagTime NE 0 THEN DO:
          RUN addTime (buffJob.startDate,buffJob.startTime,buffJob.lagTime,
                       OUTPUT lvStartDate,OUTPUT lvStartTime).
          ASSIGN
            lvStartDate = priorDate[1]
            lvStartTime = priorTime[1].
        END.
      END. /* avail buffjob */
      ELSE
      ASSIGN
        lvStartDate = priorDate[1]
        lvStartTime = priorTime[1].
      IF NOT useSequence THEN DO:
        IF NOT AVAILABLE buffJob THEN
        ASSIGN
          lvStartDate = priorDate[2]
          lvStartTime = priorTime[2].
        RUN newEnd (ttblJob.timeSpan,lvStartDate,lvStartTime,
                    OUTPUT lvEndDate,OUTPUT lvEndTime).
        ASSIGN
          lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
          lvEndDateTime = numericDateTime(lvEndDate,lvEndTime).
        RUN firstAvailable (ttblJob.resource,ROWID(ttblJob),ttblJob.timeSpan,
                            INPUT-OUTPUT lvStartDateTime,INPUT-OUTPUT lvEndDateTime,
                            INPUT-OUTPUT lvStartDate,INPUT-OUTPUT lvStartTime,
                            INPUT-OUTPUT lvEndDate,INPUT-OUTPUT lvEndTime).
      END.
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
    /*
    IF ttblJob.lagTime NE 0 THEN
    RUN addTime (ttblJob.startDate,ttblJob.startTime,ttblJob.lagTime,
                 OUTPUT lvEndDate,OUTPUT lvEndTime).
    */
    ASSIGN
      priorDate[1] = lvEndDate
      priorTime[1] = lvEndTime
      priorDateTime[2] = numericDateTime(priorDate[1],priorTime[1])
      .
  END. /* each ttblJob */
  IF NOT useSequence THEN DO:
    RUN msgFrame ('Setting Job Sequence Values').
    RUN setJobSequence.
  END.
  APPLY 'VALUE-CHANGED' TO intervals IN FRAME {&FRAME-NAME}.
