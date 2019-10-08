/* getScenario.i - used in procedure getScenario in board.w and sbReport.p */
  EMPTY TEMP-TABLE ttblJob.
  INPUT FROM VALUE(SEARCH('{&scenarios}/' + ID + '/' + scenario + '.dat')) NO-ECHO.
  IMPORT version asOfTime.
  IF version EQ '{&version}' THEN
  REPEAT:
    CREATE ttblJob.
    IMPORT ttblJob.
    ASSIGN
      ttblJob.startDate = ttblJob.origStartDate
      ttblJob.startTime = ttblJob.origStartTime
      ttblJob.endDate = ttblJob.origEndDate
      ttblJob.endTime = ttblJob.origEndTime
      ttblJob.sequenced = ttblJob.jobSequence NE 0
      .
    IF ttblJob.timeSpan EQ 0 THEN
    ttblJob.timeSpan = timeSpan(ttblJob.startDate,ttblJob.startTime,
                                ttblJob.endDate,ttblJob.endTime).
    ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
    ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
    ttblJob.dueDateTime = numericDateTime(ttblJob.dueDate,ttblJob.dueTime).
    ASSIGN
      ttblJob.jobBGColor = jobBGColor()
      ttblJob.jobFGColor = jobFGColor()
      ttblJob.statusLabel = jobStatus()
      .
    DO i = 2 TO NUM-ENTRIES(customValueList):
      IF ttblJob.jobStatus[i - 1] THEN NEXT.
      ttblJob.prepCompleted = NO.
      LEAVE.
    END. /* do i */
    IF ttblJob.rowIDs EQ '' THEN
    ttblJob.rowIDs = STRING(ROWID(ttblJob)).
  END. /* repeat */
  ELSE
  MESSAGE 'Scenario "' + scenario + '" is Version' version SKIP
    'Scheduler is Version {&version}' VIEW-AS ALERT-BOX ERROR
    TITLE 'Scenario Load Failed'.
  IF AVAILABLE ttblJob AND ttblJob.job EQ '' THEN
  DELETE ttblJob.
  INPUT CLOSE.
