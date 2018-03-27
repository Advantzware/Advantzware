/* getPending.i - used in getScenario in board.w and sbReport.p */

  EMPTY TEMP-TABLE pendingJob.
  INPUT FROM VALUE(SEARCH('{&data}/' + ID + '/Pending.dat')) NO-ECHO.
  IMPORT version asOfTime.
  IF version EQ '{&version}' THEN
  REPEAT:
    CREATE pendingJob.
    IMPORT pendingJob.
    DO i = 2 TO NUM-ENTRIES(customValueList):
      IF pendingJob.jobStatus[i - 1] THEN NEXT.
      pendingJob.prepCompleted = NO.
      LEAVE.
    END. /* do i */
    IF pendingJob.rowIDs EQ '' THEN
    pendingJob.rowIDs = STRING(ROWID(pendingJob)).
  END.
  IF AVAILABLE pendingJob AND pendingJob.resource EQ '' THEN DELETE pendingJob.
  INPUT CLOSE.
