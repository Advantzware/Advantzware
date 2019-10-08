/* btnUndo.i */

  FIND ttblJob-do WHERE ttblJob-do.order EQ currentOrder NO-LOCK.
  FIND ttblJob WHERE ROWID(ttblJob) EQ ttblJob-do.ttblJobRowID EXCLUSIVE-LOCK NO-ERROR.
  IF AVAILABLE ttblJob THEN
  DO:
    IF NOT ttblJob-do.copyOf THEN
    DO WITH FRAME {&FRAME-NAME}:
      ASSIGN
        ttblJob.resource = ttblJob-do.resource[1]
        ttblJob.resourceSequence = ttblJob-do.resourceSequence[1]
        ttblJob.resourceDescription = ttblJob-do.resourceDescription[1]
        ttblJob.startDate = ttblJob-do.startDate[1]
        ttblJob.startTime = ttblJob-do.startTime[1]
        ttblJob.endDate = ttblJob-do.endDate[1]
        ttblJob.endTime = ttblJob-do.endTime[1]
        ttblJob.jobLocked = ttblJob-do.jobLocked[1]
        .
      ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime).
      ttblJob.endDateTime = numericDateTime(ttblJob.endDate,ttblJob.endTime).
      ASSIGN
        ttblJob.jobBGColor = jobBGColor()
        ttblJob.jobFGColor = jobFGColor()
        ttblJob.statusLabel = jobStatus()
        .
      FIND CURRENT ttblJob NO-LOCK.
      IF boardDate:SCREEN-VALUE NE STRING(ttblJob.startDate,'99/99/9999') AND
         moveUndoRedo THEN
      boardDate:SCREEN-VALUE = STRING(ttblJob.startDate).
    END.
    ELSE
    DELETE ttblJob.
    RUN setJobSequence.
  END.
  currentOrder = currentOrder - 1.
  APPLY 'ENTRY' TO intervals.
  IF currentOrder EQ 0 THEN
  DISABLE btnUndo WITH FRAME {&FRAME-NAME}.
  ENABLE btnRedo WITH FRAME {&FRAME-NAME}.
  RUN buildBoard (YES).
  RETURN NO-APPLY.
