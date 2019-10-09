/* btnRedo.i */

  currentOrder = currentOrder + 1.
  FIND ttblJob-do EXCLUSIVE-LOCK WHERE ttblJob-do.order EQ currentOrder.
  FIND ttblJob EXCLUSIVE-LOCK WHERE ROWID(ttblJob) EQ ttblJob-do.ttblJobRowID NO-ERROR.
  IF AVAILABLE ttblJob THEN
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN
      ttblJob.resource = ttblJob-do.resource[2]
      ttblJob.resourceSequence = ttblJob-do.resourceSequence[2]
      ttblJob.resourceDescription = ttblJob-do.resourceDescription[2]
      ttblJob.startDate = ttblJob-do.startDate[2]
      ttblJob.startTime = ttblJob-do.startTime[2]
      ttblJob.endDate = ttblJob-do.endDate[2]
      ttblJob.endTime = ttblJob-do.endTime[2]
      ttblJob.jobLocked = ttblJob-do.jobLocked[2]
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
    RUN setJobSequence.
  END.
  APPLY 'ENTRY' TO intervals.
  FIND LAST ttblJob-do NO-LOCK.
  IF ttblJob-do.order EQ currentOrder THEN
  DISABLE btnRedo WITH FRAME {&FRAME-NAME}.
  ENABLE btnUndo WITH FRAME {&FRAME-NAME}.
  RUN buildBoard (YES).
  RETURN NO-APPLY.
