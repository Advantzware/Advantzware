/* jobBGColor.i - used in function jobBGColor in board.w */

    &IF DEFINED(colorJobTable) EQ 0 OR '{&colorJobTable}' EQ 'ttblJob' &THEN
    IF rtnColor EQ 0 THEN
    CASE jobColors.jobValue:
      WHEN 'Job.Conflict' THEN
      IF jobConflictBGColor NE ? AND
         checkJobConflict(ttblJob.resource,ttblJob.startDateTime,
                          ttblJob.endDateTime,ROWID(ttblJob)) THEN
      ASSIGN
        rtnColor = jobColors.bgColorValue
        fgJobColor = jobColors.fgColorValue.
      WHEN 'Downtime.Conflict' THEN
      IF downtimeConflictBGColor NE ? AND
         checkDowntimeConflict(ttblJob.resource,ttblJob.startDateTime,
                               ttblJob.endDateTime) THEN
      ASSIGN
        rtnColor = jobColors.bgColorValue
        fgJobColor = jobColors.fgColorValue.
    END CASE.
    &ENDIF
