/* jobBGColor.i */

/*------------------------------------------------------------------------------
  Purpose:  set job bar color
    Notes:  conflict colors override custom values and base job colors
            need: DEFINE VARIABLE fgJobColor AS INTEGER NO-UNDO.
            need: SCOPED-DEFINE colorJobTable ttblJob|pendingJob
------------------------------------------------------------------------------*/
  DEFINE VARIABLE rtnColor AS INTEGER NO-UNDO.
  DEFINE VARIABLE n AS DECIMAL NO-UNDO. /* now, current date & time */
  DEFINE VARIABLE s AS DECIMAL NO-UNDO. /* start date & time */
  DEFINE VARIABLE e AS DECIMAL NO-UNDO. /* end date & time */
  DEFINE VARIABLE d AS DECIMAL NO-UNDO. /* due date & time */
  DEFINE VARIABLE i AS INTEGER NO-UNDO.
  
  ASSIGN
    n = numericDateTime(TODAY,TIME)
    s = numericDateTime({&colorJobTable}.startDate,{&colorJobTable}.startTime)
    e = numericDateTime({&colorJobTable}.endDate,{&colorJobTable}.endTime)
    d = IF dueDateUsed THEN numericDateTime({&colorJobTable}.dueDate,{&colorJobTable}.dueTime)
        ELSE numericDateTime({&colorJobTable}.prodDate,{&colorJobTable}.dueTime)
    fgJobColor = 0.
  IF {&colorJobTable}.jobCompleted THEN
  ASSIGN
    rtnColor = jobBGColor[2]
    fgJobColor = jobFGColor[2].
  IF rtnColor EQ 0 THEN
  FOR EACH jobColors NO-LOCK BY jobColors.priority:
    IF NOT jobColors.colorOn THEN NEXT.
    IF jobColors.idx GE 3 AND jobColors.idx LE EXTENT(jobBGColor) AND
     ((jobColors.idx EQ 3 AND n LE s AND s LE e AND e LT d) OR
      (jobColors.idx EQ 4 AND n LE s AND s LE d AND d LE e) OR
      (jobColors.idx EQ 5 AND n LE d AND d LE s AND s LE e) OR
      (jobColors.idx EQ 6 AND s LE n AND n LE e AND e LE d) OR
      (jobColors.idx EQ 7 AND s LE n AND n LE d AND d LE e) OR
      (jobColors.idx EQ 8 AND s LE d AND d LE n AND n LE e) OR
      (jobColors.idx EQ 9 AND s LE e AND e LE n AND n LE d) OR
      (jobColors.idx EQ 10 AND s LE e AND e LE d AND d LE n) OR
      (jobColors.idx EQ 11 AND s LE d AND d LE e AND e LE n) OR
      (jobColors.idx EQ 12 AND d LE s AND s LE e AND e LE n) OR
      (jobColors.idx EQ 13 AND d LE s AND s LE n AND n LE e) OR
      (jobColors.idx EQ 14 AND d LE n AND n LE s AND s LE e)) THEN
    ASSIGN
      rtnColor = jobColors.bgColorValue
      fgJobColor = jobColors.fgColorValue.
    IF rtnColor EQ 0 AND jobColors.idx GE EXTENT(jobBGColor) + 1 AND
       jobColors.idx LE EXTENT(jobBGColor) + EXTENT(customBGColor) AND
       CAN-DO({&colorJobTable}.userValue,jobColors.jobValue) AND
       NOT {&colorJobTable}.jobStatus[jobColors.idx - EXTENT(jobBGColor)] THEN
    ASSIGN
      rtnColor = jobColors.bgColorValue
      fgJobColor = jobColors.fgColorValue.
    {{&includes}/{&Board}/jobBGColor.i}
    IF rtnColor NE 0 THEN LEAVE.
  END. /* each jobcolors */
  RETURN rtnColor.
