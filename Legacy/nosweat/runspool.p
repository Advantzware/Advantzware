/* runspool.p */

{methods/defines/hndldefs.i}

IF SESSION:BATCH-MODE THEN 
  IF SEARCH('splchkup') NE ? THEN DO:
    {methods/nowait.i}.
    RETURN.
  END. /* if */
  ELSE DO:
    OUTPUT TO splchkup.
    OUTPUT CLOSE.
  END. /* else */

REPEAT:
  FOR EACH user-print WHERE user-print.batch NE '',
      FIRST user-batch NO-LOCK
      WHERE user-batch.company EQ user-print.company
        AND user-batch.batch-seq EQ user-print.batch-seq
        AND user-batch.prog-seq EQ user-print.prog-seq:
    IF (TODAY GT user-print.next-date OR
       (TODAY EQ user-print.next-date AND
        TIME GE user-print.next-time)) AND
        SEARCH(user-print.program-id) NE ? THEN DO:
      OUTPUT TO 'spoolrpt/spooler.log' APPEND.
      RUN VALUE(user-print.program-id) (INPUT user-print.batch-seq).
      RUN setNextRun.
      PUT UNFORMATTED
        user-print.program-id AT 1
        user-print.last-date FORMAT '99.99.9999' AT 30 ' @ '
        STRING(user-print.last-time,'hh:mm:ss am') SKIP.
      OUTPUT CLOSE.
    END. /* run spool */
  END. /* each user-print */

  /* started as a batch process */
  IF SESSION:BATCH-MODE THEN DO:
    /* check if shutdown issued */
    IF SEARCH('splchkdn') NE ? THEN DO:
      OS-DELETE splchkup.
      OS-DELETE splchkdn.
      LEAVE.
    END. /* if search */
    PAUSE 5 NO-MESSAGE.
  END. /* if session */
  /* not a batch process, leave after one pass */
  ELSE LEAVE.
END. /* REPEAT */

{methods/nowait.i}

PROCEDURE setNextRun:
  DEFINE VARIABLE firstDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE nextDay AS INTEGER NO-UNDO.
  DEFINE VARIABLE day# AS INTEGER NO-UNDO.
  DEFINE VARIABLE idx AS INTEGER NO-UNDO.
  
  ASSIGN
    user-print.last-date = TODAY
    user-print.last-time = TIME
    /* assume no more runs to start */
    user-print.next-date = ?
    day# = WEEKDAY(TODAY).
  /* check if future runs required */
  IF user-batch.endDate GT TODAY OR
    (user-batch.endDate EQ TODAY AND
     user-batch.endTime GE TIME) THEN DO:
    DO idx = 1 TO 7:
      IF firstDay EQ 0 AND user-batch.dayOfWeek[idx] THEN
      firstDay = idx. /* first day of week selected */
      IF nextDay EQ 0 AND idx GT day# AND user-batch.dayOfWeek[idx] THEN
      nextDay = idx. /* next day of week selected */
    END. /* do idx */
    IF nextDay NE 0 THEN /* increment to next day of week */
    ASSIGN
      user-print.next-date = TODAY + (nextDay - day#)
      user-print.next-time = user-batch.startTime.
    ELSE
    /* if weekly repeat, loop around and set to first day of week */
    IF user-batch.repeatWeekly AND firstDay NE 0 THEN
    ASSIGN
      user-print.next-date = TODAY + (7 - day# + firstDay)
      user-print.next-time = user-batch.startTime.
  END. /* if lt today */
  /* failed to assign next date, so clear values, no more runs */
  IF user-print.next-date = ? THEN
  user-print.next-time = 0.
END PROCEDURE.
