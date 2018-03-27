/* jobStatusFunc.i */

FUNCTION jobStatus RETURNS CHARACTER:
  IF AVAILABLE {&useTable} THEN DO: 
      IF {&useTable}.jobCompleted THEN DO:
        FIND jobColors NO-LOCK WHERE jobColors.priority EQ 0 AND jobColors.idx EQ 2 NO-ERROR.
        RETURN jobColors.colorLabel.
      END. /* if jobcompleted */
      ELSE
      FOR EACH jobColors NO-LOCK:
        IF jobColors.bgColorValue NE ? AND jobColors.bgColorValue EQ {&useTable}.jobBGColor THEN
        RETURN jobColors.colorLabel.
      END. /* each jobcolors */
  END. /* if avail */
  RETURN ''.
END FUNCTION.
