/* jobStatusFunc.i */

FUNCTION jobStatus RETURNS CHARACTER:
  IF AVAILABLE {&useTable} THEN DO: 
      IF {&useTable}.jobCompleted THEN DO:
        FIND FIRST jobColors
             WHERE jobColors.priority EQ 0
               AND jobColors.idx EQ 2
             NO-ERROR.
        RETURN IF AVAILABLE jobColors THEN jobColors.colorLabel ELSE "".
      END. /* if jobcompleted */
      ELSE
      FOR EACH jobColors:
        IF jobColors.bgColorValue NE ? AND jobColors.bgColorValue EQ {&useTable}.jobBGColor THEN
        RETURN jobColors.colorLabel.
      END. /* each jobcolors */
  END. /* if avail */
  RETURN ''.
END FUNCTION.
