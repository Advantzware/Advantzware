/* shifts-2.i */

DO:
  ASSIGN
    {&type}starttime = {&starttime}
    {&type}endtime = {&endtime}.
  /* calculate start and end times based on shifts */
  FOR EACH ttbl_shifts NO-LOCK WHERE ttbl_shifts.shift = {&shiftfield}:
    IF {&starttime} LT ttbl_shifts.start_time THEN
    {&type}starttime = ttbl_shifts.start_time.
    IF {&endtime} GT ttbl_shifts.end_time THEN
    {&type}endtime = ttbl_shifts.end_time.
  END. /* each ttbl_shifts */
END.
