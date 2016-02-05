/* shifts-1.i */

DO:
  ASSIGN
    startshift = ''
    endshift = ''
    {&type}shifts = ''
    realshifts = ''.
  /* get starting and ending shifts for employee */
  FOR EACH ttbl_shifts NO-LOCK:
    IF startshift = '' AND
       {&starttime} GE ttbl_shifts.start_time AND
       {&starttime} LE ttbl_shifts.end_time THEN
    startshift = ttbl_shifts.shift.
    IF endshift = '' AND
       {&endtime} GE ttbl_shifts.start_time AND
       {&endtime} LE ttbl_shifts.end_time THEN
    endshift = ttbl_shifts.shift.
    IF startshift NE '' AND endshift NE '' THEN
    LEAVE.
  END. /* each ttbl_shifts */
  /* put all shifts in comma delimited string */
  FIND FIRST ttbl_shifts WHERE ttbl_shifts.shift = startshift NO-LOCK NO-ERROR.
  DO WHILE TRUE:
    ASSIGN
      {&type}shifts = IF {&type}shifts = '' THEN ttbl_shifts.shift
                      ELSE {&type}shifts + ',' + ttbl_shifts.shift
      realshifts = IF realshifts = '' THEN ttbl_shifts.real_shift
                   ELSE realshifts + ',' + ttbl_shifts.real_shift.
    IF ttbl_shifts.shift = endshift THEN
    LEAVE.
    FIND NEXT ttbl_shifts NO-LOCK NO-ERROR.
    IF NOT AVAILABLE ttbl_shifts THEN
    FIND FIRST ttbl_shifts NO-LOCK NO-ERROR.
  END. /* do white true */
END. /* do */
