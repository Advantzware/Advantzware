/* shiftStartEndTime.i */

DEFINE VARIABLE iShiftStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE iShiftEndTime   AS INTEGER NO-UNDO INITIAL 86400.

FIND FIRST shifts NO-LOCK
     WHERE shifts.company EQ ipcCompany
       AND shifts.shift   EQ STRING(iStartShift)
     NO-ERROR.
IF AVAILABLE shifts THEN
ASSIGN
    iShiftStartTime = shifts.start_time
    iShiftEndTime = shifts.end_time
    .

IF iStartShift NE iEndShift THEN DO:
    FIND FIRST shifts NO-LOCK
         WHERE shifts.company EQ ipcCompany
           AND shifts.shift   EQ STRING(iEndShift)
         NO-ERROR.
    IF AVAILABLE shifts THEN
    iShiftEndTime = shifts.end_time.
END. /* different shifts */
