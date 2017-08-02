/* shiftStartEndTime.i */

DEFINE VARIABLE iStartTime  AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEndTime    AS INTEGER   NO-UNDO INITIAL 86400.

FIND FIRST shifts NO-LOCK
     WHERE shifts.company EQ ipcCompany
       AND shifts.shift   EQ STRING(iStartShift)
     NO-ERROR.
IF AVAILABLE shifts THEN
ASSIGN
    iStartTime = shifts.start_time
    iEndTime = shifts.end_time
    .

IF iStartShift NE iEndShift THEN DO:
    FIND FIRST shifts NO-LOCK
         WHERE shifts.company EQ ipcCompany
           AND shifts.shift   EQ STRING(iEndShift)
         NO-ERROR.
    IF AVAILABLE shifts THEN
    iEndTime = shifts.end_time.
END. /* different shifts */
