/* shiftStartEndTime.i */

DEFINE VARIABLE iShiftStartTime AS INTEGER NO-UNDO.
DEFINE VARIABLE iShiftEndTime   AS INTEGER NO-UNDO INITIAL 86400.

FUNCTION fCalcTime RETURNS INTEGER (ipcType AS CHARACTER, ipcTime AS CHARACTER, ipcAMPM AS CHARACTER):
    DEFINE VARIABLE iMinutes AS INTEGER NO-UNDO.
    DEFINE VARIABLE iHours   AS INTEGER NO-UNDO.
    DEFINE VARIABLE iPlus12  AS INTEGER NO-UNDO.
    DEFINE VARIABLE iTime    AS INTEGER NO-UNDO.

    ASSIGN
        iMinutes = INTEGER(SUBSTR(ipcTime,4,2))
        iHours   = INTEGER(SUBSTR(ipcTime,1,2))
        iHours   = IF iHours EQ 12 AND ipcAMPM EQ "AM" THEN 0 ELSE iHours
        iPlus12  = IF ipcAMPM EQ "pm" AND iHours LT 12 THEN 12 ELSE 0
        iHours   = iHours + iPlus12
        iTime    = iHours * 3600 + iMinutes * 60
        .
    IF ipcType EQ "End" AND ipcAMPM EQ "am" AND iTime EQ 0 THEN iTime = 86400.
    
    RETURN iTime.
END FUNCTION.

IF lUseTime THEN
    ASSIGN
        iShiftStartTime = fCalcTime("Start", cStartTime, cStartAMPM)        
        iShiftEndTime   = fCalcTime("End", cEndTime, cEndAMPM)
        .
ELSE DO:
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
END. /* else */
