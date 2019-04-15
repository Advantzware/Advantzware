/* pCalcNextRun.i */

FUNCTION fLastOfMonth RETURNS DATE (ipdtDate AS DATE):
    DEFINE VARIABLE iMonth AS INTEGER NO-UNDO.
    
    iMonth = MONTH(ipdtDate).        .
    DO WHILE iMonth EQ MONTH(ipdtDate):
        ipdtDate = ipdtDate + 1.
    END.    
    RETURN ipdtDate - 1.
END FUNCTION.

FUNCTION fTimeConvert RETURNS CHARACTER (ipiTime AS INTEGER):
    RETURN REPLACE(STRING(ipiTime,"HH:MM"),":","").
END FUNCTION.

PROCEDURE pCalcNextRun:
    DEFINE INPUT PARAMETER iplLastDate AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE dtNextDate AS DATE    NO-UNDO.
    DEFINE VARIABLE iNextTime  AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtDate     AS DATE    NO-UNDO.
    DEFINE VARIABLE idx        AS INTEGER NO-UNDO.
    
    IF Task.scheduled AND Task.runNow EQ NO THEN DO:
        ASSIGN
            dtNextDate = IF TIME LE Task.taskTime THEN TODAY
                    ELSE IF TIME GT Task.taskTime THEN TODAY + 1
                    ELSE ?
            iNextTime  = Task.taskTime
            .
        CASE Task.frequency:
            WHEN "Every" THEN DO:
                ASSIGN
                    dtNextDate = IF TIME LE Task.fromTime THEN TODAY
                            ELSE IF TIME GT Task.toTime   THEN TODAY + 1
                            ELSE ?
                    iNextTime  = Task.fromTime
                    .
                IF dtNextDate EQ ? THEN
                DO WHILE iNextTime LE Task.toTime:
                    IF TIME LE iNextTime THEN LEAVE.
                    ASSIGN
                        dtNextDate = TODAY
                        iNextTime  = iNextTime + Task.taskTime
                        .
                END. /* do tdx */
            END. /* every */
            WHEN "Weekly" THEN
            DO dtDate = dtNextDate TO dtNextDate + 6:
                IF NOT Task.dayOfWeek[WEEKDAY(dtDate)] THEN NEXT.
                dtNextDate = dtDate.
                LEAVE.
            END. /* do idx */
            WHEN "Monthly" THEN DO:
                DO dtDate = dtNextDate TO dtNextDate + 30:
                    IF NOT Task.dayOfMonth[DAY(dtDate)] THEN NEXT.
                    ASSIGN
                        dtNextDate = dtDate
                        dtDate     = ?
                        .
                    LEAVE.
                END. /* do idx */
                /* if not ?, no day of month checked */
                IF dtDate NE ? THEN
                dtNextDate = 12/31/2049.
                /* check if last day of month checked */
                IF Task.lastOfMonth AND fLastOfMonth(TODAY) LT dtNextDate THEN
                dtNextDate = fLastOfMonth(TODAY).
            END.
        END CASE.
    END. /* if scheduled */
    
    DO TRANSACTION:
        FIND CURRENT Task EXCLUSIVE-LOCK.
        IF iplLastDate THEN
        ASSIGN
            Task.lastDate  = TODAY
            Task.lastTime  = TIME
            Task.cLastTime = fTimeConvert(Task.lastTime)
            Task.isRunning = NO
            .
        IF Task.runNow THEN DO:
            IF Task.nextDate EQ ? THEN
            DELETE Task.
            ELSE
            Task.runNow = NO.
        END. /* if runnow */
        ELSE
        ASSIGN
            Task.nextDate  = dtNextDate
            Task.nextTime  = iNextTime
            Task.cNextTime = fTimeConvert(Task.nextTime)
            .
        IF AVAILABLE Task THEN
        FIND CURRENT Task NO-LOCK.
    END. /* do trans */
END PROCEDURE.
