/* timeLine.i - used in OCX.Tick trigger in schedule.w */

DEFINE VARIABLE externalStatusFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE externalStatusChecked AS LOGICAL NO-UNDO.
DEFINE VARIABLE externalStatusUserID AS CHARACTER NO-UNDO.

RUN timeLine IN h_board.
{&WINDOW-NAME}:TITLE = STRING(TIME,'HH:MM:SS am') + ' - '
                     + winTitle + updatesPending().

IF autoMonitor THEN DO WITH FRAME {&FRAME-NAME}:
    btnAutoMonitor:LOAD-IMAGE('{&images}/trafficlight' +
                   STRING(autoMonitorImage) + '.gif').
    autoMonitorImage = autoMonitorImage + 1.
    IF autoMonitorImage GT 7 THEN
    autoMonitorImage = 0.
END. /* if automonitor */
&IF '{&Board}' EQ 'Pro' &THEN
ELSE DO:
    FIND FIRST reftable NO-LOCK
         WHERE reftable.reftable EQ "SBCheckoffs"
           AND reftable.code     EQ ID
         NO-ERROR.
    IF AVAILABLE reftable THEN DO:
        IF reftable.dscr NE "Viewed" THEN DO TRANSACTION:
            MESSAGE
                'External Status Checkoffs in Use by'
                reftable.code2
            VIEW-AS ALERT-BOX.
            FIND CURRENT reftable EXCLUSIVE-LOCK.
            reftable.dscr = "Viewed".
            RELEASE reftable.
        END. /* if not viewed */
    END. /* if avail */
END. /* else */
&ENDIF
