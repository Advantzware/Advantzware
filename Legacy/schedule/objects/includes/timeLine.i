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
ELSE DO:
    externalStatusFile = SEARCH('{&updates}\' + ID + '\inUse.dat').
    IF externalStatusFile NE ? THEN DO:
        INPUT FROM VALUE(externalStatusFile) NO-ECHO.
        IMPORT externalStatusChecked externalStatusUserID.
        INPUT CLOSE.
        IF NOT externalStatusChecked THEN DO:
            MESSAGE
                'External Status Checkoffs in Use by'
                externalStatusUserID 
            VIEW-AS ALERT-BOX.
            externalStatusFile = SEARCH('{&updates}\' + ID + '\inUse.dat').
            IF externalStatusFile NE ? THEN DO:
                OUTPUT TO VALUE(externalStatusFile) NO-ECHO.
                EXPORT YES externalStatusUserID.
                OUTPUT CLOSE.
            END. /* if file still exists */
        END. /* if checked */
    END. /* if */
END. /* else */
