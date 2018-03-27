/* timeLine.i - used in OCX.Tick trigger in schedule.w */

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
