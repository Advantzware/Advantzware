/* btnAutoMonitor.i - trigger for btnAutoMonitor in schedule.w */

  ASSIGN
    autoMonitor = NOT autoMonitor
    SELF:TOOLTIP = SELF:PRIVATE-DATA + ' ' + STRING(NOT autoMonitor,'ON/OFF')
    .
  btnAutoMonitor:LOAD-IMAGE('{&images}/media_play.gif').
  IF autoMonitor THEN
  DISABLE {&autoMonitorObjects} WITH FRAME {&FRAME-NAME}.
  ELSE
  ENABLE {&autoMonitorObjects} WITH FRAME {&FRAME-NAME}.
  RUN closePopups.
  RUN autoMonitor in h_board (autoMonitor).
  RUN setMenuItems (NOT autoMonitor).
