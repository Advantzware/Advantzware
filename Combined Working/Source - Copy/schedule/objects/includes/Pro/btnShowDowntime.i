/* btnShowDowntime.i - trigger for btnShowDowntime in board.w */

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN
    showDowntime = NOT showDowntime
    SELF:TOOLTIP = STRING(NOT showDowntime,'Show/Hide') + SELF:PRIVATE-DATA
    ldummy = SELF:LOAD-IMAGE('{&images}/downtime' +
                             TRIM(STRING(showDowntime,'On/Off')) + '.bmp').
  RUN lockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
  {{&includes}/{&Board}/showDowntime.i}
  RUN lockWindowUpdate (0,OUTPUT i).
