/* btnShowDowntime.i - trigger for btnShowDowntime in board.w */

  DEFINE VARIABLE i AS INTEGER NO-UNDO.

  ASSIGN
    showDowntime = NOT showDowntime
    SELF:TOOLTIP = STRING(NOT showDowntime,'Show/Hide') + SELF:PRIVATE-DATA
    ldummy = SELF:LOAD-IMAGE('{&images}/downtime' +
                             TRIM(STRING(showDowntime,'On/Off')) + '.bmp').
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN lockWindowUpdate (ACTIVE-WINDOW:HWND,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = TRUE.
&ENDIF
  {{&includes}/{&Board}/showDowntime.i}
&IF DEFINED(FWD-VERSION) EQ 0 &THEN
  RUN lockWindowUpdate (0,OUTPUT i).
&ELSE
  ACTIVE-WINDOW:DISABLE-REDRAW = FALSE.
&ENDIF
