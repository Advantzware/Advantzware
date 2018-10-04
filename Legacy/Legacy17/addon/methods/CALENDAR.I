/* calendar.i */

&IF DEFINED(Calendar) = 0 &THEN
&Scoped-define Calendar YES
DEFINE VARIABLE calendar-date AS CHARACTER NO-UNDO.
&ENDIF
RUN nosweat/calendar.w (OUTPUT calendar-date).
IF calendar-date NE '' THEN
SELF:SCREEN-VALUE = calendar-date.
