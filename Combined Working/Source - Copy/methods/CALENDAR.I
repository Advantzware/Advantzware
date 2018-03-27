/* calendar.i */

&IF DEFINED(Calendar) = 0 &THEN
&Scoped-define Calendar YES
DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
&ENDIF
RUN nosweat/popupcal.w (OUTPUT calendarDate).
IF calendarDate NE '' THEN
SELF:SCREEN-VALUE = calendarDate.
RETURN NO-APPLY.
