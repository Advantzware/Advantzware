/* calendar.i - used in field dataValue HELP trigger */

&IF DEFINED(Calendar) EQ 0 &THEN
&SCOPED-DEFINE Calendar YES
DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
&ENDIF
RUN {&prompts}/popupcal.w (OUTPUT calendarDate).
IF calendarDate NE '' THEN
SELF:SCREEN-VALUE = calendarDate.
