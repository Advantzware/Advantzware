/* calendar.i */

&IF DEFINED(Calendar) EQ 0 &THEN
&Scoped-define Calendar YES
DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
&ENDIF
IF KEYLABEL(LASTKEY) EQ "CTRL-F" THEN DO:
    {methods/ctrl-f.i}
END. /* if ctrl-f */
ELSE DO:
    RUN nosweat/popupcal.w (OUTPUT calendarDate).
    IF calendarDate NE '' THEN
    SELF:SCREEN-VALUE = calendarDate.
    RETURN NO-APPLY.
END. /* else */
