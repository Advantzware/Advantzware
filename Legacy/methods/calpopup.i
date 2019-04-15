/* methods/calpopup.i */
/* Tkt #40720 Mod 12/12/18 - MYT - support misc string variable formats */

&IF DEFINED(Calendar) = 0 &THEN
&Scoped-define Calendar YES
DEFINE VARIABLE calendarDate AS CHARACTER NO-UNDO.
DEFINE VARIABLE daCalDate AS DATE NO-UNDO.
&ENDIF
RUN nosweat/popupcal2.w (OUTPUT calendarDate).
if calendarDate NE "" then do:
    assign 
        daCalDate = date(calendarDate).
    if self:format EQ "99/99/9999" then assign
        self:screen-value = string(daCalDate,"99/99/9999").
    else if self:format = "99/99/99" then assign
        self:screen-value = string(daCalDate,"99/99/99").
    else assign
        self:screen-value = string(daCalDate).
end.
/*
* IF calendarDate NE '' THEN
* SELF:SCREEN-VALUE = calendarDate.
*/
RETURN NO-APPLY.
