/* flddisp.i */

&IF "{&FIRST-EXTERNAL-TABLE}" = "track" &THEN
WHEN "date-time" THEN
IF date-time:SCREEN-VALUE = program:SCREEN-VALUE THEN
program:SCREEN-VALUE = STRING(6 - (INT(date-time:SCREEN-VALUE) + INT(user-id:SCREEN-VALUE))).
ELSE
user-id:SCREEN-VALUE = STRING(6 - (INT(date-time:SCREEN-VALUE) + INT(program:SCREEN-VALUE))).
WHEN "program" THEN
IF program:SCREEN-VALUE = date-time:SCREEN-VALUE THEN
date-time:SCREEN-VALUE = STRING(6 - (INT(program:SCREEN-VALUE) + INT(user-id:SCREEN-VALUE))).
ELSE
user-id:SCREEN-VALUE = STRING(6 - (INT(date-time:SCREEN-VALUE) + INT(program:SCREEN-VALUE))).
WHEN "user-id" THEN
IF user-id:SCREEN-VALUE = date-time:SCREEN-VALUE THEN
date-time:SCREEN-VALUE = STRING(6 - (INT(program:SCREEN-VALUE) + INT(user-id:SCREEN-VALUE))).
ELSE
program:SCREEN-VALUE = STRING(6 - (INT(date-time:SCREEN-VALUE) + INT(user-id:SCREEN-VALUE))).
&ENDIF
