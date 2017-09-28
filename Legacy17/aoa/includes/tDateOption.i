/* tDateOption.i */

ASSIGN
    {&SELF-NAME}
    {&dateObject}:READ-ONLY = {&SELF-NAME} NE "Fixed Date"
    btnCalendar-{&btnCalendar}:SENSITIVE = {&SELF-NAME} EQ "Fixed Date"
    .
IF {&SELF-NAME} NE "Fixed Date" THEN
{&dateObject}:SCREEN-VALUE = DYNAMIC-FUNCTION("fDateOptionValue" IN hContainer,{&SELF-NAME},DATE({&dateObject}:SCREEN-VALUE)).
