/* svTimeInit.i */
  
ASSIGN
    {&SELF-NAME}
    {1}:READ-ONLY = NOT {&SELF-NAME}
    {2}:SENSITIVE = {&SELF-NAME}
    {3}:READ-ONLY = NOT {&SELF-NAME}
    {4}:SENSITIVE = {&SELF-NAME}
    .
IF {&SELF-NAME} EQ NO THEN
ASSIGN
    {1}:SCREEN-VALUE = "1200"
    {2}:SCREEN-VALUE = "AM"
    {3}:SCREEN-VALUE = "1200"
    {4}:SCREEN-VALUE = "AM"
    {1} {2} {3} {4}
    .
