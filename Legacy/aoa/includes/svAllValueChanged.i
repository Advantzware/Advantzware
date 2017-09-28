/* svAllValueChanged.i */
  
ASSIGN {&SELF-NAME}
    {1}:READ-ONLY = {&SELF-NAME}
    {2}:READ-ONLY = {&SELF-NAME}
    .
IF {&SELF-NAME} THEN
ASSIGN
    {1}:SCREEN-VALUE = ""
    {2}:SCREEN-VALUE = ""
    .
APPLY "LEAVE":U TO {1}.
APPLY "LEAVE":U TO {2}.
