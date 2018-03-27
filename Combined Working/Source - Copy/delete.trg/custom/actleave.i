/* actleave.i */

IF AVAILABLE company THEN
ASSIGN
  account-format = {&SELF-NAME}:SCREEN-VALUE
  {&SELF-NAME}:FORMAT = schema-account-format
  {&SELF-NAME}:SCREEN-VALUE = IF account-format BEGINS ' ' THEN ''
                              ELSE account-format.
