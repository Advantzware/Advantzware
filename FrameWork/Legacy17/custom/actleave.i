/* actleave.i */
DEF VAR digits2 LIKE digits NO-UNDO.
DEF VAR digits3 LIKE digits NO-UNDO.
DEF VAR digits4 AS CHAR NO-UNDO.


IF AVAILABLE company THEN DO:
  ASSIGN
   digits3 = 0
   account-format = {&SELF-NAME}:SCREEN-VALUE
   {&SELF-NAME}:FORMAT = schema-account-format
   {&SELF-NAME}:SCREEN-VALUE = IF account-format BEGINS ' ' THEN ''
                               ELSE account-format
   digits4 = {&SELF-NAME}:SCREEN-VALUE.

  DO digits = 1 TO company.acc-level:
    digits3 = digits3 + 1.
    DO digits2 = 1 TO company.acc-dig[digits]:
      IF SUBSTR(digits4,digits3,1) EQ " " THEN
        SUBSTR(digits4,digits3,1) = "0".
      digits3 = digits3 + 1.
    END.
  END.

  {&SELF-NAME}:SCREEN-VALUE = digits4.
END.
