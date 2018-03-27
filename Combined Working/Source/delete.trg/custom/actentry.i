/* actentry.i */

IF NOT AVAILABLE company THEN
DO:
  {custom/getcmpny.i}
  FIND company WHERE company.company = gcompany NO-LOCK NO-ERROR.
END.
IF NOT AVAILABLE company THEN
DO:
  MESSAGE 'Company' gcompany 'record does not exist!' SKIP
          'Contact System Admistrator.'
      VIEW-AS ALERT-BOX ERROR.
  RETURN NO-APPLY.
END.
ASSIGN
  schema-account-format = {&SELF-NAME}:FORMAT
  account-format = ''.
DO digits = 1 TO company.acc-level:
  account-format = account-format + FILL('9',company.acc-dig[digits]) +
                  (IF digits NE company.acc-level THEN '-' ELSE '').
END.
{&SELF-NAME}:FORMAT = account-format.
