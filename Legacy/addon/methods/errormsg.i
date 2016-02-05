/* errormsg.i */

DEFINE VARIABLE error-item AS INTEGER NO-UNDO.
DEFINE VARIABLE error-msg AS CHARACTER NO-UNDO.

DO error-item = 1 TO EXTENT(error-message):
  IF error-message[error-item]  NE '' THEN
  ASSIGN
    error-msg = error-msg + error-message[error-item] + CHR(10)
    error-message[error-item] = ''.
END.
IF error-msg NE '' THEN
DO:
  MESSAGE 'Entry can not be UPDATED because of the Following Error(s):' SKIP(1)
      error-msg VIEW-AS ALERT-BOX TITLE 'ERROR(S) EXIST'.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'cancel-record':U ) .
  RETURN.
END.
