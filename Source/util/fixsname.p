/* fixsname.p */

DEFINE VARIABLE i AS INTEGER NO-UNDO.

FUNCTION getSman RETURNS CHARACTER (ipCompany AS CHARACTER,ipSman AS CHARACTER):
  FIND FIRST sman NO-LOCK WHERE sman.company EQ ipCompany
                            AND sman.sman EQ ipSman NO-ERROR.
  RETURN IF AVAILABLE sman THEN sman.sname ELSE ''.
END FUNCTION.
  
SESSION:SET-WAIT-STATE('General').
FOR EACH oe-ord:
  DO i = 1 TO 3:
    IF oe-ord.sman[i] NE '' THEN
    oe-ord.sname[i] = getSman(oe-ord.company,oe-ord.sman[i]).
  END.
END.
SESSION:SET-WAIT-STATE('').

MESSAGE 'Sales Rep Name Fix Complete!' VIEW-AS ALERT-BOX.
