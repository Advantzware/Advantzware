/* specialTime.i */

FUNCTION specialTime RETURNS CHARACTER (ipTime AS INTEGER):
  DEFINE VARIABLE days AS INTEGER NO-UNDO.

  days = TRUNCATE(ipTime / 86400,0).
  IF days NE 0 THEN
  ipTime = ipTime - days * 86400.
  RETURN STRING(days,'99') + ':' + STRING(ipTime,'HH:MM:SS').
END FUNCTION.
