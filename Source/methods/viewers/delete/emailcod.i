/* emailcod.i */

IF NOT CAN-DO('ASI,NoSweat',USERID('NoSweat')) THEN DO:
  MESSAGE 'Permissions for this Function Denied!' VIEW-AS ALERT-BOX.
  allow-delete = NO.
  RETURN "ADM-ERROR":U.
END.
