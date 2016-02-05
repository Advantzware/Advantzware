
DEF VAR webroot-path AS CHAR NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ gcompany
      AND sys-ctrl.name    EQ "WEBROOT"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = gcompany
   sys-ctrl.name    = "WEBROOT"
   sys-ctrl.descrip = "Web Root Path".
END.

ASSIGN
   webroot-path = sys-ctrl.char-fld.
