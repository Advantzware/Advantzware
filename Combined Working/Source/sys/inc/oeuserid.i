
DEF VAR oeuserid-log LIKE sys-ctrl.log-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OEUserID"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OEUserID"
   sys-ctrl.log-fld  = YES
   sys-ctrl.descrip  = "Update UserID when updating order?".
END.
oeuserid-log = sys-ctrl.log-fld.
