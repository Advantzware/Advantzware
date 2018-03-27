DEF VAR schdcard-cha LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ g_company
      AND sys-ctrl.name    EQ "SCHDCARD"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = g_company
   sys-ctrl.name     = "SCHDCARD"
   sys-ctrl.descrip  = "Schedule Card Format (O-U-11)".
END.

schdcard-cha = sys-ctrl.char-fld.
