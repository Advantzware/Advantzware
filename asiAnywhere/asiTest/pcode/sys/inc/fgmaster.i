  
DEF VAR fgmaster-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR fgmaster-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "FGMASTER"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGMASTER"
   sys-ctrl.char-fld = "FGITEM"
   sys-ctrl.descrip  = "Default FG Item#? ".
END.

ASSIGN
 fgmaster-log = sys-ctrl.log-fld
 fgmaster-cha = sys-ctrl.char-fld.
