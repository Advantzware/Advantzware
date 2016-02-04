  
DEF VAR jobreopn-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR jobreopn-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "JOBREOPN"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "JOBREOPN"
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "This will activate the prompt to re-open jobs"
   sys-ctrl.log-fld  = TRUE.

END.

ASSIGN
 jobreopn-log = sys-ctrl.log-fld
 jobreopn-cha = sys-ctrl.char-fld.


