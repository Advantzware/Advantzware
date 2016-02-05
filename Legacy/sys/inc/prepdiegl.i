
DEF VAR prepdiegl-chr AS CHAR NO-UNDO.
DEF VAR prepdiegl-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "prepdiegl"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "PREPDIEGL"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Default Prep Die GL Account # from Est.".
END.
ASSIGN
 prepdiegl-log = sys-ctrl.log-fld
 prepdiegl-chr = sys-ctrl.char-fld.
 
