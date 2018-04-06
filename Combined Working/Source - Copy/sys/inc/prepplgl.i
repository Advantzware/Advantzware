
DEF VAR prepplgl-chr AS CHAR NO-UNDO.
DEF VAR prepplgl-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "prepplgl"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "PREPPLGL"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Default Prep Plate GL Account # from Est.".
END.
ASSIGN
 prepplgl-log = sys-ctrl.log-fld
 prepplgl-chr = sys-ctrl.char-fld.
 
