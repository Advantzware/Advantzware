
DEF VAR prepplbin-chr AS CHAR NO-UNDO.
DEF VAR prepplbin-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "prepplbin"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "PREPPLBIN"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Default Prep Plate GL Bin from Est.".
END.
ASSIGN
 prepplbin-log = sys-ctrl.log-fld
 prepplbin-chr = sys-ctrl.char-fld.
 
