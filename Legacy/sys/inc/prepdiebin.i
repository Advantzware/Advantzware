
DEF VAR prepdiebin-chr AS CHAR NO-UNDO.
DEF VAR prepdiebin-log AS LOG NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "PREPDIEBIN"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "PREPDIEBIN"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Default Prep Die Bin from Est.".
END.
ASSIGN
 prepdiebin-log = sys-ctrl.log-fld
 prepdiebin-chr = sys-ctrl.char-fld.
 
