
DEF VAR oeautofg-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR oeautofg-chr LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OEAUTOFG"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OEAUTOFG"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "Any"
   sys-ctrl.descrip  = "Automatically Create purchase orders during OE for purchased FGs?".
   
  
END.
ASSIGN
 oeautofg-log = sys-ctrl.log-fld
 oeautofg-chr = sys-ctrl.char-fld.
