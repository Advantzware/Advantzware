
DEF VAR oepreppo-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR oepreppo-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR oepreppo-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR oepreppo-dec LIKE sys-ctrl.dec-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OEPREPPO"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OEPREPPO"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Automatically Create purchase orders during OE for Prep RMs?".
   
  
END.
ASSIGN
 oepreppo-log = sys-ctrl.log-fld
 oepreppo-chr = sys-ctrl.char-fld
 oepreppo-int = sys-ctrl.int-fld
 oepreppo-dec = sys-ctrl.dec-fld.
