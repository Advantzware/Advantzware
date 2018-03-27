
DEF VAR aptax-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR aptax-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR aptax-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR aptax-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "APTAX"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "APTAX"
   sys-ctrl.char-fld = "Vendor"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Method to charge A/P sales tax by PO?".
END.
ASSIGN
 aptax-log = sys-ctrl.log-fld
 aptax-chr = sys-ctrl.char-fld
 aptax-int = sys-ctrl.int-fld
 aptax-dec = sys-ctrl.dec-fld.
