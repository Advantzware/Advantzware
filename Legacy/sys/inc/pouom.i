
DEF VAR pouom-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR pouom-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR pouom-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR pouom-dec LIKE sys-ctrl.dec-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "POUOM"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POUOM"
   sys-ctrl.descrip  = "Default for PO Order Qty UOM to either Stock or Purchase?"
   sys-ctrl.char-fld = "Stock".
end.
ASSIGN
 pouom-chr = sys-ctrl.char-fld
 pouom-log = sys-ctrl.log-fld
 pouom-int = sys-ctrl.int-fld
 pouom-dec = sys-ctrl.dec-fld.
