
DEF VAR oecredit-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR oecredit-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR oecredit-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR oecredit-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OECREDIT"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OECREDIT"
   sys-ctrl.log-fld = YES
   sys-ctrl.descrip = "Update order status after approved by credit".
 
  FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
END.

ASSIGN
 oecredit-log = sys-ctrl.log-fld
 oecredit-cha = sys-ctrl.char-fld
 oecredit-int = sys-ctrl.int-fld
 oecredit-dec = sys-ctrl.dec-fld.
