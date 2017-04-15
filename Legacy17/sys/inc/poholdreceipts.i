/* sys/inc/POHoldReceipts.i */

DEF VAR POHoldRct-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR POHoldRct-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR POHoldRct-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR POHoldRct-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "POHoldReceipts"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POHoldReceipts"
   sys-ctrl.char-fld = ""
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Block Receipts for POs with a Hold Status?".
END.
ASSIGN
 POHoldRct-log = sys-ctrl.log-fld
 POHoldRct-chr = sys-ctrl.char-fld
 POHoldRct-int = sys-ctrl.int-fld
 POHoldRct-dec = sys-ctrl.dec-fld.
