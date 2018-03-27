
DEF VAR appaper-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR appaper-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR appaper-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR appaper-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "APPAPER"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "APPAPER"
   sys-ctrl.descrip  = "Paper Material to use MSF or PO UOM?"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "PO UOM".
   
 
END.

ASSIGN
 appaper-log = sys-ctrl.log-fld
 appaper-chr = sys-ctrl.char-fld
 appaper-int = sys-ctrl.int-fld
 appaper-dec = sys-ctrl.dec-fld.
