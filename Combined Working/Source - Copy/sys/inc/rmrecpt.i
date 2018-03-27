/* rmrecpt.i */
DEF VAR rmrecpt-int AS INT NO-UNDO.
DEF VAR rmrecpt-log AS LOG NO-UNDO.
DEF VAR rmrecpt-cha AS CHAR NO-UNDO.
DEF VAR rmrecpt-dec AS DEC NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "RMRECPT"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RMRECPT"
   sys-ctrl.descrip  = "Method to Create RM Receipt Record"
   sys-ctrl.char-fld = "RMTAG"
   sys-ctrl.int-fld  = 0.
  MESSAGE "System control record NOT found. Create RM Receipts from LoadTag?"
      UPDATE sys-ctrl.log-fld.
END.

ELSE
IF sys-ctrl.char-fld EQ "" THEN DO:
  FIND CURRENT sys-ctrl.
  sys-ctrl.char-fld = "ByItem".
END.

FIND CURRENT sys-ctrl NO-LOCK.

ASSIGN
 rmrecpt-int = sys-ctrl.int-fld
 rmrecpt-log = sys-ctrl.log-fld
 rmrecpt-cha = sys-ctrl.char-fld
 rmrecpt-dec = sys-ctrl.dec-fld.
