
DEF VAR oescreen-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR oescreen-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR oescreen-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OESCREEN"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OESCREEN"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Display a temporary Order Qty UOM when entering line items?".
  /*MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.*/
  FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
END.
ASSIGN
 oescreen-int = sys-ctrl.int-fld
 oescreen-log = sys-ctrl.log-fld
 oescreen-cha = sys-ctrl.char-fld.

