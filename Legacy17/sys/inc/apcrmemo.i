
DEF VAR apcrmemo-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR apcrmemo-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR apcrmemo-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR apcrmemo-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "APCRMEMO"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "APCRMEMO"
   sys-ctrl.log-fld = NO
   sys-ctrl.descrip = "Update RM & WIP Cost when posting AP Credit Memos".
 
  FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
END.

ASSIGN
 apcrmemo-log = sys-ctrl.log-fld
 apcrmemo-cha = sys-ctrl.char-fld
 apcrmemo-int = sys-ctrl.int-fld
 apcrmemo-dec = sys-ctrl.dec-fld.
