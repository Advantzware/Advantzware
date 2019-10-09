
DEF VAR ssbol-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR ssbol-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR ssbol-dec LIKE sys-ctrl.dec-fld NO-UNDO.
DEF VAR ssbol-log LIKE sys-ctrl.log-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company eq cocode
      AND sys-ctrl.name    eq "SSBOL"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "SSBOL"
   sys-ctrl.descrip = "Scanned Tags automatically added to Release?".
 
END.
ASSIGN
 ssbol-int = sys-ctrl.int-fld
 ssbol-cha = sys-ctrl.char-fld
 ssbol-dec = sys-ctrl.dec-fld
 ssbol-log = sys-ctrl.log-fld.
