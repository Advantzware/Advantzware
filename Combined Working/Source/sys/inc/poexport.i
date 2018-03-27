
DEF VAR poexport-log LIKE sys-ctrl.log-fld   INIT NO NO-UNDO.
DEF VAR poexport-cha LIKE sys-ctrl.char-fld          NO-UNDO.
DEF VAR poexport-int LIKE sys-ctrl.int-fld          NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "POEXPORT"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "POEXPORT"
   sys-ctrl.descrip  = "PO Export Interface"
   sys-ctrl.char-fld = "None".
END.
ASSIGN
 poexport-log = sys-ctrl.log-fld
 poexport-cha = sys-ctrl.char-fld
 poexport-int = sys-ctrl.int-fld.
