
DEF VAR cemisc-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR cemisc-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF var cemisc-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF var cemisc-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CEMISC"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CEMISC"
   sys-ctrl.descrip  = "Default Markup when adding a Misc Charge".
  FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
END.
ASSIGN
 cemisc-cha = sys-ctrl.char-fld
 cemisc-log = sys-ctrl.log-fld
 cemisc-int = sys-ctrl.int-fld
 cemisc-dec = sys-ctrl.dec-fld.
