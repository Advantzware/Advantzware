
DEF VAR cematl-cha LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR cematl-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF var cematl-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF var cematl-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CEMATL"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CEMATL"
   sys-ctrl.descrip  = "Default Markup for Direct Material?".
  FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
END.
ASSIGN
 cematl-cha = sys-ctrl.char-fld
 cematl-log = sys-ctrl.log-fld
 cematl-int = sys-ctrl.int-fld
 cematl-dec = sys-ctrl.dec-fld.
