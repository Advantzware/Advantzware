
DEF VAR celayout-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR celayout-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR celayout-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR celayout-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CELAYOUT"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CELAYOUT"
   sys-ctrl.descrip  = "CHAR=Round Net Sheet?  DEC=Tons of Board for Custom Roll?"
   sys-ctrl.log-fld  = NO
   sys-ctrl.char-fld = "None".
END.

ASSIGN
 celayout-log = sys-ctrl.log-fld
 celayout-chr = sys-ctrl.char-fld
 celayout-int = sys-ctrl.int-fld
 celayout-dec = sys-ctrl.dec-fld.
