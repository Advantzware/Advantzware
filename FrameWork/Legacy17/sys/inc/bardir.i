/* sys/inc/bardir.i*/

DEF VAR bardir-log LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEF VAR bardir-chr LIKE sys-ctrl.char-fld INIT "" NO-UNDO.
DEF VAR bardir-int LIKE sys-ctrl.int-fld INIT 0 NO-UNDO.
DEF VAR bardir-desc LIKE sys-ctrl.DESCRIP NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "BARDIR"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BARDIR"
   sys-ctrl.log-fld = NO
   sys-ctrl.descrip = "c:\ba\label".  
END.

ASSIGN
 bardir-log = sys-ctrl.log-fld
 bardir-chr = sys-ctrl.char-fld
 bardir-desc = sys-ctrl.descrip
 bardir-int = sys-ctrl.int-fld   .

