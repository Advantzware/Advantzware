/* sys/inc/bardir.i*/

DEF VAR rmbardir-log LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEF VAR rmbardir-chr LIKE sys-ctrl.char-fld INIT "" NO-UNDO.
DEF VAR rmbardir-int LIKE sys-ctrl.int-fld INIT 0 NO-UNDO.
DEF VAR rmbardir-desc LIKE sys-ctrl.DESCRIP NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "RMBARDIR"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "RMBARDIR"
   sys-ctrl.log-fld = NO
   sys-ctrl.descrip = "c:\ba\label".  
END.

ASSIGN
 rmbardir-log = sys-ctrl.log-fld
 rmbardir-chr = sys-ctrl.char-fld
 rmbardir-desc = sys-ctrl.descrip
 rmbardir-int = sys-ctrl.int-fld   .

