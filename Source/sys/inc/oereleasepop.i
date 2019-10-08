/* sys/inc/oereleasepop.i*/

DEF VAR oereleasepop-log LIKE sys-ctrl.log-fld INIT NO NO-UNDO.
DEF VAR oereleasepop-chr LIKE sys-ctrl.char-fld INIT "" NO-UNDO.
DEF VAR oereleasepop-int LIKE sys-ctrl.int-fld INIT 0 NO-UNDO.
DEF VAR oereleasepop-desc LIKE sys-ctrl.DESCRIP NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OEReleasePopUp"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OEReleasePopUp"
   sys-ctrl.log-fld = YES.
END.

ASSIGN
 oereleasepop-log = sys-ctrl.log-fld
 oereleasepop-chr = sys-ctrl.char-fld
 oereleasepop-desc = sys-ctrl.descrip
 oereleasepop-int = sys-ctrl.int-fld   .

