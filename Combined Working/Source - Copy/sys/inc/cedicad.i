
DEF VAR cedicad-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR cedicad-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR cedicad-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR cedicad-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CEDICAD" + "{1}"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CEDICAD" + "{1}"
   sys-ctrl.log-fld = YES
   sys-ctrl.descrip = "When changing Die/CAD/Image, Prompt to Update " +
                      TRIM(IF "{1}" EQ "F" THEN "Folding" ELSE "Corrugated") +
                      " Estimates".
END.

ASSIGN
 cedicad-log = sys-ctrl.log-fld
 cedicad-chr = sys-ctrl.char-fld
 cedicad-int = sys-ctrl.int-fld
 cedicad-dec = sys-ctrl.dec-fld.
