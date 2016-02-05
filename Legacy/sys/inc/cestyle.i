
DEF VAR cestyle-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR cestyle-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR cestyle-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR cestyle-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "CESTYLE" + "{1}"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "CESTYLE" + "{1}"
   sys-ctrl.log-fld = YES
   sys-ctrl.descrip = "When changing Style, Prompt to Update Box Design for " +
                      TRIM(IF "{1}" EQ "F" THEN "Folding" ELSE "Corrugated") +
                      " Estimates".
END.

ASSIGN
 cestyle-log = sys-ctrl.log-fld
 cestyle-chr = sys-ctrl.char-fld
 cestyle-int = sys-ctrl.int-fld
 cestyle-dec = sys-ctrl.dec-fld.
