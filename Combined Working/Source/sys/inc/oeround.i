
DEF VAR oeround-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR oeround-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR oeround-int LIKE sys-ctrl.int-fld NO-UNDO.
DEF VAR oeround-dec LIKE sys-ctrl.dec-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OEROUND"
    NO-ERROR.
IF NOT avail sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OEROUND"
   sys-ctrl.int-fld = 6
   sys-ctrl.descrip = "Display OE Sell Price with how many decimal places".
END.
ASSIGN
 oeround-log = sys-ctrl.log-fld
 oeround-chr = sys-ctrl.char-fld
 oeround-int = sys-ctrl.int-fld
 oeround-dec = sys-ctrl.dec-fld.


