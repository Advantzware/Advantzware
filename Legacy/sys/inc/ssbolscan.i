DEF VAR ssbolscan-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR ssbolscan-cha LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SSBOLSCAN"
    NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SSBOLSCAN"
   sys-ctrl.char-fld = " "
   sys-ctrl.descrip  = "Scanning of Truck ID? ".
END.

ASSIGN
 ssbolscan-log = sys-ctrl.log-fld
 ssbolscan-cha = sys-ctrl.char-fld.
