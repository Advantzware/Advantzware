/* sys/inc/pushpin.i */

DEF VAR pushpin-char AS CHAR NO-UNDO.
DEF VAR pushpin-log AS log NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ g_company
      AND sys-ctrl.name    EQ "PUSHPIN"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = g_company
   sys-ctrl.name    = "PUSHPIN"
   sys-ctrl.descrip = "Pushpin Image Default Location"
   sys-ctrl.char-fld = "boximage\".
end.

ASSIGN pushpin-char = sys-ctrl.char-fld
       pushpin-log = sys-ctrl.log-fld.
