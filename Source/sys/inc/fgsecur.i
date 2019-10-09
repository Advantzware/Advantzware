
DEF VAR fgsecurity-log AS LOG NO-UNDO.
DEF VAR fgsecurity-char AS CHAR NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "FGSECURE"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGSECURE"
   sys-ctrl.descrip = "FG Cost Security?"
   sys-ctrl.char-fld = ""
   sys-ctrl.log-fld = NO.
END.

ASSIGN
   fgsecurity-log = sys-ctrl.log-fld
   fgsecurity-char = sys-ctrl.char-fld.
