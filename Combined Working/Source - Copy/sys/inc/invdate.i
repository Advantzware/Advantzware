
DEF VAR invdate-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR invdate-lst AS   CHAR INIT "Current,BOL" NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "INVDATE"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "INVDATE"
   sys-ctrl.char-fld = "BOL"
   sys-ctrl.descrip  = "When Posting BOLs, set Invoice Date to...".
  DO WHILE TRUE:
    MESSAGE TRIM(sys-ctrl.descrip) " (" + TRIM(invdate-lst) ")"
        UPDATE sys-ctrl.char-fld.
    IF LOOKUP(sys-ctrl.char-fld,invdate-lst) GT 0 THEN LEAVE.
  END.
END.
invdate-chr = sys-ctrl.char-fld.
