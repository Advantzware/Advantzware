
DEF VAR boldate-chr LIKE sys-ctrl.char-fld NO-UNDO.
DEF VAR boldate-lst AS   CHAR INIT "Current,Release" NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "BOLDATE"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "BOLDATE"
   sys-ctrl.char-fld = "Release"
   sys-ctrl.descrip  = "When Posting Releases, set BOL Date to...".
  DO WHILE TRUE:
    MESSAGE TRIM(sys-ctrl.descrip) " (" + TRIM(boldate-lst) ")"
        UPDATE sys-ctrl.char-fld.
    IF LOOKUP(sys-ctrl.char-fld,boldate-lst) GT 0 THEN LEAVE.
  END.
END.
boldate-chr = sys-ctrl.char-fld.
