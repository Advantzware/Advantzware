
DEF VAR v-boreldate-char AS CHAR NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "BORELDATE"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BORELDATE"
   sys-ctrl.module  = "OE"
   sys-ctrl.descrip = "When creating back order releases, which date to use?".
   sys-ctrl.char-fld = "Today".
end.
assign
 v-boreldate-char = sys-ctrl.char-fld.
