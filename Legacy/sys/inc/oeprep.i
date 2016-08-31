
DEF VAR oeprep-log LIKE sys-ctrl.log-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "OEPREP"
    NO-LOCK NO-ERROR.

IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OEPREP"
   sys-ctrl.descrip = "Delete Prep Charges from the Estimate when Invoicing?"
   sys-ctrl.char-fld = "OE & DC".
  
END.
ELSE
   IF sys-ctrl.char-fld EQ "" THEN
   DO:
      FIND CURRENT sys-ctrl EXCLUSIVE-LOCK NO-ERROR.

      IF AVAIL sys-ctrl THEN
      DO:
         sys-ctrl.char-fld = "OE & DC".
         FIND CURRENT sys-ctrl NO-LOCK NO-ERROR.
      END.
   END.

ASSIGN
   oeprep-log = sys-ctrl.log-fld
   oeprep-char = sys-ctrl.char-fld.
