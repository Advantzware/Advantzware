
DEF VAR sellpric-cha LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "SELLPRIC"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SELLPRIC"
   sys-ctrl.descrip  = "Method for importing the Selling Price for Stock Items"
   sys-ctrl.char-fld = "Matrix".
END.   

sellpric-cha = sys-ctrl.char-fld.
