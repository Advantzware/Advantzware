
DEF VAR fginvrec-log LIKE sys-ctrl.log-fld NO-UNDO.
DEF VAR fginvrec-chr LIKE sys-ctrl.char-fld NO-UNDO.


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "FGINVREC"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO:
  CREATE sys-ctrl.
  ASSIGN
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGINVREC"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Invoice Drop Shipped FGs when Received?".
   
  
END.
ASSIGN
 fginvrec-log = sys-ctrl.log-fld
 fginvrec-chr = sys-ctrl.char-fld.
