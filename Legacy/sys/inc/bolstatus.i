/* bolstatus.i */

DEFINE VARIABLE cbolstatus LIKE sys-ctrl.char-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
      and sys-ctrl.name     EQ "BolStatus"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN 
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "BolStatus" 
        sys-ctrl.log-fld  = NO 
        sys-ctrl.char-fld = ""
        sys-ctrl.descrip  = "Set initial status of BOLs"    .
END.

FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK 
     WHERE sys-ctrl-shipto.cust-vend-no eq {1}
     NO-ERROR.

IF avail sys-ctrl-shipto THEN
ASSIGN
   cbolstatus = sys-ctrl-shipto.char-fld   .
else cbolstatus  = sys-ctrl.char-fld .
