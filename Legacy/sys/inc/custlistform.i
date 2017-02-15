/* CustListForm.i */

DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ cocode
      and sys-ctrl.name     EQ "CustomerList"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN 
        sys-ctrl.company  = cocode
        sys-ctrl.name     = "CustomerList" 
        sys-ctrl.log-fld  = NO 
        sys-ctrl.char-fld = ""
        sys-ctrl.descrip  = "Define Customer List for Reporting"
        .
END.

FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK 
     WHERE sys-ctrl-shipto.char-fld EQ {1}
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl-shipto THEN DO TRANSACTION:
    CREATE sys-ctrl-shipto.
    ASSIGN 
        sys-ctrl-shipto.company  = cocode
        sys-ctrl-shipto.name     = "CustomerList"
        sys-ctrl-shipto.module   = sys-ctrl.module
        sys-ctrl-shipto.char-fld = {1}
        sys-ctrl-shipto.log-fld  = NO
        .
END.

IF sys-ctrl.log-fld THEN
ASSIGN
    ou-log = sys-ctrl-shipto.log-fld
    ou-cust-int = sys-ctrl-shipto.int-fld
    .
