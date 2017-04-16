
/* CustListForm.i */
DEFINE INPUT PARAMETER ip-char AS CHARACTER NO-UNDO .
DEFINE INPUT PARAMETER ip-cocode AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETE  ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE OUTPUT PARAMETE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ ip-cocode
      and sys-ctrl.name     EQ "CustomerList"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO :
    CREATE sys-ctrl.
    ASSIGN 
        sys-ctrl.company  = ip-cocode
        sys-ctrl.name     = "CustomerList" 
        sys-ctrl.log-fld  = NO 
        sys-ctrl.char-fld = ""
        sys-ctrl.descrip  = "Define Customer List for Reporting"
        .
   
END .

FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK 
     WHERE sys-ctrl-shipto.char-fld EQ ip-char
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl-shipto THEN DO :
    CREATE sys-ctrl-shipto.
    ASSIGN 
        sys-ctrl-shipto.company  = ip-cocode
        sys-ctrl-shipto.name     = "CustomerList"
        sys-ctrl-shipto.module   = sys-ctrl.module
        sys-ctrl-shipto.char-fld = ip-char
        sys-ctrl-shipto.log-fld  = NO
        .
END .

IF sys-ctrl.log-fld THEN
ASSIGN
    ou-log = sys-ctrl-shipto.log-fld
    ou-cust-int = sys-ctrl-shipto.int-fld
    .


