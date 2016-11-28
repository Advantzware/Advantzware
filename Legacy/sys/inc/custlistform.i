/* CustListForm.i */

DEFINE VARIABLE ou-log      LIKE sys-ctrl.log-fld NO-UNDO INITIAL NO.
DEFINE VARIABLE ou-cust-int LIKE sys-ctrl.int-fld NO-UNDO.

FIND FIRST sys-ctrl NO-LOCK
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CustomerList"
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl THEN DO :
   create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CustomerList" 
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
        sys-ctrl.descrip  = "Define Customer List for Reporting"
        .
  
end.

FIND FIRST sys-ctrl-shipto OF sys-ctrl NO-LOCK 
     WHERE sys-ctrl-shipto.char-fld EQ {1}
     NO-ERROR.
IF NOT AVAILABLE sys-ctrl-shipto THEN DO :
          create sys-ctrl-shipto .
          assign
              sys-ctrl-shipto.company = cocode
              sys-ctrl-shipto.name = "CustomerList"
              sys-ctrl-shipto.module = sys-ctrl.module
              sys-ctrl-shipto.char-fld = {1}
        sys-ctrl-shipto.log-fld  = NO
        .
      end.

IF sys-ctrl.log-fld THEN
       ASSIGN
           ou-log = sys-ctrl-shipto.log-fld
    ou-cust-int = sys-ctrl-shipto.int-fld
    .


