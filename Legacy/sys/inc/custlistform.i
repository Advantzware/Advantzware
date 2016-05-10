
def var ou-log like sys-ctrl.log-fld INIT NO no-undo.
def var ou-cust-int like sys-ctrl.int-fld  no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CustomerList"
    no-lock no-error.
if not avail sys-ctrl then do:
   create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CustomerList" 
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "Define Customer List for Reporting".
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.

      FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE 
          sys-ctrl-shipto.char-fld = {1} NO-LOCK NO-ERROR.

      IF NOT AVAIL sys-ctrl-shipto THEN do:
          
          create sys-ctrl-shipto .
          assign
              sys-ctrl-shipto.company = cocode
              sys-ctrl-shipto.name = "CustomerList"
              sys-ctrl-shipto.module = sys-ctrl.module
              sys-ctrl-shipto.char-fld = {1}
              sys-ctrl-shipto.log-fld  = no           .
      end.

   IF sys-ctrl.log-fld THEN do:
       ASSIGN
           ou-log = sys-ctrl-shipto.log-fld
           ou-cust-int = sys-ctrl-shipto.int-fld .
   END.



