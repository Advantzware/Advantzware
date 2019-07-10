
def var oeinq as log no-undo.

find first sys-ctrl
    where /*sys-ctrl.company eq cocode and */ 
    sys-ctrl.name    eq "OEINQ"
    no-lock no-error.
if not avail sys-ctrl then do TRANSACTION:
  create sys-ctrl.
  assign
   /*sys-ctrl.company = cocode*/
   sys-ctrl.name    = "OEINQ"
   sys-ctrl.descrip = "Sort Inquiry by Due Date and Last Jobs First?"
   sys-ctrl.log-fld = yes.
   
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
oeinq = sys-ctrl.log-fld.

