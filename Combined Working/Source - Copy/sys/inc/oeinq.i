
def var oeinq as log no-undo.
DEF VAR oeinq-char AS CHAR NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OEINQ"
    no-lock no-error.
if not avail sys-ctrl then do TRANSACTION:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OEINQ"
   sys-ctrl.descrip = "Sort Inquiry by Due Date and Last Jobs First?"
   sys-ctrl.log-fld = yes.
   
 
end.
ASSIGN
oeinq = sys-ctrl.log-fld
oeinq-char = sys-ctrl.char-fld.
