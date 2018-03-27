
def var apsecure-log like sys-ctrl.log-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "APSECURE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "APSECURE"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Protect AP Invoices by UserID?".
   
 
end.
apsecure-log = sys-ctrl.log-fld.
