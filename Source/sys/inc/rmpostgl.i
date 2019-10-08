
def var rmpostgl like sys-ctrl.log-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "RMPOSTGL"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "RMPOSTGL"
   sys-ctrl.log-fld  = no
   sys-ctrl.descrip  = "Post GL for RM Receipts?".
   
  
end.

rmpostgl = sys-ctrl.log-fld.
