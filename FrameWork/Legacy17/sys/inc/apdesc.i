
def var apdesc-log like sys-ctrl.log-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "APDESC"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "APDESC"
   sys-ctrl.descrip  = "Copy Description Line by Line?"
   sys-ctrl.log-fld  = NO.
   
 
end.
apdesc-log = sys-ctrl.log-fld.
