
def var oedate-log like sys-ctrl.log-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OEDATE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OEDATE"
   sys-ctrl.log-fld  = yes
   sys-ctrl.descrip  = "Is Order Date modifiable?".
   
 
end.
oedate-log = sys-ctrl.log-fld.
