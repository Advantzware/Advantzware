
def var cercrout like sys-ctrl.log-fld no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CERCROUT"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CERCROUT"
   sys-ctrl.log-fld  = no
   sys-ctrl.descrip  = "Import RC Dept via style routing regardless " +
                       "of #out or sheet size?".
   
 
end.
cercrout = sys-ctrl.log-fld.

