
def var v-fgpostgl like sys-ctrl.log-fld.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOSTGL"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "FGPOSTGL"
   sys-ctrl.log-fld  = yes
   sys-ctrl.descrip  = "Post GL for purchased FG Receipts".
   
  message sys-ctrl.descrip update sys-ctrl.log-fld.
end.
v-fgpostgl = sys-ctrl.log-fld.