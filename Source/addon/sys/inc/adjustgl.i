
def var v-adjustgl like sys-ctrl.log-fld.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "ADJUSTGL"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "ADJUSTGL"
   sys-ctrl.log-fld  = yes
   sys-ctrl.descrip  = "Post GL for FG adjustments".
   
  message sys-ctrl.descrip update sys-ctrl.log-fld.
end.
v-adjustgl = sys-ctrl.log-fld.
