/* sys/inc/ssfgscan.i */

def var ssfgscan like sys-ctrl.log-fld.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "SSFGSCAN"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "SSFGSCAN"
   sys-ctrl.log-fld  = no
   sys-ctrl.descrip  = "Prompt for Warehous/Bin?".
   
 
end.
ssfgscan = sys-ctrl.log-fld.

