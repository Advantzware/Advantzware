
def var oedelete like sys-ctrl.log-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OEDELETE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OEDELETE"
   sys-ctrl.log-fld = yes
   sys-ctrl.descrip = "Delete Combo/Tandem blank record when deleting " +
                      "order line".
   
  
end.

oedelete = sys-ctrl.log-fld.
