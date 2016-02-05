
def var bolwhse like sys-ctrl.char-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "BOLWHSE"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "BOLWHSE"
   sys-ctrl.descrip = "Default Warehouse for Adding Release/BOL"
   sys-ctrl.log-fld = no.
  message "System control record NOT found. " sys-ctrl.descrip
  update sys-ctrl.char-fld.
end.
bolwhse = sys-ctrl.char-fld.
