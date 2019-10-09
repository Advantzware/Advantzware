
def var v-invpass AS LOG no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "INVPASS"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "INVPASS"
   sys-ctrl.descrip = "Password to Print O/E Invoices?".
end.
v-invpass = sys-ctrl.log-fld.
