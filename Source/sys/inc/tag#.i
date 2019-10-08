
def var v-tag# like sys-ctrl.log-fld.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "TAG#"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "TAG#"
   sys-ctrl.descrip = "Assign RM Receipt Tag# Using PO# and Sequence?"
   sys-ctrl.log-fld = no.
 
end.

v-tag# = sys-ctrl.log-fld.
