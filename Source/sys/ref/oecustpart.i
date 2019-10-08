def {1} var v-oecustpart as log.
def {1} var v-oeCustPartInt as INT NO-UNDO.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OECUSTPART#"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "OECUSTPART#"
   sys-ctrl.descrip = "Order Entry Update Old Orders with new Cust Part#?"
   sys-ctrl.log-fld = YES.
end.
v-oecustpart = sys-ctrl.log-fld.
v-oeCustPartInt = sys-ctrl.int-fld.
