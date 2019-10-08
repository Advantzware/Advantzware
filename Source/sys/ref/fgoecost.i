def {1} var v-full-cost as log.
def {1} var v-cost-from-receipt as CHAR.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGOECOST"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company = cocode
   sys-ctrl.name    = "FGOECOST"
   sys-ctrl.descrip = "Order Entry FG Item Cost? Yes=Full No=Direct Factory"
   sys-ctrl.log-fld = no.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
end.
v-full-cost = sys-ctrl.log-fld.
v-cost-from-receipt = sys-ctrl.char-fld.
