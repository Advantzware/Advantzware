
def var v-round as char no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEROUND"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CEROUND"
   sys-ctrl.char-fld = "Penny"
   sys-ctrl.descrip  = "Corrware to round up sell price to the...".
/*  message sys-ctrl.descrip update sys-ctrl.char-fld. */
end.
v-round = sys-ctrl.char-fld.
