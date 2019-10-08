
def var ceprep-cha as char no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEPREP"
    no-lock no-error.
if not avail sys-ctrl then do transaction:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CEPREP"
   sys-ctrl.char-fld = "Penny"
   sys-ctrl.descrip  = "Prep Charge to round up to the...".
end.
ceprep-cha = sys-ctrl.char-fld.
