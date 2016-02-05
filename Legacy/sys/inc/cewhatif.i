
def var cewhatif-cha as char no-undo.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CEWHATIF"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CEWHATIF"
   sys-ctrl.char-fld = ""
   sys-ctrl.descrip  = "Whatif Sell Price Calculation Method".
end.
cewhatif-cha = sys-ctrl.char-fld.
