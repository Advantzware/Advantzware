
def var oepricecheck-log like sys-ctrl.log-fld no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "OEPRICECHECK"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "OEPRICECHECK"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Compare Order Price to Quoted Price when no Estimate#?".
end.
ASSIGN oepricecheck-log = sys-ctrl.log-fld.
