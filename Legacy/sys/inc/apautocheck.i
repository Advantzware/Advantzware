
def var apautocheck-log AS LOG no-undo.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "APAutoCheck"
    no-lock no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "APAutoCheck"
   sys-ctrl.log-fld  = NO
   sys-ctrl.descrip  = "Automatically create Manual Check?".
end.
apautocheck-log = sys-ctrl.log-fld.
