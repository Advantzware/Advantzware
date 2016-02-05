
def var v-close-job as int.


find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "CLOSEJOB"
    no-error.
if not avail sys-ctrl then do:
  create sys-ctrl.
  assign
   sys-ctrl.company  = cocode
   sys-ctrl.name     = "CLOSEJOB"
   sys-ctrl.descrip  = "When/how to Close Job? (Manually, OrdClose, FGPost)"
   sys-ctrl.log-fld  = no
   sys-ctrl.char-fld = "Manually".
  message sys-ctrl.descrip update sys-ctrl.char-fld.
end.

if sys-ctrl.int-fld  gt 1        then sys-ctrl.int-fld = 1.
if sys-ctrl.char-fld ne "FGPost" then sys-ctrl.int-fld = 0.

assign
 sys-ctrl.descrip = "char: Close Job?, " +
                    "log: Calc Prod Waste?, " + 
                    "int: Ignore Stat? FGPost"
 v-close-job      = int(sys-ctrl.char-fld eq "{1}") + sys-ctrl.int-fld.
 
