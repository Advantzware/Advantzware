
def var v-corr as log.

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "MSFCALC"
    no-lock no-error.

