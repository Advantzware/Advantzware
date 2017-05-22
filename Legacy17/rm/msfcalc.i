def var v-corr as log.

find first sys-ctrl where sys-ctrl.company = cocode
                      and sys-ctrl.name    = "MSFCALC"
    no-lock no-error.

v-corr = avail sys-ctrl and sys-ctrl.char-fld eq "Corrware".

