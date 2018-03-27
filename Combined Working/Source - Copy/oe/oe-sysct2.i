/* oe/oe-sysct2.i  sames as oe-sysct.i but no creation records */

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "JOB#"
    no-lock no-error.

v-job-meth = if avail sys-ctrl then sys-ctrl.char-fld else v-job-meth.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "OECOUNT"
          no-lock no-error.
v-oecount = if avail sys-ctrl then sys-ctrl.log-fld else v-oecount.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGOECOST"
     no-lock no-error.
v-full-cost = if avail sys-ctrl then sys-ctrl.log-fld else v-full-cost.

find first sys-ctrl where sys-ctrl.company eq cocode
       and sys-ctrl.name    eq "QUOPRICE"
       no-lock no-error.
if avail sys-ctrl then 
  assign
   v-quo-price-log = sys-ctrl.log-fld
   v-quo-price-int = sys-ctrl.int-fld 
   v-quo-price-dec = sys-ctrl.dec-fld.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "FGITEM#"
                      no-lock no-error.
assign v-est-fg  = if avail sys-ctrl then sys-ctrl.log-fld else v-est-fg
       v-est-fg1 = if avail sys-ctrl then sys-ctrl.char-fld else v-est-fg1.
