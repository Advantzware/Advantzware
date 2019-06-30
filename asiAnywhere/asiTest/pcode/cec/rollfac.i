def var v-rollfac  as log  init no.
def var v-sqft-fac as dec  init 1.
def var v-fac-hdr  as char.

if xest.est-type    eq 5    and
   xeb.yld-qty      gt 1    and
   xeb.len          eq 12   and
   xeb.dep          eq 0    then

  assign
   v-rollfac  = yes
   v-sqft-fac = if v-corr then (xeb.t-sqin * .007) else (xeb.t-sqin / 144).
