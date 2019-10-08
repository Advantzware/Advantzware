/* --------------------------------------------------- oe/invpost.p 09/98 JLF */
/* o/e invoicing - update with latest fg cost                                 */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}


find ar-inv where recid(ar-inv) eq v-recid.

ar-inv.t-cost = 0.

for each ar-invl where ar-invl.x-no eq ar-inv.x-no:
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq ar-invl.i-no
      no-lock no-error.

  if avail itemfg then ar-invl.sf-sht = (ar-invl.qty * itemfg.t-sqft) / 1000.

  ar-inv.t-cost = ar-inv.t-cost + ar-invl.t-cost.
end.

