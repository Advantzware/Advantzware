/* --------------------------------------------  pc/rep/mch-effhr.i 8/94 gb */
/* Machine Efficiency Report - Hour                                           */
/* -------------------------------------------------------------------------- */
ASSIGN
   qty-hr = {1}-qty / {1}-run-hrs
   chg-hrs = {1}-run-hrs + {1}-mr-hrs + {1}-chg-hrs
   tot-hrs = chg-hrs + {1}-nochg-hrs
   eff-pct = ({1}-std-hrs / chg-hrs) * 100.00
   pct-utl = ({1}-std-hrs / tot-hrs) * 100.00
   pct-dt  = (tot-hrs / ({1}-nochg-hrs + {1}-chg-hrs)) * 100.00.

if eff-pct = ? then eff-pct = 0.
if pct-utl = ? then pct-utl = 0.
if pct-dt = ? then pct-dt = 0.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
