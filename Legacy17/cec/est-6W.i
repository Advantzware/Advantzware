/* ---------------------------------------------------- cec/est-6W.i 08/97 JLF */
/* Estimate Page 6 Maintenance - Box Design - where clause                    */
/* -------------------------------------------------------------------------- */

use-index design
where ({1}.design-no eq 0
  AND  {1}.company   EQ xest.company
  and  {1}.est-no    eq xest.est-no)

/* end ---------------------------------- copr. 1997  advanced software, inc. */
