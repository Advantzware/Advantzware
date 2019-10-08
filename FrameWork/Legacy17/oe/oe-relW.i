/* ---------------------------------------------------- oe/oe-relW.i  6/93 rd  */
/*                                                                            */
/* o/e - where statement , Release for line items                             */
/*                                                                            */
/* ------------------------------------------------------------------------- */

use-index item
where (oe-rel.company eq cocode
  and  oe-rel.ord-no  eq xoe-ordl.ord-no
  and  oe-rel.i-no    eq xoe-ordl.i-no
  and  oe-rel.line    eq xoe-ordl.line)

/* end ---------------------------------- copr. 1993  advanced software, inc. */
