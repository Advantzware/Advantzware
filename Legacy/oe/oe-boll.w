/* --------------------------------------------------- oe/oe-boll.w  6/93 rd  */
/*                                                                            */
/* o/e - where statement , bill of lading   items                             */
/*                                                                            */
/* -------------------------------------------------------------------------- */

use-index b-no2
where (oe-bolh.company eq oe-boll.company
  and  oe-bolh.b-no    eq oe-boll.b-no)

/* end ---------------------------------- copr. 1993  advanced software, inc. */
