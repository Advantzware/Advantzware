/* --------------------------------------------- po/look/pobyven1.w 01/01 JLF */
/* -------------------------------------------------------------------------- */

where po-ordl.company EQ po-ord.company AND
      po-ordl.po-no   EQ po-ord.po-no AND
    (po-ordl.stat       ne "X"  
  and  po-ordl.stat       ne "F"
  and  (po-ordl.t-rec-qty NE 0 or
        (po-ordl.item-type and
         can-find(first item where item.company eq po-ordl.company
                               and item.i-no    eq po-ordl.i-no
                               and item.i-code  eq "R"
                               and item.stocked eq no
                              use-index i-no))))

/* end ---------------------------------- copr. 2001  advanced software, inc. */
