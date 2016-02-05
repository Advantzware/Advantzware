/* ----------------------------------------------- po/look/poitem.w 02/97 JLF */
/*                                                                            */
/* -------------------------------------------------------------------------- */


where po-ordl.company EQ po-ord.company AND
      po-ordl.po-no   EQ po-ord.po-no AND
    
    ((not can-find(ap-invl where ap-invl.i-no       eq ap-inv.i-no
                               and ap-invl.po-no      eq po-ordl.po-no
                               and {ap/invlline.i -1} eq po-ordl.line
                             use-index i-no))
                               
  and  po-ordl.stat      ne "X"                 /* not deleted or cancelled */
  and  po-ordl.stat      ne "F"                 /* not deleted or cancelled */
  and  (po-ordl.t-rec-qty NE 0 or
        (po-ordl.item-type and
         can-find(first item where item.company eq cocode
                               and item.i-no    eq po-ordl.i-no
                               and item.i-code  eq "R"
                               and item.stocked eq no
                              use-index i-no))))

/* end ---------------------------------- copr. 1997  advanced software, inc. */
