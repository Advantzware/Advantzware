/* -------------------------------------------- sys/look/item-posW.i 02/01 JLF */
/* -------------------------------------------------------------------------- */

where (po-ordl.company   eq cocode
  and  po-ordl.i-no      eq item.i-no
  and  po-ordl.opened    eq yes
  and  po-ordl.stat      ne "C"
  and  po-ordl.item-type eq yes)

/* end ---------------------------------- copr. 2001  advanced software, inc. */
