/* ---------------------------------------------- sys/look/itemb1W.i 02/99 JLF */
/*                                                                            */
/* display statement  - item pop up                                           */
/*                                                                            */
/* -------------------------------------------------------------------------- */

where (item.company                   eq cocode
  and  ((index("BPR",item.mat-type)   ne 0 and not lv-foam) or
        (index("1234",item.mat-type)  ne 0 and lv-foam))
  and  item.industry                  eq lv-industry
  and  (item.i-code eq lv-i-code or lv-i-code eq "B"))

/* end ---------------------------------- copr. 1999  advanced software, inc. */
