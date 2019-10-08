/* ------------------------------------------------ sys/look/itembW.i 2/92 cd  */
/*                                                                            */
/* display statement  - item pop up                                           */
/*                                                                            */
/* -------------------------------------------------------------------------- */

where (item.company               eq cocode 
  and  index("BPR",item.mat-type) ne 0)

/* end ---------------------------------- copr. 1992  advanced software, inc. */
