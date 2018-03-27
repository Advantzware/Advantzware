/* ----------------------------------------------- sys/look/itemivW.i 2/92 cd  */
/*                                                                            */
/* where statement  - item pop up  "Inks & Varnishes"                         */
/*                                                                            */
/* -------------------------------------------------------------------------- */

where (item.company = cocode and 
       (item.mat-type = "I" or item.mat-type = "V"))

/* end ---------------------------------- copr. 1992  advanced software, inc. */
