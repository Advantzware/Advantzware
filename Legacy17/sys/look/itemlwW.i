/* ----------------------------------------------- sys/look/itemlwW.i 2/92 cd  */
/*                                                                            */
/* where statement - item file   "Window & Leaf"                              */
/*                                                                            */
/* -------------------------------------------------------------------------- */

where (item.company = cocode and
       index("WLF",item.mat-type) > 0)

/* end ---------------------------------- copr. 1992  advanced software, inc. */
