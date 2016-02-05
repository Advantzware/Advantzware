/* ------------------------------------------------ sys/look/itemg.w 2/92 cd  */
/*                                                                            */
/* where statement  - item pop up  "Glues"                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

where (item.company = cocode and
	 index("GTS",item.mat-type) > 0 )

/* end ---------------------------------- copr. 1992  advanced software, inc. */
