/* ------------------------------------------------ ce/com/probeit.v 7/92 cd  */
/*                                                                            */
/* display statement - probe item file                                        */
/*                                                                            */
/* -------------------------------------------------------------------------- */

display
     probeit.cust-no
     probeit.part-no
     probeit.yrprice
     probeit.bl-qty
     probeit.yld-qty
     /*** /*probeit.full-cost*/ (probe.fact-cost / num-probeit)
	when available(probe) @ probe.fact-cost /* CTS */ ***/
     probeit.fact-cost
     probeit.full-cost
     probeit.sell-price.

/* end ---------------------------------- copr. 1992  advanced software, inc. */
