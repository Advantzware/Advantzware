/* ------------------------------------------------ cec/pr4-mcln.i 02/96 JLF  */

     find first mclean
	 where mclean.descr   eq {1}
	   and mclean.form-no eq v-form-no
	 no-error.

     if not avail mclean then create mclean.

     assign
      mclean.form-no   = v-form-no
      mclean.descr     = {1}
      mclean.cost[{2}] = mclean.cost[{2}] + {3}
      mclean.total-field = {4}.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
