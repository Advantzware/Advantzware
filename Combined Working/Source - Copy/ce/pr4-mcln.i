/* ------------------------------------------------- ce/pr4-mcln.i 02/96 JLF  */

     find first mclean
	 where mclean.descr eq {1}
	 no-error.

     if not avail mclean then create mclean.

     assign
      mclean.descr     = {1}
      mclean.cost[{2}] = mclean.cost[{2}] + ({3}).

/* end ---------------------------------- copr. 1996  advanced software, inc. */
