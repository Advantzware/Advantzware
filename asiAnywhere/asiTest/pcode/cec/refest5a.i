/* ---------------------------------------------- cec/refest5a.i 05/08/96 JLF */
/* Find reftable for Miscellaneous Material & Labor Cost/M                    */
/* -------------------------------------------------------------------------- */

	 find first reftable
         where reftable.reftable eq "EST-MISC"
	       and reftable.company  eq xef.company
	       and reftable.loc      eq xef.loc
	       and reftable.code     eq trim(xef.est-no) +
					string(xef.form-no,"/99")
	       and reftable.code2    eq "{1}" +
					string({2},"99")
	     /*{cec/refest5a.w {1} {2}} {3}*/ .

/* end ---------------------------------- copr. 1996  advanced software, inc. */
