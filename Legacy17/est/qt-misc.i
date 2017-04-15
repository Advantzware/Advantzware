/* est/qt-misc.i   get misc charge for the quantity
   {MAT 1}     {LAB 1}
*/
      ld-cost = 0.
      find first reftable   /* {cec/refest5aW.i {1} {2}} {3}. */
            where reftable.reftable eq "EST-MISC"
	       and reftable.company  eq bf-ef.company
	       and reftable.loc      eq bf-ef.loc
	       and reftable.code     eq trim(bf-ef.est-no) + string(bf-ef.form-no,"/99")
	       and reftable.code2    eq "{1}-QTY" + string({2},"99")
	       no-lock no-error.
      li-value = 1.
      if avail reftable then
      do li-cnt = 2 to 10:

         if reftable.val[li-cnt] >= quotechg.prep-qty and
            reftable.val[li-cnt - 1] < quotechg.prep-qty
         then li-value = li-cnt.
          
      end.
      find first reftable where reftable.reftable eq "EST-MISC"
	       and reftable.company  eq bf-ef.company
	       and reftable.loc      eq bf-ef.loc
	       and reftable.code     eq trim(bf-ef.est-no) + string(bf-ef.form-no,"/99")
	       and reftable.code2    eq "{1}-CST" + string({2},"99")
	       no-lock no-error.
      if avail reftable then ld-cost = reftable.val[li-value] /** quoteqty.qty / 1000*/ .

