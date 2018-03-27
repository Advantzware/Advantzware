/* --------------------------------------------------- ap/ap-dreg.i 04/97 JLF */
/*                                                                            */
/* cash disbursements - edit register                                         */
/*                                                                            */
/* -------------------------------------------------------------------------- */

   for each ap-dis where not ap-dis.posted and ap-dis.company = cocode and
			 (ap-dis.check-date >= v-s-date and 
			  ap-dis.check-date <= v-e-date)
      break by ap-dis.{1} by ap-dis.check-no
   with frame a1-{1}:
      if first-of(ap-dis.{1}) or ap-dis.vend-no = "" then do:
	 find vend WHERE vend.company = cocode and vend.vend-no = ap-dis.vend-no
	 no-lock no-error.
	 if avail vend then
	 put vend.vend-no space(1)
	     vend.name.
	 else
	 put ap-dis.vend-no space(1)
	     ap-dis.payee.
      end.
      else if first-of(ap-dis.check-no) then put skip(1).
      put ap-dis.check-no   to 55
	  ap-dis.check-date at 60
	  ap-dis.check-amt  at 72.
      v2 = v2 + check-amt.
      for each ap-disl where ap-disl.d-no = ap-dis.d-no break by ap-disl.line
	  with frame a2-{1} no-box no-labels width 132:
	 put
	  ap-disl.actnum at 90 space(1)
	  ap-disl.amt skip.
      end. /* each ap-disl */
      if last-of(ap-dis.{1}) then do:
	 if sort-by-vend then
	   display  "*  VENDOR TOTALS" at 90 v2 to 128 "*" skip(1)
	       with frame vtot-{1} no-box no-labels width 132 STREAM-IO.
	 g1 = g1 + v1.
	 g2 = g2 + v2.
	 v1 = 0.
	 v2 = 0.
      end.
   end. /* each ap-dis */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
