/* ---------------------------------------------- oe/rep/invmulti.p 03/98 JLF */
/* PRINT INVOICE when sys-ctrl.char-fld eq "MultiWall" - O/E Module           */
/* -------------------------------------------------------------------------- */

	page {1}.

	v-printline = 0.

	for each w-invl,
	    first inv-line where recid(inv-line) eq w-invl.rec-id no-lock
	    by w-invl.po-noh
	    by w-invl.po-nol
	    by w-invl.i-no:

	  find first oe-ord
	      where oe-ord.company eq cocode
		and oe-ord.ord-no  eq inv-line.ord-no
	      no-lock no-error.
	  v-po-no = if avail oe-ord then oe-ord.po-no else "".

	  assign
	   v-case-line = ""
	   v-part-line = ""
	   v-case-cnt  = ""
	   v-cs-qty    = 0.

	  for each oe-boll no-lock where oe-boll.company eq inv-line.company
			and oe-boll.bol-no eq inv-head.bol-no
			/*and oe-boll.b-no eq inv-line.b-no*/
			and oe-boll.i-no eq inv-line.i-no
			use-index bol-no:

				       /** Build Case Count Display Lines **/

	    if oe-boll.cases ne 0 and oe-boll.qty-case ne 0 then
	    assign v-case-line = string(oe-boll.cases) + " @ " +
				     string(oe-boll.qty-case)
		   v-cs-qty = v-cs-qty + oe-boll.cases.
	    else assign v-case-line = "".
	    if oe-boll.partial ne 0 then
	    assign v-part-line = "1" + " @ " + string(oe-boll.partial)
		   v-cs-qty = v-cs-qty + 1.
	    else assign v-part-line = "".

	    do i = 1 to 5:
	      if (80 - length(v-case-cnt[i])) > length(v-case-line) and
		v-case-line ne "" then
	      assign v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
		     v-case-line = "".
	      if (80 - length(v-case-cnt[i])) > length(v-part-line) and
		v-part-line ne "" then
	      assign v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
		     v-part-line = "".
	    end. /* 1 to 5 */
	  end. /* each oe-boll */

	    assign
	     v-po-nol = ""
	     v-line   = v-line + 1.

	    find first oe-ordl
		where oe-ordl.company eq cocode
		  and oe-ordl.ord-no  eq inv-line.ord-no
		  and oe-ordl.i-no    eq inv-line.i-no
		no-lock no-error.
	    if avail oe-ordl then
	      assign
	       v-po-nol = oe-ordl.po-no
	       v-bo-qty = if (inv-line.qty - inv-line.ship-qty -
			      oe-ordl.t-ship-qty) < 0 then 0 else
				   (inv-line.qty - inv-line.ship-qty -
				    oe-ordl.t-ship-qty).
	    else
	      v-bo-qty = if ( inv-line.qty - inv-line.ship-qty ) < 0
			 then 0 else inv-line.qty - inv-line.ship-qty.

	    if v-po-nol ne "" then v-printline = v-printline + 1.

	    if v-printline gt 28 then do:
	      put {1} skip(29 - v-printline) "* CONTINUED *" at 66.
	      v-printline = 1.
	      page {1}.
	    end.

	    v-printline = v-printline + 4.

	    if v-printline gt 28 then do:
	      put {1} skip(29 - v-printline) "* CONTINUED *" at 66.
	      v-printline = 4.
	      page {1}.
	    end.

	    assign v-inv-qty = inv-line.qty
		   v-ship-qty = inv-line.ship-qty
		   v-i-no = inv-line.i-no
		   v-i-dscr = inv-line.part-dscr1
		   v-price = inv-line.price
		   v-t-price = inv-line.t-price
		   v-subtot-lines = v-subtot-lines + v-t-price.

	    if inv-line.pr-uom ne "CS" then
	      assign v-cs-qty = inv-line.qty
		     v-cs-ship-qty = inv-line.ship-qty.
	    else
	      assign v-cs-ship-qty = inv-line.ship-qty / inv-line.cas-cnt.

	    find first itemfg where itemfg.company eq cocode
				and itemfg.i-no eq inv-line.i-no
				no-lock no-error.
	    if avail itemfg then
	      assign v-cust-part-no = itemfg.part-no.
	    else
	      assign v-cust-part-no = "".

	    display {1}
	       inv-line.i-no
	       v-cust-part-no
	       v-cs-qty
	       v-cs-ship-qty
	       v-price
	       v-po-no
	       v-po-nol
	       inv-line.i-name
	       v-bo-qty
	       v-t-price
	       v-i-dscr
	       with frame detail.
	    down with frame detail.
	end. /* each inv-line */

	for each inv-misc no-lock
	    where inv-misc.company eq inv-head.company
	      and inv-misc.r-no    eq inv-head.r-no
	      and inv-misc.bill    eq "Y"
	    break by ord-no with frame detailm:
	  if first(inv-misc.ord-no) then do:
	    put {1} "** Miscellaneous Items **" at 23 skip(1).
	    assign v-printline = v-printline + 2.
	  end.

	    if v-printline gt 28 then do:
	      put {1} skip(29 - v-printline) "* CONTINUED *" at 66.
	      assign v-printline = 3.
	      page {1}.
	    end.

	    display {1} inv-misc.charge inv-misc.dscr inv-misc.amt.
	    down {1}.

	    assign
	     v-line        = v-line + 1
	     v-printline   = v-printline + 2
	     v-subtot-misc = v-subtot-misc + inv-misc.amt.
	end. /* each inv-misc */

	/* T O T A L S */
	put {1} skip(28 - v-printline).

	find first inv-line where inv-line.r-no eq inv-head.r-no
			    no-lock no-error.
	if avail inv-line then
	do:
	  find first uom where uom.uom eq inv-line.pr-uom no-lock no-error.

	  if avail uom then
	    assign v-price-head = caps(substring(uom.dscr,1,8))
		   v-price-per = caps(substring(uom.dscr,1,8)).
	  else
	    assign v-price-per = inv-line.pr-uom.
	end.

	if not inv-head.f-bill then
	  assign v-frt-display = 0.
	else
	  assign v-frt-display = inv-head.t-inv-freight.

	display {1} v-subtot-lines inv-head.t-inv-tax
		v-frt-display
		v-subtot-misc
		inv-head.t-inv-rev
		with frame total-frame.

/* END ---------------------------------- copr. 1998 Advanced Software, Inc. */
