/* ----------------------------------------------- oe/rep/invsumm.i 06/99 JLF */
/* Truckload Invoice Summary                                                  */
/* -------------------------------------------------------------------------- */

  view {1} frame head.

  page {1}.

  view {1} frame inv-bot1.

  assign
   v-lines        = 0
   v-price-head   = ""
   v-price-per    = ""
   v-subtot-lines = 0
   v-bol-cases    = 0.

  for each w-inv,

      first inv-head
      where recid(inv-head) eq w-inv.rec-id
      no-lock,

      each inv-line
      where inv-line.r-no   eq inv-head.r-no
      no-lock

      break by inv-line.i-no
	    by inv-line.ord-no:

    for each oe-bolh
	where oe-bolh.b-no   eq inv-line.b-no
	  and oe-bolh.ord-no eq inv-line.ord-no
	no-lock,
	each oe-boll
	where oe-boll.company eq oe-bolh.company
	  and oe-boll.b-no    eq oe-bolh.b-no
	  and oe-boll.i-no    eq inv-line.i-no
	no-lock:

      v-bol-cases = v-bol-cases + oe-boll.cases.
      if oe-boll.partial ne 0 then v-bol-cases = v-bol-cases + 1.
    end. /* each oe-bolh */

    find first oe-ordl
	where oe-ordl.company eq cocode
	  and oe-ordl.ord-no  eq inv-line.ord-no
	  and oe-ordl.i-no    eq inv-line.i-no
	no-lock no-error.

    if not avail oe-ordl then
      assign
       v-bo-qty = v-bo-qty +
		  (if inv-line.qty - inv-line.ship-qty lt 0 then 0
		   else (inv-line.qty - inv-line.ship-qty))
       v-cs-qty = v-cs-qty + inv-line.qty.

    else
    if first-of(inv-line.ord-no) then v-cs-qty = v-cs-qty + oe-ordl.qty.

    assign
     v-t-price      = v-t-price      + inv-line.t-price
     v-cs-ship-qty  = v-cs-ship-qty  + inv-line.ship-qty.

    if last-of(inv-line.ord-no) and avail oe-ordl then
      v-bo-qty = v-bo-qty +
		 (if v-cs-qty - v-cs-ship-qty - oe-ordl.t-ship-qty lt 0 then 0
		 else (v-cs-qty - v-cs-ship-qty - oe-ordl.t-ship-qty)).

    if last-of(inv-line.i-no) then do:
      find first itemfg
	  where itemfg.company eq inv-line.company
	    and itemfg.i-no    eq inv-line.i-no
	  no-lock no-error.

      if v-lines gt 32 then do:
	page.
	v-lines = 0.
      end.

      find first uom where uom.uom eq inv-line.pr-uom no-lock no-error.
      if avail uom then
	assign
	 v-price-head = caps(substr(uom.dscr,1,8))
	 v-price-per  = caps(substr(uom.dscr,1,8)).
      else
	assign
	 v-price-head = inv-line.pr-uom
	 v-price-per  = inv-line.pr-uom.

      assign
       v-cs-qty      = v-cs-qty      / (if inv-line.pr-uom eq "CS" then
					  inv-line.cas-cnt else
					if avail uom then uom.mult else 1)
       v-bo-qty      = v-bo-qty      / (if inv-line.pr-uom eq "CS" then
					  inv-line.cas-cnt else
					if avail uom then uom.mult else 1)
       v-cs-ship-qty = v-cs-ship-qty / (if inv-line.pr-uom eq "CS" then
					  inv-line.cas-cnt else
					if avail uom then uom.mult else 1)
       v-price       = v-t-price / v-cs-ship-qty.

      display {1} inv-line.i-no
		  itemfg.i-name
		  v-cs-qty
		  v-cs-ship-qty
		  v-price
		  itemfg.i-dscr
		  v-bo-qty
		  v-t-price
	  with frame detail.
      down {1} with frame detail.

      assign
       v-subtot-lines = v-subtot-lines + v-t-price
       v-t-price      = 0
       v-bo-qty       = 0
       v-cs-qty       = 0
       v-cs-ship-qty  = 0

      v-lines = v-lines + 3.
    end.
  end.

  v-lines = v-lines + 1.

  assign
   v-t-inv-tax    = 0
   v-frt-display  = 0
   v-subtot-misc  = 0
   v-t-inv-weight = 0
   v-t-inv-rev    = 0
   v-last         = no
   v-inv-list     = "".

  for each w-inv,

      first inv-head
      where recid(inv-head) eq w-inv.rec-id
      no-lock

      break by inv-head.inv-no:

    if first(inv-head.inv-no) then do:
      v-lines = v-lines + 3.

      if v-lines gt 32 then do:
	page.
	v-lines = 0.
      end.

      put {1} skip(2) "INVOICE LIST:" skip.
    end.

    if last-of(inv-head.inv-no) then do while true:
      if length(trim(v-inv-list) +
		trim(string(inv-head.inv-no,">>>>>>")) + ",") gt 75 or
	 v-last                                                     then do:

	if substr(v-inv-list,length(trim(v-inv-list)),1) eq "," then
	  substr(v-inv-list,length(trim(v-inv-list)),1) = "".

	v-lines = v-lines + 1.

	if v-lines gt 32 then do:
	  page.
	  v-lines = 0.
	end.

	put {1} v-inv-list at 3 skip.

	if v-last then leave.

	v-inv-list = "".
      end.

      v-inv-list =
	      trim(v-inv-list) + trim(string(inv-head.inv-no,">>>>>>")) + ",".

      if last(inv-head.inv-no) then v-last = yes.

      else leave.
    end.

    if last-of(inv-head.inv-no) then do:
      for each inv-misc
	  where inv-misc.company eq inv-head.company
	    and inv-misc.r-no    eq inv-head.r-no
	    and inv-misc.bill    eq "Y"
	  no-lock:
	v-subtot-misc = v-subtot-misc + inv-misc.amt.
      end. /* each inv-misc */

      assign
       v-t-inv-tax    = v-t-inv-tax    + inv-head.t-inv-tax
       v-frt-display  = v-frt-display  +
			(if inv-head.f-bill then inv-head.t-inv-freight else 0)
       v-t-inv-weight = v-t-inv-weight + inv-head.t-inv-weight
       v-t-inv-rev    = v-t-inv-rev    + inv-head.t-inv-rev.
    end.
  end.

  hide {1} frame head     no-pause.
  hide {1} frame inv-bot1 no-pause.
  view {1} frame inv-bot2.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */

