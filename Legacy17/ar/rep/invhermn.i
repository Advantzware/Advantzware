/* ---------------------------------------------- ar/rep/invempir.i 10/99 FWK */
/* PRINT Empire Invoice                                                       */
/* -------------------------------------------------------------------------- */

page {1}.

hide {1} frame inv-bot2.
view {1} frame inv-bot1.

assign
 j              = int("{1}" eq "") + 1
 v-tot-sf[j]    = 0
 v-sub-total[1] = 0.

for each ar-invl
    where ar-invl.x-no eq ar-inv.x-no
    no-lock
    by ar-invl.line:

  v-lines = 2.
  if ar-invl.i-dscr     ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr1 ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr2 ne "" then v-lines = v-lines + 1.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page.

  find first oe-ordl
      where oe-ordl.company eq cocode
	and oe-ordl.ord-no  eq ar-invl.ord-no
	and oe-ordl.i-no    eq ar-invl.i-no
	and oe-ordl.line    eq ar-invl.line
      no-lock no-error.

  assign
   v-part-comp    = if not avail oe-ordl              or
		      oe-ordl.ship-qty ge oe-ordl.qty then "C"
		    else "P"
   v-inv-price    = ar-invl.amt / ar-invl.qty
   v-sub-total[1] = v-sub-total[1] + ar-invl.amt.

  if v-inv-price eq ? then v-inv-price = 0.

  display {1}
	  oe-ordl.qty               when avail oe-ordl
	  ar-invl.qty               when not avail oe-ordl  @ oe-ordl.qty
	  ar-invl.i-no              when not avail oe-ordl
          ar-invl.part-no	    when avail oe-ordl @ ar-invl.i-no
	  ar-invl.ship-qty          when avail oe-ordl @ ar-invl.qty
	  ar-invl.qty               when not avail oe-ordl
	  ar-invl.unit-pr @ v-inv-price
	  oe-ordl.pr-uom            when avail oe-ordl @ ar-invl.pr-qty-uom
	  ar-invl.pr-qty-uom        when not avail oe-ordl
	  ar-invl.amt
	  ar-invl.i-name
	  ar-invl.part-dscr1        when avail oe-ordl @ ar-invl.i-dscr
	  ar-invl.i-dscr            when not avail oe-ordl
	  oe-ordl.disc              when avail oe-ordl @ ar-invl.disc
	  ar-invl.disc              when not avail oe-ordl
	  oe-ordl.tax               when avail oe-ordl @ ar-invl.tax
	  ar-invl.tax               when not avail oe-ordl

      with frame inv-mid1.
  down {1} with frame inv-mid1.

  if not avail oe-ord then
  do i = 1 to 2:
    v-part-dscr = if i eq 1 then ar-invl.part-dscr1
		  else if i eq 2 then ar-invl.part-dscr2
		  else "".

    if v-part-dscr ne "" then put {1} v-part-dscr at 23.
  end.

  put {1} skip(1).

  find first itemfg
      where itemfg.company eq cocode
	and itemfg.i-no    eq ar-invl.i-no
      no-lock no-error.
  v-tot-sf[j] = v-tot-sf[j] +
		(if avail itemfg then (ar-invl.qty * itemfg.t-sqft) else
		 if ar-invl.amt-msf ne 0 then (ar-invl.amt-msf * 1000)
		 else (ar-invl.qty * ar-invl.sf-sht)).
end. /* for each ar-invl */

hide {1} frame inv-bot1.
view {1} frame inv-bot2.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

