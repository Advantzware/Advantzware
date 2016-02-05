/* ---------------------------------------------- oe/rep/invdnbry.i 12/98 JLF */
/* PRINT Danbury Invoice                                                      */
/* -------------------------------------------------------------------------- */

page {1}.

hide {1} frame inv-bot2.
view {1} frame inv-bot1.

for each inv-line
    where inv-line.r-no eq inv-head.r-no
    no-lock,

    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq inv-line.i-no
    no-lock

    by inv-line.i-no
    by inv-line.line:

  v-lines = 2.
  if inv-line.part-dscr1 ne "" then v-lines = v-lines + 1.
  if inv-line.part-dscr2 ne "" then v-lines = v-lines + 1.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page.

  display {1}
	  inv-line.inv-qty
	  inv-line.i-no
	  inv-line.i-name
	  inv-line.price
	  inv-line.pr-uom
	  inv-line.t-price

      with frame inv-mid1.
  down {1} with frame inv-mid1.

  do i = 1 to 2:
    v-part-dscr = if i eq 1 then inv-line.part-dscr1
		  else           inv-line.part-dscr2.

    if v-part-dscr ne "" then put {1} v-part-dscr at 26.
  end.

  put {1} skip(1).
end. /* for each report */

for each inv-misc
    where inv-misc.company eq cocode
      and inv-misc.r-no    eq inv-head.r-no
      and inv-misc.bill    eq "Y"
    no-lock

    break by inv-misc.charge:

  if first(inv-misc.charge) then
    put {1} "** Miscellaneous Items **" at 27 skip(1).

  display {1}
	  inv-misc.charge
	  inv-misc.dscr
	  inv-misc.amt

      with frame inv-mid2.
  down {1} with frame inv-mid2.
end. /* each inv-misc */

hide {1} frame inv-bot1.
view {1} frame inv-bot2.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
