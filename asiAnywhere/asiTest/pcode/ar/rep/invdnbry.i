/* ---------------------------------------------- ar/rep/invdnbry.i 01/99 JLF */
/* PRINT Danbury Invoice                                                      */
/* -------------------------------------------------------------------------- */

page {1}.

hide {1} frame inv-bot2.
view {1} frame inv-bot1.

for each ar-invl
    where ar-invl.x-no eq ar-inv.x-no
    no-lock
    by ar-invl.line:

  v-lines = 2.
  if ar-invl.i-dscr     ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr1 ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr2 ne "" then v-lines = v-lines + 1.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page.

  display {1}
	  ar-invl.qty
	  ar-invl.i-no
	  ar-invl.i-name
	  ar-invl.unit-pr
	  ar-invl.pr-uom
	  ar-invl.amt

      with frame inv-mid1.
  down {1} with frame inv-mid1.

  do i = 1 to 3:
    v-part-dscr = if i eq 1 then ar-invl.i-dscr
		  else
		  if i eq 2 then ar-invl.part-dscr1
		  else           ar-invl.part-dscr2.

    if v-part-dscr ne "" then put {1} v-part-dscr at 27.
  end.

  put {1} skip(1).
end. /* for each report */

hide {1} frame inv-bot1.
view {1} frame inv-bot2.

/* END ---------------------------------- copr. 1999  Advanced Software, Inc. */
