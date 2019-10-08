/* ------------------------------------------------ ar/rep/invhop.i 01/00 FWK */
/* PRINT HOP Invoice                                                          */
/* -------------------------------------------------------------------------- */

page {1}.

hide {1} frame inv-bot2.
view {1} frame inv-bot1.

assign
 j           = int("{1}" eq "") + 1
 v-tot-sf[j] = 0.

for each ar-invl
    where ar-invl.x-no eq ar-inv.x-no
      and ar-invl.misc eq no
    no-lock
    by ar-invl.line:

  find first oe-ordl
      where oe-ordl.company eq cocode
	and oe-ordl.ord-no  eq ar-invl.ord-no
	and oe-ordl.i-no    eq ar-invl.i-no
	and oe-ordl.line    eq ar-invl.line
      no-lock no-error.

  assign
   v-ship-qty  = if avail oe-ordl then ar-invl.ship-qty else ar-invl.qty
   v-item-number = if avail oe-ordl then ar-invl.i-no else ""
   v-po-number = if avail oe-ordl then ar-invl.po-no else ar-inv.po-no
   v-unit-price = ar-invl.unit-pr
   v-price = ar-invl.amt
   v-cust-part = if avail oe-ordl then ar-invl.part-no else ""
   v-order-qty = if avail oe-ordl then oe-ordl.qty else 0
   v-backorder = if avail oe-ordl then 
		   if (ar-invl.qty - ar-invl.ship-qty - 
			oe-ordl.t-ship-qty) gt 0 then
		     ar-invl.qty - ar-invl.ship-qty - oe-ordl.t-ship-qty
    		   else 0
		 else 0
   v-job-no = fill(" ",6 - length(trim(ar-invl.job-no))) +
		trim(ar-invl.job-no) +
	      (if ar-invl.job-no ne "" then 
		("-" + string(ar-invl.job-no2,"99")) else "")
   v-part-comp = if not avail oe-ordl              or
		   oe-ordl.ship-qty ge oe-ordl.qty then "C"
		 else "P"
   v-inv-price = ar-invl.amt / ar-invl.qty.

  find first itemfg
      where itemfg.company eq cocode
	and itemfg.i-no    eq ar-invl.i-no
      no-lock no-error.
  v-item-size = 
    if avail itemfg then string(itemfg.t-len) + " x " + string(itemfg.t-wid)
    else "".

  if v-inv-price eq ? then v-inv-price = 0.

  EMPTY TEMP-TABLE tt-line.

  IF ar-invl.i-name NE "" THEN DO:
    CREATE tt-line.
    tt-i-name = ar-invl.i-name.
  END.

  IF ar-invl.part-dscr1 NE "" THEN DO:
    CREATE tt-line.
    tt-i-name = ar-invl.part-dscr1.
  END.

  IF ar-invl.part-dscr2 NE "" THEN DO:
    CREATE tt-line.
    tt-i-name = ar-invl.part-dscr2.
  END.

  IF NOT CAN-FIND(FIRST tt-line) THEN CREATE tt-line.

  IF v-item-size NE "" THEN DO:
    CREATE tt-line.
    tt-i-name = v-item-size.
  END.

  v-lines = 0.
  FOR EACH tt-line:
    v-lines = v-lines + 1.
  END.

  DO WHILE v-lines LT 2:
    CREATE tt-line.
    v-lines = v-lines + 1.
  END.

  v-lines = v-lines + 1.

  IF LINE-COUNTER {2} - 1 + v-lines GT PAGE-SIZE {2} + 1 THEN PAGE {1}.

  RELEASE tt-line.

  FIND FIRST tt-line NO-ERROR.

  DISPLAY {1}
	      v-cust-part
          tt-i-name WHEN AVAIL tt-line
          v-order-qty
          v-ship-qty
          v-unit-price
          v-price
      WITH FRAME detail.
  DOWN {1} WITH FRAME detail.

  FIND NEXT tt-line NO-ERROR.

  DISPLAY {1}
	      v-item-number
          v-po-number
          tt-i-name WHEN AVAIL tt-line
          v-backorder
          v-job-no
      WITH FRAME detail2.
  DOWN {1} WITH FRAME detail2.

  DO WHILE TRUE:
    FIND NEXT tt-line NO-ERROR.
    IF NOT AVAIL tt-line THEN LEAVE.

    DISPLAY {1} tt-i-name WITH FRAME detail2.
    DOWN {1} WITH FRAME detail2.
  END.

  PUT {1} SKIP(1).

  v-billinst = "".

  do i = 1 to 4:
    v-billinst[1 + int(i gt 2)] = trim(v-billinst[1 + int(i gt 2)]) + " " +
  				    trim(ar-inv.bill-i[i]).
  end. /* 1 to 4 */

  do i = 1 to 2:
    if v-billinst[i] ne "" then
      put {1} v-billinst[i] at 7 format "x(121)" skip.
  end.

  if v-billinst[1] ne "" or v-billinst[2] ne "" then put {1} skip(1).

  find first itemfg
      where itemfg.company eq cocode
	and itemfg.i-no    eq ar-invl.i-no
      no-lock no-error.
  v-tot-sf[j] = v-tot-sf[j] +
		(if avail itemfg then (ar-invl.qty * itemfg.t-sqft) else
		 if ar-invl.amt-msf ne 0 then (ar-invl.amt-msf * 1000)
		 else (ar-invl.qty * ar-invl.sf-sht)).
end. /* for each ar-invl */

v-misc-charges = 0.
for each ar-invl
    where ar-invl.x-no eq ar-inv.x-no
      and ar-invl.misc eq yes
      and ar-invl.billable eq yes
    no-lock
    break by ar-invl.line by ar-invl.ord-no
    with frame detailm:

  if first(ar-invl.ord-no) then do:
    hide {1} frame inv-bot2.
    view {1} frame inv-bot1.
  end.

  v-lines = 2 + if first-of(ar-invl.ord-no) then 2 else 0.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.

  if last(ar-invl.ord-no) then do:
    hide {1} frame inv-bot1.
    view {1} frame inv-bot2.
  end.

  if first-of(ar-invl.ord-no) then do:
    display {1} "** Miscellaneous Items **" @ ar-invl.prep-dscr.
    down {1}.
  end.

  display {1}
    ar-invl.i-name
    ar-invl.prep-dscr
    "Taxable" when ar-invl.tax and ar-invl.billable @ ar-invl.tax
    ar-invl.amt
    "      N/C" when NOT ar-invl.billable @ ar-invl.amt format "$->>,>>9.99".
  down {1}.
end.

hide {1} frame inv-bot1.
view {1} frame inv-bot2.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
