
page {1}.

for each inv-line where inv-line.r-no eq inv-head.r-no no-lock
    break by inv-line.i-no
	  by inv-line.line:

  find first oe-ordl
      where oe-ordl.company eq cocode
	and oe-ordl.ord-no = inv-line.ord-no
	and oe-ordl.line = inv-line.line
      no-lock no-error.

  v-backorder = inv-line.qty - inv-line.ship-qty.
  if avail oe-ordl then
  assign
   v-backorder = v-backorder - oe-ordl.t-ship-qty
   v-order-qty = oe-ordl.qty.

  if v-backorder lt 0 then v-backorder = 0.
  find itemfg where itemfg.company = inv-line.company
    and itemfg.i-no = inv-line.i-no
    no-lock no-error.

  v-item-size =
    if avail itemfg then string(itemfg.t-len) + " x " + string(itemfg.t-wid)
    else "".

  assign
   v-ship-qty    = inv-line.ship-qty
   v-item-number = inv-line.i-no
   v-po-number   = inv-line.po-no
   v-unit-price  = inv-line.price
   v-price       = inv-line.t-price
   v-cust-part   = inv-line.part-no
   v-job-no      = fill(" ",6 - length(trim(inv-line.job-no))) +
		   trim(inv-line.job-no) +
		   (if inv-line.job-no ne "" then
		      ("-" + string(inv-line.job-no2,"99")) else "").

  EMPTY TEMP-TABLE tt-line.

  IF inv-line.i-name NE "" THEN DO:
    CREATE tt-line.
    tt-i-name = inv-line.i-name.
  END.

  IF inv-line.part-dscr1 NE "" THEN DO:
    CREATE tt-line.
    tt-i-name = inv-line.part-dscr1.
  END.

  IF inv-line.part-dscr2 NE "" THEN DO:
    CREATE tt-line.
    tt-i-name = inv-line.part-dscr2.
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

  IF FIRST(inv-line.i-no) THEN DO:
    HIDE {1} FRAME inv-bot2.
    VIEW {1} FRAME inv-bot1.
  END.

  IF LINE-COUNTER {2} - 1 + v-lines GT PAGE-SIZE {2} + 1 THEN PAGE {1}.

  IF LAST(inv-line.i-no) THEN DO:
    HIDE {1} FRAME inv-bot1.
    VIEW {1} FRAME inv-bot2.
  END.

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
end. /* each inv-line */

if v-prntinst then do:
  v-billinst = "".

  do i = 1 to 4:
    v-billinst[1 + int(i gt 2)] = trim(v-billinst[1 + int(i gt 2)]) + " " +
				  trim(inv-head.bill-i[i]).
  end. /* 1 to 4 */

  do i = 1 to 2:
    if v-billinst[i] ne "" then
      put {1} v-billinst[i] at 7 format "x(121)" skip.
  end.

  if v-billinst[1] ne "" or v-billinst[2] ne "" then put {1} skip(1).
end.

v-misc-charges = 0.
for each inv-misc
    where inv-misc.company eq cocode
      and inv-misc.r-no    eq inv-head.r-no
      and inv-misc.bill    eq "y"
    no-lock
    break by inv-misc.ord-no
    with frame detailm:

  if first(inv-misc.ord-no) then do:
    hide {1} frame inv-bot2.
    view {1} frame inv-bot1.
  end.

  v-lines = 2 + if first-of(inv-misc.ord-no) then 2 else 0.

  if line-counter {2} - 1 + v-lines gt page-size {2} + 1 then page {1}.

  if last(inv-misc.ord-no) then do:
    hide {1} frame inv-bot1.
    view {1} frame inv-bot2.
  end.

  if first-of(inv-misc.ord-no) then do:
    display {1} "** Miscellaneous Items **" @ inv-misc.dscr.
    down {1}.
  end.

  display {1}
    inv-misc.charge
    inv-misc.dscr
    "Taxable" when inv-misc.tax and inv-misc.bill ne "N" @ inv-misc.tax
    inv-misc.amt
    "      N/C" when inv-misc.bill = "N" @ inv-misc.amt format "$->>,>>9.99".
  down {1}.
end.

