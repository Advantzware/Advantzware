/* ----------------------------------------------- oe/rep/acknhop.i 03/99 JLF */
/* ORDER ACKNOLEDGEMENT for HOP                                               */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

      view {1} frame ackhead.

      page {1}.

      for each oe-ordl
	  where oe-ordl.company eq oe-ord.company
	    and oe-ordl.ord-no  eq oe-ord.ord-no
	  no-lock with frame detail:
	release style.
	release item.

	find first eb
	    where eb.company    EQ oe-ordl.company
          AND eb.est-no     EQ oe-ordl.est-no
	      and eb.part-no    EQ oe-ordl.part-no
	      and eb.stock-no   EQ oe-ordl.i-no
	    no-lock NO-ERROR.

    IF AVAIL eb THEN
	find first ef
	    where ef.company   EQ eb.company
          AND ef.est-no    EQ eb.est-no
	      and ef.form-no   EQ eb.form-no
	    no-lock no-error.

	if avail ef then
	  find first item
	      where item.company EQ cocode
		and item.i-no        EQ ef.board
	      no-lock no-error.

	assign
	 v-item-style = if avail item then
			  if item.est-dscr eq "" then item.i-dscr
						 else item.est-dscr
			else ""
	 v-trim-size  = "".

	release fgcat.
	if avail eb then do:
	  find first fgcat
	      where fgcat.company eq cocode
		    and fgcat.procat  eq eb.procat
	      no-lock no-error.

	  if avail fgcat and fgcat.comm gt 0 then
	  do j = 1 to 3:
	    run sys/inc/dec-frac.p (INPUT (if j eq 1 then eb.len else
				    if j eq 2 then eb.wid else eb.dep),
				    INPUT 64, OUTPUT v-str[j]).
	  end.
	  else
	    assign
	     v-str[1] = string(eb.len,">>>9.9<<<")
	     v-str[2] = string(eb.wid,">>>9.9<<<")
	     v-str[3] = string(eb.dep,">>>9.9<<<").

	  assign
	   v-trim-size  = trim(v-str[1]) + " x " +
			  trim(v-str[2]) + " x " +
			  trim(v-str[3])
	   v-item-style = string(substr(v-item-style,1,19),"x(20)") +
			  eb.style.
	end.

    EMPTY TEMP-TABLE tt-line.

    IF oe-ordl.i-name NE "" THEN DO:
      CREATE tt-line.
      tt-i-name = oe-ordl.i-name.
    END.

    IF oe-ordl.part-dscr1 NE "" THEN DO:
      CREATE tt-line.
      tt-i-name = oe-ordl.part-dscr1.
    END.

    IF oe-ordl.part-dscr2 NE "" THEN DO:
      CREATE tt-line.
      tt-i-name = oe-ordl.part-dscr2.
    END.

    IF NOT CAN-FIND(FIRST tt-line) THEN CREATE tt-line.

    IF v-trim-size NE "" THEN DO:
      CREATE tt-line.
      tt-i-name = v-trim-size.
    END.

    lcnt = 0.
    FOR EACH tt-line:
      lcnt = lcnt + 1.
    END.

    DO WHILE lcnt LT 2:
      CREATE tt-line.
      lcnt = lcnt + 1.
    END.

	IF v-printline + lcnt + 1 GT pagebreak THEN DO:
	  v-printline = 0.
	  PAGE {1}.
	END.

    RELEASE tt-line.

    FIND FIRST tt-line NO-ERROR.

	DISPLAY {1}
		    oe-ordl.part-no
		    tt-i-name WHEN AVAIL tt-line
		    eb.i-coldscr WHEN AVAIL eb
		    oe-ordl.qty
		    oe-ordl.price
		    oe-ordl.pr-uom
		    oe-ordl.t-price.
	DOWN {1}. 
    
    FIND NEXT tt-line NO-ERROR.

    DISPLAY {1}
	        STRING(oe-ordl.i-no,"x(16)") + oe-ordl.po-no
		  	             @ oe-ordl.part-no
		    tt-i-name    WHEN AVAIL tt-line
		    v-item-style @ eb.i-coldscr.
	down {1}.

	v-printline = v-printline + 2.

    DO WHILE TRUE:
      FIND NEXT tt-line NO-ERROR.
      IF NOT AVAIL tt-line THEN LEAVE.

	  DISPLAY {1} tt-i-name.
	  DOWN {1}.
	  v-printline = v-printline + 1.
    END.

    v-printline = v-printline + 1.

	for each oe-rel
        where oe-rel.company  eq oe-ordl.company
          and oe-rel.ord-no   eq oe-ordl.ord-no
          and oe-rel.i-no     eq oe-ordl.i-no
          and oe-rel.line     eq oe-ordl.line
	      and ((oe-rel.link-no eq 0 and v-schrel)
	       or  (oe-rel.link-no ne 0 and v-actrel))
	    no-lock break by oe-rel.link-no desc with frame sched-rel STREAM-IO down:

	  if first-of(oe-rel.link-no) then
	    if oe-rel.link-no eq 0 then lcnt = 1.
	    else
	    if first(oe-rel.link-no) then lcnt = 1.

	  if (v-printline ge pagebreak) or
	     (v-printline ge pagebreak - 1 and lcnt eq 1) then do:
	    v-printline = 0.
	    page {1}.
	  end.

	  if first-of(oe-rel.link-no) then do:
	    if oe-rel.link-no eq 0 then do:
	      put {1}
		  /* skip(1) */
		  "Scheduled Releases:" skip.
	      v-printline = v-printline + 2.
	    end.
	    else
	    if first(oe-rel.link-no) then do:
	      put {1}
		  skip(1)
		  "Actual Releases:" skip.
	      v-printline = v-printline + 2.
	    end.
	  end.

      {oe/rel-stat.i lv-stat}
      IF AVAIL oe-rell THEN
      FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.

	  display {1}
		  lcnt
		  (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty)
		  oe-rel.po-no
		  oe-rel.rel-date
            oe-relh.rel-date WHEN AVAIL oe-relh @ oe-rel.rel-date
		  oe-rel.ship-addr
		  oe-rel.ship-city
		  oe-rel.ship-state
		  oe-rel.ship-zip.
	  down {1} with frame sched-rel.

	  assign
	   v-printline = v-printline + 1
	   lcnt        = lcnt + 1.
	end.   /* for each oe-rel  */

	if v-printline ge pagebreak then do:
	  v-printline = 0.
	  page {1}.
	end.

	    if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
          v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.
                             
        else
        if oe-ordl.pr-uom eq "CS" then
        do:
          find first itemfg {sys/look/itemfgrlW.i}
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

          v-totlin = oe-ordl.qty /
                     (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                      if avail itemfg and itemfg.case-count ne 0
                      then itemfg.case-count else 1) *
                     oe-ordl.price.
        end.
        else
        if oe-ordl.pr-uom eq "C" then
          v-totlin = oe-ordl.qty / 100 * oe-ordl.price.

        else
        if oe-ordl.pr-uom eq "M" then
          v-totlin = oe-ordl.qty / 1000 * oe-ordl.price.

        else /** DEFAULT TO EACH **/
          v-totlin = oe-ordl.qty * oe-ordl.price.

        v-totlin = ROUND(v-totlin,2).

        IF oe-ordl.disc NE 0 THEN
           v-totlin = IF ll-calc-disc-first THEN 
                        (v-totlin - ROUND(v-totlin * oe-ordl.disc / 100,2))
                      ELSE
                        ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

        v-totord = v-totord + v-totlin.

	put {1} skip(1).

	if v-printline ge pagebreak then do:
	  v-printline = 0.
	  page {1}.
	end.
      end. /* each oe-ordl */

      v-line = 1.

      for each oe-ordm
	  where oe-ordm.company eq oe-ord.company
	    and oe-ordm.ord-no  eq oe-ord.ord-no
	  no-lock break by ord-no:
	if first(oe-ordm.ord-no) then do:
	  put {1}
	      "** Miscellaneous Items **" at 32
	      "Taxable" at 100
	      skip(1).
	  v-printline = v-printline + 2.
	end.

	if v-printline ge pagebreak then do:
	  v-printline = 0.
	  page {1}.
	end.

	display {1}
		v-line
		oe-ordm.charge
		oe-ordm.dscr
		"Lot Charge"  when oe-ordm.dscr eq ""   @ oe-ordm.dscr
		oe-ordm.tax   when oe-ordm.bill ne "N"
		oe-ordm.amt
		"     N/C"    when oe-ordm.bill eq "N"  @ oe-ordm.amt
	    with frame detailm.
	down {1} with frame detailm.

	assign
	 v-line      = v-line + 1
	 v-printline = v-printline + 2.

	if oe-ordm.bill ne "N" then v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */

      v-printline = v-printline + 3.

      if v-printline ge pagebreak then do:
	v-printline = 3.
	page {1}.
      end.

      put {1}
	  "Total Order Value:" to 112
	  v-totord
	  skip.

      if oe-ord.tax gt 0 then do:
	run ar/cctaxrt.p (input cocode, oe-ord.tax-gr,
			  output v-tax-rate, output v-frt-tax-rate).

	put {1}
	    "Tax " + string(v-tax-rate,">9.99%") + ":" format "x(11)" to 112
	    oe-ord.tax format "->>,>>>,>>9.99" skip
	    "Total w/Tax:" to 112
	    v-totord + oe-ord.tax format "->>,>>>,>>9.99" skip.
      end.

      put {1} skip(1).

      v-bill-i = "".
      if v-prntinst then do:
	do i = 1 to 2:
	  if oe-ord.bill-i[i] ne "" then
	    v-bill-i[1] = v-bill-i[1] + " " + oe-ord.bill-i[i].
	end. /* 1 to 2 */
	do i = 3 to 4:
	  if oe-ord.bill-i[i] ne "" then
	    v-bill-i[2] = v-bill-i[2] + " " + oe-ord.bill-i[i].
	end. /* 1 to 2 */
	do i = 1 to 2:
	  v-bill-i[i] = trim(v-bill-i[i]).
	  if v-bill-i[i] ne "" then put {1} v-bill-i[i] skip.
	end.
      end.

      assign
       v-totord        = 0
       oe-ord.ack-prnt = yes.

      v-printline = 0.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
