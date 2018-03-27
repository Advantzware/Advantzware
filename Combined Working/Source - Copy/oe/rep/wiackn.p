/* -------------------------------------------------oe/rep/wiackn.p 04/97 FWK */
/* West Indies ORDER ACKNOLEDGEMENT                                           */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/acknowl.i}

def var v-salesman as char format "x(3)".
def var v-fob as char format "x(27)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-addr4 as char format "x(30)".
def var v-sold-addr3 as char format "x(30)".
def var v-line as int.
def var v-printline as int.
def var v-ackhead as char format "x(32)" init
  "A C K N O W L E D G E M E N T".
def var v-len as int.
def var v-totord as dec format "->>,>>>,>>9.99".
def var v-totlin as dec format "->>,>>>,>>9.99".
def var v-ans as log init NO.
def var lcnt as int init 1 no-undo.
def var pagebreak as int init 28 no-undo.
def var v-hdr as char format "x(25)" extent 15 no-undo.
def var v-vert-line as char format "x(1)" extent 6 no-undo.
def var cnt as int no-undo.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.

  format
    skip(12)
    v-hdr[1] at 55 skip
    v-hdr[2] at 55 skip
    v-hdr[3] at 55 format "x(3)"
    oe-ord.ord-no at 62 skip
    v-hdr[4] at 55 format "x(5)"
    oe-ord.ord-date at 62 FORMAT "99/99/99" skip(1)
    v-hdr[5] at 5 v-hdr[6] at 49 skip
    v-hdr[7] at 5 v-hdr[8] at 49 skip(1)
    oe-ord.cust-name at 10 oe-ord.sold-name    at 54 format "X(25)" skip
    oe-ord.addr[1]   at 10 oe-ord.sold-addr[1] at 54 format "X(25)" skip
    oe-ord.addr[2]   at 10 oe-ord.sold-addr[2] at 54 format "X(25)" skip
    v-addr3          at 10 v-sold-addr3        at 54 format "X(25)" skip(1)
    v-hdr[9] format "x(80)" skip
    v-hdr[10] format "x(80)" skip
    v-hdr[13] at 1 format "x(1)" oe-ord.terms-d at 2 format "x(20)"
    v-shipvia at 23 format "x(20)" v-salesman at 44
    oe-ord.po-no at 54 oe-ord.due-date TO 79 v-hdr[14] at 80 format "x(1)" skip
    v-hdr[12] format "x(80)" skip
    v-hdr[11] format "x(80)" skip
    v-hdr[15] format "x(80)" skip
    with frame ackhead-head no-labels no-box no-underline stream-io width 80.

  format
    v-vert-line[1] at 1 v-vert-line[2] at 6 v-vert-line[3] at 23
    v-vert-line[4] at 53 v-vert-line[5] at 66 v-vert-line[6] at 80 skip
    with frame blank-line no-labels no-box no-underline down stream-io width 80.

  format
    v-vert-line[1] at 1 v-line         TO  4 format ">>9"
    v-vert-line[2] at 6 oe-ordl.i-no   at  8
    v-vert-line[3] at 23 oe-ordl.i-name at 25 format "x(27)"
    v-vert-line[4] at 53 oe-ordl.qty    TO 64 format ">>,>>>,>>9"
    v-vert-line[5] at 66 oe-ordl.price  TO 78 format "$>>>,>>9.99<<<<" space(0)
    v-vert-line[6] at 80
    with frame detail no-labels no-box no-underline down stream-io width 80.

  format
    v-vert-line[1] at 1 v-vert-line[2] at 6 v-vert-line[3] at 23
    oe-ordl.part-dscr1 at 25 format "x(27)"
    v-vert-line[4] at 53 v-vert-line[5] at 66 v-vert-line[6] at 80 skip
    with frame dscr no-labels no-box no-underline down stream-io width 80.

  format
    v-vert-line[1] at 1 v-vert-line[2] at 6 v-vert-line[3] at 23
    "** Miscellaneous Items **" at 25
    v-vert-line[4] at 53 v-vert-line[5] at 66 v-vert-line[6] at 80
    with frame misc-hdr no-labels no-box no-underline down stream-io width 80.

  format
    v-vert-line[1] at 1 v-line TO 4 format ">>9"
    v-vert-line[2] at 6 oe-ordm.charge at 8 format "x(15)"
    v-vert-line[3] at 23 oe-ordm.dscr at 25 format "x(27)"
    v-vert-line[4] at 53
    v-vert-line[5] at 66 oe-ordm.amt TO 78 format "$->>>,>>9.99"
    v-vert-line[6] at 80 skip
    with frame detailm no-labels no-box no-underline down stream-io width 80.

  format
    lcnt format ">9" to 10
    oe-rel.qty          space(3)
    oe-rel.rel-date FORMAT "99/99/99" skip
    with frame sched-rel no-labels no-box no-underline down stream-io width 80.

  format
    shipto.ship-name at 18 skip
    shipto.ship-addr at 18 skip
    v-addr4          at 18 skip
    with frame shipto-rel no-labels no-box no-underline down stream-io width 80.


  ll-calc-disc-first = NO.
  FOR EACH sys-ctrl
      WHERE sys-ctrl.company  EQ cocode
        AND sys-ctrl.name     EQ "INVPRINT"
        AND sys-ctrl.char-fld EQ "Dayton"
      NO-LOCK:
    ll-calc-disc-first = YES.
    LEAVE.
  END.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  find first company where company.company eq cocode no-lock no-error.

  if v-print-head then do:
    assign
     v-vert-line[1] = "|"
     v-vert-line[2] = "|"
     v-vert-line[3] = "|"
     v-vert-line[4] = "|"
     v-vert-line[5] = "|"
     v-vert-line[6] = "|"

     v-hdr[1]  = "Order Acknowledgement"
     v-hdr[2]  = "---------------------"
     v-hdr[3]  = "No:"
     v-hdr[4]  = "Date:"
     v-hdr[5]  = "Bill To:"
     v-hdr[6]  = "Sold To:"
     v-hdr[7]  = "--------"
     v-hdr[8]  = "--------"
     v-hdr[9]  = "|" + fill("-",78) + "|"
     v-hdr[10] = "|Terms:               Shipped Via:         Salesman: Cust Ord No:    Date Delvy|"
     v-hdr[11] = "|No. |  Item Number   |         Description         |  Quantity  | Price Per M |"
     v-hdr[12]  = "|" + fill("-",78) + "|"
     v-hdr[13] = "|"
     v-hdr[14] = "|"
     v-hdr[15]  = "|" + fill("-",78) + "|".
  end.

  else do:
    do cnt = 1 to 6:
      assign v-vert-line[cnt] = "".
    end.
    do cnt = 1 to 15:
      assign v-hdr[cnt] = "".
    end.
  end.

  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:

      v-salesman = "".
      do i = 1 to 3:
	if oe-ord.sman[i] ne "" then
	  v-salesman = v-salesman +
		       (if i gt 1 then ", " else "") + oe-ord.sman[i].
      end.

      v-fob = if oe-ord.fob-code = "ORIG" then "Origin" else "Destination".

      find first carrier
	  where carrier.company eq oe-ord.company
	    and carrier.carrier eq oe-ord.carrier
	  no-lock no-error.
      v-shipvia = if avail carrier then carrier.dscr else "".

      assign
	v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
	v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
	"  " + oe-ord.sold-zip
	v-line = 1.

	display
	  oe-ord.ord-no  oe-ord.ord-date
	  oe-ord.cust-name oe-ord.sold-name
	  oe-ord.addr[1] oe-ord.sold-addr[1]
	  oe-ord.addr[2] oe-ord.sold-addr[2]
	  v-addr3 v-sold-addr3
	  oe-ord.terms-d v-shipvia v-salesman
	  oe-ord.po-no oe-ord.due-date v-hdr
	  with frame ackhead-head no-labels no-box stream-io width 80.

	  display v-vert-line with frame blank-line.

      /******  Process each Line Item of Order  *******/

      for each oe-ordl
	  where oe-ordl.company eq oe-ord.company
	    and oe-ordl.ord-no  eq oe-ord.ord-no
	  no-lock:

	if (v-printline + 3) gt pagebreak then do:
	  assign v-printline = 0.
	  page.
	  display
		oe-ord.ord-no  oe-ord.ord-date
		oe-ord.cust-name oe-ord.sold-name
		oe-ord.addr[1] oe-ord.sold-addr[1]
		oe-ord.addr[2] oe-ord.sold-addr[2]
		v-addr3 v-sold-addr3
		oe-ord.terms-d v-shipvia v-salesman
		oe-ord.po-no oe-ord.due-date v-hdr
		with frame ackhead-head no-labels no-box stream-io width 80.

	  display v-vert-line with frame blank-line.

	end.

	display
	  v-vert-line v-line oe-ordl.i-no  oe-ordl.i-name oe-ordl.qty
	  oe-ordl.price
	  with frame detail no-attr-space.

	v-printline = v-printline + 1.

	if oe-ordl.part-dscr1 ne "" then do:
	  display oe-ordl.part-dscr1 v-vert-line with frame dscr.
	  v-printline = v-printline + 1.
	end.

	if oe-ordl.part-dscr2 ne "" then do:
	  down with frame dscr.
	  display oe-ordl.part-dscr2 @ oe-ordl.part-dscr1 v-vert-line
		  with frame dscr.
	  v-printline = v-printline + 1.
	end.

	/******* Process Schedule Releases *******/
	FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND ((oe-rel.link-no EQ 0 AND v-schrel)
           OR  (oe-rel.link-no NE 0 AND v-actrel))
        NO-LOCK BREAK BY oe-rel.link-no DESC WITH FRAME sched-rel DOWN:

	  if first-of(oe-rel.link-no) then
	    if oe-rel.link-no eq 0 then lcnt = 1.
	    else
	    if first(oe-rel.link-no) then lcnt = 1.

	  if (v-printline ge pagebreak) or
	     (v-printline ge pagebreak - 1 and lcnt eq 1) then do:
	    v-printline = 0.
	    page.
	  end.

	  if first-of(oe-rel.link-no) then do:
	    if oe-rel.link-no eq 0 then do:
	      put "Scheduled Releases:" at 10 skip.
	      v-printline = v-printline + 1.
	    end.
	    else
	    if first(oe-rel.link-no) then do:
	      put "Actual Releases:" at 10 skip.
	      v-printline = v-printline + 1.
	    end.
	  end.

	  display lcnt (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) @ oe-rel.qty oe-rel.rel-date.
	  down with frame sched-rel.
	  assign
	   v-printline = v-printline + 1
	   lcnt        = lcnt + 1.
	  if v-shipto then do:
	    find first shipto
		where shipto.company eq cocode
		  and shipto.cust-no eq oe-rel.cust-no
		  and shipto.ship-id eq oe-rel.ship-id
		no-lock no-error.
	    if avail shipto then
	      v-addr4 = shipto.ship-city + ", " +
			shipto.ship-state + "  " + shipto.ship-zip.

	    if v-printline + 4 ge pagebreak then do:
	      v-printline = 0.
	      page.
	    end.

	    display shipto.ship-name shipto.ship-addr v-addr4
		with frame shipto-rel.
	    v-printline = v-printline + 4.
	  end.
	end.   /* for each oe-rel  */

	display v-vert-line with frame blank-line.

	assign v-line = v-line + 1
	  v-printline = v-printline + 1.  /* DAR - 2 */

	if v-printline ge pagebreak then do:
	  assign v-printline = 0.
	  page.
	  display
		oe-ord.ord-no  oe-ord.ord-date
		oe-ord.cust-name oe-ord.sold-name
		oe-ord.addr[1] oe-ord.sold-addr[1]
		oe-ord.addr[2] oe-ord.sold-addr[2]
		v-addr3 v-sold-addr3
		oe-ord.terms-d v-shipvia v-salesman
		oe-ord.po-no oe-ord.due-date v-hdr
		with frame ackhead-head no-labels no-box stream-io width 80.

	  display v-vert-line with frame blank-line.

	end.   /* if v-printline = */

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

	    down with frame detail.
      end. /* each oe-ordl */

      /*******  Misc Charges on Order  *******/

      for each oe-ordm
	  where oe-ordm.company eq oe-ord.company
	    and oe-ordm.ord-no  eq oe-ord.ord-no
	  no-lock break by ord-no:
	if first(oe-ordm.ord-no) then do:
	  display v-vert-line with frame misc-hdr.
	  display v-vert-line with frame blank-line.
	  assign v-printline = v-printline + 2.
	end.

	if v-printline ge pagebreak then do:
	  v-printline = 0.
	  page.
	  display
		oe-ord.ord-no  oe-ord.ord-date
		oe-ord.cust-name oe-ord.sold-name
		oe-ord.addr[1] oe-ord.sold-addr[1]
		oe-ord.addr[2] oe-ord.sold-addr[2]
		v-addr3 v-sold-addr3
		oe-ord.terms-d v-shipvia v-salesman
		oe-ord.po-no oe-ord.due-date v-hdr
		with frame ackhead-head no-labels no-box stream-io width 80.

	  display v-vert-line with frame blank-line.

	end.

	if oe-ordm.bill = "N" then
	  display
		v-line oe-ordm.charge oe-ordm.dscr "     N/C" @ oe-ordm.amt
		v-vert-line with frame detailm.
	else
	  display
		v-line oe-ordm.charge oe-ordm.dscr oe-ordm.amt
		v-vert-line with frame detailm.
	down with frame detailm.

	assign
	 v-line = v-line + 1
	 v-printline = v-printline + 2.
	if oe-ordm.bill ne "N" then v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */

      /*******  Print Instructions   ********/

      if v-prntinst then do:
	do i = 1 to 4:
	  if oe-ord.bill-i[i] ne "" then do:
	    if v-printline ge pagebreak then do:
	      v-printline = 0.
	      page.
	      display
		oe-ord.ord-no  oe-ord.ord-date
		oe-ord.cust-name oe-ord.sold-name
		oe-ord.addr[1] oe-ord.sold-addr[1]
		oe-ord.addr[2] oe-ord.sold-addr[2]
		v-addr3 v-sold-addr3
		oe-ord.terms-d v-shipvia v-salesman
		oe-ord.po-no oe-ord.due-date v-hdr
		with frame ackhead-head no-labels no-box stream-io width 80.

	      display v-vert-line with frame blank-line.

	    end.

	    if i eq 1 then do:
	      put skip(1).
	      v-printline = v-printline + 1.
	    end.

	    put oe-ord.bill-i[i] at 5 skip.
	    v-printline = v-printline + 1.
	  end.
	end. /* 1 to 4 */
      end.
      if oe-ctrl.p-ack then do:
	put skip(1) "Total Order Value:" TO 60 v-totord TO 78 skip.
      end.
      assign
       v-totord = 0.
       oe-ord.ack-prnt = yes.
      page.
      assign v-printline = 0.
    end. /* each oe-ord */

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */
