/* ----------------------------------------------- oe/rep/acknhop.p 03/99 JLF */
/* ORDER ACKNOLEDGEMENT for HOP                                               */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/rep/acknowl.i}

def var v-salesman like sman.sname format "x(14)".
def var v-fob as char format "x(14)".
def var v-frt-pay as char format "x(14)".
def var v-shipvia like carrier.dscr.
def var v-addr3 as char format "x(30)".
def var v-addr4 as char format "x(30)".
def var v-sold-addr3 as char format "x(30)".
def var v-line as int.
def var v-printline as int.
def var v-ackhead as char format "x(32)" init "A C K N O W L E D G E M E N T".
def var v-len as int.
def var v-totord as dec format "->>,>>>,>>9.99".
def var v-totlin as dec format "->>,>>>,>>9.99".
def var v-ans as log init no.
def var lcnt as int init 1.
def var pagebreak as int init 27.
def var v-cust-phone as char format "(999) 999-9999" no-undo.
def var v-part like oe-ordl.part-no no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
def var v-trim-size like quoteit.size no-undo.
def var v-item-style as char format "x(27)" no-undo.
def var v-bill-i as char format "x(121)" extent 2 no-undo.
def var v-last-page as int.
def var v-page-tot as dec format ">>9".
def var v-str as char extent 3.
def var v-copy   as int.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.

DEF TEMP-TABLE tt-line NO-UNDO FIELD tt-i-name LIKE oe-ordl.i-name.
  
format oe-ordl.part-no    to 30   format "x(30)"
       tt-i-name          to 61
       eb.i-coldscr       to 89   format "x(27)"
       oe-ordl.qty        to 100  format ">>,>>>,>>9"
       oe-ordl.price      to 110  format "$>>,>>9.99<<<<"
       oe-ordl.pr-uom     to 114
       oe-ordl.t-price    to 127  format "$>>>>,>>9.99"

    with frame detail no-box no-underline no-labels down STREAM-IO width 132.

format v-line             to 3    format ">>9"
       oe-ordm.charge     at 5
       oe-ordm.dscr       to 61   format "x(30)"
       oe-ordm.tax        to 103
       oe-ordm.amt        to 126  format "$->>,>>9.99" skip(1)

    with frame detailm no-labels no-box no-underline down STREAM-IO width 132.

format lcnt                       format ">9"
       oe-rel.qty
       space(3)
       oe-rel.po-no
       oe-rel.rel-date            FORMAT "99/99/99"
       oe-rel.ship-addr           format "x(25)"
       oe-rel.ship-city
       oe-rel.ship-state
       oe-rel.ship-zip
    with frame sched-rel no-labels no-box no-underline down STREAM-IO width 132.

def stream last-page.


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

  output stream last-page to value("acknhop.txt") page-size VALUE(v-lines-per-page).

  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id:
   
      v-salesman = "".
      do i = 1 to 3:
	if oe-ord.sman[i] ne "" then do:
	  find first sman
	      where sman.company eq cocode
		and sman.sman    eq oe-ord.sman[1]
	      no-lock no-error.
	  if avail sman then do:
	    v-salesman = sman.sname.
	    leave.
	  end.
	end.
      END.

      assign
       v-fob     = if oe-ord.fob-code eq "ORIG" then "Origin" else
						     "Destination"
       v-frt-pay = if oe-ord.frt-pay  eq "P" then "Prepaid" else
		   if oe-ord.frt-pay  eq "C" then "Collect" else "Prepay & Add".

      find carrier
	  where carrier.company eq oe-ord.company
	    and carrier.carrier eq oe-ord.carrier
	  no-lock no-error.
      v-shipvia = if avail carrier then carrier.dscr else "".

      assign
       v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
       v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
		      "  " + oe-ord.sold-zip
       v-line = 1.

      find first cust
	  where cust.company eq cocode
	    and cust.cust-no eq oe-ord.cust-no
	  no-lock no-error.
      if avail cust then v-cust-phone = cust.area-code + cust.phone.

      format header
	     skip(1)
	     oe-ord.cust-name     at 4
	     oe-ord.sold-name     at 37
	     v-fob                at 69
	     v-frt-pay            at 84
	     oe-ord.ord-no        at 101
	     oe-ord.ord-date      at 117    FORMAT "99/99/99"
	     oe-ord.addr[1]       at 4
	     oe-ord.sold-addr[1]  at 37
	     oe-ord.addr[2]       at 4
	     oe-ord.sold-addr[2]  at 37
	     oe-ord.terms-d       at 69
	     oe-ord.po-no         at 101
	     trim(string(oe-ord.over-pct,">9.9<")) + "-" +
	     trim(string(oe-ord.under-pct,">9.9<"))
				  at 117 format "x(11)"
	     v-addr3              at 4
	     v-sold-addr3         at 37
	     v-cust-phone         at 4
	     v-shipvia            at 69
	     v-salesman           at 101
	     string(trim(string(page-number - v-last-page,">9")) + " OF " +
		    trim(string(v-page-tot,">9")))
				  at 117 format "x(8)"
	     skip(3)

	  with frame ackhead page-top no-labels no-box no-underline STREAM-IO width 132.

	{oe/rep/acknhop.i "stream last-page"}

	v-page-tot = page-number (last-page) - v-last-page.

	{oe/rep/acknhop.i}

	v-last-page = page-number.
  END.

/* END ---------------------------------- copr. 1999  Advanced Software, Inc. */
