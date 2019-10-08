/* ---------------------------------------------- ar/rep/invhermn.p 12/98 FWK */
/* PRINT Herman Invoice                                                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def buffer xar-inv for ar-inv.

def workfile w-sman field sman as char.

{ar/rep/invoice.i}

def var v-inv-no            like ar-inv.inv-no.
def var v-salesman          as   char format "x(25)".
def var v-trailer           as   char format "x(15)".
def var v-fob               as   char format "x(10)".
def var v-bot-lab           as   char format "x(41)" extent 2.
def var v-tot-wt            as   dec  format ">,>>>,>>9.9<".
def var v-tot-sf            as   dec  format ">,>>>,>>9.9<" extent 2.
def var v-tot-inv           as   dec  format "->>>>>>9.99".

def var v-lines             as   int.
def var v-inv-price         as   dec  format "->>>>9.99".
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
def var v-po-no like inv-line.po-no init "" no-undo.
def var v-ord-no as char format "x(8)" init "" no-undo.
def var v-bol-no LIKE ar-invl.bol-no no-undo.
def var v-ord-date like oe-ord.ord-date init "" no-undo.
def var v-sub-total as dec format "->>,>>>,>>9.99" init 0 extent 2 no-undo.
def var v-freight as dec format "->>,>>9.99" init 0 no-undo.
def var v-tax as dec format "->>,>>9.99" init 0 no-undo.
def var v-if-paid as date format "99/99/9999" no-undo.
def var v-discount like inv-head.t-inv-cost no-undo.

find first ar-inv no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(1)
       trim(string(ar-inv.inv-no,">>>>>>"))
				    to 78
       skip(1)
       ar-inv.inv-date         to 78 FORMAT "99/99/99" 
       skip(1)
       string(trim(string(page-number - v-last-page,"99")) + " OF " +
	      trim(string(v-page-tot,"99")))
				    to 78 format "x(8)"
       skip(4)
       cust.name               at 10 format "x(30)"
       v-ship-name             at 49 format "x(30)"
       cust.addr[1]            at 10 format "x(30)"
       v-ship-addr[1]          at 49 format "x(30)"
       cust.addr[2]            at 10 format "x(30)"
       v-ship-addr[2]          at 49 format "x(30)"
       v-cust-addr3            at 10 format "x(30)"
       v-ship-addr3            at 49 format "x(30)"
       skip(4)
       ar-inv.cust-no          at 2
       carrier.dscr            at 11 format "x(16)" when avail carrier
       v-fob                   at 30
       ar-inv.terms-d          at 58 format "x(21)"
       skip(3)
       v-po-no                 at 2 
       v-salesman              at 27
       v-ord-date              at 57 FORMAT "99/99/99" when avail oe-ord
       v-ord-no                at 66                   when avail oe-ord
       v-bol-no                to 80 format ">>>>>>" 
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 85.


form oe-ordl.qty                    to 6  format "->>>9"
     ar-invl.qty                    at 12 format "->,>>>,>>9"
     ar-invl.i-no                   at 23
     v-inv-price                    to 63 format "->>>,>>9.99<<"
     ar-invl.pr-qty-uom             to 68 format "x(4)"
     ar-invl.amt                    to 80 format "->>>>>9.99" skip
     ar-invl.i-name                 at 23 format "x(30)"
     ar-invl.disc                   to 63 format ">9.99"
     ar-invl.tax                    at 68 format "Y/N" skip
     ar-invl.i-dscr                 at 23 format "x(30)"
    with frame inv-mid1 down no-box no-labels stream-io width 85.

form header
     " "
     skip(7)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     v-bot-lab[1]         at 30
     skip
     v-sub-total[2]       to 37
     v-freight            to 51
     v-tax                to 63
     v-tot-inv            to 80
     skip(1)
     v-bot-lab[2]         at 30 format "x(37)"
     v-discount           to 80
     skip(2)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 85.

def stream last-page.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invhermn.txt") page-size VALUE(v-lines-per-page).

view frame inv-top.
view stream last-page frame inv-top.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq ar-inv.cust-no
    no-lock:

  assign v-po-no = ""
	 v-salesman = "".

  find first oe-ord
      where oe-ord.company eq cocode
	and oe-ord.ord-no   eq ar-inv.ord-no
      no-lock no-error.
  if avail oe-ord and oe-ord.ord-no ne 0 then
    assign v-po-no  = oe-ord.po-no
	   v-ord-no = string(oe-ord.ord-no)
	   v-ord-date = oe-ord.ord-date
	   v-salesman = oe-ord.sman[1].
  else
    assign v-po-no    = ar-inv.po-no
	   v-ord-no   = ""
	   v-ord-date = ?.

  find terms where terms.company = ar-inv.company
	and terms.t-code = ar-inv.terms
	no-lock no-error.

  if available terms then
    assign v-if-paid = terms.disc-days + ar-inv.inv-date
	    v-discount = ar-inv.due * terms.disc-rate / 100.

  find first carrier
      where carrier.company eq cocode
	and carrier.carrier eq ar-inv.carrier
      no-lock no-error.

  find first shipto
      where shipto.company eq cocode
	and shipto.cust-no eq ar-inv.cust-no
	and shipto.ship-id eq ar-inv.ship-id
      no-lock no-error.

  if available shipto then
  assign
   v-ship-name    = shipto.ship-name
   v-ship-addr[1] = shipto.ship-addr[1]
   v-ship-addr[2] = shipto.ship-addr[2]
   v-ship-city    = shipto.ship-city
   v-ship-state   = shipto.ship-state
   v-ship-zip     = shipto.ship-zip
   v-ship-addr3   = v-ship-city + ", " +
		    v-ship-state + "  " +
		    v-ship-zip
   v-cust-addr3   = cust.city + ", " +
		    cust.state + "  " +
		    cust.zip.

  if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
  if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

  v-bol-no = 0.
  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      no-lock:
    IF v-bol-no EQ 0 THEN v-bol-no = ar-invl.bol-no.
    do i = 1 to 3:
      if ar-invl.sman[i] ne "" then do:
	find first w-sman where w-sman.sman eq ar-invl.sman[i] no-error.
	if not avail w-sman then do:
	  create w-sman.
	  w-sman.sman = ar-invl.sman[i].
	end.
      end.
    end.
  end.

  if v-salesman eq "" then
  do:
    v-salesman = "".
    for each w-sman:
      if length(trim(v-salesman) + " " + trim(w-sman.sman) + ",") le 15 then
	v-salesman = trim(v-salesman) + " " + trim(w-sman.sman) + ",".
      delete w-sman.
    end.

    v-salesman = trim(v-salesman).

    if v-salesman ne "" then
      if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
	substr(v-salesman,length(trim(v-salesman)),1) = "".
  end.

  v-fob = if ar-inv.fob-code begins "ORIG" then "Origin" else "Destinatn".

  {ar/rep/invhermn.i "stream last-page" "(last-page)" }

  v-page-tot = page-number (last-page) - v-last-page.

  {ar/rep/invhermn.i}

  assign
   v-bot-lab[1]   = "Subtotal       Freight   Sales Tax"
   v-sub-total[2] = v-sub-total[1]
   v-freight      = ar-inv.freight
   v-tax          = ar-inv.tax-amt
   v-tot-inv      = ar-inv.due
   v-last-page    = page-number
   v-bot-lab[2]   = "If paid by " +
		    string(v-if-paid,"99/99/9999") +
		    " you may deduct:"
   ar-inv.printed = yes.
end. /* for each ar-inv */

output stream last-page close.
page.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

