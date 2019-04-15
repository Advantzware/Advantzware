/* ---------------------------------------------- oe/rep/boltrist.p 12/98 JLF */
/* Print Tri-State BOL                                                        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.f}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.

{oe/rep/oe-lad.i}

def var v-frt-pay-dscr      as   char.
def var v-qty-uom           like oe-ordl.pr-uom.

def var v-to-ship           like oe-boll.qty.
def var v-tot-sqft          like itemfg.t-sqft init 0
			    format ">>>,>>9.99" no-undo.
def var v-pallets           like oe-bolh.tot-pallets.
def var v-partial           like oe-boll.qty.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
DEF VAR v-ord-no LIKE oe-boll.ord-no NO-UNDO.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(5)
       "BILL TO:"                       at 1
       oe-bolh.cust-no                  to 30
       "SHIP TO:"                       at 41
       oe-bolh.ship-id                  to 70
       cust.name                        at 6
       v-ship-name                      at 46
       cust.addr[1]                     at 6
       v-ship-addr[1]                   at 46
       cust.addr[2]                     at 6
       v-ship-addr[2]                   at 46
       v-cust-addr3                     at 6
       v-ship-addr3                     at 46
       skip(1)
       "OUR ORDER#"
       v-ord-no                                                       skip
       "BOL#"
       oe-bolh.bol-no                   format "999999"
       "TO SHIP VIA"                    at 13
       carrier.dscr when avail carrier  format "x(30)"
       "DUE ON"                         at 55
       oe-bolh.bol-date                 at 63
       "CUSTOMER PO#"                   at 1
       oe-ord.po-no when avail oe-ord   at 13
       "TRAILER #"                      at 35
       oe-bolh.trailer
       v-frt-pay-dscr
       "PAGE"                           at 63
       page-number - v-last-page        to 69       format "99"
       "OF"                             at 71
       v-page-tot                       to 75       format "99"
       tmpstore                         at 1
       "LOC"                            at 1
       "ITEM NUMBER"                    at 9
       "PRODUCT DESCRIPTION"            at 26
       "QUANTITY IN"                    at 59
       caps(v-qty-uom)                  at 71
       "------"                         at 1
       "---------------"                at 9
       "------------------------------" at 26
       "----------------------"         at 58
/*
       "TO SHIP"                        at 59
*/
       "QTY ORDERED"                    at 58
       "SHIPPED"                        at 74
       "--------------"                   at 58
       "-----------"                      at 74

    with frame bol-top page-top no-box no-underline stream-io width 85.

form oe-boll.loc-bin                at 1  format "x(6)"
/*
     oe-boll.i-no                   at 9  format "x(15)"
*/
     oe-ordl.part-no                at 9  format "x(15)"
     itemfg.i-name                  at 26 format "x(30)"
/*
     v-to-ship                      to 66 format ">>>,>>>"
*/
     oe-ordl.qty                    to 70 format ">>>>>>>>>"
/*
     "_______"                      at 71
*/

    with frame bol-mid down no-box no-labels stream-io width 85.

form header
     " "
     skip

    with frame bol-bot page-bottom no-box no-underline stream-io width 85.

def stream last-page.


tmpstore = fill("-",80).

output stream last-page to value(tmp-dir + "boltrist.txt") page-size VALUE(v-lines-per-page).

view frame bol-top.
view frame bol-bot.
view stream last-page frame bol-top.
view stream last-page frame bol-bot.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first carrier
	where carrier.company eq oe-bolh.company
	  and carrier.carrier eq oe-bolh.carrier
	no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
		      shipto.ship-state + "  " +
		      shipto.ship-zip
     v-cust-addr3   = cust.city + ", " +
		      cust.state + "  " +
		      cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    v-frt-pay-dscr = "".
    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      v-ord-no = oe-boll.ord-no.

      if oe-ord.frt-pay eq "p" then v-frt-pay-dscr = "PREPAID".
      else
      if oe-ord.frt-pay eq "c" then v-frt-pay-dscr = "COLLECT".
      else
      if oe-ord.frt-pay eq "b" then v-frt-pay-dscr = "PPD/CHG".

      find first oe-ordl
	      where oe-ordl.company eq oe-ord.company
	        and oe-ordl.ord-no  eq oe-ord.ord-no
	      no-lock no-error.
      if avail oe-ordl then do:
	    find first uom where uom.uom eq oe-ordl.pr-uom no-lock no-error.
	    v-qty-uom = if avail uom then substr(uom.dscr,1,8) else oe-ordl.pr-uom.
      end.
    end.

    page.
    page stream last-page.
  end. /* first-of(oe-bolh.bol-no) */

  {oe/rep/boltrist.i "stream last-page"}

  v-page-tot = page-number (last-page) - v-last-page.

  {oe/rep/boltrist.i}

  v-last-page = page-number.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

output stream last-page close.

hide frame bol-top no-pause.
page.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
