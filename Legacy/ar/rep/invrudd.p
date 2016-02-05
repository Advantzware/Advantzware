/* ----------------------------------------------- ar/rep/invrudd.p 02/98 FWK */
/*                                                                            */
/* A/R Invoice Rudd Print Program - A/R Module                                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{ar/rep/invoice.i}

def var save_id            as   recid no-undo.
def var v-dscr-lines       as   int.
def var v-printline        as   int.
def var v-part-info        like ar-invl.part-dscr1.
def var v-addr3            as   char format "x(30)".
def var v-sold-addr3       as   char format "x(30)".
def var v-fob              as   char format "x(11)".
def var v-net              like ar-inv.due.
def var tmp1               as   dec.
def var tmp2               as   date.
def var net1               as   dec.
def var net2               as   dec.
def var net3               as   dec.
def var v-hdr              as   char format "x(80)" extent 4.
def var v-max-lines        as   int.

assign
 substr(v-hdr[1],60,20) = "INVOICE#  INV DATE"

 substr(v-hdr[2],01,07) = "SOLD TO"
 substr(v-hdr[2],46,07) = "SHIP TO"

 v-hdr[3] = "YOUR ORDER NO  DELIV DATE SHIP VIA             F.O.B.   " +
	    "   TERMS            SLMN"

 v-hdr[4] = "    QTY  P  C JOB NUMBER    DESCRIPTION                 " +
	    "      PRICE       TOTAL ".

form ar-invl.qty               format ">>>>>>9-"
     ar-invl.i-name    at 15   format "x(30)"
     ar-invl.unit-pr   at 51   format ">>>>9.99-"
     ar-invl.amt       to 79   format ">>>>>9.99-"
     ar-invl.i-dscr    at 15   format "x(30)"
  with frame detail no-attr-space no-labels no-box no-underline down stream-io width 80.

form v-part-info       at 15
  with frame dscr no-labels no-box no-underline down stream-io width 80.

form
     "Delivery Charge" to 65 ar-inv.freight format "->,>>9.99" to 79
     "-------------" to 79
     "Net Amount" TO 65 ar-inv.due format "->,>>>,>>9.99" to 79
     "=============" to 79
     "Salesman:" at 1 cust.sman skip
  with frame totals-comp no-labels no-box no-underline stream-io width 80.

form skip(1)
  with frame blankl no-labels no-box no-underline stream-io width 80.


if not v-print-head then v-hdr = "".

find first company where company.company = cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

v-max-lines = 34.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id

    break by ar-inv.inv-no:

  find first cust
      where cust.company eq cocode
	and cust.cust-no eq ar-inv.cust-no
      no-lock.

  find first shipto
      where shipto.company eq cocode
	and shipto.cust-no eq ar-inv.cust-no
	and shipto.ship-id eq ar-inv.ship-id
      no-lock.

  find first carrier
      where carrier.company eq cocode
	and carrier.carrier eq ar-inv.carrier
      no-lock no-error.

  assign
   v-addr3      = cust.city + ", " +
		  cust.state + "  " +
		  cust.zip
   v-sold-addr3 = shipto.ship-city + ", " +
		  shipto.ship-state + "  " +
		  shipto.ship-zip
   v-fob        = if ar-inv.fob-code begins "ORIG" then "Origin"
						   else "Destination"
   v-printline  = 0.

  if first(ar-inv.inv-no) then
    form header
     "INVOICE" at 40
     ar-inv.inv-date       to 66 FORMAT "99/99/99" 
     ar-inv.inv-no         to 77
     skip(7)
     "SOLD TO" at 2 "SHIP TO" at 47 skip(1)
     cust.name             at 2
     shipto.ship-name      at 47
     cust.addr[1]          at 2
     shipto.ship-addr[1]   at 47
     cust.addr[2]          at 2
     shipto.ship-addr[2]   at 47
     v-addr3               at 2
     v-sold-addr3          at 47
     skip(2)
     "CUST ORDER#" at 1 ar-inv.po-no
     "TERM:" to 48 ar-inv.terms-d to 79
     skip(1)
     "QUANTITY" at 1 "DESCRIPTION" at 15 "PRICE" at 51 "TOTAL " to 79
     "--------" at 1 "-----------" at 15 "-----" at 51 "----- " to 79
     skip(1)
  with frame invhead page-top no-labels no-box no-underline stream-io width 80.

  view frame invhead.
  page.

  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      break by ar-invl.line:

    v-printline = v-printline + 2.

    if v-printline gt 30 then do:
      v-printline = v-printline - (2 + v-dscr-lines).
      put skip(32 - v-printline) "* CONTINUED *" to 80.
      v-printline = 2 + v-dscr-lines.
      page.
    end.

    display ar-invl.qty
	    ar-invl.i-name
	    ar-invl.unit-pr
	    ar-invl.amt
	    ar-invl.i-dscr
	with frame detail.
    down with frame detail.

    if ar-invl.i-dscr ne "" then
    do:
      assign v-printline = v-printline + 1.
      put skip(1).
    end.
  end. /* for each ar-invl record */

  if v-printline gt v-max-lines then v-printline = v-max-lines.

  put skip(v-max-lines - v-printline).

  find first terms
      where terms.company eq ar-inv.company
	and terms.t-code  eq ar-inv.terms
      no-lock no-error.

  if avail terms then
    assign
     tmp1 = ar-inv.net * (round(terms.disc-rate / 100, 2))
     tmp2 = today + terms.disc-days.

  display ar-inv.freight
	  ar-inv.due
	  cust.sman
	with frame totals-comp.

  ar-inv.printed = yes.
end. /* for each ar-inv record */

/* End ----------------------------------- Copr. 1997  Advanced Software Inc. */
