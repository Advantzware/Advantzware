/* ---------------------------------------------- oe/rep/bolbluer.p 01/99 JLF */
/* Print Blue Ridge BOL                                                       */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.

{oe/rep/oe-lad.i}

def var v-frt-pay-dscr      as   char.

def var v-part-dscr         as   char.
def var v-job-po            as   char.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

def workfile w2 no-undo
    field cases            as   int format ">>9"
    field cas-cnt          as   int format ">>>>>9".

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(6)
       "BILL TO:"                       at 1
       "SHIP TO:"                       at 41
       cust.name                        at 6
       v-ship-name                      at 46
       cust.addr[1]                     at 6
       v-ship-addr[1]                   at 46
       cust.addr[2]                     at 6
       v-ship-addr[2]                   at 46
       v-cust-addr3                     at 6
       v-ship-addr3                     at 46
       skip(2)
       "BOL DATE"                       at 53
       oe-bolh.bol-date                 at 63
       "BOL#"                           at 1
       oe-bolh.bol-no                   format "999999"
       "TRAILER #"                      at 35
       oe-bolh.trailer
       v-frt-pay-dscr
       "PAGE"                           at 63
       page-number - v-last-page        to 69   format "99"
       "OF"                             at 71
       v-page-tot                       to 75   format "99"
       tmpstore                         at 1    format "x(82)"
       "P.O.#"                          at 17
       "PER"                            at 67
       "ITEM NUMBER"                    at 1
       "JOB#"                           at 17
       "PRODUCT DESCRIPTION"            at 33
       "UNIT"                           at 62
       "UNIT"                           at 67
       "TOTAL"                          at 74
       "---------------"                at 1
       "---------------"                at 17
       "----------------------------"   at 33
       "----"                           at 62
       "------"                         at 67
       "---------"                         at 74

    with frame bol-top page-top no-box no-underline stream-io width 85.

form /*
     oe-boll.i-no                   at 1  format "x(15)"
     */
     oe-ordl.part-no                at 1  format "x(15)" /* replaced above */
     v-job-po                       at 17 format "x(15)"
     v-part-dscr                    at 33 format "x(28)"
     w2.cases                       to 65 format ">>>9"
     w2.cas-cnt                     to 72 format ">>>>>9"
     oe-boll.qty                    to 82 format ">>>>>>>>9"

    with frame bol-mid down no-box no-labels stream-io width 85.

form header
     " "
     skip(1)

    with frame bol-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     "Received by:"
     fill("_",44) format "x(44)"
     "Date:"
     fill("_",15) format "x(15)"
     skip

    with frame bol-bot2 page-bottom no-box no-underline stream-io width 85.

def stream last-page.


tmpstore = fill("-",82).

output stream last-page to value(tmp-dir + "bolbluer.txt") page-size VALUE(v-lines-per-page).

view frame bol-top.
view stream last-page frame bol-top.

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

	  case oe-ord.frt-pay:
           when "P" THEN v-frt-pay-dscr = "PREPAID".
           when "C" THEN v-frt-pay-dscr = "COLLECT".
           when "B" THEN v-frt-pay-dscr = "PPD/CHG".
           when "T" THEN v-frt-pay-dscr = "3rdPARTY".
      end case.
	
      LEAVE.
    END.

    page.
    page stream last-page.
  end. /* first-of(oe-bolh.bol-no) */

  {oe/rep/bolbluer.i "stream last-page"}

  v-page-tot = page-number (last-page) - v-last-page.

  {oe/rep/bolbluer.i}

  v-last-page = page-number.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

output stream last-page close.

hide frame bol-top no-pause.
page.

/* end ---------------------------------- copr. 1999  Advanced Software, Inc. */
