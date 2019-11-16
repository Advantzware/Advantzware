/* ---------------------------------------------- ar/rep/invtriad.p 12/98 JLF */
/* PRINT Triad Invoice                                                        */
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
def var v-bot-lab           as   char format "x(21)" extent 2.
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
def var v-scity      like company.city.

find first ar-inv no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

find first company where company.company eq cocode no-lock no-error.
if avail company then
  v-scity = company.city.

format header
       skip(3)
       trim(string(ar-inv.inv-no,">>>>>>"))
				    at 65
       skip(1)
       ar-inv.inv-date              at 66 FORMAT "99/99/99" 
       skip(1)
       string(trim(string(page-number - v-last-page,">9")) + " OF " +
	      trim(string(v-page-tot,">9")))
				    at 61 format "x(8)"
       skip(3)
       cust.name                    at 10
       v-ship-name                  at 55
       cust.addr[1]                 at 10
       v-ship-addr[1]               at 55
       cust.addr[2]                 at 10
       v-ship-addr[2]               at 55
       v-cust-addr3                 at 10
       v-ship-addr3                 at 55
       skip(2)
       ar-inv.terms-d               at 2  format "x(14)"
       v-salesman                   at 17
       carrier.dscr                 at 43 format "x(15)" when avail carrier
       v-scity                      at 60
       v-fob                        at 76
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 85.

form oe-ordl.qty                    to 6  format "->>>9"
     ar-invl.i-no                   at 8  format "x(30)"
     ar-invl.po-no                  at 41 format "x(15)"
     ar-invl.qty                    to 62 format "->>>9"
     v-part-comp                    at 64
     v-inv-price                    to 74
     ar-invl.amt                    to 85 format "->>>>>9.99"

    with frame inv-mid1 down no-box no-labels stream-io width 85.

form header
     " "
     skip(7)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     v-bot-lab[1]         at 65
     v-bot-lab[2]         at 65
     skip(2)
     v-tot-sf[2]          to 42
     v-tot-wt             to 55
     v-tot-inv            to 85
     skip(2)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 85.

def stream last-page.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invtriad.txt") page-size VALUE(v-lines-per-page).

view frame inv-top.
view stream last-page frame inv-top.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq ar-inv.cust-no
    no-lock:

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

  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      no-lock:

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

  v-fob = if ar-inv.fob-code begins "ORIG" then "Origin" else "Destinatn".

  {ar/rep/invtriad.i "stream last-page" "(last-page)" }

  v-page-tot = page-number (last-page) - v-last-page.

  {ar/rep/invtriad.i}

  assign
   v-bot-lab[1]   = if ar-inv.freight eq 0 OR NOT ar-inv.f-bill then "" else
		    ("Freight: " + string(ar-inv.freight,"->>>>>>>9.99"))
   v-bot-lab[2]   = if ar-inv.tax-amt eq 0 then "" else
		    ("    Tax: " + string(ar-inv.tax-amt,"->>>>>>>9.99"))
   v-tot-wt       = if ar-inv.t-weight ne 0 then ar-inv.t-weight
		    else v-tot-wt
   v-tot-inv      = ar-inv.due
   v-last-page    = page-number
   ar-inv.printed = yes.
end. /* for each ar-inv */

output stream last-page close.
page.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

