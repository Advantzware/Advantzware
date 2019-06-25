/* ---------------------------------------------- ar/rep/invdnbry.p 01/99 JLF */
/* PRINT Danbury Invoice                                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}
def var save_id as recid.

def buffer xar-inv for ar-inv.

{ar/rep/invoice.i}

def var v-inv-no            like ar-inv.inv-no.
def var v-bot-lab           as   char format "x(21)" extent 3.

def var v-lines             as   int.
def var v-part-dscr         as   char format "x(30)".

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".


find first ar-inv no-lock no-error.
find first ar-invl no-lock no-error.
find first cust no-lock no-error.

format header
       skip(4)
       trim(string(ar-inv.inv-no,">>>>>>"))
                                    to 80
       skip(1)
       "PAGE " + string(trim(string(page-number - v-last-page,">9")) + " OF " +
                        trim(string(v-page-tot,">9")))
                                    to 80 format "x(13)"
       skip(2)
       cust.name                    at 4
       v-ship-name                  at 51
       cust.addr[1]                 at 4
       v-ship-addr[1]               at 51
       cust.addr[2]                 at 4
       v-ship-addr[2]               at 51
       v-cust-addr3                 at 4
       v-ship-addr3                 at 51
       skip(4)
       ar-inv.inv-date              at 1  FORMAT "99/99/99" 
       ar-invl.cust-no              at 11
       ar-invl.po-no                at 24
       trim(string(ar-invl.ord-no,">>>>>>")) when ar-invl.ord-no ne 0
                                    at 46
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 80.

form ar-invl.qty                   to 8  format "->>>>>>9"
     ar-invl.i-no                  at 10 format "x(15)"
     ar-invl.i-name                at 26 format "x(29)"
     ar-invl.unit-pr               to 64 format "->,>>9.99<<<<"
     ar-invl.pr-uom
     ar-invl.amt                   to 80 format "->>,>>9.99"

    with frame inv-mid1 down no-box no-labels stream-io width 80.

form header
     " "
     skip(8)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 80.

form header
     v-bot-lab[1]         to 80
     v-bot-lab[2]         to 80
     skip(1)
     v-bot-lab[3]         to 80
     skip(4)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 80.

def stream last-page.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invdnbry.txt") page-size VALUE(v-lines-per-page).

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
      no-lock
      by ar-invl.po-no:
    if ar-invl.po-no ne "" then leave.
  end.

  {ar/rep/invdnbry.i "stream last-page" "(last-page)"}

  v-page-tot = page-number (last-page) - v-last-page.

  {ar/rep/invdnbry.i}

  assign
   v-bot-lab[1]   = if ar-inv.freight eq 0 then "" else
                    ("Freight: " + string(ar-inv.freight,"->>>>>>>9.99"))
   v-bot-lab[2]   = if ar-inv.tax-amt eq 0 then "" else
                    ("    Tax: " + string(ar-inv.tax-amt,"->>>>>>>9.99"))
   v-bot-lab[3]   = ("  Total: " + string(ar-inv.due,    "->>>>>>>9.99"))

   v-last-page    = page-number
   ar-inv.printed = yes.
end. /* for each ar-inv */

output stream last-page close.

hide frame inv-top no-pause.
PAGE.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
