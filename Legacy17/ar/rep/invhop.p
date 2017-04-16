/* ------------------------------------------------ ar/rep/invhop.p 01/00 FWK */
/* PRINT HOP Invoice                                                          */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def buffer xar-inv for ar-inv.

def workfile w-sman field sman as char.

{ar/rep/invoice.i}

def var v-inv-no            like ar-inv.inv-no.
def var v-salesman          as   char format "x(11)".
def var v-trailer           as   char format "x(15)".
def var v-fob               as   char format "x(11)".
def var v-bot-lab           as   char format "x(21)" extent 2.
def var v-tot-wt            as   dec  format ">,>>>,>>9.9<".
def var v-tot-sf            as   dec  format ">,>>>,>>9.9<" extent 2.
def var v-tot-inv           as   dec  format "->>>>>>9.99".
def var v-bol-no            as   int  format ">>>>>>" no-undo.
def var v-ship-date         as   date format "99/99/9999" init today no-undo.
def var v-cust-phone        like cust.phone no-undo.
def var v-ord-no            as   int  format ">>>>>>" no-undo.

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
def var v-unit-price as dec format "$>>,>>9.99" no-undo.
def var v-cust-part like ar-invl.part-no no-undo.
def var v-order-qty as dec format "->,>>>,>>9" no-undo.
def var v-ship-qty as dec format "->,>>>,>>9" no-undo.
def var v-po-number like oe-ord.po-no no-undo.
def var v-item-number like ar-invl.i-no no-undo.
def var v-price as dec format "$->>,>>9.99" no-undo.
def var v-item-size as char no-undo format "x(20)".
def var v-backorder as dec format ">>,>>>,>>9" no-undo.
def var v-job-no as char format "x(9)" no-undo.
def var v-freight like inv-head.t-inv-cost no-undo.
def var v-tax like inv-head.t-inv-cost no-undo.
def var v-discount as char format "x(24)" no-undo.
def var v-if-paid as char format "x(22)" no-undo.
def var v-invoice-amount like inv-head.t-inv-cost no-undo.
def var v-billinst as char extent 2 no-undo.
def var v-misc-charges like inv-head.t-inv-cost no-undo.

DEF TEMP-TABLE tt-line NO-UNDO FIELD tt-i-name LIKE inv-line.i-name.


find first ar-inv no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(1)
       cust.name                    at 4
       v-ship-name                  at 37
       v-fob                        at 69
       v-salesman                   at 85
       trim(string(ar-inv.inv-no,">>>>>>"))
                                    at 101
       v-bol-no                     at 117
       cust.addr[1]                 at 4
       v-ship-addr[1]               at 37
       cust.addr[2]                 at 4
       v-ship-addr[2]               at 37
       ar-inv.terms-d               at 69  format "x(20)"
       ar-inv.inv-date              at 101 format "99/99/9999"
       v-ship-date                  at 117
       v-cust-addr3                 at 4
       v-ship-addr3                 at 37
       v-cust-phone                 at 4
       carrier.dscr                 at 69
       v-ord-no                     at 101
       string(trim(string(page-number - v-last-page,">9")) + " OF " +
              trim(string(v-page-tot,">9")))
                                    at 117 format "x(8)"
       skip(4)
       
    with frame inv-top page-top no-box no-underline stream-io width 132.

form v-cust-part at 1
     tt-i-name at 33
     v-order-qty at 70
     v-ship-qty at 90
     v-unit-price at 105
     v-price at 117
    with frame detail no-labels no-box no-underline down stream-io width 132.

form v-item-number at 1
     v-po-number
     tt-i-name at 33
     v-backorder at 70
     v-job-no at 91
    with frame detail2 no-labels no-box no-underline down stream-io width 132.

form ar-invl.i-name at 1  format "x(5)"
     ar-invl.prep-dscr at 33   format "x(36)"
     ar-invl.tax at 70
     ar-invl.amt at 117 format "$->>,>>9.99"
    with frame detailm no-labels no-box no-underline down stream-io width 132.

form header
     " "
     skip(4)
    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     skip(1)
     "Freight:" at 90 v-freight at 113
     "Tax:" at 90 v-tax at 113
     v-discount
     v-if-paid at 33
     "Net Amount:" at 90 v-invoice-amount at 113
    with frame inv-bot2 page-bottom stream-io width 132 no-box no-labels.

def stream last-page.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invhop.txt") page-size VALUE(v-lines-per-page).

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

  assign
   v-bol-no    = 0
   v-ship-date = ar-inv.inv-date.
  RELEASE oe-bolh.
  find first ar-invl
      where ar-invl.x-no   eq ar-inv.x-no
        and ar-invl.bol-no ne 0
      no-lock no-error.
  if avail ar-invl then 
  find first oe-bolh where oe-bolh.company eq ar-invl.company
                      and oe-bolh.bol-no eq ar-invl.bol-no
                     no-lock no-error.
  if avail oe-bolh then
    assign v-bol-no = oe-bolh.bol-no
           v-ship-date = oe-bolh.bol-date.
  
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

  {ar/rep/invhop.i "stream last-page" "(last-page)" }

  v-page-tot = page-number (last-page) - v-last-page.

  {ar/rep/invhop.i}

  assign
   v-bot-lab[1]   = if ar-inv.freight eq 0 then "" else
                    ("Freight: " + string(ar-inv.freight,"->>>>>>>9.99"))
   v-bot-lab[2]   = if ar-inv.tax-amt eq 0 then "" else
                    ("    Tax: " + string(ar-inv.tax-amt,"->>>>>>>9.99"))
   v-tot-wt       = if ar-inv.t-weight ne 0 then ar-inv.t-weight
                    else v-tot-wt
   v-tot-inv      = ar-inv.due
   v-last-page    = page-number
   v-freight          = ar-inv.freight
   v-tax          = ar-inv.tax-amt
   v-invoice-amount = ar-inv.gross
   ar-inv.printed = yes.
end. /* for each ar-inv */

output stream last-page close.
page.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */


