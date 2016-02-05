/* ---------------------------------------------- oe/rep/invtriad.p 12/98 JLF */
/* PRINT Triad Invoice                                                        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def buffer xinv-head for inv-head.

def workfile w-sman field sman as char.

{oe/rep/invoice.i}

def var v-inv-no            like inv-head.inv-no.
def var v-salesman          as   char format "x(25)".
def var v-trailer           as   char format "x(15)".
def var v-fob               as   char format "x(10)".
def var v-bot-lab           as   char format "x(22)" extent 3.
def var v-tot-wt            as   dec  format ">,>>>,>>9.9<" extent 2.
def var v-tot-sf            as   dec  format ">,>>>,>>9.9<" extent 2.
def var v-tot-inv           as   dec  format "->>>>>>9.99".
def var v-tot-disc          like v-tot-inv extent 2.

def var v-lines             as   int.
def var v-inv-price         as   dec  format ">>>>9.99".
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-scity      like company.city.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

find first inv-head no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(3)
       trim(string(v-inv-no,">>>>>>"))
                                    at 65
       skip(1)
       inv-head.inv-date            at 66
       skip(1)
       string(trim(string(page-number - v-last-page,">9")) + " OF " +
              trim(string(v-page-tot,">9")))
                                    at 61 format "x(8)"
       skip(1)
       trim(string(inv-head.bol-no,">>>>>>"))
                                    at 71
       skip(1)
       cust.name                    at 10
       v-ship-name                  at 55
       cust.addr[1]                 at 10
       v-ship-addr[1]               at 55
       cust.addr[2]                 at 10
       v-ship-addr[2]               at 55
       v-cust-addr3                 at 10
       v-ship-addr3                 at 55
       skip(5)
       inv-head.terms-d             at 2  format "x(14)"
       v-salesman                   at 17
       carrier.dscr                 at 43 format "x(15)" when avail carrier
       v-scity                      at 60
       v-fob                        at 76
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 85.

form oe-ordl.qty                    to 6  format "->>>9"
     inv-line.i-no                  at 8  format "x(15)"
     inv-line.po-no                 at 41 format "x(15)"
     inv-line.ship-qty              to 62 format "->>>9"
     v-part-comp                    at 64
     v-inv-price                    to 73 /*format ">>>>9.99"*/
     inv-line.t-price               to 83 format "->>>>>9.99"
     with frame inv-mid1 down no-box no-labels stream-io width 85.

form inv-misc.charge at 8
     inv-misc.dscr
     inv-misc.amt    to 83 format "->>>>>9.99"
     skip(1)

    with frame inv-mid2 no-labels no-box no-underline down stream-io width 85.

form header
     " "
     skip(8)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     v-bot-lab[1]         to 83 /*85*/
     v-bot-lab[2]         to 83
     v-bot-lab[3]         to 83
     skip(2)
     v-tot-sf[2]          to 42
     v-tot-wt[2]          to 55
     v-tot-inv            to 83
     skip(2)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 85.

def stream last-page.


find first company where company.company eq cocode no-lock no-error.
if avail company THEN v-scity = company.city.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invtriad.txt") page-size VALUE(v-lines-per-page).

view frame inv-top.
view stream last-page frame inv-top.

for each report where report.term-id eq v-term-id no-lock,

    first inv-head where recid(inv-head) eq report.rec-id no-lock,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq inv-head.cust-no
    no-lock

    break by report.key-01
          by report.key-02:

  IF NOT v-reprint OR inv-head.inv-no EQ 0 THEN DO TRANSACTION:
    RUN oe/get-inv#.p (ROWID(inv-head)).
  END.

  FIND xinv-head WHERE ROWID(xinv-head) EQ ROWID(inv-head) NO-LOCK.

  ASSIGN
   v-inv-no       = xinv-head.inv-no
   v-ship-name    = inv-head.sold-name
   v-ship-addr[1] = inv-head.sold-addr[1]
   v-ship-addr[2] = inv-head.sold-addr[2]
   v-ship-city    = inv-head.sold-city
   v-ship-state   = inv-head.sold-state
   v-ship-zip     = inv-head.sold-zip.

  find first carrier
      where carrier.company eq inv-head.company
        and carrier.carrier eq inv-head.carrier
      no-lock no-error.

  find first oe-bolh
      where oe-bolh.company eq inv-head.company
        and oe-bolh.bol-no  eq inv-head.bol-no
      use-index bol-no no-lock no-error.

  if avail oe-bolh then do:
    v-trailer = oe-bolh.trailer.

    find first shipto
        where shipto.company eq oe-bolh.company
          and shipto.cust-no eq oe-bolh.cust-no
          and shipto.ship-id eq oe-bolh.ship-id
        no-lock no-error.

    if avail shipto then
      assign
       v-ship-name    = shipto.ship-name
       v-ship-addr[1] = shipto.ship-addr[1]
       v-ship-addr[2] = shipto.ship-addr[2]
       v-ship-city    = shipto.ship-city
       v-ship-state   = shipto.ship-state
       v-ship-zip     = shipto.ship-zip.
  end. /* avail oe-bolh */

  assign
   v-ship-addr3   = v-ship-city + ", " +
                    v-ship-state + "  " +
                    v-ship-zip
   v-cust-addr3   = cust.city + ", " +
                    cust.state + "  " +
                    cust.zip.

  if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
  if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

  for each inv-line
     where inv-line.r-no eq inv-head.r-no
     no-lock
     by inv-line.i-no:

    do i = 1 to 3:
      if inv-line.sman[i] ne "" then do:
        find first w-sman where w-sman.sman eq inv-line.sman[i] no-error.
        if not avail w-sman then do:
          create w-sman.
          w-sman.sman = inv-line.sman[i].
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

  v-fob = if inv-head.fob-code begins "ORIG" then "Origin" else "Destinatn".

  {oe/rep/invtriad.i "stream last-page" "(last-page)"}

  v-page-tot = page-number (last-page) - v-last-page.

  {oe/rep/invtriad.i}

  assign
   v-tot-disc[2] = v-tot-disc[2] * -1

   v-bot-lab[1] = if inv-head.t-inv-freight ne 0 and inv-head.f-bill then
                  (" Freight: " + string(inv-head.t-inv-freight,"->>>>>>>9.99"))
                  else ""
   v-bot-lab[2] = if inv-head.t-inv-tax     ne 0 then
                  ("     Tax: " + string(inv-head.t-inv-tax,    "->>>>>>>9.99"))
                  else ""
   v-bot-lab[3] = if v-tot-disc[2]          ne 0 then
                  ("Discount: " + string(v-tot-disc[2],         "->>>>>>>9.99"))
                  else ""

   v-tot-wt[2]  = if inv-head.t-inv-weight ne 0 then inv-head.t-inv-weight
                  else v-tot-wt[2]
   v-tot-inv    = inv-head.t-inv-rev
   v-last-page  = page-number.
end. /* for each inv-head */

output stream last-page close.

hide frame inv-top no-pause.
page.


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

