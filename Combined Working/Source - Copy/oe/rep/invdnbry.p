/* ---------------------------------------------- oe/rep/invdnbry.p 12/98 JLF */
/* PRINT Danbury Invoice                                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def buffer xinv-head for inv-head.

{oe/rep/invoice.i}

def var v-inv-no            like inv-head.inv-no.
def var v-po-no             like inv-line.po-no.
def var v-ord-no            like inv-line.ord-no.
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


find first inv-head no-lock no-error.
find first inv-line no-lock no-error.
find first cust no-lock no-error.

format header
       skip(4)
       trim(string(v-inv-no,">>>>>>"))
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
       inv-head.inv-date            at 1
       inv-head.cust-no             at 11
       v-po-no                      at 24
       trim(string(v-ord-no,">>>>>>"))
                                    at 46
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 80.

form inv-line.inv-qty               to 8  format "->>>>>>9"
     inv-line.i-no                  at 10 format "x(15)"
     inv-line.i-name                at 26 format "x(29)"
     inv-line.price                 to 64 format ">>,>>9.99<<<<"
     inv-line.pr-uom
     inv-line.t-price               to 80 format "->>,>>9.99"

    with frame inv-mid1 down no-box no-labels stream-io width 80.

form inv-misc.charge at 10
     inv-misc.dscr   at 26
     inv-misc.amt    to 80 format "->>>,>>9.99"
     skip(1)

    with frame inv-mid2 no-labels no-box no-underline down stream-io width 80.

form header
     " "
     skip(9)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 80.

form header
     v-bot-lab[1]         to 80
     v-bot-lab[2]         to 80
     skip(1)
     v-bot-lab[3]         to 80
     skip(5)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 80.

def stream last-page.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invdnbry.txt") page-size 41.

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

  find first oe-bolh
      where oe-bolh.company eq inv-head.company
        and oe-bolh.bol-no  eq inv-head.bol-no
      use-index bol-no no-lock no-error.

  if avail oe-bolh then do:
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
      by inv-line.po-no:
    if inv-line.po-no ne "" then do:
      assign  
       v-po-no  = inv-line.po-no
       v-ord-no = inv-line.ord-no.
      leave.
    end.
  end.

  {oe/rep/invdnbry.i "stream last-page" "(last-page)"}

  v-page-tot = page-number (last-page) - v-last-page.

  {oe/rep/invdnbry.i}

  assign
   v-bot-lab[1] = if inv-head.t-inv-freight ne 0 and inv-head.f-bill then
                  ("Freight: " + string(inv-head.t-inv-freight,"->>>>>>>9.99"))
                  else ""
   v-bot-lab[2] = if inv-head.t-inv-tax     ne 0 then
                  ("    Tax: " + string(inv-head.t-inv-tax,    "->>>>>>>9.99"))
                  else ""
   v-bot-lab[3] = ("  Total: " + string(inv-head.t-inv-rev,    "->>>>>>>9.99"))

   v-last-page = page-number.
end. /* for each inv-head */

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

