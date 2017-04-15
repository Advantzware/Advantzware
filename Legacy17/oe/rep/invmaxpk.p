/* ---------------------------------------------- oe/rep/invmaxpk.p 10/00 JLF */
/* PRINT Max Pak Invoice                                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def buffer xinv-head for inv-head.
def buffer xreport   for report.

{oe/rep/invoice.i}

def var v-inv-no            like inv-head.inv-no.
def var v-bot-lab           as   char format "x(20)" extent 2.
def var v-tot-inv           as   dec  format "->>>>>>9.99".
def var v-tot-disc          like v-tot-inv.

def var v-inv-price         as   dec.
def var v-part-dscr         as   char format "x(25)".
def var v-lines             as   int. 

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

find first inv-head no-lock no-error.
find first inv-line no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(5)
       trim(string(v-inv-no,">>>>>>"))
                                    at 60
       skip(1)
       inv-head.inv-date            at 60
       skip(1)
       inv-line.po-no               at 60   format "x(15)"
       skip(3)
       cust.name                    at 8
       v-ship-name                  at 50
       cust.addr[1]                 at 8
       v-ship-addr[1]               at 50
       cust.addr[2]                 at 8
       v-ship-addr[2]               at 50
       v-cust-addr3                 at 8
       v-ship-addr3                 at 50
       skip(4)
       carrier.dscr                 at 8    when avail carrier
       inv-head.terms-d             at 46
       skip(2)

    with frame inv-top page-top no-box no-underline stream-io width 85.

form oe-ordl.qty                    to 12 format "->,>>>,>>>"
     inv-line.inv-qty               to 22 format "->,>>>,>>>"
     inv-line.i-no                  at 24
     inv-line.i-name                at 40 format "x(25)"
     v-inv-price                    to 72 format "->>>9.99<<<"
     inv-line.t-price               to 82 format "->>>>>9.99"

    with frame inv-mid1 down no-box no-labels stream-io width 85.

form inv-misc.charge                at 14
     inv-misc.dscr                  
     inv-misc.amt                   to 82 format "->>>>>9.99"
     skip(1)

    with frame inv-mid2 no-labels no-box no-underline down stream-io width 85.

form header
     " "
     skip(6)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     v-bot-lab[1]         to 82
     v-bot-lab[2]         to 82
     skip(1)
     v-tot-inv            to 82
     skip(2)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 85.


find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

build-report:
for each xreport WHERE xreport.term-id EQ v-term-id NO-LOCK,

    FIRST inv-head WHERE RECID(inv-head) EQ xreport.rec-id,
        
    first cust
    where cust.company eq cocode
      and cust.cust-no eq inv-head.cust-no
    NO-LOCK.
        
  {oe/rep/bolcheck.i inv-head build-report}
      
  for each inv-line
      where inv-line.r-no eq inv-head.r-no
      no-lock:

    create report.
    assign
     report.term-id = v-term-id
     report.key-01  = if v-sort then
                        if v-sort-name then cust.name
                        else inv-head.cust-no
                      else ""
     report.key-02  = string(inv-head.bol-no,"9999999999")
     report.key-03  = string(inv-head.r-no,"9999999999")
     report.key-04  = inv-line.po-no
     report.rec-id  = recid(inv-line).
  end.

  DELETE xreport.
end.

view frame inv-top.

for each report where report.term-id eq v-term-id no-lock,

    first inv-line where recid(inv-line) eq report.rec-id no-lock,
    
    first inv-head where inv-head.r-no eq inv-line.r-no no-lock,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq inv-head.cust-no
    no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04
          by inv-line.i-no
          by inv-line.line:

  if first-of(report.key-03) then do:

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
        where carrier.company eq cocode
          and carrier.carrier eq inv-head.carrier
        no-lock no-error.

    find first oe-bolh
        where oe-bolh.company eq cocode
          and oe-bolh.bol-no  eq inv-head.bol-no
        use-index bol-no no-lock no-error.

    if avail oe-bolh then do:
      find first shipto
          where shipto.company eq cocode
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
    
    page.
    
    hide frame inv-bot2.
    view frame inv-bot1.
  end.
  
  else
  if first-of(report.key-04) then page.
  
  v-lines = 2.
  if inv-line.part-dscr1 ne "" then v-lines = v-lines + 1.
  if inv-line.part-dscr2 ne "" then v-lines = v-lines + 1.
  
  if line-counter - 1 + v-lines gt page-size + 1 then page.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq inv-line.ord-no
        and oe-ordl.i-no    eq inv-line.i-no
        and oe-ordl.line    eq inv-line.line
      no-lock no-error.

  v-inv-price = inv-line.t-price / inv-line.inv-qty.

  if v-inv-price eq ? then v-inv-price = 0.

  display oe-ordl.qty       when avail oe-ordl
          inv-line.inv-qty  when not avail oe-ordl @ oe-ordl.qty
          inv-line.inv-qty
          inv-line.i-no
          inv-line.i-name
          v-inv-price
          inv-line.t-price

      with frame inv-mid1.
  down with frame inv-mid1.

  do i = 1 to 2:
    v-part-dscr = if i eq 1 then inv-line.part-dscr1
                            else inv-line.part-dscr2.

    if v-part-dscr ne "" then put v-part-dscr at 40 skip.
  end.

  put skip(1).

  if last-of(report.key-03) then do:
    hide frame inv-bot1.
    view frame inv-bot2.
  
    for each inv-misc
        where inv-misc.company eq cocode
          and inv-misc.r-no    eq inv-head.r-no
          and inv-misc.bill    eq "Y"
        no-lock

        break by inv-misc.charge:

      if first(inv-misc.charge) then do:
        hide frame inv-bot2.
        view frame inv-bot1.
      end.

      v-lines = 2.

      if line-counter - 1 + v-lines gt page-size + 1 then page.

      if last(inv-misc.charge) then do:
        hide frame inv-bot1.
        view frame inv-bot2.
      end.

      display inv-misc.charge
              inv-misc.dscr
              inv-misc.amt

          with frame inv-mid2.
      down with frame inv-mid2.
    end. /* each inv-misc */

    assign
     v-tot-disc   = v-tot-disc * -1

     v-bot-lab[1] = if inv-head.t-inv-freight ne 0 and inv-head.f-bill then
                      (" Freight: " +
                       string(inv-head.t-inv-freight,"->>>>>9.99")) else ""
     v-bot-lab[2] = if inv-head.t-inv-tax     ne 0 then
                      ("     Tax: " +
                       string(inv-head.t-inv-tax,    "->>>>>9.99")) else ""

     v-tot-inv    = inv-head.t-inv-rev.
  end.   
end. /* for each inv-head */

hide frame inv-top no-pause.
page.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
