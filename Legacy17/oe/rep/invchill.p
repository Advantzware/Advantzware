/* ---------------------------------------------- oe/rep/invchill.p 10/01 JLF */
/* PRINT Chillicothe Invoice                                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def buffer xinv-head for inv-head.

{oe/rep/invoice.i}

def var v-inv-no     like inv-head.inv-no.
def var v-bot-lab    as   char format "x(20)" extent 2.
def var v-tot-inv    as   dec  format "->>>>>>9.99".
def var v-tot-disc   as   dec  format "$->>>>>9.99".
def var v-dsc-date   as   date format "99/99/99".
def var v-due-date   like v-dsc-date.

def var v-sman-no    as   char.
def var v-sman-name  like sman.sname.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 like shipto.ship-name.
def var v-cust-addr3 like shipto.ship-name.

DEF TEMP-TABLE tt-report LIKE report.

find first inv-head no-lock no-error.
find first inv-line no-lock no-error.
find first cust no-lock no-error.

format header
       skip(8)
       v-ship-name          format "x(35)"      at 10
       v-ship-addr[1]       format "x(35)"      at 10
       v-ship-addr[2]       format "x(35)"      at 10
       v-ship-addr3         format "x(35)"      at 10
       skip(1)
       inv-head.cust-no     format "x(8)"       at 72
       trim(string(v-inv-no,">>>>>>"))          at 70
       inv-line.po-no                           at 73
       cust.name            format "x(35)"      at 10
       v-sman-name          format "x(23)"      at 65
       cust.addr[1]         format "x(35)"      at 10
       cust.addr[2]         format "x(35)"      at 10
       inv-head.inv-date    format "99/99/99"   at 63
       v-cust-addr3         format "x(35)"      at 10
       trim(string(page-number - v-last-page,">>>>>>"))
                                                at 63
       inv-head.terms-d     format "x(23)"      at 64
       skip(4)

    with frame inv-top page-top no-box no-underline stream-io width 87.

form inv-line.inv-qty       format "->>>>>>>"       to 10
     inv-line.i-no                                  at 12
     inv-line.i-name                                at 28
     inv-line.price         format ">>>>>9.99"      to 73
     inv-line.t-price       format "->>>>>>9.99"    to 85

    with frame inv-mid1 down no-box no-labels stream-io width 87.

form inv-misc.charge                                at 10
     inv-misc.dscr          format "x(30)"          at 26
     inv-misc.amt           format "->>>>>>>9.99"   to 85

    with frame inv-mid2 no-labels no-box no-underline down stream-io width 87.

form header
     " "
     skip
     skip(8)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 87.

form header
     v-bot-lab[1]                                           to 85
     v-bot-lab[2]                                           to 85
     skip(1)
     "A SERVICE CHARGE OF 1-1/2% PER MONTH WILL BE ADDED"   at 26
     "TO INVOICES NOT PAID WITHIN 30 DAYS.  18% APR"        at 26
     "INVOICE TOTAL >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"    at 26
     v-tot-inv                                              to 85
     "Discount of"                                          at 43
     v-tot-disc
     "if paid by"
     v-dsc-date
     "Invoice Due"                                          at 66
     v-due-date
     skip(1)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 87.


find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each report where report.term-id eq v-term-id,
    
    FIRST inv-head WHERE RECID(inv-head) EQ report.rec-id NO-LOCK:
           
  v-sman-no = "".
        
  for each inv-line
      where inv-line.r-no eq inv-head.r-no
      by inv-line.i-no:
        
    if v-sman-no eq "" then
    do i = 1 to 3:
      if inv-line.sman[i] ne "" then do:
        v-sman-no = inv-line.sman[i].
        leave.
      end.  
    end.
  end.
  
  for each inv-line where inv-line.r-no eq inv-head.r-no no-lock:
    create tt-report.
    assign
     tt-report.term-id = v-term-id
     tt-report.key-01  = if v-sort then
                           if v-sort-name then cust.name
                           else inv-head.cust-no
                         else ""
     tt-report.key-02  = string(inv-head.bol-no,"9999999999")
     tt-report.key-03  = string(inv-head.r-no,"9999999999")
     tt-report.key-04  = inv-line.po-no
     tt-report.key-05  = inv-line.i-no
     tt-report.key-06  = v-sman-no
     tt-report.rec-id  = recid(inv-line).
  end.

  DELETE report.
end.

view frame inv-top.

for each tt-report no-lock,

    first inv-line where recid(inv-line) eq tt-report.rec-id no-lock,
    
    first inv-head where inv-head.r-no eq inv-line.r-no no-lock,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq inv-head.cust-no
    no-lock
  
    break by tt-report.key-01
          by tt-report.key-02
          by tt-report.key-03
          by tt-report.key-04
          by inv-line.i-no
          by inv-line.line:

  if first-of(tt-report.key-03) then do:
    IF NOT v-reprint OR inv-head.inv-no EQ 0 THEN DO TRANSACTION:
      RUN oe/get-inv#.p (ROWID(inv-head)).
    END.

    FIND xinv-head WHERE ROWID(xinv-head) EQ ROWID(inv-head) NO-LOCK.

    assign
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
    
    v-sman-name = tt-report.key-06.
  
    if tt-report.key-06 ne "" then
    find first sman
        where sman.company eq cocode
          and sman.sman    eq tt-report.key-06
        no-lock no-error.
    if avail sman then v-sman-name = sman.sname.
  
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
  if first-of(tt-report.key-04) then page.
  
  display inv-line.inv-qty
          inv-line.i-no
          inv-line.i-name
          inv-line.price
          inv-line.t-price
          
      with frame inv-mid1.
  down with frame inv-mid1.
                
  if last-of(tt-report.key-03) then do:
    hide frame inv-bot1.
    view frame inv-bot2.

    for each inv-misc
        where inv-misc.company eq cocode
          and inv-misc.r-no    eq inv-head.r-no
          and inv-misc.bill    eq "Y"
        no-lock

        break by inv-misc.charge:

      display inv-misc.charge
              inv-misc.dscr
              inv-misc.amt

          with frame inv-mid2.
      down with frame inv-mid2.
    end. /* each inv-misc */
  
    hide frame inv-bot1.
    view frame inv-bot2.
    
    assign
     v-bot-lab[1] = if inv-head.t-inv-freight ne 0 and inv-head.f-bill then
                      ("FREIGHT: " +
                       string(inv-head.t-inv-freight,"->>>>>>9.99")) else ""
     v-bot-lab[2] = if inv-head.t-inv-tax     ne 0 then
                      ("TAXABLE: " +
                       string(inv-head.t-inv-tax,    "->>>>>>9.99")) else ""

     v-tot-inv    = inv-head.t-inv-rev
     v-tot-disc   = 0
     v-dsc-date   = ?
     v-last-page  = page-number.
     
    find first terms where terms.t-code eq inv-head.terms no-lock no-error.

    if avail terms then
      assign
       v-tot-disc = round((inv-head.t-inv-rev -
                           inv-head.t-inv-tax -
                           inv-head.t-inv-freight) *
                          (terms.disc-rate / 100),2)
       v-dsc-date = today + terms.disc-days
       v-due-date = today + terms.net-days.
  end.
end. /* for each tt-report */

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
