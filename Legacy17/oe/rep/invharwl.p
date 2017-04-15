/* ---------------------------------------------- oe/rep/invharwl.p 11/00 JLF */
/* PRINT Harwell Invoice                                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

def buffer xinv-head for inv-head.

{oe/rep/invoice.i}

def var v-inv-no     like inv-head.inv-no.
def var v-bot-lab    as   char format "x(63)" extent 3.
def var v-tot-sqft   as   dec  format "->>>>>>>9.99".
def var v-tot-wght   as   dec  format "->>>>>>>9.99".
def var v-tot-inv    as   dec  format "->>>>>>9.99".

def var v-part-dscr  as   char format "x(25)".
def var v-lines      as   int. 

def var v-sman-no    as   char.
def var v-fob        as   char.
def var v-addr-info  as   char.
def var v-cust       like inv-head.addr extent 4.
def var v-ship       like v-cust.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 like shipto.ship-name.
def var v-cust-addr3 like shipto.ship-name.
def var v-p-c        as   char.
def var v-t-price    like inv-line.t-price.
def var v-t-tax      as   dec extent 3.

def workfile w-lin
    field w-par like inv-line.i-name init ""
    field w-lin as   int.
    
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.

find first inv-head no-lock no-error.
find first inv-line no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(5)
       trim(string(v-inv-no,">>>>>>"))          at 70
       skip(1)
       inv-head.inv-date    format "99/99/9999" at 70
       skip(1)
       string(trim(string(page-number - v-last-page,">>>>>>")))
                                                at 70
       skip(1)
       trim(string(inv-head.bol-no,">>>>>>"))   at 70
       skip(1)
       inv-head.cust-no     format "x(8)"       at 10
       v-cust[1]            format "x(41)"      at 10
       v-ship[1]            format "x(34)"      at 52
       v-cust[2]            format "x(41)"      at 10
       v-ship[2]            format "x(34)"      at 52
       v-cust[3]            format "x(41)"      at 10
       v-ship[3]            format "x(34)"      at 52
       v-cust[4]            format "x(41)"      at 10
       v-ship[4]            format "x(34)"      at 52
       skip(4)
       inv-head.terms-d     format "x(14)"      at 2
       v-sman-no            format "x(25)"      at 17
       carrier.dscr         format "x(15)"      at 43   when avail carrier
       v-fob                format "x(11)"      at 75
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 85.

form oe-ordl.qty            format "->>>>"          to 6
     w-par                  format "x(32)"          at 8
     inv-line.po-no         format "x(14)"          at 41
     inv-line.inv-qty       format "->>>>>>"        to 62
     v-p-c                  format "x"              at 64
     inv-line.price         format ">>>>9.99"       to 73
     inv-line.pr-uom        format "x"              at 74
     inv-line.t-price       format "->>>>>>9.99"    to 85

    with frame inv-mid1 down no-box no-labels stream-io width 85.

form inv-misc.charge                                at 8
     inv-misc.dscr
     inv-misc.amt           format "->>>>>>9.99"    to 85

    with frame inv-mid2 no-labels no-box no-underline down stream-io width 85.

form header
     " "
     skip(7)

    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     v-bot-lab[1]                   to 85
     v-bot-lab[2]                   to 85
     v-bot-lab[3]                   to 85
     skip(2)
     v-tot-sqft                     to 42
     v-tot-wght                     to 55
     v-tot-inv                      to 85
     skip(1)

    with frame inv-bot2 page-bottom no-box no-underline stream-io width 85.


find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

view frame inv-top.

for each report where report.term-id eq v-term-id no-lock,

    first inv-head where recid(inv-head) eq report.rec-id,

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

  v-sman-no = "".
    
  for each inv-line
      where inv-line.r-no eq inv-head.r-no
      by inv-line.i-no:
    
    do i = 1 to 3:
      if inv-line.sman[i] ne "" then do:
        find first sman
            where sman.company eq cocode
              and sman.sman    eq inv-line.sman[i]
            no-lock no-error.
        v-sman-no = if avail sman then sman.sname else inv-line.sman[i].
        
        leave.
      end.  
    end.
    
    if v-sman-no ne "" then leave.
  end.

  assign
   v-fob          = if inv-head.fob-code begins "ORIG" 
                    then "ORIGIN" else "DESTINATION"
   v-ship-addr3   = v-ship-city + ", " +
                    v-ship-state + "  " +
                    v-ship-zip
   v-cust-addr3   = cust.city + ", " +
                    cust.state + "  " +
                    cust.zip.
     
  if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
  if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".
  
  assign
   j      = 0
   v-cust = "".
         
  do i = 1 to 4:
    v-addr-info = if i eq 1 then cust.name    else
                  if i eq 2 then cust.addr[1] else
                  if i eq 3 then cust.addr[2] else v-cust-addr3.
          
    if v-addr-info ne "" then
      assign
       j         = j + 1
       v-cust[j] = v-addr-info.
  end.
        
  assign
   j      = 0
   v-ship = "".
         
  do i = 1 to 4:
    v-addr-info = if i eq 1 then v-ship-name    else
                  if i eq 2 then v-ship-addr[1] else
                  if i eq 3 then v-ship-addr[2] else v-ship-addr3.
                         
    if v-addr-info ne "" then
      assign
       j         = j + 1
       v-ship[j] = v-addr-info.
  end.
  
  release stax.
  
  if cust.sort eq "Y" then
  find first stax
      where stax.company   eq cocode
        and stax.tax-group eq inv-head.tax-gr
      no-lock no-error.

  page.
    
  hide frame inv-bot2.
  view frame inv-bot1.
  
  assign
   v-t-tax    = 0
   v-tot-sqft = 0
   v-tot-wght = 0.
  
  for each inv-line
      where inv-line.r-no eq inv-head.r-no
      by inv-line.i-no:
      
    v-p-c = "P".
    
    for each oe-boll
        where oe-boll.b-no eq inv-line.b-no
          and oe-boll.i-no eq inv-line.i-no
          and oe-boll.line eq inv-line.line
        no-lock:
        
      if oe-boll.p-c then v-p-c = "C".
    end.
    
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq inv-line.ord-no
          and oe-ordl.i-no    eq inv-line.i-no
          and oe-ordl.line    eq inv-line.line
        no-lock no-error.
    
    v-lines = 0.
    
    if inv-line.i-no ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.i-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
        
    if inv-line.i-name ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.i-name
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
        
    if inv-line.part-dscr1 ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.part-dscr1
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    if inv-line.part-dscr2 ne "" then do:
      create w-lin.
      assign
       w-par   = inv-line.part-dscr2
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    v-t-price = inv-line.t-price.
    
    if inv-line.tax and avail stax then
    do i = 1 to 3:
      if stax.tax-code[i] ne "" then do:
        create w-tax.
        assign
         w-dsc      = stax.tax-dscr[i]
         w-tax      = round((if stax.accum-tax then v-t-price
                                               else inv-line.t-price) *
                            stax.tax-rate[i] / 100,2)
         v-t-price  = v-t-price + w-tax
         v-t-tax[i] = v-t-tax[i] + w-tax
         v-lines    = v-lines + 1.
      end.
    end.
    
    if v-t-price ne inv-line.t-price then do:
      create w-tax.
      assign
       w-dsc     = "******ITEM TOTAL:"
       w-tax     = v-t-price
       v-lines   = v-lines + 1.
    end.
    
    v-lines = v-lines + 1.
    
    if line-counter - 1 + v-lines gt page-size + 1 then page.
    
    for each w-lin by w-lin with frame inv-mid1:
      display w-par.
      
      if w-lin eq 1 then
        display oe-ordl.qty        when avail oe-ordl 
                  inv-line.inv-qty when inv-line.ord-no eq 0 @ oe-ordl.qty
                inv-line.po-no
                inv-line.inv-qty
                v-p-c
                inv-line.price
                inv-line.pr-uom
                inv-line.t-price.
                
      down with frame inv-mid1.  
        
      delete w-lin.  
    end.
    
    for each w-tax with frame inv-mid1:
      display fill(" ",15) + w-dsc @ w-par
              w-tax                @ inv-line.t-price.
      
      down with frame inv-mid1.
      
      delete w-tax.
    end.

    put skip(1).
    
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq inv-line.i-no
        no-lock no-error.
    if avail itemfg then
      assign
       v-tot-sqft = v-tot-sqft + (itemfg.t-sqft * inv-line.ship-qty)
       v-tot-wght = v-tot-wght + (itemfg.weight-100 * inv-line.ship-qty / 100).
  end.  
  
  for each inv-misc
      where inv-misc.company eq cocode
        and inv-misc.r-no    eq inv-head.r-no
        and inv-misc.bill    eq "Y"
      no-lock

      break by inv-misc.charge:
      
    assign
     v-lines   = 0
     v-t-price = inv-misc.amt.
    
    if inv-misc.tax and avail stax then
    do i = 1 to 3:
      if stax.tax-code[i] ne "" then do:
        create w-tax.
        assign
         w-dsc      = stax.tax-dscr[i]
         w-tax      = if stax.accum-tax then v-t-price
                      else inv-misc.amt
         w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
         v-t-price  = v-t-price + w-tax
         v-t-tax[i] = v-t-tax[i] + w-tax
         v-lines    = v-lines + 1.
      end.
    end.
    
    if v-t-price ne inv-misc.amt then do:
      create w-tax.
      assign
       w-dsc     = "******ITEM TOTAL:"
       w-tax     = v-t-price
       v-lines   = v-lines + 1.
    end.
    
    v-lines = v-lines + 2.

    if line-counter - 1 + v-lines gt page-size + 1 then page.

    display inv-misc.charge
            inv-misc.dscr
            inv-misc.amt

        with frame inv-mid2.
    down with frame inv-mid2.
    
    for each w-tax with frame inv-mid1:
      display fill(" ",15) + w-dsc @ w-par
              w-tax                @ inv-line.t-price.
      
      down with frame inv-mid1.
      
      delete w-tax.
    end.
    
    put skip(1).
  end. /* each inv-misc */
  
  hide frame inv-bot1.
  view frame inv-bot2.

  do i = 1 to 3:
    v-bot-lab[i] = if v-t-tax[i] ne 0 then
                     ("**TOTAL " +
                      (if avail stax then string(stax.tax-dscr[i],"x(25)")
                       else fill(" ",25)) +
                      fill(" ",19) +
                      string(v-t-tax[i],"->>>>>>9.99")) else "".
  end.
  
  if inv-head.t-inv-weight ne 0 then v-tot-wght = inv-head.t-inv-weight.
  
  assign
   v-tot-inv    = inv-head.t-inv-rev
   v-last-page  = page-number.
end. /* for each inv-head */

PAGE.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
