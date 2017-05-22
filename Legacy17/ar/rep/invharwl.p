/* ---------------------------------------------- ar/rep/invharwl.p 11/00 JLF */
/* PRINT Harwell Invoice                                                      */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

{ar/rep/invoice.i}

def var v-bot-lab    as   char format "x(63)" extent 3.
def var v-tot-sqft   as   dec  format "->>>>>>>9.99".
def var v-tot-wght   as   dec  format "->>>>>>>9.99".
def var v-tot-inv    as   dec  format "->>>>>>9.99".

def var v-addr-info  as   char.
def var v-cust       like inv-head.addr extent 4.
def var v-ship       like v-cust.
def var v-lines      as   int.

def var v-bol-no     like ar-invl.bol-no.
def var v-sman-no    as   char.
def var v-fob        as   char.
def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 like shipto.ship-name.
def var v-cust-addr3 like shipto.ship-name.
def var v-t-price    like inv-line.t-price.
def var v-t-tax      as   dec extent 3.

def workfile w-lin
    field w-par like ar-invl.i-name init ""
    field w-lin as   int.
    
def workfile w-tax
    field w-dsc as   char
    field w-tax as   dec.

find first ar-inv no-lock no-error.
find first carrier no-lock no-error.
       
format header
       skip(5)
       trim(string(ar-inv.inv-no,">>>>>>"))     at 70
       skip(1)
       ar-inv.inv-date      format "99/99/99"   at 70
       skip(1)
       string(trim(string(page-number - v-last-page,">>>>>>")))
                                                at 70
       skip(1)                                         
       trim(string(v-bol-no,">>>>>>"))          at 70
       skip(1)
       ar-inv.cust-no       format "x(8)"       at 10
       v-cust[1]            format "x(41)"      at 10
       v-ship[1]            format "x(33)"      at 52
       v-cust[2]            format "x(41)"      at 10
       v-ship[2]            format "x(33)"      at 52
       v-cust[3]            format "x(41)"      at 10
       v-ship[3]            format "x(33)"      at 52
       v-cust[4]            format "x(41)"      at 10
       v-ship[4]            format "x(33)"      at 52
       skip(4)
       ar-inv.terms-d       format "x(14)"      at 2
       v-sman-no            format "x(25)"      at 17
       carrier.dscr         format "x(15)"      at 43   when avail carrier
       v-fob                format "x(11)"      at 75
       skip(3)

    with frame inv-top page-top no-box no-underline stream-io width 85.
    
form oe-ordl.qty            format "->>>>"          to 6
     w-par                  format "x(32)"          at 8
     ar-invl.po-no          format "x(14)"          at 41
     ar-invl.inv-qty        format "->>>>>>"        to 62
     ar-invl.p-c            format "C/P"            at 64
     ar-invl.unit-pr        format "->>>9.99"       to 73
     ar-invl.pr-uom         format "x"              at 74
     ar-invl.amt            format "->>>>>>9.99"    to 85

    with frame inv-mid1 down no-box no-labels stream-io width 85.

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

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq ar-inv.cust-no
    no-lock
    
    break by ar-inv.cust-no
          by ar-inv.inv-no:
          
  assign
   v-ship-name    = ar-inv.sold-name
   v-ship-addr[1] = ar-inv.sold-addr[1]
   v-ship-addr[2] = ar-inv.sold-addr[2]
   v-ship-city    = ar-inv.sold-city
   v-ship-state   = ar-inv.sold-state
   v-ship-zip     = ar-inv.sold-zip.        

  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq ar-inv.carrier
      no-lock no-error.

  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
        and shipto.ship-id eq ar-inv.ship-id
      no-lock no-error.

  if avail shipto then
    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-city    = shipto.ship-city
     v-ship-state   = shipto.ship-state
     v-ship-zip     = shipto.ship-zip
     v-ship-addr3   = v-ship-city + ", " +
                      v-ship-state + "  " +
                      v-ship-zip.
                      
  assign
   v-bol-no  = 0
   v-sman-no = "".
    
  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      by ar-invl.i-no:
      
    if v-bol-no eq 0 then v-bol-no = ar-invl.bol-no.  
    
    if v-sman-no eq "" then
    do i = 1 to 3:
      if ar-invl.sman[i] ne "" then do:
        find first sman
            where sman.company eq cocode
              and sman.sman    eq ar-invl.sman[i]
            no-lock no-error.
        v-sman-no = if avail sman then sman.sname else ar-invl.sman[i].
        
        leave.
      end.  
    end.
  end.

  assign
   v-fob        = if ar-inv.fob-code begins "ORIG" 
                  then "ORIGIN" else "DESTINATION"
   v-cust-addr3 = cust.city + ", " +
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
        and stax.tax-group eq ar-inv.tax-code
      no-lock no-error.
  
  assign
   v-t-tax    = 0
   v-tot-sqft = 0
   v-tot-wght = 0. 
  
  page.

  hide frame inv-bot2.
  view frame inv-bot1.
  
  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no
      by ar-invl.misc
      by ar-invl.i-no:
      
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq ar-invl.ord-no
          and oe-ordl.i-no    eq ar-invl.i-no
          and oe-ordl.line    eq ar-invl.line
        no-lock no-error.  

    v-lines = 0.
    
    if ar-invl.i-no ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.i-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    /* if ar-invl.part-no ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.part-no
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end. */
        
    if ar-invl.i-name ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.i-name
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
        
    if ar-invl.i-dscr ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.i-dscr
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    if ar-invl.part-dscr1 ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.part-dscr1
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    if ar-invl.part-dscr2 ne "" then do:
      create w-lin.
      assign
       w-par   = ar-invl.part-dscr2
       v-lines = v-lines + 1
       w-lin   = v-lines.
    end.
    
    v-t-price = ar-invl.amt.
    
    if ar-invl.tax and avail stax then
    do i = 1 to 3:
      if stax.tax-code[i] ne "" then do:
        create w-tax.
        assign
         w-dsc      = stax.tax-dscr[i]
         w-tax      = if stax.accum-tax then v-t-price
                      else ar-invl.amt
         w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
         v-t-price  = v-t-price + w-tax
         v-t-tax[i] = v-t-tax[i] + w-tax
         v-lines    = v-lines + 1.
      end.
    end.
    
    if v-t-price ne ar-invl.amt then do:
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
        display oe-ordl.qty       when avail oe-ordl
                  ar-invl.inv-qty when ar-invl.ord-no eq 0 @ oe-ordl.qty
                ar-invl.po-no
                ar-invl.inv-qty
                ar-invl.p-c
                ar-invl.unit-pr
                ar-invl.pr-uom
                ar-invl.amt.
                
      down with frame inv-mid1.
      
      delete w-lin.
    end.
    
    for each w-tax with frame inv-mid1:
      display fill(" ",15) + w-dsc @ w-par
              w-tax                @ ar-invl.amt.
      
      down with frame inv-mid1.
      
      delete w-tax.
    end.

    put skip(1).
    
    find first itemfg
        where itemfg.company eq cocode
          and itemfg.i-no    eq ar-invl.i-no
        no-lock no-error.
        
    v-tot-sqft = v-tot-sqft +
                 ((if avail itemfg then itemfg.t-sqft
                   else ar-invl.sf-sht) * ar-invl.qty).
    
    if avail itemfg then
      v-tot-wght = v-tot-wght + (itemfg.weight-100 * ar-invl.ship-qty / 100).
  end.
  
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
  
  if ar-inv.t-weight ne 0 then v-tot-wght = ar-inv.t-weight.
  
  assign
   v-tot-inv      = ar-inv.due
   v-last-page    = page-number
   ar-inv.printed = yes.
end. /* for each ar-inv */

hide frame inv-top no-pause.
page.

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
