/* ---------------------------------------------- ar/rep/invchill.p 10/01 JLF */
/* PRINT Chillicothe Invoice                                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def buffer xar-invl for ar-invl.

{ar/rep/invoice.i}

def var v-inv-no     like ar-inv.inv-no.
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

find first ar-inv no-lock no-error.
find first ar-invl no-lock no-error.
find first cust no-lock no-error.

format header
       skip(8)
       v-ship-name          format "x(35)"      at 10
       v-ship-addr[1]       format "x(35)"      at 10
       v-ship-addr[2]       format "x(35)"      at 10
       v-ship-addr3         format "x(35)"      at 10
       skip(1)
       ar-inv.cust-no       format "x(8)"       at 72
       trim(string(ar-inv.inv-no,">>>>>>"))     at 70
       ar-invl.po-no                            at 73
       cust.name            format "x(35)"      at 10
       v-sman-name          format "x(23)"      at 65
       cust.addr[1]         format "x(35)"      at 10
       cust.addr[2]         format "x(35)"      at 10
       ar-inv.inv-date      format "99/99/99"   at 63
       v-cust-addr3         format "x(35)"      at 10
       trim(string(page-number - v-last-page,">>>>>>"))
                                                at 63
       ar-inv.terms-d       format "x(23)"      at 64
       skip(4)

    with frame inv-top page-top no-box no-underline stream-io width 87.

form ar-invl.inv-qty       format "->>>>>>>"       to 10
     ar-invl.i-no                                  at 12
     ar-invl.i-name                                at 28
     ar-invl.unit-pr       format ">>>>>9.99"      to 73
     ar-invl.amt           format "->>>>>>9.99"    to 85

    with frame inv-mid1 down no-box no-labels stream-io width 87.

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

view frame inv-top.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq ar-inv.cust-no
    no-lock,
    
    each ar-invl where ar-invl.x-no eq ar-inv.x-no no-lock
    
    break by ar-inv.cust-no
          by ar-inv.inv-no
          by ar-invl.po-no
          by ar-invl.i-no
          by ar-invl.line:

  if first-of(ar-inv.inv-no) then do:
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
       v-ship-zip     = shipto.ship-zip.
    
    assign
     v-ship-addr3   = v-ship-city + ", " +
                      v-ship-state + "  " +
                      v-ship-zip
     v-cust-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".
    
    v-sman-no = "".
    
    for each xar-invl
        where xar-invl.x-no eq ar-inv.x-no
        by xar-invl.i-no:
        
      if v-sman-no eq "" then
      do i = 1 to 3:
        if xar-invl.sman[i] ne "" then do:
          v-sman-no = xar-invl.sman[i].
          leave.
        end.  
      end.
    end.
    
    v-sman-name = v-sman-no.
  
    if v-sman-no ne "" then
    find first sman
        where sman.company eq cocode
          and sman.sman    eq v-sman-no
        no-lock no-error.
    if avail sman then v-sman-name = sman.sname.

    page.
    
    hide frame inv-bot2.
    view frame inv-bot1.
  end.
  
  else
  if first-of(ar-invl.po-no) then page.
  
  display ar-invl.inv-qty
          ar-invl.i-no
          ar-invl.i-name
          ar-invl.unit-pr
          ar-invl.amt
          
      with frame inv-mid1.
  down with frame inv-mid1.
                
  if last-of(ar-inv.inv-no) then do:
    hide frame inv-bot1.
    view frame inv-bot2.
    
    assign
     v-bot-lab[1] = if ar-inv.freight eq 0 then "" else
                      (" FREIGHT: " + string(ar-inv.freight,"->>>>>>9.99"))
     v-bot-lab[2] = if ar-inv.tax-amt eq 0 then "" else
                      (" TAXABLE: " + string(ar-inv.tax-amt,"->>>>>>9.99"))

     v-tot-inv    = ar-inv.due
     v-tot-disc   = 0
     v-dsc-date   = ?
     v-due-date   = ar-inv.due-date
     v-last-page  = page-number.
     
    if ar-inv.disc-% gt 0 and ar-inv.net gt 0 then do:
      v-tot-disc = round(ar-inv.net * (ar-inv.disc-% / 100),2).
      if v-tot-disc ne 0 then v-dsc-date = ar-inv.inv-date + ar-inv.disc-days.
    end.
  end.
  
  ar-inv.printed = yes.
end. /* for each ar-inv */

hide frame inv-top no-pause.
page.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
