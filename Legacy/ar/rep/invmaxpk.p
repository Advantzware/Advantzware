/* ---------------------------------------------- ar/rep/invmaxpk.p 10/00 JLF */
/* PRINT maxpk Invoice                                                        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def buffer xar-inv for ar-inv.

{ar/rep/invoice.i}

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

find first ar-inv no-lock no-error.
find first ar-invl no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(5)
       trim(string(ar-inv.inv-no,">>>>>>"))
                                    at 60
       skip(1)
       ar-inv.inv-date              at 60   FORMAT "99/99/99" 
       skip(1)
       ar-invl.po-no                at 60   format "x(15)"
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
       ar-inv.terms-d               at 46
       skip(2)

    with frame inv-top page-top no-box no-underline stream-io width 85.

form oe-ordl.qty                    to 12 format "->,>>>,>>>"
     ar-invl.qty                    to 22 format "->,>>>,>>>"
     ar-invl.i-no                   at 24
     ar-invl.i-name                 at 40 format "x(25)"
     v-inv-price                    to 72 format "->>>9.99<<<"
     ar-invl.amt                    to 82 format "->>>>>9.99"

    with frame inv-mid1 down no-box no-labels stream-io width 85.

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
                        
    v-cust-addr3 = cust.city + ", " +
                   cust.state + "  " +
                   cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    page.
    
    hide frame inv-bot2.
    view frame inv-bot1.
  end.
  
  else
  if first-of(ar-invl.po-no) then page.
  
  v-lines = 2.
  if ar-invl.i-dscr     ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr1 ne "" then v-lines = v-lines + 1.
  if ar-invl.part-dscr2 ne "" then v-lines = v-lines + 1.

  if line-counter - 1 + v-lines gt page-size + 1 then page.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq ar-invl.ord-no
        and oe-ordl.i-no    eq ar-invl.i-no
        and oe-ordl.line    eq ar-invl.line
      no-lock no-error.

  v-inv-price = ar-invl.amt / ar-invl.qty.

  if v-inv-price eq ? then v-inv-price = 0.

  display oe-ordl.qty       when avail oe-ordl
          ar-invl.qty       when not avail oe-ordl  @ oe-ordl.qty
          ar-invl.qty
          ar-invl.i-no              
          ar-invl.i-name
          v-inv-price
          ar-invl.amt

      with frame inv-mid1.
  down with frame inv-mid1.

  do i = 1 to 3:
    v-part-dscr = if i eq 1 then ar-invl.i-dscr
                  else
                  if i eq 2 then ar-invl.part-dscr1
                            else ar-invl.part-dscr2.

    if v-part-dscr ne "" then put v-part-dscr at 40 skip.
  end.

  put skip(1).
  
  if last-of(ar-inv.inv-no) then do:
    hide frame inv-bot1.
    view frame inv-bot2.
    
    assign
     v-bot-lab[1]   = if ar-inv.freight eq 0 then "" else
                      (" Freight: " + string(ar-inv.freight,"->>>>>9.99"))
     v-bot-lab[2]   = if ar-inv.tax-amt eq 0 then "" else
                      ("     Tax: " + string(ar-inv.tax-amt,"->>>>>9.99"))
     v-tot-inv      = ar-inv.due
     ar-inv.printed = yes.
  end.  
end. /* for each ar-inv */

page.

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
