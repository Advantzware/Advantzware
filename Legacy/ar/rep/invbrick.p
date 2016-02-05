/* ---------------------------------------------- ar/rep/invbrick.p 02/00 EKW */
/* PRINT Brick Invoice                                                        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var save_id as recid.

def buffer xar-inv for ar-inv.

def workfile w-sman field sman as char.

{ar/rep/invoice.i}

def var v-inv-no            like ar-inv.inv-no.
def var v-salesman          as   char format "x(3)".
def var v-trailer           as   char format "x(15)".
def var v-fob               as   char format "x(10)".
def var v-bot-lab           as   char format "x(21)" extent 2.
def var v-tot-wt            as   dec  format ">,>>>,>>9.9<".
def var v-tot-sf            as   dec  format ">,>>>,>>9.9<" extent 2.
def var v-tot-inv           as   dec  format "->>>>>>9.99".

def var v-lines             as   int.
def var v-inv-price         as   dec  format "->>>>9.99".
def var v-part-dscr         as   char format "x(18)".        /* ekw02010006 */
def var v-part-comp         as   char format "x".

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

def var v-disc-pct    like ar-inv.disc-%                    no-undo.                        /* ekw */
def var v-tot-tax      like ar-inv.tax-amt                  no-undo.                        /* ekw */
def var v-tot-freight like ar-inv.freight                   no-undo.                        /* ekw */
def var v-tot-disc     like ar-inv.t-disc                   no-undo.                       /* ekw */
def var v-tot-amt        like  ar-invl.amt                  no-undo.                        /* ekw */
def var okbill           like ar-invl.billable              no-undo.                       /* ekw */
def var v-tot-disc-qty like ar-invl.qty                     no-undo.                       /* ekw */
def var v-inv-tax-amt like ar-inv.tax-amt                   no-undo.                       /* ekw */
def var v-ord-no like ar-inv.ord-no no-undo.
def var v-po-no like oe-ord.po-no no-undo.
def var v-t-price as dec format ">>>>9.99" no-undo.
def var v-nt-tot       as dec format "->>>>>>>>>9.99" no-undo.
def var v-tax-total as dec.

find first ar-inv no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

format header
       skip(6)                                         /* ekw02010006 */
       trim(string(ar-inv.inv-no,">>>>>>")) to 74
       skip(6)                                         /* ekw02010006 */
       cust.name                    at 12
       v-ship-name                  at 51   skip(1)      /* ekw02010006 */
       cust.addr[1]                 at 12
       v-ship-addr[1]               at 51
       cust.addr[2]                 at 12
       v-ship-addr[2]               at 51   
       v-cust-addr3                 at 12
       v-ship-addr3                 at 51
       skip(2)                              
       v-po-no                      to 13   format "x(10)"                   
       v-salesman                   at 17                               
       ar-inv.terms-d               at 26   format "x(11)"
       space (10)                                         
       carrier.dscr                 at 48   format "x(6)" when avail carrier 
       v-fob                        at 58   format "x(4)"             
       v-ord-no                     at 64   format ">>>>>>"
       ar-inv.inv-date              to 80   FORMAT "99/99/99" 
       

    with frame inv-top page-top no-box no-underline stream-io width 80. /* ekw02010006 */


form oe-ordl.qty                    to 9 format "->>>9"      
     v-inv-price                    to 68                      
     ar-invl.pr-qty-uom             at 69  format "x(3)"  
     ar-invl.amt                    to 80 format "->>>>9.99" 

    with frame inv-mid1 down no-box no-labels stream-io width 80.

form header
     " "
     skip(2)                                              /* ekw02010006 */

    with frame inv-bot1 no-box no-underline stream-io width 80.
    
form header                   
       "Less "                             at 42
       v-tot-disc                          to 80
       "Freight "                          at 42
       space(11)
       v-tot-freight                       to 80
       "NonTaxable SubTotal"               at 42
        v-nt-tot
                                           to 80 format "->>>>>>>>>9.99" skip
       "Taxable SubTotal   "               at 42
       v-tax-total
                                           to 80 format "->>>>>>>>>9.99" skip
       "Tax                "               at 42
       v-inv-tax-amt                       to 80 format "->>>>>>>>>9.99" skip
       "Total              "               at 42
       v-tot-inv                                 format "->>>>>>>>>9.99" to 80
       skip(7)                                

      with frame inv-bot2 page-bottom no-box no-underline stream-io width 80.
      
def stream last-page.


find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invbrick.txt") page-size VALUE(v-lines-per-page).

view frame inv-top.
view stream last-page frame inv-top.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq ar-inv.cust-no
    no-lock:

   find first oe-ord where oe-ord.company eq ar-inv.company
                             and oe-ord.ord-no     eq ar-inv.ord-no
    no-lock no-error.
    if avail oe-ord then
      v-po-no = oe-ord.po-no.

   v-ord-no = ar-inv.ord-no.
   view frame inv-top.

   assign v-inv-tax-amt = ar-inv.tax-amt.             /* ekw02010006 */
 
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

  assign v-tot-freight = 0.
  
  {ar/rep/invbrick.i "stream last-page" "(last-page)" }  /* ekw02010006 */

  v-page-tot = page-number (last-page) - v-last-page.
  assign v-tot-freight = 0.
  
  {ar/rep/invbrick.i}

  assign
   v-tot-wt       = if ar-inv.t-weight ne 0 then ar-inv.t-weight
                    else v-tot-wt                                      
   v-last-page    = page-number
   ar-inv.printed = yes.
end. /* for each ar-inv */

output stream last-page close.
page.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

