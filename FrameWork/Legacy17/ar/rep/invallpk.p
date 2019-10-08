/* --------------------------------------------- ar/rep/invallpk.p  03/00 EKW */
/* PRINT ALL PACKAGING INVOICE - A/R MODULE */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

def stream mess.
 
def var save_id as recid.

def var time_stamp as ch.
time_stamp = string(time, "hh:mmam").

{ar/rep/invoice.i }

def var v-salesman                                     as char format "x(14)".
def var v-fob                                             as char format "x(27)".
def var v-shipvia                                        like carrier.dscr.
def var v-addr3                                          as char format "x(30)".
def var v-sold-addr3                                   as char format "x(30)".
def var v-shipto-no                                     like inv-head.sold-no.
def var v-shipto-name                                 as char format "x(30)".
def var v-shipto-addr                                  as char format "x(30)" extent 2.
def var v-shipto-city                                   as char format "x(15)".
def var v-shipto-state                                 as char format "x(2)".
def var v-shipto-zip                                    as char format "x(10)".
def var v-line                                            as int.
def var v-printline                                      as int.
def var v-invhead                                      as char format "x(13)" init "I N V O I C E".                                                        
def var v-inv-no                                        as int.
def var v-tot-qty                                         as int.
def var v-inv-date                                       as date init today.
def var v-date-ship                                     as date init today.
def var v-net                                               like ar-invl.amt.
def var cnt                                                 as int.
def var v-part-info                                       as char format "x(30)".
def var v                                                    as int.
def var v-ord#                                            as int format ">>>>>>" no-undo.

def var v-c-name         like company.name.
def var v-c-addr         like company.addr.
def var v-c-city           like company.city.
def var v-c-state        like company.state.
def var v-c-zip           like company.zip.

def var v-c-phone      as char format "x(12)".
def var v-c-fax           as char format "x(12)".
def var v-uline           as char format "x(80)". 
def var v-tot-merch     like ar-invl.amt.
def var v-tax-gr          like inv-head.tax-gr.
def var v-inv-price      as dec  format "->>>>9.99".
def var v-tot-freight     like ar-inv.freight.
def var v-tot-tax          like ar-inv.tax-amt.
def var v-inv-tax-amt   like ar-inv.tax-amt                   no-undo.
def var v-bol-no          like oe-bolh.bol-no.
 
def buffer xar-inv for ar-inv.

def workfile w-sman field sman as char format "x(4)".

find first ar-inv  no-lock no-error.
find first carrier no-lock no-error.
find first cust     no-lock no-error.

assign v-uline = fill("-",80).

form header
  "I N V O I C E"        at 12  skip(1)
  v-c-name                 at 12 
  "Invoice #"             at 47       
  ar-inv.inv-no            at 57
  v-c-addr[1]              at 12 skip
  v-c-addr[2]              at 12
  "Date:"                   at 47 
  v-inv-date                at 57 FORMAT "99/99/99" skip
  v-c-city                    at 12 ", "
  v-c-state
  v-c-zip skip
  "Phone: "                at 12
  v-c-phone
  "Salesperson "         at 47 
  v-salesman  skip
  "Fax: "                    at 12
  v-c-fax                    skip(2)
   "Bill To:"                at 3
  ar-inv.cust-no          at 21 
   "Ship To:"              at 38
   v-shipto-no             at 57 skip
   cust.name              at 12 v-shipto-name    at 47 skip
   cust.addr[1]            at 12 v-shipto-addr[1] at 47 skip
   cust.addr[2]            at 12 v-shipto-addr[2] at 47 skip
   v-addr3                   at 12 v-sold-addr3      at 47 skip(1)
    "Terms:   "             at 1 ar-inv.terms-d format "x(15)"
    "Order #  "             at 29 ar-inv.ord-no 
    "B O L #  "             at 48 v-bol-no  skip(1)
    "Ship Date"            at 1 
    "Ship Via"              at 21
    "F O B"                  at 47
    "---------"                 at 1
    "--------------------"      at 21
    "-----------"               at 47 skip
    v-date-ship             at 1 FORMAT "99/99/99" 
    v-shipvia                at 21   format "x(20)"
    v-fob                      at 47  format "x(11)"skip(2)
    "Item"                      at 11
    "Qty Inv'd"                at 37 skip
    "Cust P O  Description"   at 1
    "Qty Shipped"            at 37
    "Price     Amount"       at 58 skip
    v-uline                        at 1  skip
with frame invhead-allpkg page-top no-labels no-box no-underline stream-io width 90.

form
  ar-invl.po-no           at 1   format "x(9)"
  ar-invl.i-no             at 11
  ar-invl.inv-qty          to 46 format "->,>>>,>>9"
  v-inv-price             to 62 format "->>,>>9.99" space(0)
  ar-invl.pr-qty-uom    at 63 format "x(3)"
  ar-invl.amt              at 66 format "->>>,>>9.99"
  ar-invl.i-name          at 11 format "x(20)"
  ar-invl.ship-qty        to 46 format "->,>>>,>>9" skip
with frame allpkg-detail no-labels no-box no-underline down stream-io width 90.

form skip (3)
    "Merchandise Total"    to 62  v-tot-merch at 63 skip(1)  
    "Freight" at 3 "Tax" at 18
    "State" at 28 /* "Finance Chg" at 37 */ "Invoice" at 55
    v-tot-freight format "->,>>9.99" at 1
    v-tot-tax format "->,>>9.99" at 13
    v-tax-gr at 30  /* v-fin-chrg at 37 */ "Total"  at 55
    v-net  format "->,>>>,>>9.99" at 64
with frame allpkg-totals no-labels no-box no-underline stream-io width 90.

form " " to 80
     with frame blankl no-labels no-box no-underline stream-io width 90.

find first company where company.company eq cocode no-lock no-error.
 
find first cust where cust.company eq cocode and
                     active eq "X" no-lock no-error.
if avail cust then do:
    v-c-phone = substring(cust.area-code,1,3) + "-" + substring(cust.phone,1,3) + "-" +  substring(cust.phone,4,4).
    v-c-fax     =  substring(cust.fax,1,3) + "-" + substring(cust.fax,4,3) + "-" +  substring(cust.fax,7,4).
end.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

if oe-ctrl.prcom then
  assign
   v-c-name    = company.name
   v-c-addr[1] = company.addr[1]
   v-c-addr[2] = company.addr[2]
   v-c-city    = company.city
   v-c-state   = company.state
   v-c-zip     = company.zip.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq ar-inv.cust-no
    no-lock
         break by ar-inv.inv-no:                                      
         
     assign
      v-inv-date    = ar-inv.inv-date
      v-inv-tax-amt = ar-inv.tax-amt.             /* ekw02010006 */
      
      if ar-inv.fob-code begins "ORIG" then
         assign v-fob = "Origin".
      else
         assign v-fob = "Destination".
         
  find first oe-bolh where oe-bolh.ord-no = ar-inv.ord-no no-lock no-error.
  if avail oe-bolh then
     v-bol-no = oe-bolh.bol-no.
  else
     v-bol-no = 0.
       
  find first carrier
      where carrier.company eq cocode
        and carrier.carrier eq ar-inv.carrier
      no-lock no-error.
      
   if avail carrier then
      v-shipvia = carrier.dscr.
   else
      assign v-shipvia = "".

  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
        and shipto.ship-id eq ar-inv.ship-id
      no-lock no-error.

  if available shipto then
  assign
    v-shipto-no       = shipto.ship-id
   v-shipto-name    = shipto.ship-name
   v-shipto-addr[1] = shipto.ship-addr[1]
   v-shipto-addr[2] = shipto.ship-addr[2]
   v-shipto-city    = shipto.ship-city
   v-shipto-state   = shipto.ship-state
   v-shipto-zip     = shipto.ship-zip
   v-sold-addr3   = v-shipto-city + ", " +
                    v-shipto-state + "  " +
                    v-shipto-zip
   v-addr3   = cust.city + ", " +
                    cust.state + "  " +
                    cust.zip.

  if trim(v-sold-addr3) eq "," then v-sold-addr3 = "".
  if trim(v-addr3) eq "," then v-addr3 = "".

  if first-of(ar-inv.inv-no) and v-print-head then do:
    view frame invhead-allpkg.  /* Print headers */
    page.
    assign v-printline = 0.
  end. /* if v-print-head */
 
  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no and not ar-invl.misc
      no-lock:
     
    do i = 1 to 3:
      if ar-invl.sman[i] ne "" then do:
        find first w-sman where w-sman.sman eq ar-invl.sman[i] no-error.
        if not avail w-sman then do:
          create w-sman.
          w-sman.sman = ar-invl.sman[i].
        end. /* if not avail w-sman */
      end.    /* if ar-invl.sman[i] ne "" */
    end.      /* do i = 1 to 3 */

  v-salesman = "".
  for each w-sman:
    if length(trim(v-salesman) + " " + trim(w-sman.sman) + ",") le 15 then
      v-salesman = trim(v-salesman) + " " + trim(w-sman.sman) + ",".
    delete w-sman.
  end. /* for each w-sman */

  v-salesman = trim(v-salesman).

  if v-salesman ne "" then
    if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
      substr(v-salesman,length(trim(v-salesman)),1) = "".
  
     find first oe-ordl where oe-ordl.company eq ar-invl.company and
                                       oe-ordl.ord-no eq ar-invl.ord-no and
                                       oe-ordl.line eq ar-invl.line and
                                       oe-ordl.i-no eq ar-invl.i-no
                                       no-lock no-error.
                                       
       display ar-invl.po-no
                trim(string(oe-ordl.ord-no,">>>>>9"))
                                when avail oe-ordl @ ar-invl.i-no
               ar-invl.i-no     when not avail oe-ordl
               ar-invl.inv-qty
               ar-invl.qty      when ar-invl.inv-qty eq 0 @ ar-invl.inv-qty
               ar-invl.unit-pr @ v-inv-price
               ar-invl.pr-qty-uom
               ar-invl.amt
               ar-invl.i-name
               ar-invl.ship-qty
         with frame allpkg-detail.
 
    v-tot-merch = v-tot-merch + ar-invl.amt.
         
     assign v-printline = v-printline + 2.
     
     if v-printline ge 25 then do:
        put skip(27 - v-printline) "* CONTINUED *" at 68.
        assign v-printline = 0.
        page.
     end.
     else
       down with frame allpkg-detail. 

  end.       /*  for each ar-invl and not misc*/                                                                                                  
  
  assign v-line = 1.
  
  for each ar-invl
      where ar-invl.x-no eq ar-inv.x-no and ar-invl.misc
      no-lock:
      
      if v-line = 1 then do:
         put "** Miscellaneous Items **" at 27 skip(1).
         assign 
          v-line = v-line + 1
         v-printline = v-printline + 2.
      end.
      
      find first oe-ordl where oe-ordl.company eq ar-invl.company and
                                       oe-ordl.ord-no eq ar-invl.ord-no and
                                       oe-ordl.line eq ar-invl.line and
                                       oe-ordl.i-no eq ar-invl.i-no
                                       no-lock no-error.
                                       
       display ar-invl.po-no
                trim(string(oe-ordl.ord-no,">>>>>9"))
                                when avail oe-ordl @ ar-invl.i-no
               ar-invl.i-no     when not avail oe-ordl 
               ar-invl.inv-qty
               ar-invl.qty      when ar-invl.inv-qty eq 0 @ ar-invl.inv-qty
               ar-invl.unit-pr @ v-inv-price
               ar-invl.pr-qty-uom
               ar-invl.amt
               ar-invl.i-name
               ar-invl.ship-qty
         with frame allpkg-detail.
     
         assign v-tot-merch = v-tot-merch + ar-invl.amt.
         assign v-printline = v-printline + 2.
         
         if v-printline ge 25 then do:
           put skip(27 - v-printline) "* CONTINUED *" at 68.
           assign v-printline = 0.
           page.
         end.
         else
           down with frame allpkg-detail.
       
   end. /* each ar-invl and misc */
                
  find first oe-ord where oe-ord.company = cocode and
                          oe-ord.ord-no  = ar-inv.ord-no no-lock no-error.
  if avail oe-ord then v-tax-gr = oe-ord.tax-gr.
 
 v-tot-tax      = v-tot-tax     + ar-inv.tax-amt.
 v-tot-freight = v-tot-freight + ar-inv.freight.
 
  v-net = v-net + v-tot-freight + v-tot-tax + v-tot-merch.
  
  put skip(27 - v-printline).
            
  display 
          v-tot-merch 
          v-tot-freight 
          v-tot-tax
          v-tax-gr
          v-net
     with frame allpkg-totals.
    
    assign v-tot-merch = 0
              v-tot-freight = 0
              v-tot-tax      = 0
              v-line          = 0
              v-printline    = 0
              v-net          = 0.
   ar-inv.printed = yes.
              
end. /* for each ar-inv */                                     

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */

