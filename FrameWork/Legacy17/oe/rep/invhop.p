/*
Invoice format for HOP.
Written by Randal Lanning
05/03/1999
*/

{sys/inc/var.i shared}

DEF BUFFER xinv-head FOR inv-head.

{oe/rep/invoice.i}

def var cnt as int no-undo.
def var v-tot-qty as dec no-undo.
def var v-sold-name like cust.name no-undo.
def var v-ship-name like cust.name no-undo.
def var v-sold-addr1 like cust.name no-undo.
def var v-sold-addr2 like cust.name no-undo.
def var v-ship-addr1 like cust.name no-undo.
def var v-ship-addr2 like cust.name no-undo.
def var v-sold-city like cust.name no-undo.
def var v-ship-city like cust.name no-undo.
def var v-cust-phone like cust.phone no-undo.
def var v-invoice-no like inv-head.inv-no no-undo.
def var v-fob as char format "x(11)" no-undo.
def var v-ship-date as date format "99/99/9999" no-undo init today.
def var v-invoice-date as date format "99/99/9999" no-undo.
def var v-bol-no as int format ">>>>>>" no-undo.
def var v-order-no as int format ">>>>>>" no-undo.
def var v-unit-price as dec format "$>>,>>9.99" no-undo.
def var v-terms as char format "x(20)" no-undo.
def var v-salesman as char format "x(11)" no-undo.
def var v-cust-part like inv-line.part-no no-undo.
def var v-order-qty as dec format "->,>>>,>>9" no-undo.
def var v-ship-qty as dec format "->,>>>,>>9" no-undo.
def var v-weight as dec no-undo.
def var v-complete as char no-undo.
def var v-order-number like oe-ord.ord-no no-undo.
def var v-po-number like oe-ord.po-no no-undo.
def var v-item-number like inv-line.i-no no-undo.
def var v-ship-via like carrier.dscr no-undo.
def var time_stamp as char format "x(8)" no-undo.
def var v-price as dec format "$->>,>>9.99" no-undo.
def var v-item-size as char no-undo format "x(20)".
def var v-backorder as dec format "->,>>>,>>9" no-undo.
def var save_id as recid no-undo.
def var v-inv-tot as int no-undo.
def var v-freight like inv-head.t-inv-cost no-undo.
def var v-tax like inv-head.t-inv-cost no-undo.
def var v-net-amount like inv-head.t-inv-cost no-undo.
def var v-invoice-amount like inv-head.t-inv-cost no-undo.
def var v-discount as char format "x(24)" no-undo.
def var v-if-paid as char format "x(22)" no-undo.
def var v-if-paid-amt like inv-head.t-inv-cost no-undo.
def var v-misc-charges like inv-head.t-inv-cost no-undo.
def var v-billinst as char extent 2 no-undo.
def var v-lines as int.
def var v-job-no as char format "x(9)" no-undo.
def var v-frt-des          as char format "x(14)"    no-undo.
def var v-slsmn-name   as char format "x(20)"   no-undo.                       

def workfile w-sman field sman as char.

DEF TEMP-TABLE tt-line NO-UNDO FIELD tt-i-name LIKE inv-line.i-name.

def stream last-page.

FORM header
     skip(1)
     v-sold-name at 4
     v-ship-name at 37
     v-fob at 69
     v-frt-des at 85                                                     
     v-invoice-no at 101
     v-bol-no at 117
     v-sold-addr1 at 4
     v-ship-addr1 at 37
     v-sold-addr2 at 4
     v-ship-addr2 at 37
     v-terms at 69
     v-invoice-date at 101
     v-ship-date at 117
     v-sold-city at 4
     v-ship-city at 37
     v-cust-phone at 4
     v-ship-via at 69 
     v-slsmn-name at 101   format "x(15)"
     trim(string(page-number - v-last-page, ">9")) + " OF " +
     trim(string(v-page-tot,">9")) at 117 format "x(8)"
     skip(4)
     
    with frame inv-top page-top no-box no-underline stream-io width 132.

form v-cust-part at 1
     tt-i-name at 33
     v-order-qty at 70
     v-ship-qty at 90
     v-unit-price at 105
     v-price at 117
    with frame detail no-labels no-box no-underline down stream-io width 132.

form v-item-number at 1
     v-po-number
     tt-i-name at 33
     v-backorder at 70
     v-job-no at 91
    with frame detail2 no-labels no-box no-underline down stream-io width 132.

form inv-misc.charge at 1  format "x(5)"
     inv-misc.dscr at 33   format "x(36)"
     inv-misc.tax at 70
     inv-misc.amt at 117 format "$->>,>>9.99"
    with frame detailm no-labels no-box no-underline down stream-io width 132.

form header
     " "
     skip(4)
    with frame inv-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     skip(1)
     "Freight:" at 90 v-freight at 113
     "Tax:" at 90 v-tax at 113
     v-discount
     v-if-paid at 33
     "Net Amount:" at 90 v-invoice-amount at 113
    with frame inv-bot2 page-bottom stream-io width 132 no-box no-labels.


find first company where company.company eq cocode no-lock no-error.
find oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

output stream last-page to value("invhop.txt") page-size value(v-lines-per-page).

view frame inv-top.
view stream last-page frame inv-top.

for each report WHERE report.term-id EQ v-term-id NO-LOCK,

    FIRST xinv-head WHERE RECID(xinv-head) EQ report.rec-id NO-LOCK,
        
    first cust
    where cust.company eq cocode
      and cust.cust-no eq xinv-head.cust-no
    NO-LOCK

    break by report.key-01
          by report.key-02:

  IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN DO TRANSACTION:
    RUN oe/get-inv#.p (ROWID(xinv-head)).
  END.

  FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head) NO-LOCK.

  v-cust-phone = cust.area-code + cust.phone.

  find first terms
      where terms.company eq cocode
        and terms.t-code  eq inv-head.terms
      no-lock no-error.
  
  v-salesman = "".
  for each inv-line where inv-line.r-no eq inv-head.r-no no-lock:
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

  for each w-sman:
    if length(trim(v-salesman) + trim(w-sman.sman) + ",") le 11 then
      v-salesman = trim(v-salesman) + trim(w-sman.sman) + ",".
    delete w-sman.
  end.

  if v-salesman ne "" then
    if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
      substr(v-salesman,length(trim(v-salesman)),1) = "".

  assign
   v-fob = if inv-head.fob = "ORIG" then "Origin" else "Destination"
   v-terms = inv-head.terms-d
   v-invoice-date = inv-head.inv-date
   v-bol-no = inv-head.bol-no
   v-invoice-no = inv-head.inv-no
   v-sold-name = inv-head.cust-name
   v-sold-addr1 = inv-head.addr[1]
   v-sold-addr2 = inv-head.addr[2]
   v-sold-city = inv-head.city + " " + inv-head.state + " " + inv-head.zip
   v-ship-name = inv-head.sold-name
   v-ship-addr1 = inv-head.sold-addr[1]
   v-ship-addr2 = inv-head.sold-addr[2]
   v-ship-city = inv-head.sold-city + " " +
                 inv-head.sold-state + " " + inv-head.sold-zip
   v-ship-date = inv-head.inv-date.

  find first carrier
      where carrier.company = inv-head.company
        and carrier.carrier = inv-head.carrier
      no-lock no-error.
  v-ship-via = if avail carrier then carrier.dscr else "".
  
  RELEASE oe-bolh.
  IF inv-head.bol-no NE 0 THEN
  find first oe-bolh
      where oe-bolh.company eq inv-head.company
        and oe-bolh.bol-no  eq inv-head.bol-no
      no-lock no-error.

  if avail oe-bolh then do:
    v-ship-date = oe-bolh.bol-date.
    find first oe-relh
        where oe-relh.company eq oe-bolh.company
          and oe-relh.r-no    eq oe-bolh.r-no
        no-lock no-error.
    if avail oe-relh and v-ship-name eq "" then do:
      find first shipto
          where shipto.company eq oe-bolh.company
            and shipto.cust-no eq oe-relh.cust-no
            and shipto.ship-id = oe-bolh.ship-id
          no-lock no-error.

      if avail shipto then
        assign
         v-ship-name = shipto.ship-name
         v-ship-addr1 = shipto.ship-addr[1]
         v-ship-addr2 = shipto.ship-addr[2]
         v-ship-city = shipto.ship-city + " " + shipto.ship-state + " " +
                       shipto.ship-zip.
    end.
  end. /* find oe-bolh */
  
  find first inv-line
      where inv-line.r-no eq inv-head.r-no
      no-lock no-error.
  if avail inv-line then do transaction:
    v-order-number = inv-line.ord-no.
    v-slsmn-name   = inv-line.sname[1].                                        
    find first oe-ord
        where oe-ord.company eq inv-line.company
          and oe-ord.ord-no  eq inv-line.ord-no
        no-lock no-error.
    v-po-number = if avail oe-ord then oe-ord.po-no else "".
    if avail oe-ord then 
       if inv-head.frt-pay = "P" then
          v-frt-des = "Prepaid".
       else
       if inv-head.frt-pay = "C" then
          v-frt-des = "Collect".
       else
       if inv-head.frt-pay = "B" then
          v-frt-des = "PREPAY and ADD".
       else
       if inv-head.frt-pay = "T" then
          v-frt-des = "Third Party".
       /* if oe-ord.frt-pay = "P" then
          v-frt-des = "Prepaid".
       else
       if oe-ord.frt-pay = "C" then
          v-frt-des = "Collect".
       else
       if oe-ord.frt-pay = "B" then
          v-frt-des = "PREPAY and ADD".
       else
       if oe-ord.frt-pay = "T" then
          v-frt-des = "Third Party".
       else
       oe-ord.frt-pay = "". */
    /* end. /* if avail oe-ord */ */
  end. /* if avail inv-line */

  do cnt = 1 to prt-copies:
    {oe/rep/invhop.i "stream last-page" "(last-page)"}

    v-page-tot = page-number(last-page) - v-last-page.

    {oe/rep/invhop.i}

    assign
     v-discount = ""
     v-if-paid  = "".

    if avail terms and (inv-head.t-inv-rev * terms.disc-rate / 100 gt 0) then
      assign
       v-discount = "Discount: " + string(inv-head.t-inv-rev *
                                          terms.disc-rate / 100,"->,>>>,>>9.99")
       v-if-paid  = "If paid by: " +
                    string(terms.disc-days + inv-head.inv-date,"99/99/9999").

    assign
     v-freight        = inv-head.t-inv-freight
     v-tax            = inv-head.t-inv-tax
     v-invoice-amount = inv-head.t-inv-rev
     v-last-page      = page-number.

    if not inv-head.f-bill then v-freight = 0.
  end. /* do cnt */
end. /* Each report */

PAGE.

output stream last-page close.



