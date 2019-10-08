
{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer b-itemfg     for itemfg.

{oe/rep/oe-lad.i}

def var v-frt-pay-dscr      as   char.
def var v-due-date          like oe-ordl.prom-date.
def var v-sname             like oe-ord.sname.
def var v-lines             as   int.

def var v-part-dscr         as   char.
def var v-job-po            as   char.
def var v-part-qty          as   dec.

def var v-tot-cases         like oe-bolh.tot-pallets.
def var v-tot-wt            like oe-bolh.tot-wt.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".

def workfile w2 no-undo
    field cases            as   int format ">>9"
    field cas-cnt          as   int format ">>>>>9".

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(1)
       "B I L L   O F   L A D I N G"    to 80
       skip(4)                     
       "SHIP TO:"                       at 41
       cust.name                        at 6
       v-ship-name                      at 46
       cust.addr[1]                     at 6
       v-ship-addr[1]                   at 46
       cust.addr[2]                     at 6
       v-ship-addr[2]                   at 46
       v-cust-addr3                     at 6
       v-ship-addr3                     at 46
       skip(1)
       "DUE DATE"                       at 35
       v-due-date             
       "BOL DATE"                       at 59
       oe-bolh.bol-date                                                 skip
       "BOL#"
       oe-bolh.bol-no                           format "999999"
       "CARRIER"                        at 35    
       carrier.dscr                             format "x(15)"
       v-frt-pay-dscr                   at 59
       "PAGE"                           
       page-number - v-last-page                format "99"
       "OF"                                  
       v-page-tot                               format "99"
       fill("-",80)                             format "x(80)"
       "QTY ORDERED"                        
       "P.O.#"                          at 17
       "PER"                            to 71
       "P"                              at 80    
       "ITEM NUMBER"
       "JOB#"                           at 17
       "PRODUCT DESCRIPTION"            at 33
       "UNIT"                           to 64
       "UNIT"                           to 71
       "TOTAL"                          to 78
       "C"                              at 80
       "---------------"
       "---------------"                at 17
       "--------------------------"     at 33
       "----"                           to 64
       "------"                         to 71
       "------"                         to 78
       "-"                              at 80    

    with frame bol-top page-top no-box no-underline stream-io width 85.

form oe-ordl.i-no                         format "x(15)"
     v-job-po                       at 17 format "x(15)"
     v-part-dscr                    at 33 format "x(27)"
     w2.cases                       to 64 format ">>>9"
     w2.cas-cnt                     to 71 format ">>>>>9"
     oe-boll.qty                    to 78 format ">>>>>9"
     oe-boll.p-c                    at 80

    with frame bol-mid down no-box no-labels stream-io width 85.

form header
     " "
     skip(4)
     "Received by:"
     fill("_",44) format "x(44)"
     "Date:"
     fill("_",16) format "x(16)"
     skip(1)
     
    with frame bol-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     skip(1)
     "Total Cases:"               
     v-tot-cases                    to 22   format ">,>>>,>>9"
     "Total Weight:"                to 69
     v-tot-wt                       to 80   format ">>,>>>,>>9"
     skip(2)
     "Received by:"
     fill("_",44) format "x(44)"
     "Date:"
     fill("_",16) format "x(16)"
     skip(1)

    with frame bol-bot2 page-bottom no-box no-underline stream-io width 85.

def stream last-page.


output stream last-page to value(tmp-dir + "boldaytn.txt") page-size VALUE(v-lines-per-page).

view frame bol-top.
view stream last-page frame bol-top.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first carrier
        where carrier.company eq cocode
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-cust-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    v-frt-pay-dscr = "".
    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      v-sname[1] = oe-ord.sname[1].

	  CASE oe-ord.frt-pay:
           WHEN "P" THEN v-frt-pay-dscr = "PREPAID".
           WHEN "C" THEN v-frt-pay-dscr = "COLLECT".
           WHEN "B" THEN v-frt-pay-dscr = "PPD/CHG".
           WHEN "T" THEN v-frt-pay-dscr = "3rdPARTY".
      END CASE.
	
      LEAVE.
    END.

    v-due-date = oe-ord.due-date.
    
    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq oe-boll.ord-no
        no-lock
        by oe-ordl.prom-date:
        
      v-due-date = oe-ordl.prom-date.
      
      leave.  
    end.

    page.
    page stream last-page.
  end. /* first-of(oe-bolh.bol-no) */

  {oe/rep/boldaytn.i "stream last-page"}

  v-page-tot = page-number (last-page) - v-last-page.

  {oe/rep/boldaytn.i}

  v-last-page = page-number.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

output stream last-page close.

hide frame bol-top no-pause.
page.

