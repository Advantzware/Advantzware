/* ---------------------------------------------- oe/rep/bolfibre.p 10/00 JLF */
/* Print Fibre Container BOL                                                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer b-itemfg     for itemfg.
def buffer b-rh         for rm-rcpth.
def buffer b-rd         for rm-rdtlh.

{oe/rep/oe-lad.i}

def var v-frt-pay-dscr      as   char.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-due-date          like oe-ordl.prom-date.
def var v-lines             as   int.
def var v-part-qty          as   dec.
def var v-job-po            as   char.
def var v-tot-pal           like oe-bolh.tot-pallets.
def var v-tot-wt            like oe-bolh.tot-wt.
DEF VAR v-bol-trans         AS   CHAR FORMAT "x(27)" NO-UNDO.
def var v-ship-name         like shipto.ship-name.
def var v-ship-addr         like shipto.ship-addr.
def var v-ship-city         like shipto.ship-city.
def var v-ship-state        like shipto.ship-state.
def var v-ship-zip          like shipto.ship-zip.
def var v-ship-addr3        as   char format "x(30)".
def var v-cust-addr3        as   char format "x(30)".

DEF TEMP-TABLE w2 NO-UNDO
    FIELD cases            AS   INT FORMAT ">>9"
    FIELD cas-cnt          AS   INT FORMAT ">>>>>9"
    FIELD job-po           AS   CHAR
    FIELD part-dscr        AS   CHAR.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

format header
       skip(1)
       v-bol-trans                      to 80
       skip(4)
       "BILL TO:"                       
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
       SPACE(13)
       "FOB" 
       SPACE(17)
       "TRAILER#"
       oe-bolh.trailer             
       "BOL DATE"                       
       oe-bolh.bol-date                         format "99/99/99"       skip
       "BOL#"
       oe-bolh.bol-no                           format "999999" SPACE(2)
       caps(v-fob)                      FORM "x(12)"
       "CARRIER"                        at 35    
       oe-bolh.carrier                  SPACE(8)
       v-frt-pay-dscr                    FORM "x(11)" 
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
     w2.job-po                      at 17 format "x(15)"
     w2.part-dscr                   at 33 format "x(27)"
     w2.cases                       to 64 format "->>9"
     w2.cas-cnt                     to 70 format "->>>>9"
     oe-boll.qty                    to 78 format "->>>>>9"
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
     "Print Name:"
     fill("_",44) format "x(44)"
     "Time:"
     fill("_",16) format "x(16)"
     
    with frame bol-bot1 page-bottom no-box no-underline stream-io width 85.

form header
     skip(1)
     "Total Pallets:"               
     v-tot-pal                      to 21   format ">>,>>9"
     "Total Weight:"                to 69
     v-tot-wt                       to 80   format ">>,>>>,>>9"
     skip(2)
     "Received by:"
     fill("_",44) format "x(44)"
     "Date:"
     fill("_",16) format "x(16)"
     skip(1)
     "Print Name: "
     fill("_",44) format "x(44)"
     "Time:"
     fill("_",16) format "x(16)"

    with frame bol-bot2 page-bottom no-box no-underline stream-io width 85.

def stream last-page.


output stream last-page to value(tmp-dir + "bolfibre.txt1") page-size VALUE(v-lines-per-page).

view frame bol-top.
view stream last-page frame bol-top.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each report   where report.term-id eq v-term-id
                    AND report.key-03 <> "C",
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

    ASSIGN 
       v-frt-pay-dscr = ""
       v-fob = "".

    FIND FIRST reftable WHERE
         reftable.reftable EQ "oe-bolh.lot-no" AND
         reftable.rec_key  EQ oe-bolh.rec_key
         USE-INDEX rec_key
         NO-LOCK NO-ERROR.
    IF AVAIL reftable THEN
       ASSIGN v-fob = reftable.CODE.

    IF v-fob = "" THEN
       ASSIGN v-fob = oe-ord.fob-code.

    ASSIGN v-fob = (if v-fob begins "O" then "Origin" 
                    ELSE IF v-fob BEGINS "d" THEN "Destination" 
                    ELSE "").

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:
	
      ASSIGN v-frt-pay-dscr = if oe-bolh.frt-pay eq "P" then "Prepaid"
                              else if oe-bolh.frt-pay eq "B" then "Bill"
                              else if oe-bolh.frt-pay eq "C" then "Collect"
                              else if oe-bolh.frt-pay eq "T" then "Third Party"
                              else "".

      LEAVE.
    END.
   
    ASSIGN
     v-due-date  = oe-ord.due-date
     v-bol-trans = "B I L L   O F   L A D I N G".
    
    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no,
        FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ oe-boll.company
          AND oe-ordl.ord-no  EQ oe-boll.ord-no
          AND oe-ordl.i-no    EQ oe-boll.i-no,
        FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-ordl.company
          AND oe-ord.ord-no  EQ oe-ordl.ord-no
        BREAK BY oe-ordl.prom-date:
        
      IF FIRST(oe-ordl.prom-date) THEN v-due-date = oe-ordl.prom-date.

      IF oe-boll.s-code EQ "T" OR oe-ord.type EQ "T" THEN
        v-bol-trans = "            T R A N S F E R".
    END.

    page.
    page stream last-page.
  end. /* first-of(oe-bolh.bol-no) */
  
  {oe/rep/bolfibre.i "stream last-page"}
 
  v-page-tot = page-number (last-page) - v-last-page.

  {oe/rep/bolfibre.i}

  v-last-page = page-number.

  oe-bolh.printed = yes.
end. /* for each oe-bolh */

output stream last-page close.

hide frame bol-top no-pause.
page.

/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */
