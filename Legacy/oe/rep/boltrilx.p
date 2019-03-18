/* ---------------------------------------------- oe/rep/boltrilx.p  */
/* PRINT Trilakes Xprint BOL                                                           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.
DEF BUFFER b-oe-boll    FOR oe-boll.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-wt            as   dec format ">>,>>>,>>9" NO-UNDO.
DEF VAR v-tot-units         AS   INT NO-UNDO.

def var v-tot-pkgs          as   int format ">>9" NO-UNDO.
def var v-part-qty          as   DEC NO-UNDO.
def var v-ord-no            like oe-boll.ord-no NO-UNDO.
def var v-po-no             like oe-bolh.po-no NO-UNDO.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.

def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
def var v-cust-addr3 as   char format "x(30)" NO-UNDO.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(12)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-frt-class AS CHAR FORMAT "X(15)" NO-UNDO.
DEF VAR v-frt-desc AS CHAR FORMAT "X(30)" NO-UNDO.
def var v-pkgs as int no-undo.
def var v-pall as int no-undo.
def var v-ship-qty like oe-ordl.ship-qty NO-UNDO.
def var v-weight LIKE oe-boll.weight no-undo.
DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-broker AS LOG NO-UNDO.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def TEMP-TABLE w3 no-undo
    field ship-i           as   char format "x(60)".

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.

ASSIGN
   ls-image1 = "images\trilake2.jpg"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(56)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-trailer AS cha NO-UNDO.

DEF VAR lv-print-lot AS LOG NO-UNDO. /*switch for lot #*/

/* gdm - */
DEF VAR v-p-c AS CHAR NO-UNDO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST sys-ctrl where sys-ctrl.company = cocode
                      and sys-ctrl.NAME = "BOLFMT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "TrilakesLot#" THEN lv-print-lot = YES.
    ELSE lv-print-lot = NO.  /* controls the switch between TrilakesX and TrilakesLot# at main level*/

find first company where company.company = cocode no-lock no-error.

IF lv-display-comp THEN DO:
   FIND FIRST cust WHERE cust.company = cocode AND
                         cust.active = "X" NO-LOCK NO-ERROR.

   IF AVAIL cust THEN
      ASSIGN v-comp-add1 = cust.addr[1]
             v-comp-add2 = cust.addr[2]
             v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
             v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
             v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
             lv-email    = "Email:  " + cust.email 
             lv-comp-name = cust.NAME.
END.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.

{sa/sa-sls01.i}

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.

v-printline = 0.

DISABLE TRIGGERS FOR LOAD OF oe-bolh.
DISABLE TRIGGERS FOR LOAD OF oe-boll.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:
      
    if first-of(oe-bolh.bol-no) then do:

    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    /* tests where customer specific forms have TrilakesLot#*/
    DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ cocode
            AND sys-ctrl-shipto.NAME EQ "BOLFMT"
            AND sys-ctrl-shipto.cust-vend-no = cust.cust-no NO-ERROR.
        IF AVAIL sys-ctrl-shipto THEN
            IF sys-ctrl-shipto.char-fld = "TrilakesLot#" THEN
                lv-print-lot = YES.
            ELSE lv-print-lot = NO.
    END. /*end of Do block to test for TrilakesLot# per customer*/

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
     v-phone-num    = cust.area-code + cust.phone
     v-broker       = shipto.broker.

    IF shipto.broker THEN
    DO:
       ASSIGN
          v-comp-add1 = cust.addr[1]
          v-comp-add2 = cust.addr[2]
          v-comp-add3   = cust.city + ", " +
                    cust.state + "  " +
                    cust.zip
          v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
          v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
          lv-email    = "Email:  " + cust.email   
          lv-comp-name = cust.NAME.

       /* sold to address from order */
       FIND FIRST oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK NO-ERROR.
       IF AVAIL oe-boll THEN DO:
          FIND FIRST oe-ord WHERE
               oe-ord.company = oe-bolh.company AND
               oe-ord.ord-no = oe-boll.ord-no
               NO-LOCK NO-ERROR.

          IF AVAIL oe-ord THEN
          DO:
             ASSIGN lv-comp-name = oe-ord.sold-name
                    v-comp-add1 = oe-ord.sold-addr[1]
                    v-comp-add2 = oe-ord.sold-addr[2]
                    v-comp-add3 = oe-ord.sold-city + ", " +
                                  oe-ord.sold-state + "  " +
                                  oe-ord.sold-zip.
             RELEASE oe-ord.
          END.

          RELEASE oe-boll.
       END.
    END.

    if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-salesman = ""
     v-fob      = ""
     v-terms    = ""
     v-frt-class = ""
     v-frt-desc = "".

    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:


      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.

      do i = 1 to 3:
        if oe-ord.sman[i] ne "" then
          v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.

      assign v-terms = oe-ord.terms-d
             v-frt-terms = if index("pb",oe-bolh.frt-pay) gt 0 then "Prepaid" 
                           ELSE if oe-bolh.frt-pay eq "c" then "Collect" 
                           ELSE if oe-bolh.frt-pay eq "t" then "Third Party" 
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
         find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
         if avail terms then
            assign v-terms = terms.dscr.
      end.
      
      ASSIGN
         v-salesman = trim(v-salesman)
         v-po-no = oe-boll.po-no
         v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>"))
         v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".

      FIND FIRST itemfg WHERE
           itemfg.company EQ oe-boll.company AND
           itemfg.i-no EQ oe-boll.i-no
           NO-LOCK NO-ERROR.

      IF AVAIL itemfg THEN
         ASSIGN
            v-frt-class = "Class: " + itemfg.frt-class
            v-frt-desc = itemfg.frt-class-dscr.

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".
      
      LEAVE.
    end.

    EMPTY TEMP-TABLE w3.
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  END.

  for each oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:
  
      create report.
      assign
         report.term-id  = v-term-id
         report.key-01   = oe-boll.i-no
         report.key-02   = string(oe-boll.ord-no,"9999999999")
         report.rec-id   = recid(oe-boll)
         oe-boll.printed = yes.
  end.

  if last-of(oe-bolh.bol-no) then do:
     
     IF v-ship-addr[2] = "" THEN
        ASSIGN v-ship-addr[2] = v-ship-addr3
               v-ship-addr3 = "".

     v-trailer = oe-bolh.trailer.
     {oe/rep/boltrilx2.i}
    
    {oe/rep/boltrilx.i}

    v-last-page = page-number.

    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
        delete report.
    end.
  end.
                 
  PUT "<R52><C53><#8><FROM><R+4><C+27><RECT> " 
      "<=8><R+1> Total Units       :" v-tot-units format ">>,>>>,>>9" 
      "<=8><R+3> Total Weight      :" v-tot-wt .

PUT "<FArial><R51><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)
    oe-bolh.ship-i[1] AT 7 SKIP
    oe-bolh.ship-i[2] AT 7 SKIP
    oe-bolh.ship-i[3] AT 7 SKIP
    oe-bolh.ship-i[4] AT 7 SKIP.

PUT "<R57><C26><P10>ALL CLAIMS ON SHIPMENT MUST BE MADE WITHIN 10 DAYS</B>"
    "<SAVE=LPI><ADJUST=LPI><P7>" 
    "<R58><C27.5><From><R+10><C27.5><LINE><||3>"
    "<R58><C43.7><From><R+10><C43.7><LINE><||3>"

    "<R59.8><C28><From><C43><LINE><||3>"
    "<R60><C28><From><C43><LINE><||3>"
    "<R61><C28><From><C43><LINE><||3>"
    "<R62><C28><From><C43><LINE><||3>"
    "<R64><C29><From><C43><LINE><||3>"
    "<R65><C54><From><C78><LINE><||3>"
    "<R66><C29><From><C43><LINE><||3>"
    "<R66.5><C50><From><C78><LINE><||3>"
    "<R67.8><C33><From><C43><LINE><||3><C50><From><C78><LINE><||3>"

    "<R58><C1><P7>Subject To Section 7 of Conditions of applicable bill of lading, <C28>If charges are to be prepaid write or   <C44>If the shipment moves between two ports by a carrier by water, the law requires that"
    "<R59><C1>this shipment is to be delivered to the consignee without re-   <C28>stamp here, ""To be Prepaid.""          <C44>the bill of lading shall state whether it is carrier's or shipper's weight."
    "<R60><C1>course on the consignor, the consignor shall sign the follo-                                                <C44>NOTE:Where the rate is dependent on value, shippers are required to state specific-"
    "<R61><C1>wing statement.     <C28>Received $                              <C44>ally in writing the agreed or declared value of the property. The agreed or declared"
    "<R62><C1>The carrier shall not make delivery of this statement without<C28><P6>  to apply in prepayment of the charges<P7>     <C44>value of the property is hereby specifically stated by the shipper to be not exceeding."
    "<R63><C1>payment of freight and all other lawful charges.   <C28><P6> on the property described hereon.<P7>"
    "<R64><C28><P6>              Agent or Cashier <P7>  <C44>  SHIPPER PER"
    "<R65><C1><From><C27><LINE><||3><R65><C9>(Signature of consignor)"
    "<R64.5><C28>Per "
    "<R65.5><C44>  AGENT"   
    "<R65.2><C28><From><C43><LINE><||3>"
    "<R65.2><C28><P4>(The signature here acknowledges only the amount proposed.)<P7>"
    "<R66><C28><From><C43><LINE><||3>"
    "<R66> <C28>Charges"
    "<R67><C28><P6>advanced:$ <P7><C45>  PER"
    "<RESTORE=LPI>".

  v-printline = v-printline + 14. 
  PAGE.   

  ASSIGN
     v-printline = 0.
     v-tot-units = 0.
     oe-bolh.printed = yes.
end. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

