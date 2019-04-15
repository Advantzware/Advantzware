/* ---------------------------------------------- oe/rep/bolpacifp 10/02 YSK */
/* PRINT Empire BOL                                                           */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)".
def var v-fob               as   char format "x(12)".
def var v-tot-cases         as   int format ">>>>9".
def var v-tot-wt            as   dec format ">>,>>>,>>9".

def var v-tot-pkgs          as   int format ">>9".
def var v-ord-qty           like oe-ordl.qty.
def var v-bol-qty           like oe-boll.qty.
def var v-ship-qty          like oe-ordl.ship-qty.
def var v-bol-wt            as   dec.
def var v-part-dscr         as   char format "x(30)".
def var v-part-comp         as   char format "x".
def var v-part-qty          as   dec.
def var v-ord-no            like oe-boll.ord-no.
def var v-po-no             like oe-bolh.po-no.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-comp-name  like company.name.
def var v-comp-addr  like company.addr.
def var v-comp-city  like company.city.
def var v-comp-state like company.state.
def var v-comp-zip   like company.zip.
def var v-comp-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(11)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\pacific1.bmp"
       ls-image2 = "images\pacific2.bmp".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

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

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.
ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = ""
       v-comp-add5 = ""
       lv-email = ""
       lv-comp-name = "".

IF lv-display-comp THEN DO:
   FIND FIRST cust WHERE cust.company = cocode AND
                         cust.active = "X" NO-LOCK NO-ERROR.
 /*  ASSIGN v-comp-add1 = company.addr[1]
          v-comp-add2 = company.addr[2]
          v-comp-add3 = company.city + ", " + company.st + "  " + company.zip
          v-comp-add4 = "Phone:  " + IF AVAIL cust THEN string(cust.area-code,"(999)") + string(cust.phone,"999-9999") ELSE "" 
          v-comp-add5 = "Fax     :  " + IF AVAIL cust THEN string(cust.fax,"(999)999-9999") ELSE ""
          lv-email    = "Email:  " + IF AVAIL cust THEN cust.email ELSE ""
          lv-comp-name = company.NAME   
          .
 */ 
  IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            .
END.

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
/*
form header
     skip(2)
     v-comp-name                  at 1
     "Bill of Lading #:"          to 70
        trim(string(oe-bolh.bol-no,">>>>>>>9"))
                                  to 80
     v-comp-addr[1]               at 1
     "Page #:"                    to 70
        string(trim(string(page-number - v-last-page,">9")) + " of " +
               trim(string(v-page-tot,">9")))
                                  to 80 format "x(8)"
     v-comp-addr[2]               at 1
     "Date:"                      to 70
     oe-bolh.bol-date             to 80
     v-comp-addr3                 at 1
     skip(2)
     "Ship To:"                   at 10 skip
     v-ship-name                  at 10
     "Carrier:"                   to 60
     carrier.dscr                 at 65 format "x(15)" when avail carrier
     v-ship-addr[1]               at 10
     "Trailer #:"                 to 60
     oe-bolh.trailer              at 65
     v-ship-addr[2]               at 10
     "Freight Terms:"             to 60 
     v-frt-terms                  at 65 skip
     v-ship-addr3                 at 10
     "Zone:"                      to 60
     v-zone                       at 65 skip
     v-phone-num                  at 10 format "(999)999-9999"
     "FOB:"                       to 60
     v-fob                        at 65
     skip(1)
     "Shipping Instructions:"     to 23
     "Payment Terms:"             to 60
     v-terms                      at 65 format "x(15)"
     oe-bolh.ship-i               at 1
     "Part #          PO#"        at 1
     "File #          JOB #           Description                    Unit-Qty  P/C Wgt"                          at 1 skip
     tmpstore                     at 1 format "x(80)"
   with frame bol-top page-top no-box no-underline stream-io width 80.

form oe-ordl.part-no                at 1
     oe-bolh.po-no                  at 17
     itemfg.i-name                  at 33
     oe-boll.cases                  at 64 format ">>9"
     "@"                            at 67
     oe-boll.qty-case               at 68 format ">>>>9"
     oe-boll.i-no                   at 1
     v-job-no                       at 17
     itemfg.part-dscr1              at 33
     v-1                            at 66
     oe-boll.partial                at 68 format ">>>>9" skip
     "-------------"                at 61 skip
     v-tot-pkgs                     at 61 format ">>>9"
     "="                            at 65
     oe-boll.qty                    at 66 format ">>>>>>9"
     v-part-comp                    at 75
     oe-boll.weight                 to 80 format ">>>>9"
    with frame bol-mid1 down no-box no-labels stream-io width 80.

form header
     " "
     skip(4)
    with frame bol-bot1 page-bottom no-box no-underline stream-io width 80.

form header
     skip(2)
     "Total Pallets:"               to 60
     v-tot-cases                    to 80
     "Total Weight:"                to 60
     v-tot-wt                       to 80
     skip(1)
     tmpstore                       at 1 format "x(80)" skip(1)
     "Customer_____________________                      Carrier_____________________"
     skip(1)
     "    Date____________                                  Date____________"
    with frame bol-bot2 page-bottom no-box no-underline stream-io width 80.

def stream last-page.
*/
{sa/sa-sls01.i}
/*
output stream last-page to value(tmp-dir + "bolempir.txt") page-size 62.
*/

find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
      v-printline = 0.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,
    first cust where cust.company eq cocode
                 and cust.cust-no eq oe-bolh.cust-no NO-LOCK
    break by oe-bolh.bol-no:
      
    if first-of(oe-bolh.bol-no) then do:
       find first carrier where carrier.company eq oe-bolh.company
                            and carrier.carrier eq oe-bolh.carrier
                            no-lock no-error.

       RUN oe/custxship.p (oe-bolh.company,
                           oe-bolh.cust-no,
                           oe-bolh.ship-id,
                           BUFFER shipto).

       ASSIGN v-ship-name    = shipto.ship-name
              v-ship-addr[1] = shipto.ship-addr[1]
              v-ship-addr[2] = shipto.ship-addr[2]
              v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
              v-phone-num    = cust.area-code + cust.phone.
     
    
       ASSIGN v-comp-name    = cust.name
              v-comp-addr[1] = cust.addr[1]
              v-comp-addr[2] = cust.addr[2]
              v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

       if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
              
       if v-comp-addr[2] eq "" then
          ASSIGN v-comp-addr[2] = v-comp-addr3
                 v-comp-addr3   = "".
       if v-ship-addr[2] eq "" then
          ASSIGN v-ship-addr[2] = v-ship-addr3
                 v-ship-addr3   = "".
       if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
       if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

       ASSIGN v-salesman = ""
              v-fob      = ""
              v-terms    = "".

       FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
           first oe-ord where oe-ord.company eq oe-boll.company
	                      and oe-ord.ord-no  eq oe-boll.ord-no NO-LOCK:

          if not available carrier then
             find first carrier where carrier.company = oe-ord.company
                        and carrier.carrier = oe-ord.carrier no-lock no-error.

          do i = 1 to 3:
             if oe-ord.sman[i] ne "" then
                v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
          end.

/*    if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d. */
          assign v-terms = oe-ord.terms-d
                 v-frt-terms = if oe-bolh.frt-pay eq "P" then "Prepaid"
                           else if oe-bolh.frt-pay eq "B" then "Bill"
                           else if oe-bolh.frt-pay eq "C" then "Collect"
                           else if oe-bolh.frt-pay eq "T" then "Third Party"
                           else ""
                 v-zone = cust.del-zone.
             
          if v-terms eq "" then
          do:
            find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
            if avail terms THEN assign v-terms = terms.dscr.
          end.
      
          v-salesman = trim(v-salesman).
          v-po-no = oe-boll.po-no.
          v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
          if v-salesman gt '' then
            if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
               substr(v-salesman,length(trim(v-salesman)),1) = "".

          v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
          LEAVE.
       end.  /* for each oe-boll */

       for each w3:
           delete w3.
       end.
    end. /* first-of(oe-bolh.bol-no) */

    do i = 1 to 4:
      if oe-bolh.ship-i[i] ne "" then do:
         find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
         if not avail w3 then create w3.
         w3.ship-i = oe-bolh.ship-i[i].
      end.
    end.

    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
      create report.
      ASSIGN report.term-id  = v-term-id
             report.key-01   = oe-boll.i-no
             report.key-02   = string(oe-boll.ord-no,"9999999999")
             report.rec-id   = recid(oe-boll)
             oe-boll.printed = yes.
    end.

    if last-of(oe-bolh.bol-no) then do:
       IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
       IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
       v-trailer = oe-bolh.trailer.
       {oe/rep/bolxinl2.i}
      
       {oe/rep/bolxinld.i}
       v-last-page = page-number.


       for each report where report.term-id eq v-term-id,
           first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
           delete report.
       end.
    
                 
    PUT "<R51><C52><#8><FROM><R+4><C+30><RECT> " 
        "<=8><R+1> Total Units         :" v-tot-cases format ">>,>>>,>>9" 
        "<=8><R+3> Total Weight        :" v-tot-wt format ">>,>>>,>>9".
                   

    PUT "<FArial><R51><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)
    oe-bolh.ship-i[1] AT 7 SKIP
    oe-bolh.ship-i[2] AT 7 SKIP
    oe-bolh.ship-i[3] AT 7 SKIP
    oe-bolh.ship-i[4] AT 7 SKIP    
    "__________________________________________________________________________________________________________________" SKIP
    "<P10><ADJUST=LPI><B>  Signature of Receipt </B>" SKIP
    "Customer ________________________________________                       Carrier ___________________________________" AT 15 SKIP(1)
    "Date ____________________________________________                       Date _____________________________________" AT 15 SKIP(1)   
    "<P6><ADJUST=LPI>If the shipment moves between two ports by a carrier by water, the law requires that "
    "the bill of lading shall state whether it is a "'"carrier`s or shipper`s weight"'"." skip
    "NOTE-Where the rate is dependent on value,shippers are required to state specifically in writing "
    "the agreed or declared value of the property. The agreed or declared value of the property is hereby specifically stated "SKIP
    "by the shipper to be not exceeding__________________________per__________________________." SKIP(1)
    "<P8><ADJUST=LPI>Check This Shipment Carefully, If shipment does not agree with this manifest, Note Discrepancy on Receipt before signing. Please notify us immediately"
    /*"<P6>Subject to Section 7 of Conditions of applicable bill of lading." AT 75 SKIP
    "If this shipment is to be delivered to the consignee" AT 100 SKIP
    "without recourse on the consignor, the consignor shall" AT 100 SKIP
    "sign the following statement: The carrier shall not make delivery" AT 100 */
    "<P6>" "Subject to Section 7 of Conditions of applicable bill of lading." AT 4
    " If this shipment is to be delivered to the consignee " 
    "without recourse on the consignor, the consignor shall sign the following statement:" skip
    "The carrier shall not make delivery" AT 4

    .

     v-printline = v-printline + 14. 
     /*IF v-printline < 45 THEN */
     PAGE.   

     v-printline = 0.
     v-tot-cases = 0.
  end. /* last-of (oe-bolh.bol-no) */
  oe-bolh.printed = yes.
end. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

