/* ---------------------------------------------- oe/rep/bolkni.p           */
/* PRINT Knight                                                             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer xitemfg      for itemfg.
def buffer xxreport     for report.

{oe/rep/oe-lad.i}

def var v-salesman          as   char format "x(26)" NO-UNDO.
def var v-fob               as   char format "x(12)" NO-UNDO.
def var v-tot-cases         as   int format ">>>>9" NO-UNDO.
def var v-tot-wt            as   dec format ">>,>>>,>>9" NO-UNDO.

def var v-tot-pkgs          as   int format ">>9" NO-UNDO.
def var v-ord-qty           like oe-ordl.qty NO-UNDO.
def var v-bol-qty           like oe-boll.qty NO-UNDO.
def var v-ship-qty          like oe-ordl.ship-qty NO-UNDO.
def var v-bol-wt            as   dec NO-UNDO.
def var v-part-dscr         as   char format "x(30)" NO-UNDO.
def var v-part-comp         as   char format "x" NO-UNDO.
def var v-part-qty          as   DEC NO-UNDO.
def var v-ord-no            like oe-boll.ord-no NO-UNDO.
def var v-po-no             like oe-bolh.po-no NO-UNDO.
def var v-job-no            as   char format "x(9)" no-undo.
def var v-phone-num         as   char format "x(13)" no-undo.
DEF VAR v-tot-palls AS INT FORM ">>>9" NO-UNDO.
def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
def var v-comp-name  like company.name NO-UNDO.
def var v-comp-addr  like company.addr NO-UNDO.
def var v-comp-city  like company.city NO-UNDO.
def var v-comp-state like company.state NO-UNDO.
def var v-comp-zip   like company.zip NO-UNDO.
def var v-comp-addr3 as   char format "x(30)" NO-UNDO.
def var v-cust-addr3 as   char format "x(30)" NO-UNDO.
def var v-1          LIKE oe-boll.cases INIT 1 no-undo.
DEF VAR lv-cases LIKE oe-boll.cases NO-UNDO.
DEF VAR lv-bolfmt-int AS INT NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.

DEF VAR lv1-cases-tot AS INTE NO-UNDO.
DEF VAR lv1-qty-tot AS INTE NO-UNDO.
DEF VAR lv1-qcase-tot AS INTE NO-UNDO.
DEF VAR lv1-partial-tot AS INTE NO-UNDO.
DEF VAR lv1-pal-tot AS INTE NO-UNDO.

DEF VAR v-weight    AS DECI NO-UNDO.
DEF VAR v-pal-cnt   AS INTE NO-UNDO.
DEF VAR v-pal-line  AS INTE NO-UNDO.
def var v-job-po            as   char format "x(9)" no-undo.
DEF VAR tot-ino AS INTE NO-UNDO.

def var v-terms like oe-ord.terms-d NO-UNDO.
def var v-frt-terms as char format "x(13)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-pal AS INT NO-UNDO.

def workfile w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9".

def workfile w3 no-undo
    field ship-i           as   char format "x(60)".

DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
ASSIGN
   ls-image1 = "images\knight.jpg"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR lv-comp-unit AS INT NO-UNDO.
DEF VAR lv-cases-tot AS INT NO-UNDO.
DEF VAR lv-qty-tot AS INT NO-UNDO.
DEF VAR lv-qcase-tot AS INT NO-UNDO.
DEF VAR lv-partial-tot AS INT NO-UNDO.
DEF VAR lv-pal-tot AS INT NO-UNDO.
DEF VAR v-unit-qty AS cha NO-UNDO.
DEF VAR v1-unit-qty AS cha NO-UNDO.

assign tmpstore = fill("-",80).

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
{sa/sa-sls01.i}

find first company where company.company eq cocode no-lock.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
ASSIGN v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.533.2545" 
       v-comp-add4 = "Fax  : 604.533.2633"
       v-printline = 0.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" 
                      AND sys-ctrl.char-fld = "Knight" no-lock no-error.
IF AVAIL sys-ctrl THEN 
   ASSIGN lv-display-comp = sys-ctrl.log-fld 
          lv-bolfmt-int = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
            lv-bolfmt-int = 0.

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
     v-phone-num    = cust.area-code + cust.phone.
   
    assign
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

    if trim(v-comp-addr3) eq "," then v-comp-addr3 = "".
              
    if v-comp-addr[2] eq "" then
      assign
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    assign
     v-salesman = ""
     v-fob      = ""
     v-terms    = "".

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
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
/* gdm - 10060907 */
             v-frt-terms = IF oe-bolh.frt-pay EQ "P" 
                              THEN "Prepaid"
                              ELSE 
                               IF oe-bolh.frt-pay EQ "B" 
                                 THEN "Bill"
                                 ELSE 
                                  IF oe-bolh.frt-pay EQ "C" 
                                    THEN "Collect"
                                    ELSE 
                                     IF oe-bolh.frt-pay EQ "T" 
                                      THEN "Third Party"
                                      ELSE ""
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
      v-ord-no = oe-boll.ord-no.
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      FIND FIRST reftable WHERE
           reftable.reftable EQ "oe-bolh.lot-no" AND
           reftable.rec_key  EQ oe-bolh.rec_key
           USE-INDEX rec_key
           NO-LOCK NO-ERROR.
      IF AVAIL reftable THEN
         ASSIGN v-fob = reftable.CODE.

      IF v-fob = "" THEN
        ASSIGN v-fob = oe-ord.fob-code.

      ASSIGN v-fob = if v-fob begins "O" then "Origin" 
                      ELSE IF v-fob BEGINS "d" THEN "Destination" 
                      ELSE "".
      
      LEAVE.
    end.

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
    assign
     report.term-id  = v-term-id
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

     /* duplicate loop for total freight */
     ASSIGN
     v-tot-wt = 0
     lv-tot-pg = 1
     ln-cnt = 0
     tot-ino = 0.
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
         first itemfg where itemfg.company eq oe-boll.company
                      and itemfg.i-no    eq oe-boll.i-no no-lock
                break by report.key-01
                by report.key-02:
         IF first-of(report.key-01) THEN 
             tot-ino = tot-ino + 1.

         ASSIGN v-tot-wt = v-tot-wt + oe-boll.weight.
   
         if oe-boll.weight eq 0 then
             v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).

          /*========*/
          ln-cnt = ln-cnt + 4.          
     END.
     /* end of dup loop */
     
     IF lv-bolfmt-int = 0 THEN
        lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 20,0) .  /* 23->47 25 po detail lines */
     ELSE
        lv-tot-pg = ( TRUNC( tot-ino / 5,0) + (IF  tot-ino MOD 5 > 0 THEN 1 ELSE 0) ). 
                                                                                             
      /*  end of getting total page per po */

    {oe/rep/bolkni1.i}
    {oe/rep/bolkni2.i}

    v-last-page = page-number.


    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.
  end.

  PUT "<R45><C40><#7><C-6>Shipper Signature"
      "<=7><C+10><FROM><R+2><C+20><RECT> " 
      "<R47><C50><#8><FROM><R+4><C+30><RECT> " 
      "<=8><R+1> Total Pallets      :" v-tot-palls FORM ">,>>>,>>9"
      "<=8><R+2> Total Cases        :" v-tot-cases FORM ">,>>>,>>9"
      "<=8><R+3> Total Weight       :" v-tot-wt FORM ">>,>>9.99".
    
PUT "<FBook Antiqua><R45><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)
    oe-bolh.ship-i[1] AT 7 SKIP
    oe-bolh.ship-i[2] AT 7 SKIP
    oe-bolh.ship-i[3] AT 7 SKIP
    oe-bolh.ship-i[4] AT 7 SKIP
    "_________________________________________________________________________________________________________________________________" SKIP
    "<B>  Signature of Receipt </B>" SKIP
    "Customer ________________________________________                       Carrier _______________________________________" AT 23 SKIP(1)
    "Date ____________________________________________                       Date __________________________________________" AT 23 SKIP   
    "Page " AT 220 string(PAGE-NUM - lv-pg-num,">>9") + " of " + string(lv-tot-pg) FORM "x(20)" SKIP
    "<P6>RECEIVED, SUBJECT TO THE CLASSIFCATION AND LAWFULLY FILED TARIFFS IN EFFECT ON THE DATE OF THIS Bill of Lading. The property described above, except as noted, marked or consigned and destined" SKIP
    "as indicated below, which said carrier (the word carrier being understood through this contract as meaning any person or corporation in possession of the property under the contract) agrees to carry to its usual place of" SKIP
    "delivery at said destination. Its is mutually agreed, as to each carrier of all or any property over all or any portion of said route to destination, as to each party at any time interested in all or any of said property, that" SKIP
    "every service be performed hereunder shall be subject to all the terms and conditions of the Uniform Domestic Straight Bill of Lading set forth (1) in Uniform Freight Classification in effect of the date hereof, if this is rail" SKIP
    "or water shipment or (2) in the applicable motor carrier classification or tariff if this is a motor shipment. Shipper/Receiver hereby certifies the he/she is familiar with all the terms and conditions of the said bill of lading," SKIP
    "set forth in the classification or tariff which governs the transportation of this shipment, and the said terms and conditions are herby agreed to by the shipper/receiver and accepted for himself/herself and his assigns.".

  v-printline = v-printline + 14.
  IF last-of(oe-bolh.bol-no) THEN lv-pg-num = PAGE-NUM .

  IF v-printline < 45 THEN PUT SKIP(60 - v-printline).
  ASSIGN
     v-printline = 0
     oe-bolh.printed = yes.

end. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

PROCEDURE get-pallets-num:
DEF OUTPUT PARAM op-pallets AS INT NO-UNDO.
DEF VAR v-qty-pal AS dec NO-UNDO.

find first fg-bin
    where fg-bin.company eq cocode
      and fg-bin.i-no    eq oe-boll.i-no
      and fg-bin.job-no  eq oe-boll.job-no
      and fg-bin.job-no2 eq oe-boll.job-no2
      and fg-bin.loc     eq oe-boll.loc
      and fg-bin.loc-bin eq oe-boll.loc-bin
      and fg-bin.tag     eq oe-boll.tag
    no-lock no-error.

if avail fg-bin then
  assign
   v-qty-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
   op-pallets = oe-boll.QTY / v-qty-pal.

else
  assign
   op-pallets = 1.

IF op-pallets = ? THEN op-pallets = 1.

{sys/inc/roundup.i op-pallets}

if op-pallets lt 0 then op-pallets = op-pallets * -1.

END.
