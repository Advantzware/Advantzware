/* ---------------------------------------------- oe/rep/bolkdwlx.p           */
/* PRINT KDwillsX Xprint BOL                                                           */
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
DEF VAR v-i-part-no  AS CHAR NO-UNDO.
DEF VAR v-ord-po-no  AS CHAR NO-UNDO.
DEF VAR v-descr      AS CHAR NO-UNDO.
def var v-job-po            as   CHAR NO-UNDO.
DEF VAR v-grand-total-cases as   int format ">>>>9".
DEF VAR v-summ-case-tot AS INTE NO-UNDO.

def var v-terms like oe-ord.terms-d no-undo.
def var v-frt-terms as char format "x(10)" no-undo.
def var v-zone like carr-mtx.del-zone no-undo.
DEF VAR v-total-weight LIKE tt-boll.weight.

def TEMP-TABLE w2 no-undo
    field cases            as   int format ">9"
    field cas-cnt          as   int format ">>>>9"
    FIELD ord-po-no        AS CHAR
    FIELD rec-id AS RECID
    FIELD i-no LIKE oe-ordl.i-no
    FIELD job-po AS cha
    FIELD qty AS INT 
    FIELD dscr LIKE oe-ordl.part-dscr1.

def TEMP-TABLE w3 no-undo
    field ship-i           as   char format "x(60)".

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
DEF VAR comp-ctr AS INTE NO-UNDO.

def buffer b-itemfg     for itemfg.
DEF BUFFER bf-ttboll FOR tt-boll.
DEF VAR v-tot-case-qty AS INT NO-UNDO.
DEF VAR ll-consol-bolls AS LOG NO-UNDO.
DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
def var v-job            as   CHAR NO-UNDO.
DEF VAR v-description       AS CHAR NO-UNDO.
DEF VAR v-i-no              AS CHAR NO-UNDO.
DEF VAR v-item-part-no      AS CHAR NO-UNDO.
DEF VAR v-case-tot          AS INTE NO-UNDO.

form 
     w2.i-no                         format "x(15)"
     w2.ord-po-no                    format "x(16)"
     w2.dscr                         format "x(32)"
     w2.cases                        format "->>>>>9"
     w2.cas-cnt                      format "->>>>,>>Z"
     v-case-tot                      FORMAT "->>,>>>,>>z"
    with frame bol-mid down no-box no-labels stream-io width 110.
form 
     v-item-part-no                       format "x(15)"
     v-ord-po-no                          format "x(16)"
     v-part-dscr                          format "x(32)"
     w2.cases                             format "->>>>>9"
     w2.cas-cnt                           format "->>>>,>>Z"
     v-case-tot                           FORMAT "->>,>>>,>>z"
    with frame bol-mid2 down no-box no-labels stream-io width 110.
           

 ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
ASSIGN
 ll-consol-bolls = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.

ASSIGN v-comp-add1  = "KD WILLS PACKAGING"
        v-comp-add2 = "PO BOX 32061" 
        v-comp-add3 = "LOUISVILLE, KY 40231-9989"
        v-comp-add4 = "502-962-7621"
        v-comp-add5 = "".

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
            lv-comp-name = cust.NAME   
            .
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
     
    /*if shipto.broker then
      assign
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.
    
    else
      assign
       v-comp-name    = company.name
       v-comp-addr[1] = company.addr[1]
       v-comp-addr[2] = company.addr[2]
       v-comp-addr3   = company.city + ", " +
                        company.state + "  " +
                        company.zip.
    */                    
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

/*    if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d. */
      assign v-terms = oe-ord.terms-d
             v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
                           else ""
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      v-salesman = trim(v-salesman).
      v-po-no = oe-boll.po-no.
      v-job-po = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".

      v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
      
      LEAVE.
    end.

    for each w3:
      delete w3.
    end.
    FOR EACH tt-boll:
       DELETE tt-boll.
    END.
  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.
  FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    IF ll-consol-bolls THEN DO:
      IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).

      IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).
    END.

    ELSE DO:
      CREATE tt-boll.
      BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
    END.

    oe-boll.printed = YES.
  END.
/*   for each oe-boll                                         */
/*       WHERE oe-boll.company EQ oe-bolh.company             */
/*         AND oe-boll.b-no    EQ oe-bolh.b-no:               */
/*     create report.                                         */
/*     assign                                                 */
/*      report.term-id  = v-term-id                           */
/*      report.key-01   = oe-boll.i-no                        */
/*      report.key-02   = string(oe-boll.ord-no,"9999999999") */
/*      report.rec-id   = recid(oe-boll)                      */
/*      oe-boll.printed = yes.                                */
/*   end.                                                     */

  if last-of(oe-bolh.bol-no) then do:
     IF v-comp-addr[2] = "" THEN
           ASSIGN v-comp-addr[2] = v-comp-addr3
                  v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
           ASSIGN v-ship-addr[2] = v-ship-addr3
                  v-ship-addr3 = "".
     v-trailer = oe-bolh.trailer.
/*      {oe/rep/bolprmx2.i} */

    /* 
    {oe/rep/bolempir.i "stream last-page"}

    v-page-tot = page-number (last-page) - v-last-page.
    */
    {oe/rep/bolkdwl2.i} /* header */
/*     {oe/rep/bolkdwlx.i} */
    {oe/rep/bolkdwl3.i}

    v-last-page = page-number.


    for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.
  end.
                 
  PUT "<R52><C53><#8><FROM><R+4><C+27><RECT> " 
    "<=8><R+1> Total Units       :" v-grand-total-cases format ">>,>>>,>>9"  /* oe-bolh.tot-pallets */
    "<=8><R+3> Total Weight      :" v-tot-wt FORM ">>,>>>,>>9".
                   

PUT "<FArial><R51><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)
    oe-bolh.ship-i[1] AT 7 SKIP
    oe-bolh.ship-i[2] AT 7 SKIP
    oe-bolh.ship-i[3] AT 7 SKIP
    oe-bolh.ship-i[4] AT 7 /*SKIP    */
/*    "__________________________________________________________________________________________________________________" SKIP
    "<P10><ADJUST=LPI><B>  Signature of Receipt </B>" SKIP
    "Customer ________________________________________                       Carrier ___________________________________" AT 15 SKIP(1)
    "Date ____________________________________________                       Date _____________________________________" AT 15 SKIP(1)   
    "<P8><ADJUST=LPI>Check This Shipment Carefully, If shipment does not agree with this manifest, Note Discrepancy on Receipt before signing. Please notify us immediately"
    */.

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
    "<R65.2><C28><From><C43><LINE><||3>"  /* per line*/
    "<R65.2><C28><P4>(The signature here acknowledges only the amount proposed.)<P7>"
    "<R66><C28><From><C43><LINE><||3>"
    "<R66> <C28>Charges"
    "<R67><C28><P6>advanced:$ <P7><C45>  PER"
    "<RESTORE=LPI>"
    .

  v-printline = v-printline + 14. 
  /*IF v-printline < 45 THEN */
      PAGE.   

  v-printline = 0.
  v-grand-total-cases = 0.
  oe-bolh.printed = yes.
end. /* for each oe-bolh */
PROCEDURE create-tt-boll.
  DEF INPUT PARAM ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-boll.cases NO-UNDO.


  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll
      WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.line
        AND tt-boll.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-boll THEN DO:
    CREATE tt-boll.
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
     tt-boll.partial  = 0.
  END.

  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight).

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

