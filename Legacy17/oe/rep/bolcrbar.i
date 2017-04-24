/* ---------------------------------------------- oe/rep/bolcapbc.p 12/05 YSK */
/* PRINT BOL for Consolidated Box  */
/* -------------------------------------------------------------------------- */

DEF VAR vcarrdesc AS CHAR NO-UNDO.
DEF VAR vzonedesc AS CHAR NO-UNDO.
DEF VAR vterrdesc AS CHAR NO-UNDO.

DEF VAR li-hh AS INT NO-UNDO.
DEF VAR li-ss AS INT NO-UNDO.
DEF VAR li-mm AS INT NO-UNDO.
DEF VAR appt-time AS CHAR FORMAT "x(6)" NO-UNDO.

assign tmpstore = fill("-",80).

DEF VAR check-des AS INT NO-UNDO .
DEF VAR v-rel-no AS CHAR NO-UNDO .
find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
/*{sa/sa-sls01.i}*/
/*DEF VAR lv-bolfmt-int AS INT NO-UNDO.*/
find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "BOLFMT" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-bolfmt-int = sys-ctrl.int-fld.
ELSE lv-bolfmt-int = 0.

find first company where company.company eq cocode no-lock.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock.
ASSIGN v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.533.2545" 
       v-comp-add4 = "Fax  : 604.533.2633"
       v-printline = 0.

for each xxreport where xxreport.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh)   eq xxreport.rec-id,

    first cust
    where cust.company eq cocode
      and cust.cust-no eq oe-bolh.cust-no
    no-lock

    break by oe-bolh.bol-no:
    
      
  if first-of(oe-bolh.bol-no) then do:

    ASSIGN
       lv-qty-sum = 0
       lv-qty-sum2 = 0.

    ASSIGN
     li-mm       = TRUNC(oe-bolh.upd-time / 60,0)
     li-hh       = TRUNC(li-mm / 60,0)
     li-mm       = li-mm - (li-hh * 60)
     li-ss       = oe-bolh.upd-time - (li-hh * 3600) - (li-mm * 60) 
     appt-time   = STRING(li-hh,"99") + ":" +
                   STRING(li-mm,"99") 
                   /*STRING(li-ss,"99")*/. 
    
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

    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      if not available carrier then
      find first carrier where carrier.company = oe-ord.company
        and carrier.carrier = oe-ord.carrier no-lock no-error.
      IF AVAIL carrier THEN
      ASSIGN vcarrdesc = carrier.dscr .
      ELSE vcarrdesc = ""  .

      FIND FIRST carr-mtx WHERE carr-mtx.company = cocode
                                AND carr-mtx.del-zone = cust.del-zone NO-LOCK NO-ERROR.
      IF AVAIL carr-mtx THEN
          ASSIGN
          vzonedesc = carr-mtx.del-dscr .
      ELSE vzonedesc = "" .

     FIND FIRST terr WHERE terr.company = cocode
                                AND terr.terr = cust.terr NO-LOCK NO-ERROR.
      IF AVAIL terr THEN
          ASSIGN
          vterrdesc = terr.dscr .
      ELSE vterrdesc = "" .

      

      do i = 1 to 3:
        if oe-ord.sman[i] ne "" then
          v-salesman = trim(v-salesman) + " " + oe-ord.sman[i] + ",".
      end.

/*    if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d. */
      assign v-terms = oe-ord.terms-d
             /*v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
                           else ""*/                           
             v-zone = cust.del-zone.
             
      if v-terms eq "" then
      do:
        find first terms where terms.t-code eq oe-ord.terms no-lock no-error.
        if avail terms then
          assign v-terms = terms.dscr.
      end.
      
      ASSIGN
         v-salesman = trim(v-salesman)
         /*v-po-no = oe-boll.po-no*/
         v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>"))
         v-ord-no = oe-boll.ord-no
         v-fob = if oe-ord.fob-code begins "ORIG" then "Origin" else "Destination".
        
         ASSIGN
            check-des =  INDEX(oe-boll.po-no,"-") .
         IF check-des > 0 THEN
             ASSIGN
             v-po-no   =  SUBSTRING(oe-boll.po-no,1,check-des - 1)
             v-rel-no  =  SUBSTRING(oe-boll.po-no,check-des + 1,40) .
         ELSE
            ASSIGN
                v-po-no   =  oe-boll.po-no
                v-rel-no  =  "" .

      if v-salesman gt '' then
        if substr(v-salesman,length(trim(v-salesman)),1) eq "," then
          substr(v-salesman,length(trim(v-salesman)),1) = "".
      
      LEAVE.
    end.

    for each w3:
      delete w3.
    end.
    FOR EACH tt-boll:
      DELETE tt-boll.
      FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll) NO-ERROR.
      IF AVAIL tt-boll2 THEN DELETE tt-boll2.
    END.
    v-frt-terms = if oe-bolh.frt-pay eq "P" then "Prepaid"
                  else if oe-bolh.frt-pay eq "B" then "Bill"
                  else if oe-bolh.frt-pay eq "C" then "Collect"
                  else if oe-bolh.frt-pay eq "T" then "Third Party"
                  else "".

  end. /* first-of(oe-bolh.bol-no) */

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then do:
      find first w3 where w3.ship-i eq oe-bolh.ship-i[i] no-error.
      if not avail w3 then create w3.
      w3.ship-i = oe-bolh.ship-i[i].
    end.
  end.
  v-frt-class  = "".
  for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
    /*create report.
    assign
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = string(oe-boll.ord-no,"9999999999")
     report.rec-id   = recid(oe-boll) */
      IF v-frt-class = "" THEN do:
         FIND FIRST itemfg WHERE itemfg.company = oe-bolh.company
                             AND itemfg.i-no = oe-boll.i-no NO-LOCK NO-ERROR.
         v-frt-class = IF AVAIL itemfg AND itemfg.frt-class <> "" THEN itemfg.frt-class 
                       ELSE "C".
                      
      END.
     /* IF ll-consol-bolls THEN DO:
         IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
            RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).

         IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
            RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).
      END.
      ELSE DO: */
        CREATE tt-boll.
        BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
        CREATE tt-boll2.
        RUN get-pallets-num ( OUTPUT v-pal).
        ASSIGN
           tt-boll2.rec-id = RECID(tt-boll)
           tt-boll2.pallets = v-pal.
     /* END.*/
               
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
        ln-cnt = 0.
     /*
     for each report where report.term-id eq v-term-id,
         first oe-boll where recid(oe-boll) eq report.rec-id,
         first xoe-bolh where xoe-bolh.b-no eq oe-boll.b-no no-lock,
         first itemfg where itemfg.company eq oe-boll.company
                      and itemfg.i-no    eq oe-boll.i-no no-lock
                break by report.key-01
                by report.key-02:
         ASSIGN v-tot-wt = v-tot-wt + oe-boll.weight.
   
         if oe-boll.weight eq 0 then
             v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).

          /*========*/
          ln-cnt = ln-cnt + 4.          
     END. */
     FOR EACH tt-boll,
         first xoe-bolh where xoe-bolh.b-no eq tt-boll.b-no no-lock,
         first itemfg where itemfg.company eq tt-boll.company
                      and itemfg.i-no    eq tt-boll.i-no no-lock
        BREAK BY tt-boll.i-no
         BY tt-boll.po-no
         BY tt-boll.ord-no
         BY tt-boll.line
         BY tt-boll.cases DESC:                 
         ASSIGN v-tot-wt = v-tot-wt + tt-boll.weight.
        IF (lv-bolfmt-int = 1 AND FIRST-OF(tt-boll.ord-no)) OR
            lv-bolfmt-int <> 1
        THEN DO:
         ln-cnt = ln-cnt + IF lv-bolfmt-int = 1 THEN 5 ELSE 3.
         if itemfg.isaset AND v-print-components then
            for each fg-set where fg-set.company eq cocode
	                    and fg-set.set-no  eq itemfg.i-no   no-lock:

                find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.
                FIND FIRST fg-bin where fg-bin.company eq cocode
                            and fg-bin.i-no    eq xitemfg.i-no
                            and fg-bin.job-no = tt-boll.job-no
                            AND fg-bin.job-no2 = tt-boll.job-no2 NO-LOCK NO-ERROR.

                IF AVAIL fg-bin THEN DO:
                   ln-cnt = ln-cnt + 1.
                   IF fg-bin.partial-count <> 0 THEN ln-cnt = ln-cnt + 1.
                END.
                ELSE ln-cnt = ln-cnt + 1.
         END.
       END.
     END.
     /* end of dup loop */
     IF lv-bolfmt-int = 1 THEN
        lv-tot-pg = IF ln-cnt MOD 26 = 0 THEN TRUNC( ln-cnt / 26,0)
                    ELSE lv-tot-pg + TRUNC( ln-cnt / 26,0) .  /* 16->34 19 po detail lines */
     ELSE lv-tot-pg = IF ln-cnt MOD 24 = 0 THEN TRUNC( ln-cnt / 24,0)
                    ELSE lv-tot-pg + TRUNC( ln-cnt / 24,0) .  /* 16->34 19 po detail lines */
      /*  end of getting total page per po */

     {oe/rep/bolcr1bar.i}
     {oe/rep/bolcr2bar.i}
     
     v-last-page = page-number.

    /*for each report where report.term-id eq v-term-id,
        first oe-boll where recid(oe-boll) eq report.rec-id no-lock:
      delete report.
    end.*/

  end.
  v-tot-palls = oe-bolh.tot-pallet.
  /*PUT 
      "<R54><C50><#8><FROM><R+3><C+30><RECT> " 
      "<=8> Total Pieces Shipped :" lv-qty-sum2 FORM "->,>>>,>>9"
      "<=8><R+1> Total Pallets        :" /*oe-bolh.tot-pallets*/ v-tot-palls FORM ">,>>>,>>9"
      /*"<=8><R+2> Total Cases          :" v-tot-cases FORM ">,>>>,>>9"*/
      "<=8><R+2> Total weight         :" v-tot-wt FORM ">,>>>,>>9.99".*/
  

CASE v-frt-class:
    WHEN "A" THEN v-frt-class = v-frt-class + "                                 55".
    WHEN "B" THEN v-frt-class = v-frt-class + "                                150".
    WHEN "C" THEN v-frt-class = v-frt-class + "                                250".
END CASE.

/*PUT "<FBook Antiqua><R52><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP(1)
    oe-bolh.ship-i[1] AT 7 SKIP
    oe-bolh.ship-i[2] AT 7 SKIP
    oe-bolh.ship-i[3] AT 7 SKIP
    oe-bolh.ship-i[4] AT 7 SKIP
    "_________________________________________________________________________________________________________________________________" SKIP
    "<B>  Signature of Receipt </B>" SKIP
    "Customer ________________________________________                       Carrier _______________________________________" AT 23 SKIP(1)
    "Date ____________________________________________                       Date __________________________________________" AT 23 SKIP   
    .*/


  v-printline = v-printline + 14.
  IF last-of(oe-bolh.bol-no) THEN lv-pg-num = PAGE-NUM .

  IF v-printline < 45 THEN PUT SKIP(60 - v-printline).
  ASSIGN
     v-printline = 0
     oe-bolh.printed = yes.

end. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
/*
PROCEDURE get-pallets-num:
DEF OUTPUT PARAM op-pallets AS dec NO-UNDO.
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
   op-pallets = (IF AVAIL fg-bin AND fg-bin.cases-unit > 1 THEN oe-boll.qty - oe-boll.partial
                 ELSE oe-boll.qty) / v-qty-pal.

else
  assign
   op-pallets = 1.

IF op-pallets = ? THEN op-pallets = 1.
{sys/inc/roundup.i op-pallets}
if op-pallets lt 0 then op-pallets = op-pallets * -1.
END.


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
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
    CREATE tt-boll2.
    tt-boll2.rec-id = RECID(tt-boll).
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
     tt-boll.partial  = 0
     tt-boll2.pallets = 0.
  END.
  RUN get-pallets-num ( OUTPUT v-pal). 
  FIND FIRST tt-boll2 WHERE tt-boll2.rec-id = RECID(tt-boll).
  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight)
   tt-boll2.pallets = tt-boll2.pallets + v-pal   .

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.
*/
