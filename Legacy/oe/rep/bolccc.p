/* ---------------------------------------------- oe/rep/bolccc.p 01/07 RDR */
/* PRINT Colonial Carton                                                         */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

DEF BUFFER xoe-bolh     FOR oe-bolh.
DEF BUFFER xoe-boll     FOR oe-boll.
DEF BUFFER xitemfg      FOR itemfg.
DEF BUFFER xxreport     FOR report.

{oe/rep/oe-lad.i}

DEF VAR v-salesman          AS   CHAR FORMAT "x(26)" NO-UNDO.
DEF VAR v-fob               AS   CHAR FORMAT "x(11)" NO-UNDO.
DEF VAR v-tot-cases         AS   INT FORMAT ">>>>9" NO-UNDO.
DEF VAR v-tot-wt            AS   DEC FORMAT "->,>>>,>>9" NO-UNDO.

DEF VAR v-tot-pkgs          AS   INT FORMAT ">>9" NO-UNDO.
DEF VAR v-ord-qty           LIKE oe-ordl.qty NO-UNDO.
DEF VAR v-bol-qty           LIKE oe-boll.qty NO-UNDO.
DEF VAR v-ship-qty          LIKE oe-ordl.ship-qty NO-UNDO.
DEF VAR v-bol-wt            AS   DEC NO-UNDO.
DEF VAR v-part-dscr         AS   CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-part-comp         AS   CHAR FORMAT "x" NO-UNDO.
DEF VAR v-part-qty          AS   DEC NO-UNDO.
DEF VAR v-ord-no            LIKE oe-boll.ord-no NO-UNDO.
DEF VAR v-po-no             LIKE oe-bolh.po-no NO-UNDO.
DEF VAR v-job-no            AS   CHAR FORMAT "x(13)" NO-UNDO.
DEF VAR v-phone-num         AS   CHAR FORMAT "x(13)" NO-UNDO.
DEF VAR v-dock-note AS   CHAR FORMAT "x(20)" NO-UNDO.

DEF VAR v-ship-name  LIKE shipto.ship-name NO-UNDO.
DEF VAR v-ship-addr  LIKE shipto.ship-addr NO-UNDO.
DEF VAR v-ship-city  LIKE shipto.ship-city NO-UNDO.
DEF VAR v-ship-state LIKE shipto.ship-state NO-UNDO.
DEF VAR v-ship-zip   LIKE shipto.ship-zip NO-UNDO.
DEF VAR v-ship-addr3 AS   CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-name  LIKE company.name NO-UNDO.
DEF VAR v-comp-addr  LIKE company.addr NO-UNDO.
DEF VAR v-comp-city  LIKE company.city NO-UNDO.
DEF VAR v-comp-state LIKE company.state NO-UNDO.
DEF VAR v-comp-zip   LIKE company.zip NO-UNDO.
DEF VAR v-comp-addr3 AS   CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-cust-addr3 AS   CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-1          LIKE oe-boll.cases INIT 1 NO-UNDO.
DEF VAR lv-cases LIKE oe-boll.cases NO-UNDO.

DEF VAR v-terms LIKE oe-ord.terms-d NO-UNDO.
DEF VAR v-frt-terms AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-zone LIKE carr-mtx.del-zone NO-UNDO.
DEF VAR v-pal AS INT NO-UNDO.

DEF WORKFILE w2 NO-UNDO
    FIELD cases            AS   INT FORMAT ">9"
    FIELD cas-cnt          AS   INT FORMAT ">>>>9".

DEF WORKFILE w3 NO-UNDO
    FIELD ship-i           AS   CHAR FORMAT "x(60)".

DEF VAR lv-pg-num AS INT NO-UNDO.
DEF VAR lv-tot-pg AS INT NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEFINE VARIABLE dReqDate AS DATE FORMAT "99/99/9999" NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\cccw.jpg".

FILE-INFO:FILE-NAME = ls-image1. 
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR ls-image2 AS cha NO-UNDO.
DEF VAR ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image2 = "images\worldpaclogo9.jpg".

FILE-INFO:FILE-NAME = ls-image2. 
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
DEF VAR v-t-tax      AS   DEC EXTENT 3 NO-UNDO.
DEF VAR v-bot-lab    AS   CHAR FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR lv-comp-unit AS INT NO-UNDO.
DEF VAR lv-cases-tot AS INT NO-UNDO.
DEF VAR lv-qty-tot AS INT NO-UNDO.
DEF VAR lv-qcase-tot AS INT NO-UNDO.
DEF VAR lv-partial-tot AS INT NO-UNDO.
DEF VAR lv-pal-tot AS INT NO-UNDO.
DEF VAR v-unit-qty AS cha NO-UNDO.
DEF VAR lv-prt-date AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR lv-prt-time AS CHARACTER NO-UNDO.
DEF VAR lv-prt-sts AS cha NO-UNDO.
DEF VAR lv-ord-type-code AS cha INIT ["C,N,O,Q,R,T,X"] NO-UNDO.   
DEF VAR lv-ord-type-list AS cha INIT 
    ["Change,New Customer,Original,Quality/Re-work,Repeat,Transfer,Complete Re-run"] NO-UNDO. 
DEF VAR v-lot-no AS CHAR NO-UNDO.
DEF VAR lv-print-img AS LOG NO-UNDO.
DEF VAR cPrintFormat AS CHARACTER NO-UNDO.

DEF VARIABLE li-hh AS INTEGER NO-UNDO.
DEF VARIABLE li-ss AS INTEGER NO-UNDO.
DEF VARIABLE li-mm AS INTEGER NO-UNDO.

DEFINE VARIABLE opcParsedText AS CHARACTER NO-UNDO EXTENT 100.
DEFINE VARIABLE opiArraySize AS INTEGER NO-UNDO.
Define Variable hNotesProc as Handle NO-UNDO.

RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProc.


ASSIGN tmpstore = FILL("-",80).

FIND FIRST oe-bolh NO-LOCK NO-ERROR.
FIND FIRST carrier NO-LOCK NO-ERROR.
FIND FIRST cust NO-LOCK NO-ERROR.
{sa/sa-sls01.i}

FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company = cocode
    AND sys-ctrl.NAME = "BOLFMT" NO-ERROR.

IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "CCCWPP" THEN lv-print-img = YES.
    ELSE lv-print-img = NO.
IF AVAIL sys-ctrl THEN cPrintFormat = sys-ctrl.char-fld .

FIND FIRST company NO-LOCK WHERE company.company EQ cocode .
FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ cocode .
ASSIGN v-comp-add1 = company.addr[1]
       v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
       v-comp-add3 = "Phone: 604.533.2545" 
       v-comp-add4 = "Fax  : 604.533.2633"
       v-printline = 0.

FOR EACH xxreport WHERE xxreport.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh)   EQ xxreport.rec-id,

    FIRST cust
    WHERE cust.company EQ cocode
      AND cust.cust-no EQ oe-bolh.cust-no
    NO-LOCK

    BREAK BY oe-bolh.bol-no:
      
    IF FIRST-OF(oe-bolh.bol-no) THEN DO:
    FIND FIRST carrier
        WHERE carrier.company EQ oe-bolh.company
          AND carrier.carrier EQ oe-bolh.carrier
        NO-LOCK NO-ERROR.

    /* tests where customer specific forms have CCCWPP*/
    DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ cocode
            AND sys-ctrl-shipto.NAME EQ "BOLFMT"
            AND sys-ctrl-shipto.cust-vend-no = cust.cust-no NO-ERROR.
        IF AVAIL sys-ctrl-shipto THEN
            IF sys-ctrl-shipto.char-fld = "CCCWPP" THEN
                lv-print-img = YES.
            ELSE lv-print-img = NO.
    END. /*end of Do block to test for CCCWPP per customer*/

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).
    ASSIGN
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-phone-num    = cust.area-code + cust.phone
     v-comp-name    = cust.name
     v-comp-addr[1] = cust.addr[1]
     v-comp-addr[2] = cust.addr[2]
     v-comp-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip
     v-dock-note = shipto.dock-hour
     dReqDate = oe-bolh.bol-date + shipto.spare-int-2
     lv-prt-date = oe-bolh.upd-date
     li-mm       = TRUNC(oe-bolh.upd-time / 60,0)
     li-hh       = TRUNC(li-mm / 60,0)
     li-mm       = li-mm - (li-hh * 60)
     li-ss       = oe-bolh.upd-time - (li-hh * 3600) - (li-mm * 60) 
     lv-prt-time = STRING(li-hh,"99") + ":" +
                     STRING(li-mm,"99") + ":" +
                     STRING(li-ss,"99") .
    
    IF TRIM(v-comp-addr3) EQ "," THEN v-comp-addr3 = "".
              
    IF v-comp-addr[2] EQ "" THEN
      ASSIGN
       v-comp-addr[2] = v-comp-addr3
       v-comp-addr3   = "".
    IF v-ship-addr[2] EQ "" THEN
      ASSIGN
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".
    IF TRIM(v-cust-addr3) EQ "," THEN v-cust-addr3 = "".

    ASSIGN
     v-salesman = ""
     v-fob      = ""
     v-terms    = "".


    FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK,
        FIRST oe-ord
	    WHERE oe-ord.company EQ oe-boll.company
	      AND oe-ord.ord-no  EQ oe-boll.ord-no
	    NO-LOCK:

      IF NOT AVAILABLE carrier THEN
      FIND FIRST carrier NO-LOCK WHERE carrier.company = oe-ord.company
        AND carrier.carrier = oe-ord.carrier NO-ERROR.

      DO i = 1 TO 3:
        IF oe-ord.sman[i] NE "" THEN
          v-salesman = TRIM(v-salesman) + " " + oe-ord.sman[i] + ",".
      END.

/*    if can-do("COD,CIA", oe-ord.terms) then v-terms = oe-ord.terms-d. */
      ASSIGN v-terms = oe-ord.terms-d
            /* v-frt-terms = if cust.frt-pay eq "P" then "Prepaid"
                           else if cust.frt-pay eq "B" then "Bill"
                           else if cust.frt-pay eq "C" then "Collect"
                           else if cust.frt-pay eq "T" then "Third Party"
                           else ""*/
          v-frt-terms = IF oe-bolh.frt-pay EQ "P" THEN "Prepaid"
                           ELSE IF oe-bolh.frt-pay EQ "B" THEN "PPD/Add"
                           ELSE IF oe-bolh.frt-pay EQ "C" THEN "Collect"
                           ELSE IF oe-bolh.frt-pay EQ "T" THEN "Third Party"
                           ELSE ""                             
             v-zone = cust.del-zone.
             
      IF v-terms EQ "" THEN
      DO:
        FIND FIRST terms NO-LOCK WHERE terms.t-code EQ oe-ord.terms  NO-ERROR.
        IF AVAIL terms THEN
          ASSIGN v-terms = terms.dscr.
      END.
      
      ASSIGN
      v-salesman = TRIM(v-salesman)
      v-po-no = oe-boll.po-no
      /*v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>"))*/ .
      IF v-salesman GT '' THEN
        IF substr(v-salesman,LENGTH(TRIM(v-salesman)),1) EQ "," THEN
          substr(v-salesman,LENGTH(TRIM(v-salesman)),1) = "".

      v-fob = IF oe-ord.fob-code BEGINS "ORIG" THEN "Origin" ELSE "Destination".
      
      LEAVE.
    END.

    FOR EACH w3:
      DELETE w3.
    END.
  END. /* first-of(oe-bolh.bol-no) */

  DO i = 1 TO 4:
    IF oe-bolh.ship-i[i] NE "" THEN DO:
      FIND FIRST w3 WHERE w3.ship-i EQ oe-bolh.ship-i[i] NO-ERROR.
      IF NOT AVAIL w3 THEN CREATE w3.
      w3.ship-i = oe-bolh.ship-i[i].
    END.
  END.


  FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no:


/*     FIND FIRST reftable WHERE                       */
/*          reftable.reftable = "oe-boll.lot-no" AND   */
/*          reftable.rec_key  = STRING(RECID(oe-boll)) */
/*          USE-INDEX rec_key                          */
/*          NO-LOCK NO-ERROR.                          */
/*                                                     */
/*     IF AVAIL reftable THEN                          */
/*        v-lot-no = reftable.CODE.                    */
/*     ELSE                                            */
/*        v-lot-no = "".                               */
    v-lot-no = oe-boll.lot-no.

    CREATE report.
    ASSIGN
     report.term-id  = v-term-id
     report.key-01   = oe-boll.i-no
     report.key-02   = STRING(oe-boll.ord-no,"9999999999")
     /* key-03 is for the break by to separate lines if any of these 4 are different */
     report.key-03   = oe-boll.i-no + oe-boll.po-no + string(oe-boll.qty-case) + v-lot-no
     report.rec-id   = RECID(oe-boll)
     oe-boll.printed = YES.
  END.
  IF LAST-OF(oe-bolh.bol-no) THEN DO:
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
     FOR EACH report WHERE report.term-id EQ v-term-id,
         FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id,
         FIRST xoe-bolh WHERE xoe-bolh.b-no EQ oe-boll.b-no NO-LOCK,
         FIRST itemfg WHERE itemfg.company EQ oe-boll.company
                      AND itemfg.i-no    EQ oe-boll.i-no NO-LOCK
                BREAK BY report.key-01
                BY report.key-02:
         ASSIGN v-tot-wt = v-tot-wt + oe-boll.weight.
   
         IF oe-boll.weight EQ 0 THEN
             v-tot-wt = v-tot-wt + (oe-boll.qty / 100 * itemfg.weight-100).

          /*========*/
          ln-cnt = ln-cnt + 2.
          FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ cocode
                          AND oe-ordl.ord-no  EQ oe-boll.ord-no
                          AND oe-ordl.i-no    EQ oe-boll.i-no NO-ERROR.

          IF oe-ordl.part-dscr1 <> "" OR oe-boll.partial > 0 THEN ln-cnt = ln-cnt + 1.
          IF itemfg.isaset THEN
          FOR EACH fg-set WHERE fg-set.company EQ cocode
	                       AND fg-set.set-no  EQ itemfg.i-no   NO-LOCK:

             FIND FIRST xitemfg NO-LOCK WHERE xitemfg.company EQ cocode
	                           AND xitemfg.i-no    EQ fg-set.part-no NO-ERROR.

             FIND FIRST fg-bin NO-LOCK WHERE fg-bin.company EQ cocode
                            AND fg-bin.i-no    EQ xitemfg.i-no
                            AND fg-bin.job-no = oe-boll.job-no
                            AND fg-bin.job-no2 = oe-boll.job-no2  NO-ERROR.
             ln-cnt = ln-cnt + 1.
             IF AVAIL fg-bin AND fg-bin.partial-count <> 0 THEN ln-cnt = ln-cnt + 1.
          END.
     END.
     /* end of dup loop */  
     ASSIGN
      lv-tot-pg = IF (ln-cnt MOD 18) = 0 THEN TRUNC( ln-cnt / 18,0)
                  ELSE lv-tot-pg + TRUNC( ln-cnt / 18,0)  /* 16->33 18 detail lines */
      /*  end of getting total page per po */

     lv-prt-sts = xxreport.key-09.

    {oe/rep/bolccc1.i}
  
    {oe/rep/bolcccx.i}

    v-last-page = PAGE-NUMBER.

    
    FOR EACH report WHERE report.term-id EQ v-term-id,
        FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK:
      DELETE report.
    END.
  END.

/*  PUT "<R39><C50><#7>Initial"                                                               */
/*      "<=7><C+10><FROM><R+2><C+20><RECT> "                                                  */
/*      "<R41><C50><#8><FROM><R+3><C+30><RECT> "                                              */
/*      "<=8><R+1> Total Pallets      :" v-tot-cases /*oe-bolh.tot-pallets*/ FORM ">,>>>,>>9".*/
/*                                                                                            */
/*PUT "<FBook Antiqua><R39><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP  .        */
/*                                                                                            */
/*IF v-dock-note NE "" THEN PUT v-dock-note AT 7 SKIP .                                       */
/*                                                                                            */
/*PUT                                                                                         */
/*    oe-bolh.ship-i[1] AT 7 SKIP                                                             */
/*    oe-bolh.ship-i[2] AT 7 SKIP                                                             */
/*    oe-bolh.ship-i[3] AT 7 SKIP                                                             */
/*    oe-bolh.ship-i[4] AT 7 SKIP                                                             */

PUT "<R32><C47><#7>Initial"
      "<=7><C+10><FROM><R+2><C+20><RECT> " 
      "<R34><C47><#8><FROM><R+3><C+30><RECT> " 
      "<=8><R+1> Total Pallets      :" v-tot-cases /*oe-bolh.tot-pallets*/ FORM ">,>>>,>>9".

PUT "<FBook Antiqua><R36><C1><P12><B>Shipping Instructions:</B><P9>" AT 1 SKIP.
PUT "<R36.5><C1>" AT 1 SKIP.

IF v-dock-note NE "" THEN PUT v-dock-note AT 1 SKIP .

/*PUT "<R39><C30>" SKIP.*/
RUN GetNotesArrayForObject IN hNotesProc (INPUT oe-bolh.rec_key, "ES", "", 130, NO, OUTPUT opcParsedText, OUTPUT opiArraySize).

DO i = 1 TO opiArraySize: 
    ASSIGN opcParsedText[i] = REPLACE(opcParsedText[i], CHR(13), "").
    ASSIGN opcParsedText[i] = REPLACE(opcParsedText[i], CHR(10), "").
    PUT
        opcParsedText[i] FORMAT "X(130)" AT 1 SKIP.
END.



PUT
    "_____________________________________________________________________________________________________________________________" SKIP
    "<B>  Signature of Receipt </B>" SKIP
    "Customer ________________________________________                       Carrier _______________________________________" AT 23 SKIP(1)
    "Date ____________________________________________                       Date __________________________________________" AT 23 SKIP   
    "<C1>" lv-prt-date "  " lv-prt-time "   "  caps(oe-bolh.USER-ID)  "   " lv-prt-sts "  " 
    "Page " AT 202 STRING(PAGE-NUMBER) /*STRING(PAGE-NUM - lv-pg-num,">>9")*/ + " of <#PAGES> "  FORM "x(20)" SKIP
    "<R51><C1><P6>RECEIVED, SUBJECT TO THE CLASSIFCATION AND LAWFULLY FILED TARIFFS IN EFFECT ON THE DATE OF THIS Bill of Lading. The property described above, except as noted, marked or consigned and" 
    "<R51.6><C1>destined as indicated below, which said carrier (the word carrier being understood through this contract as meaning any person or corporation in possession of the property under the contract) agrees to carry to" SKIP
    "<R52.2><C1>its usual place of delivery at said destination. Its is mutually agreed, as to each carrier of all or any property over all or any portion of said route to destination, as to each party at any time interested" SKIP
    "<R52.8><C1>in all or any of said property, that every service be performed hereunder shall be subject to all the terms and conditions of the Uniform Domestic Straight Bill of Lading set forth (1) in Uniform Freight Classification" SKIP
    "<R53.4><C1>in effect of the date hereof, if this is rail or water shipment or (2) in the applicable motor carrier classification or tariff if this is a motor shipment. Shipper/Receiver hereby certifies the he/she is familiar" SKIP
    "<R54.0><C1>with all the terms and conditions of the said bill of lading, set forth in the classification or tariff which governs the transportation of this shipment, and the said terms and conditions are herby agreed to" SKIP
    "<R54.6><C1>by the shipper/receiver and accepted for himself/herself and his assigns."  .

PUT "</B><P10><R56><C48><#9><FROM><R65><C80><RECT><||3>" SKIP.
PUT "<R57><C48><FROM><R57><C80><LINE><||3>" SKIP
    "<R58><C48><FROM><R58><C80><LINE><||3>" SKIP
    "<R57><C65><FROM><R58><C65><LINE><||3>" SKIP

    "<R63><C48><FROM><R63><C80><LINE><||3>" SKIP.
    
  PUT  "<R56><C58><P9><b> Shipment Inspection  </b>"     SKIP
    "<R57><C49>               Truck                                   <C70> Shipment  "    SKIP
    "<R58><C49> No Odors Present <C59.5> _______    All Qtys Match <C74> _______   "    SKIP
    "<R59><C49> No Debris Present <C59.5> _______  All Items Match <C74> _______   "    SKIP
    "<R60><C49> No Visible Leaks <C59.5> _______         No Damage <C74> _______   "    SKIP
    "<R61><C49> No Infestation <C59.5> _______       Cases Sealed <C74> _______   "    SKIP
                                            
    "<R63.5><C49>Sign: _______________             Date: _______________   "    SKIP
    .

    PUT "<FGCOLOR=RED><BGCOLOR=RED><LINECOLOR=RED>"
      /*"<=9><C+10><FROM><R+4><C+20><RECT> " */
      "<R57><C5><#15><FROM><R+4><C+25><RECT>" 
      "<=15><R+1>       DO NOT DOUBLE STACK      " SKIP 
      "<=15><R+2>       DO NOT BREAK DOWN        " "<FGCOLOR=BLACK><BGCOLOR=BLACK><LINECOLOR=BLACK>"  SKIP.

  v-printline = v-printline + 14.
  IF LAST-OF(oe-bolh.bol-no) THEN lv-pg-num = PAGE-NUM .
 
  /*IF v-printline < 45 THEN PUT SKIP(60 - v-printline).*/
  PAGE.
  ASSIGN
     v-printline = 0
     oe-bolh.printed = YES.

END. /* for each oe-bolh */


/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */

PROCEDURE get-pallets-num:

  DEF OUTPUT PARAM op-pallets AS DEC NO-UNDO.

  DEF VAR v-qty-pal AS DEC NO-UNDO.
  DEF VAR v-int AS DEC NO-UNDO.

  IF oe-boll.tot-pallets EQ 0 THEN
  DO:
     op-pallets = 0.
     RETURN.
  END.

  FIND FIRST fg-bin NO-LOCK
      WHERE fg-bin.company EQ cocode
        AND fg-bin.i-no    EQ oe-boll.i-no
        AND fg-bin.job-no  EQ oe-boll.job-no
        AND fg-bin.job-no2 EQ oe-boll.job-no2
        AND fg-bin.loc     EQ oe-boll.loc
        AND fg-bin.loc-bin EQ oe-boll.loc-bin
        AND fg-bin.tag     EQ oe-boll.tag
      NO-ERROR.  
  v-qty-pal = 1.
  IF AVAIL fg-bin THEN
     v-qty-pal = v-qty-pal *
                 (IF fg-bin.cases-unit EQ 0 THEN 1 ELSE fg-bin.cases-unit) *
                 (IF fg-bin.units-pallet EQ 0 THEN 1 ELSE fg-bin.units-pallet).

  IF v-qty-pal LE 1 THEN DO:
     v-int = INT(oe-boll.partial NE 0).
     IF oe-boll.qty LT 0 THEN v-int = v-int * -1.
  END.
  ELSE v-int = 0.

  v-qty-pal = (oe-boll.cases + v-int) / v-qty-pal.

  {sys/inc/roundup.i v-qty-pal}

  IF v-qty-pal LT 0 THEN
     v-qty-pal = v-qty-pal * -1.

  op-pallets = v-qty-pal.
  
END.
