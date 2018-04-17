/* ---------------------------------------------- oe/rep/bolacpi.p 05/18 RES */
/* PRINT ACPI BOL (from Elite)                                                */ 
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

DEFINE BUFFER xoe-bolh     FOR oe-bolh.
DEFINE BUFFER xoe-boll     FOR oe-boll.
DEFINE BUFFER xitemfg      FOR itemfg.
DEFINE BUFFER xxreport     FOR report.

{oe/rep/oe-lad.i}

DEFINE VARIABLE v-salesman          AS   CHARACTER FORMAT "x(26)" NO-UNDO.
DEFINE VARIABLE v-fob               AS   CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE VARIABLE v-tot-cases         AS   INTEGER FORMAT "->,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-tot-palls         AS   INTEGER FORMAT "->,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-tot-wt            AS   DECIMAL FORMAT "->>,>>>,>>9" NO-UNDO.

DEFINE VARIABLE v-tot-pkgs          AS   INTEGER FORMAT ">>9" NO-UNDO.
DEFINE VARIABLE v-pal-cnt           AS   DECIMAL NO-UNDO.
DEFINE VARIABLE v-ord-qty           LIKE oe-ordl.qty NO-UNDO.
DEFINE VARIABLE v-bol-qty           LIKE oe-boll.qty NO-UNDO.
DEFINE VARIABLE v-ship-qty          LIKE oe-ordl.ship-qty NO-UNDO.
DEFINE VARIABLE v-bol-wt            AS   DECIMAL NO-UNDO.
DEFINE VARIABLE v-part-dscr         AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-part-comp         AS   CHARACTER FORMAT "x" NO-UNDO.
DEFINE VARIABLE v-part-qty          AS   DECIMAL NO-UNDO.
DEFINE VARIABLE v-ord-no            LIKE oe-boll.ord-no NO-UNDO.
DEFINE VARIABLE v-po-no             LIKE oe-bolh.po-no NO-UNDO.
DEFINE VARIABLE v-job-no            AS   CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE VARIABLE v-phone-num         AS   CHARACTER FORMAT "x(13)" NO-UNDO.

DEFINE VARIABLE v-ship-name  LIKE shipto.ship-name NO-UNDO.
DEFINE VARIABLE v-ship-addr  LIKE shipto.ship-addr NO-UNDO.
DEFINE VARIABLE v-ship-city  LIKE shipto.ship-city NO-UNDO.
DEFINE VARIABLE v-ship-state LIKE shipto.ship-state NO-UNDO.
DEFINE VARIABLE v-ship-zip   LIKE shipto.ship-zip NO-UNDO.
DEFINE VARIABLE v-ship-addr3 AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-name  LIKE company.NAME NO-UNDO.
DEFINE VARIABLE v-comp-addr  LIKE company.addr NO-UNDO.
DEFINE VARIABLE v-comp-city  LIKE company.city NO-UNDO.
DEFINE VARIABLE v-comp-state LIKE company.state NO-UNDO.
DEFINE VARIABLE v-comp-zip   LIKE company.zip NO-UNDO.
DEFINE VARIABLE v-comp-addr3 AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-cust-addr3 AS   CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-1          LIKE oe-boll.cases INIT 1 NO-UNDO.
DEFINE VARIABLE v-terms LIKE oe-ord.terms-d NO-UNDO.
DEFINE VARIABLE v-frt-terms AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-zone LIKE carr-mtx.del-zone NO-UNDO.
DEFINE VARIABLE v-lines AS INTEGER NO-UNDO.
DEFINE VARIABLE v-job-po            AS   CHARACTER NO-UNDO.
DEFINE TEMP-TABLE w2 NO-UNDO
    FIELD cases            AS   INTEGER FORMAT ">9"
    FIELD cas-cnt          AS   INTEGER FORMAT ">>>>9"
    FIELD rec-id AS RECID
    FIELD i-no LIKE oe-ordl.i-no
    FIELD job-po AS cha
    FIELD qty AS INTEGER 
    FIELD dscr LIKE oe-ordl.part-dscr1
    FIELD partial          AS INTEGER
    FIELD unitcount        AS INTEGER 
    FIELD qty-sum          AS INTEGER .

DEFINE TEMP-TABLE w3 NO-UNDO
    FIELD ship-i           AS   CHARACTER FORMAT "x(60)".

/* === with xprint ====*/
DEFINE VARIABLE ls-image1 AS cha NO-UNDO.
DEFINE VARIABLE ls-image2 AS cha NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE ls-full-img2 AS cha FORM "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\AmericanPoly.jpg"
       ls-image2 = "images\AmericanPoly.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">"
       FILE-INFO:FILE-NAME = ls-image2
       ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEFINE VARIABLE v-tel AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact AS cha FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

DEFINE VARIABLE v-line-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-quo-total AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-t-tax      AS   DECIMAL EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab    AS   CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-q-no LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE v-printline AS INTEGER NO-UNDO.
DEFINE VARIABLE ll-display-comp AS LOG NO-UNDO.  /* display company address */
DEFINE VARIABLE ll-consol-bolls AS LOG NO-UNDO.
DEFINE VARIABLE lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-email AS cha FORM "x(56)" NO-UNDO.
DEFINE VARIABLE lv-bolfmt-int AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-comp-color AS cha NO-UNDO.
DEFINE VARIABLE lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEFINE VARIABLE v-cusx-add1 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add2 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add3 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add4 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-add5 AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-email AS cha NO-UNDO.
DEFINE VARIABLE v-cusx-name AS cha NO-UNDO.
DEFINE VARIABLE lv-pg-num AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-tot-pg AS INTEGER NO-UNDO.
DEFINE VARIABLE ln-cnt AS INTEGER NO-UNDO.
DEFINE BUFFER b-itemfg     FOR itemfg.
DEFINE BUFFER bf-ttboll FOR tt-boll.
DEFINE VARIABLE v-tot-case-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE icountpallet AS INTEGER NO-UNDO .
FORM w2.i-no                         FORMAT "x(15)"
     w2.job-po                       AT 17 FORMAT "x(15)"
     w2.dscr                    AT 33 FORMAT "x(30)"
     w2.cases                       TO 70 FORMAT "->>>>"
     /*w2.cas-cnt*/ icountpallet    TO 78 FORMAT "->>>>>>"
     tt-boll.partial                TO 85 FORMAT "->>>>>>"
     tt-boll.qty                    TO 92 FORMAT "->>>>>>"
     bf-ttboll.p-c                  AT 96
    WITH FRAME bol-mid DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 110.

FORM oe-ordl.i-no                         FORMAT "x(15)"
     v-job-po                       AT 17 FORMAT "x(15)"
     v-part-dscr                    AT 33 FORMAT "x(30)"
     w2.cases                       TO 70 FORMAT "->>>9"
     icountpallet                   TO 78 FORMAT "->>>>>9"
     tt-boll.partial                TO 85 FORMAT "->>>9"
     tt-boll.qty                    TO 93 FORMAT "->>>>>9"
     tt-boll.p-c                    AT 96
    WITH FRAME bol-mid2 DOWN NO-BOX NO-LABELS STREAM-IO WIDTH 130.
ASSIGN tmpstore = FILL("-",130).

FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "BOLFMT"  NO-ERROR.
ASSIGN
 ll-display-comp = AVAILABLE sys-ctrl AND sys-ctrl.log-fld
 ll-consol-bolls = AVAILABLE sys-ctrl AND sys-ctrl.int-fld NE 0
 lv-bolfmt-int   = IF AVAILABLE sys-ctrl THEN sys-ctrl.int-fld ELSE 0.

FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "LOGOCOLR" NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST company NO-LOCK WHERE company.company = cocode  NO-ERROR.
ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = ""
       v-comp-add5 = ""
       lv-email = ""
       lv-comp-name = "".

IF ll-display-comp THEN DO:
   FIND FIRST cust NO-LOCK WHERE cust.company = cocode AND
                         cust.active = "X"  NO-ERROR.
  IF AVAILABLE cust THEN
     IF cust.addr[2] EQ '' THEN
     ASSIGN v-comp-add1 = ''
            v-comp-add2 = cust.addr[1].
     ELSE
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2].
     ASSIGN v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            v-cusx-add1 = v-comp-add1
            v-cusx-add2 = v-comp-add2
            v-cusx-add3 = v-comp-add3
            v-cusx-add4 = v-comp-add4
            v-cusx-add5 = v-comp-add5
            v-cusx-email = lv-email
            v-cusx-name = lv-comp-name
            .
END.

FIND FIRST oe-bolh NO-LOCK NO-ERROR.
FIND FIRST carrier NO-LOCK NO-ERROR.
FIND FIRST cust NO-LOCK NO-ERROR.

{sa/sa-sls01.i}

FIND FIRST oe-ctrl NO-LOCK WHERE oe-ctrl.company EQ cocode .

v-printline = 0.

FOR EACH xxreport WHERE xxreport.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh)   EQ xxreport.rec-id,
    FIRST cust
    WHERE cust.company EQ cocode
      AND cust.cust-no EQ oe-bolh.cust-no
    NO-LOCK
    BREAK BY oe-bolh.bol-no:
      
    IF FIRST-OF(oe-bolh.bol-no) THEN DO:
    FIND FIRST carrier NO-LOCK
        WHERE carrier.company EQ oe-bolh.company
          AND carrier.carrier EQ oe-bolh.carrier
        NO-ERROR.

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
     v-phone-num    = cust.area-code + cust.phone.
     
    IF shipto.broker THEN 
       ASSIGN
       v-comp-add1 = cust.addr[1]
       v-comp-add2 = cust.addr[2]
       v-comp-add3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip
       v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
       v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
       lv-email    = "Email:  " + cust.email   
       lv-comp-name = cust.NAME .

    ELSE ASSIGN v-comp-add1 = v-cusx-add1
                v-comp-add2 = v-cusx-add2    
                v-comp-add3 = v-cusx-add3    
                v-comp-add4 = v-cusx-add4                
                v-comp-add5 = v-cusx-add5
                lv-email    = v-cusx-email
                lv-comp-name = v-cusx-name.
        
    ASSIGN
       v-comp-name    = cust.name
       v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
       v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

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

    FOR EACH oe-boll NO-LOCK WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no ,
        FIRST oe-ord NO-LOCK
	    WHERE oe-ord.company EQ oe-boll.company
	      AND oe-ord.ord-no  EQ oe-boll.ord-no
	    :

      IF NOT AVAILABLE carrier THEN
      FIND FIRST carrier NO-LOCK
           WHERE carrier.company = oe-ord.company
             AND carrier.carrier = oe-ord.carrier  NO-ERROR.

      DO i = 1 TO 3:
        IF oe-ord.sman[i] NE "" THEN
          v-salesman = TRIM(v-salesman) + " " + oe-ord.sman[i] + ",".
      END.

      ASSIGN v-terms = oe-ord.terms-d
             v-frt-terms = IF oe-ord.frt-pay EQ "P" THEN "Prepaid"
                           ELSE IF oe-ord.frt-pay EQ "B" THEN "Bill"
                           ELSE IF oe-ord.frt-pay EQ "C" THEN "Collect"
                           ELSE IF oe-ord.frt-pay EQ "T" THEN "Third Party"
                           ELSE ""
             v-zone = cust.del-zone.
             
      IF v-terms EQ "" THEN
      DO:
        FIND FIRST terms NO-LOCK WHERE terms.t-code EQ oe-ord.terms  NO-ERROR.
        IF AVAILABLE terms THEN
          ASSIGN v-terms = terms.dscr.
      END.
      
      ASSIGN
      v-salesman = TRIM(v-salesman)
      v-po-no = oe-boll.po-no
      v-job-no = IF oe-boll.job-no = "" THEN "" ELSE (oe-boll.job-no + "-" + STRING(oe-boll.job-no2,">>")).
      IF v-salesman GT '' THEN
        IF substr(v-salesman,LENGTH(TRIM(v-salesman)),1) EQ "," THEN
          substr(v-salesman,LENGTH(TRIM(v-salesman)),1) = "".

      v-fob = IF oe-ord.fob-code BEGINS "ORIG" THEN "Origin" ELSE "Destination".
      LEAVE.
    END.

    FOR EACH w3:
      DELETE w3.
    END.

    FOR EACH tt-boll:
      DELETE tt-boll.
    END.
  END. /* first-of(oe-bolh.bol-no) */

  DO i = 1 TO 4:
    IF oe-bolh.ship-i[i] NE "" THEN DO:
      FIND FIRST w3 WHERE w3.ship-i EQ oe-bolh.ship-i[i] NO-ERROR.
      IF NOT AVAILABLE w3 THEN CREATE w3.
      w3.ship-i = oe-bolh.ship-i[i].
    END.
  END.

  FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company AND oe-boll.b-no EQ oe-bolh.b-no:
    IF ll-consol-bolls THEN DO:
      IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).
    
      /*IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
        RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).*/
        
    END.

    ELSE DO:
      CREATE tt-boll.
      BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll.
    END.

    oe-boll.printed = YES.
  END.

  IF LAST-OF(oe-bolh.bol-no) THEN DO:
     IF v-comp-addr[2] = "" THEN
        ASSIGN v-comp-addr[2] = v-comp-addr3
               v-comp-addr3 = "".
     IF v-ship-addr[2] = "" THEN
        ASSIGN v-ship-addr[2] = v-ship-addr3
               v-ship-addr3 = "".

     ASSIGN
        lv-tot-pg = 1
        ln-cnt = 0.

     FOR EACH tt-boll,
         FIRST xoe-bolh NO-LOCK WHERE xoe-bolh.b-no EQ tt-boll.b-no ,
         FIRST itemfg NO-LOCK WHERE itemfg.company EQ tt-boll.company
                      AND itemfg.i-no    EQ tt-boll.i-no 
         BREAK BY tt-boll.i-no
         BY tt-boll.po-no
         BY tt-boll.ord-no
         BY tt-boll.line
         BY tt-boll.cases DESCENDING:                 
         IF ll-consol-bolls THEN DO:
            IF FIRST-OF(tt-boll.LINE) THEN DO:
               ln-cnt = ln-cnt + 3.
               FIND FIRST oe-ordl NO-LOCK
                             WHERE oe-ordl.company EQ cocode
                               AND oe-ordl.ord-no  EQ tt-boll.ord-no
                               AND oe-ordl.i-no    EQ tt-boll.i-no
                               AND oe-ordl.line    EQ tt-boll.LINE  NO-ERROR.
               IF AVAILABLE oe-ordl AND oe-ordl.part-dscr1 <> "" THEN ln-cnt = ln-cnt + 1.
               IF AVAILABLE oe-ordl AND oe-ordl.part-dscr2 <> "" THEN ln-cnt = ln-cnt + 1.

               IF v-print-components AND itemfg.alloc NE YES THEN
               FOR EACH fg-set NO-LOCK
                   WHERE fg-set.company EQ cocode
                     AND fg-set.set-no  EQ tt-boll.i-no ,
                 FIRST b-itemfg NO-LOCK
                    WHERE b-itemfg.company EQ cocode
                      AND b-itemfg.i-no    EQ fg-set.part-no :
                 ln-cnt = ln-cnt + 1.
               END.
               i = 0.
               FOR EACH bf-ttboll WHERE bf-ttboll.i-no = tt-boll.i-no
                             AND bf-ttboll.po-no = tt-boll.po-no
                             AND bf-ttboll.ord-no = tt-boll.ord-no
                             AND bf-ttboll.LINE = tt-boll.LINE:
                   i = i + 1.
               END.
               ln-cnt = IF i > 2 THEN (ln-cnt + i - 2)
                        ELSE ln-cnt.
            END.
         END. /* ll-consol-bolls */
         ELSE DO:
           ln-cnt = ln-cnt + 3.
           FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ cocode
                               AND oe-ordl.ord-no  EQ tt-boll.ord-no
                               AND oe-ordl.i-no    EQ tt-boll.i-no
                               AND oe-ordl.line    EQ tt-boll.LINE  NO-ERROR.
           IF AVAILABLE oe-ordl AND oe-ordl.part-dscr1 <> "" THEN ln-cnt = ln-cnt + 1.
           IF AVAILABLE oe-ordl AND oe-ordl.part-dscr2 <> "" THEN ln-cnt = ln-cnt + 1.

           IF v-print-components AND itemfg.alloc NE YES THEN
             FOR EACH fg-set NO-LOCK WHERE fg-set.company EQ cocode
                               AND fg-set.set-no  EQ tt-boll.i-no ,
                 FIRST b-itemfg NO-LOCK WHERE b-itemfg.company EQ cocode
                                AND b-itemfg.i-no    EQ fg-set.part-no :
                 ln-cnt = ln-cnt + 1.
           END.
         END.
     END.
     /* end of dup loop */
      lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 27,0) .  /* 23->47 25 po detail lines */
      /*  end of getting total page per po */

     {oe/rep/bolacpi.i}
     {oe/rep/bolacpi2.i}

  v-last-page = PAGE-NUMBER.

  IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.

  PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
      "<=8><R+1> Total Units       :" v-tot-palls
      "<=8><R+3> Total Weight      :" v-tot-wt.

  IF oe-bolh.ship-i[1] NE "" OR
     oe-bolh.ship-i[2] NE "" OR
     oe-bolh.ship-i[3] NE "" OR
     oe-bolh.ship-i[4] NE "" THEN
     PUT "<FArial><R52><C1><P12><B>     Shipping Instructions: </B> <P9> " 
         "<R54><C1>" oe-bolh.ship-i[1] 
         "<R55><C1>" oe-bolh.ship-i[2] 
         "<R56><C1>" oe-bolh.ship-i[3] 
         "<R57><C1>" oe-bolh.ship-i[4].

  PUT
    "<R59><C1>"
    "__________________________________________________________________________________________________________________" 
    "<R60><C1>" "<B>  Signature of Receipt </B>" 
    "<R61><C1>" "Customer ________________________________________                       Carrier _______________________________________" 
    "<R63><C1>" "Date ____________________________________________                       Date _________________________________________".

  v-printline = v-printline + 14.
  IF LAST-OF(oe-bolh.bol-no) THEN lv-pg-num = PAGE-NUMBER .

  PAGE.
  v-printline = 0.

  FOR EACH report WHERE report.term-id EQ v-term-id,
      FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id :
    DELETE report.
  END.

  END.  /* last-of*/

  oe-bolh.printed = YES.
END. /* for each oe-bolh */

RETURN.

PROCEDURE create-tt-boll.
  DEFINE INPUT PARAMETER ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEFINE INPUT PARAMETER ip-cases    LIKE oe-boll.cases NO-UNDO.

  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll NO-LOCK
      WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.LINE 
        AND (tt-boll.qty-sum EQ (ip-qty-case * ip-cases))
      NO-ERROR.

  IF NOT AVAILABLE tt-boll THEN DO:
      
    CREATE tt-boll.
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
     tt-boll.qty-sum = ip-qty-case * ip-cases
     tt-boll.partial  = 0 .
   
  END.

  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight)
   tt-boll.partial = tt-boll.partial + oe-boll.partial 
   tt-boll.unitCount =  tt-boll.unitCount + 1 .

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
