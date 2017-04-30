/* ---------------------------------------------- oe/rep/cocpryst.p 07/06 YSK */
/* Print Xprint COC (Certificate of Compliance)                                */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEFINE BUFFER xoe-bolh     FOR oe-bolh.
DEFINE BUFFER xoe-boll     FOR oe-boll.
DEFINE BUFFER xitemfg      FOR itemfg.
DEFINE BUFFER bf-oe-boll   FOR oe-boll .

{oe/rep/oe-lad.i}

DEFINE VARIABLE v-fob               AS   CHARACTER FORMAT "x(7)".
DEFINE VARIABLE v-terms             AS   CHARACTER FORMAT "x(12)".
DEFINE VARIABLE v-name              AS   CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-price             AS   INTEGER FORMAT ">>>>>>>>".
DEFINE VARIABLE v-tot-price         AS   INTEGER FORMAT ">>>>>>>>>>".
DEFINE VARIABLE v-dscr              AS   CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-ord-bol           AS   CHARACTER FORMAT "x(15)".
DEFINE VARIABLE v-ord-no            AS   INTEGER  FORMAT ">>>>>>>".
DEFINE VARIABLE v-ord-date          LIKE oe-ord.ord-date.
DEFINE VARIABLE v-part-qty          AS   DECIMAL.
DEFINE VARIABLE v-po-no             LIKE oe-bolh.po-no EXTENT 2.
DEFINE VARIABLE v-bol-qty           LIKE oe-boll.qty.
DEFINE VARIABLE v-qty-alf           AS   CHARACTER FORMAT "x(10)".
DEFINE VARIABLE v-bin               AS   CHARACTER FORMAT "x(22)" EXTENT 4.
DEFINE VARIABLE v-ship-i            AS   CHARACTER EXTENT 2 FORMAT "x(60)".
DEFINE VARIABLE cases               AS   INTEGER NO-UNDO.
DEFINE VARIABLE qty-cases           AS   INTEGER NO-UNDO.
DEFINE VARIABLE pallet              AS   INTEGER NO-UNDO.
DEFINE VARIABLE v-line-count        AS   INTEGER NO-UNDO.


DEFINE VARIABLE v-ship-name  LIKE shipto.ship-name.
DEFINE VARIABLE v-ship-addr  LIKE shipto.ship-addr.
DEFINE VARIABLE v-ship-city  LIKE shipto.ship-city.
DEFINE VARIABLE v-ship-state LIKE shipto.ship-state.
DEFINE VARIABLE v-ship-zip   LIKE shipto.ship-zip.
DEFINE VARIABLE v-ship-addr3 AS   CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-cust-addr3 AS   CHARACTER FORMAT "x(30)".
/* === with xprint ====*/
DEFINE VARIABLE ls-image1      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-image2      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1   AS CHARACTER FORM "x(150)" NO-UNDO.
DEFINE VARIABLE ls-full-img2   AS CHARACTER FORM "x(150)" NO-UNDO.
DEFINE VARIABLE v-printline    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-carrier-dscr LIKE carrier.dscr NO-UNDO.

ASSIGN ls-image1 = "images\prystup.bmp".

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
FILE-INFO:FILE-NAME = ls-image2.
ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEFINE WORKFILE w1
  FIELD part-no LIKE fg-set.part-no
  FIELD rec-id  AS   RECID.

FIND FIRST oe-bolh NO-LOCK NO-ERROR.
FIND FIRST carrier NO-LOCK NO-ERROR.
FIND FIRST cust    NO-LOCK NO-ERROR.
FIND FIRST report  NO-LOCK  NO-ERROR.
/*
form header
     skip(9)
     cust.name                    at 17
     v-ship-name                  at 63
     cust.addr[1]                 at 17
     v-ship-addr[1]               at 63
     cust.addr[2]                 at 17
     v-ship-addr[2]               at 63
     v-cust-addr3                 at 17
     v-ship-addr3                 at 63
     skip(1)
     v-po-no[1]                   at 33
     v-po-no[2]                   at 77
     skip(2)
     v-ord-date                   at 9
     oe-bolh.bol-date             at 23
     carrier.dscr                 at 37 format "x(20)" when avail carrier
     v-fob                        at 58
     v-ord-bol                    at 78
     skip(4)
    with frame coc-top page-top no-box no-underline width 102.
*/
FORM v-qty-alf                    AT 7
     SPACE(4)
     itemfg.i-name
     SKIP(1)
     
    WITH FRAME coc-mid NO-BOX NO-UNDERLINE NO-LABELS DOWN WIDTH 102.

FORM HEADER
     SKIP(5)
     v-ship-i[1]                  AT 19 
     v-ship-i[2]                  AT 8     
    WITH FRAME coc-tot PAGE-BOTTOM NO-BOX NO-UNDERLINE NO-LABELS DOWN WIDTH 102.

/*view frame coc-top.*/

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.
FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

{sa/sa-sls01.i}

FOR EACH report NO-LOCK WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh) EQ report.rec-id:

  FOR EACH oe-boll NO-LOCK
      WHERE oe-boll.company EQ cocode
      AND oe-boll.b-no    EQ oe-bolh.b-no:
    RELEASE oe-rel.

    FIND FIRST oe-rell NO-LOCK
        WHERE oe-rell.company EQ cocode
          AND oe-rell.r-no    EQ oe-boll.r-no
          AND oe-rell.ord-no  EQ oe-boll.ord-no
          AND oe-rell.i-no    EQ oe-boll.i-no
          AND oe-rell.line    EQ oe-boll.line
          NO-ERROR.
    IF AVAIL oe-rell THEN DO:
      FIND FIRST oe-relh OF oe-rell NO-LOCK.
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company EQ cocode
            AND oe-rel.ord-no  EQ oe-relh.ord-no
            AND oe-rel.line    EQ oe-rell.line
            AND oe-rel.link-no EQ oe-rell.r-no
            AND oe-rel.ship-no EQ oe-relh.ship-no
            AND oe-rel.i-no    EQ oe-rell.i-no
            NO-ERROR.
      IF NOT AVAIL oe-rel THEN
      FIND FIRST oe-rel NO-LOCK
          WHERE oe-rel.company  EQ cocode
            AND oe-rel.ord-no   EQ oe-relh.ord-no
            AND oe-rel.line     EQ oe-rell.line
            AND oe-rel.rel-date EQ oe-relh.rel-date
            AND oe-rel.ship-no  EQ oe-relh.ship-no
            AND oe-rel.i-no     EQ oe-rell.i-no
            NO-ERROR.
    END.

    CREATE xreport.
    ASSIGN
     xreport.term-id = v-term-id
     xreport.key-01  = report.key-01
     xreport.key-02  = report.key-02
     xreport.key-03  = report.key-03
     xreport.key-04  = /*report.key-04*/ STRING(oe-boll.ord-no )
     xreport.key-05  = /*IF AVAIL oe-rel THEN oe-rel.po-no ELSE "" */ oe-boll.po-no  /* ticket 16064 */ 
     xreport.key-06  = oe-boll.i-no
     xreport.key-07  = STRING(oe-boll.cases)
     xreport.key-08  = STRING(oe-boll.qty-case)
     xreport.rec-id  = RECID(oe-boll).
  END.

  DELETE report.
END.

FOR EACH report   NO-LOCK WHERE report.term-id EQ v-term-id,
    FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
    FIRST itemfg NO-LOCK
    WHERE itemfg.company EQ cocode
      AND itemfg.i-no    EQ oe-boll.i-no,
    FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no   EQ oe-boll.b-no,
    FIRST cust    NO-LOCK WHERE cust.cust-no   EQ oe-bolh.cust-no

    BREAK BY report.key-01
          BY report.key-02
          BY report.key-03
          BY report.key-04
          BY report.key-05
          BY report.key-06
          BY report.key-07
          BY report.key-08
    WITH FRAME coc-mid:

  ASSIGN
   v-po-no[1] = report.key-04
   v-po-no[2] = report.key-05.

  IF FIRST-OF(report.key-06) THEN DO:
    FIND FIRST carrier NO-LOCK
        WHERE carrier.company EQ cocode
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
     v-cust-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.

    IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".
    IF TRIM(v-cust-addr3) EQ "," THEN v-cust-addr3 = "".

    ASSIGN
     v-fob      = ""
     v-dscr     = ""
     v-name     = ""
     v-terms    = ""
     v-price    = 0
     v-tot-price  = 0 
     v-ord-bol  = TRIM(STRING(oe-bolh.ord-no,">>>>>9")) + " / " +
                  TRIM(STRING(oe-bolh.bol-no,">>>>>9"))
     v-ord-date = oe-bolh.bol-date
     cases      = 0   
     qty-cases  = 0
     pallet     = 0 
     v-line-count = 0
        .

    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ cocode
          AND oe-ord.ord-no  EQ oe-boll.ord-no
         NO-ERROR.

    IF AVAIL oe-ord THEN DO:
      IF NOT AVAIL carrier THEN
      FIND FIRST carrier NO-LOCK
          WHERE carrier.company EQ cocode
            AND carrier.carrier EQ oe-ord.carrier
          NO-ERROR.

      ASSIGN
       v-fob      = IF oe-ord.fob-code BEGINS "ORIG" THEN "ORIG"
                                                     ELSE "DEST"
       v-ord-date = oe-ord.ord-date
       v-terms    = oe-ord.terms     
       v-ord-no   = oe-ord.ord-no          .

    END.
    IF FIRST-OF (report.key-06) THEN DO:
        IF NOT FIRST(report.key-06) THEN
            PAGE.
        {oe/rep/cocpryst.i}
    END.
    /*page.
      hide frame coc-bot no-pause.
    */
    ASSIGN
     v-ship-i[1] = oe-bolh.ship-i[3] 
     v-ship-i[2] = oe-bolh.ship-i[4].
     
    VIEW FRAME coc-bot.
  END.
  
  v-bol-qty = v-bol-qty + oe-boll.qty.
  
  ASSIGN
     cases      = oe-boll.cases  
     qty-cases  = oe-boll.qty-case 
     pallet     = pallet +  oe-boll.tot-pallets  .

  
  IF FIRST-OF(report.key-06) THEN DO:
    
    ASSIGN v-bol-qty = 0 .
    FOR EACH bf-oe-boll WHERE bf-oe-boll.bol-no EQ oe-boll.bol-no 
                        AND bf-oe-boll.i-no EQ report.key-06 
                        AND bf-oe-boll.po-no EQ string(report.key-05)
                        AND bf-oe-boll.ord-no EQ INTEGER(report.key-04) NO-LOCK:
      
        v-bol-qty = v-bol-qty + bf-oe-boll.qty .      
    END.
    v-qty-alf = TRIM(STRING(v-bol-qty,">>>>>>>>>9")).
    
    FIND FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ cocode
          AND oe-ordl.ord-no  EQ oe-boll.ord-no
          AND oe-ordl.i-no    EQ oe-boll.i-no
          AND oe-ordl.line    EQ oe-boll.line
        NO-ERROR.
    ASSIGN
        v-name = IF AVAIL oe-ordl AND oe-ordl.i-name NE "" THEN oe-ordl.i-name ELSE itemfg.i-name 
        v-dscr = IF AVAIL oe-ordl AND oe-ordl.part-dscr1 NE "" THEN oe-ordl.part-dscr1 ELSE itemfg.part-dscr1
        v-price = IF AVAIL oe-ordl  THEN oe-ordl.price ELSE 0
        v-tot-price = IF AVAIL oe-ordl  THEN oe-ordl.t-price ELSE 0
        v-ord-no   = IF AVAIL oe-ordl THEN oe-ordl.ord-no ELSE oe-boll.ord-no  .


    PUT  "<R30></b>    " v-qty-alf  SPACE(5)
            /*" <C59>" v-price 
            "<C70>"  v-tot-price*/
            "<c19>" itemfg.i-no SKIP
             "<c19>" v-name SKIP
             "<c19>" v-dscr  /*
              oe-ordl.i-name when avail oe-ordl and oe-ordl.i-name ne ""
              @ itemfg.i-name */ .
                                 
    ASSIGN
     i     = 0
     j     = 0
     v-bin = ""
     v-line-count = 33.

    FIND FIRST fg-set
        WHERE fg-set.company EQ cocode
          AND fg-set.set-no  EQ itemfg.i-no
         NO-ERROR.

    IF itemfg.isaset THEN DO WHILE AVAIL fg-set:
      i = i + 1.
     
      IF i GT 8 THEN DO:
        PUT SKIP(3).

        ASSIGN
         i     = 1
         j     = 0
         v-bin = ""
         v-line-count = v-line-count + 3.
      END.

      {sys/inc/part-qty.i v-part-qty fg-set}

      IF i MODULO 4 EQ 1 OR i EQ 1 THEN PUT SPACE(6).

      PUT TRIM(STRING(v-bol-qty * v-part-qty,">>>>>9")) FORMAT "x(6)"
          SPACE(1)
          fg-set.part-no
          SPACE(1).

      FIND FIRST fg-bin NO-LOCK
          WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ fg-set.part-no
           NO-ERROR.

      IF AVAIL fg-bin THEN
        v-bin[IF i MODULO 4 EQ 0 THEN 4 ELSE (i MODULO 4)] = fg-bin.loc-bin.

      IF i MODULO 4 EQ 0 THEN DO:
        PUT SKIP
            SPACE(6)
            v-bin
            SKIP.

        j = j + 2.
         v-line-count = v-line-count + 1.
      END.
    
      FIND NEXT fg-set NO-LOCK
          WHERE fg-set.company EQ cocode
            AND fg-set.set-no  EQ itemfg.i-no
          NO-ERROR.
    END.

    IF i MODULO 4 NE 0 THEN DO:
      PUT SKIP
          SPACE(6)
          v-bin
          SKIP.

      j = j + 2.
      v-line-count = v-line-count + 1.
    END.

    DO j = (j + 1) TO 5:
      PUT SKIP(1).
    END.
     v-line-count = v-line-count + 5.
   /* IF NOT last(report.key-06)  THEN DO:
        PAGE.
        {oe/rep/cocpryst.i}
    END.*/
    v-bol-qty = 0.
  END.
  
   IF LAST-OF(report.key-08) THEN DO:
      PUT "<R" v-line-count ">" SPACE(30) pallet " Pallet(s) @ " cases FORMAT ">>>>>"  " cases  @" qty-cases "/case " SKIP 
        .
      ASSIGN 
          pallet = 0 
          cases  = 0
          qty-cases = 0
          v-line-count = v-line-count + 2. 

   END.
 
END. /* for each report */

/*hide frame coc-top no-pause.*/
PAGE.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */



