/* ------------------------------------------ oe/rep/invprstl.p 11200902 GDM */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = Loylang & LoylangBSF                      */
/* ------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}

DEF VAR v-salesman      AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR v-salesname     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fob           AS CHAR FORMAT "x(27)" NO-UNDO.
DEF VAR v-shipvia       LIKE carrier.dscr NO-UNDO.
DEF VAR v-addr3         AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sold-addr3    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-name   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-addr   AS CHAR FORMAT "x(30)" extent 2 NO-UNDO.
DEF VAR v-shipto-city   AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-shipto-state  AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR v-shipto-zip    AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-soldto-name   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-soldto-addr   AS CHAR FORMAT "x(30)" extent 2 NO-UNDO.

DEF VAR v-line          AS INT NO-UNDO.
DEF VAR v-printline     AS INT NO-UNDO.
DEF VAR v-t-weight      LIKE inv-line.t-weight NO-UNDO.
DEF VAR v-tot-cas       AS DEC FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tot-pallets   AS INT NO-UNDO.
DEF VAR v-tot-qty       AS INT NO-UNDO.
DEF VAR v-inv-date      AS DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.
def shared var v-fr-tax AS LOG INIT no NO-UNDO.
DEF VAR v-tax-rate      AS DEC FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tax-code      LIKE stax.tax-code NO-UNDO.
DEF VAR v-tx-rate       LIKE stax.tax-rate NO-UNDO.
DEF VAR v-ans           AS LOG INIT no NO-UNDO.
DEF VAR v-date-ship     AS DATE INIT today NO-UNDO.
DEF VAR v-del-no        AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-bol-cases     LIKE oe-boll.cases NO-UNDO.
DEF VAR v-set-qty       AS INT NO-UNDO.
DEF VAR v-part-qty      AS DEC FORMAT "999.9999" NO-UNDO.
DEF VAR v-net           LIKE inv-head.t-inv-rev NO-UNDO.
DEF VAR v-case-cnt      AS CHAR FORMAT "x(80)" extent 5 NO-UNDO.
DEF VAR v-case-line     AS CHAR NO-UNDO.
DEF VAR v-part-line     AS CHAR NO-UNDO.
DEF VAR v-pc            AS CHAR NO-UNDO. /* partial or complete */

DEF buffer xinv-head FOR inv-head .
DEF buffer xinv-line FOR inv-line .

DEF TEMP-TABLE w-sman NO-UNDO
  FIELD sman AS CHAR FORMAT "x(4)".

DEF VAR v-ord-del-hdr  AS CHAR FORMAT "x(3)" init "Del".
DEF VAR v-beeler-lines AS INT.
DEF VAR v-part-info    AS CHAR FORMAT "x(30)".
DEF VAR v              AS INT.
DEF VAR v-bo-qty       AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-inv-qty      AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-ship-qty     AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-i-no         AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-i-dscr       AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR v-price        AS DEC FORMAT "->>>>9.9999" NO-UNDO.
DEF VAR v-t-price      AS DEC FORMAT "->>>>>>9.99" NO-UNDO.
DEF VAR v-po-no        LIKE inv-line.po-no NO-UNDO.
DEF VAR v-bill-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEF VAR v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEF VAR v-ship-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-rel-po-no    LIKE oe-rel.po-no NO-UNDO.
DEF VAR v-price-head   AS CHAR FORMAT "x(5)" NO-UNDO.
DEF VAR v-subtot-lines AS DEC NO-UNDO.

DEF TEMP-TABLE w-tax NO-UNDO
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.

DEF VAR v-t-tax       AS DEC extent 3 NO-UNDO.
DEF VAR v-bot-lab     AS CHAR FORMAT "x(63)" extent 3 NO-UNDO.
DEF VAR v-lines       AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax     AS DEC NO-UNDO.

DEF VAR v-bol-no AS INT FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR v-inv-total AS DECI NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.

/* === with xprint ====*/
DEF VAR ls-image1    AS CHAR NO-UNDO.
DEF VAR ls-crdimg    AS CHAR NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS CHAR FORMAT "x(200)" NO-UNDO.

ASSIGN ls-image1 = "images\LoyINV.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

ASSIGN ls-crdimg = "images\creditgraphic.jpg"
       FILE-INFO:FILE-NAME = ls-crdimg
       ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORMAT "x(30)" NO-UNDO.

DEF VAR v-custno      LIKE cust.cust-no NO-UNDO.

DEF VAR lv-print-bsf AS LOG INIT NO NO-UNDO.  /* controls the switch between Loylang and LoylangBSF*/
DEF VAR lv-print-bsf-item AS LOG INIT NO NO-UNDO. /*controls BSF printing per item*/
DEF VAR ld-bsf LIKE eb.t-sqin INIT 1 NO-UNDO. /* store bsf for price/bsf calculation */
DEF VAR ld-price-per-m AS DEC INIT 1 NO-UNDO. /*for calculating BSF*/


FIND FIRST sys-ctrl where sys-ctrl.company = cocode
                      and sys-ctrl.NAME = "INVPRINT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "LoylangBSF" THEN lv-print-bsf = YES.
    ELSE lv-print-bsf = NO.  /* controls the switch between Loylang and LoylangBSF at main level*/
FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
  FIRST xinv-head WHERE RECID(xinv-head) EQ report.rec-id NO-LOCK
  BREAK BY report.key-01
          BY report.key-02:

    FIND FIRST reftable WHERE
         reftable.reftable EQ "brokerbol" AND
         reftable.CODE EQ STRING(xinv-head.inv-no)
         NO-LOCK NO-ERROR.

      IF AVAIL reftable AND reftable.code2 NE "" THEN
      DO:
         v-bol-no = INT(reftable.code2).
         RELEASE reftable.
      END.
      ELSE
         v-bol-no = xinv-head.bol-no.

    FIND FIRST cust WHERE cust.company = xinv-head.company
                      AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

    ASSIGN  
      v-custno      = cust.cust-no
      v-shipto-name = xinv-head.sold-name
      v-shipto-addr[1] = xinv-head.sold-addr[1]
      v-shipto-addr[2] = xinv-head.sold-addr[2]
      v-shipto-city = xinv-head.sold-city
      v-shipto-state = xinv-head.sold-state
      v-shipto-zip = xinv-head.sold-zip.

      ASSIGN v-del-no = 0.
    /* tests where customer specific forms have LoylangBSF*/
    DO:
        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ cocode 
            AND sys-ctrl-shipto.NAME EQ "INVPRINT"
            AND sys-ctrl-shipto.cust-vend-no = v-custno NO-ERROR.
        IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.char-fld = "LoylangBSF" THEN
                lv-print-bsf = YES.
            ELSE lv-print-bsf = NO.   
    END. /*end of Do block to test for LoylangBSF per customer*/
    FIND FIRST oe-bolh WHERE oe-bolh.company = xinv-head.company 
                           AND oe-bolh.bol-no = xinv-head.bol-no 
                         USE-INDEX bol-no 
                         NO-LOCK NO-ERROR.
      IF AVAIL oe-bolh THEN DO:

        FIND FIRST shipto WHERE shipto.company EQ cocode
                            AND shipto.cust-no EQ oe-bolh.cust-no
                            AND shipto.ship-id EQ oe-bolh.ship-id
                            USE-INDEX ship-id NO-LOCK NO-ERROR.
        IF AVAIL shipto 
          THEN
           ASSIGN  
             v-shipto-name = shipto.ship-name
             v-shipto-addr[1] = shipto.ship-addr[1]
             v-shipto-addr[2] = shipto.ship-addr[2]
             v-shipto-city = shipto.ship-city
             v-shipto-state = shipto.ship-state
             v-shipto-zip = shipto.ship-zip.
      
      END. /* avail oe-bolh */
      

      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
         RUN oe/get-inv#.p (ROWID(xinv-head)).

      DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).

        IF inv-head.inv-date NE ? THEN v-inv-date = inv-head.inv-date.

        IF inv-head.fob-code BEGINS "ORIG" 
          THEN ASSIGN v-fob = "Origin".
          ELSE ASSIGN v-fob = "Destination".

        Find FIRST carrier WHERE carrier.company EQ inv-head.company 
                            AND  carrier.carrier = inv-head.carrier 
                           NO-LOCK NO-ERROR.
        IF AVAIL carrier 
          THEN ASSIGN v-shipvia = carrier.dscr.
          ELSE ASSIGN v-shipvia = "".

        ASSIGN
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                         "  " + v-shipto-zip
          v-line = 1 v-printline = 0.

        FIND FIRST stax
          {sys/ref/stax1W.i}
          AND {sys/ref/taxgroup.i stax} EQ inv-head.tax-gr NO-LOCK NO-ERROR.
        IF NOT AVAIL stax 
          THEN
           FIND FIRST stax 
             WHERE stax.tax-group EQ inv-head.tax-gr NO-LOCK NO-ERROR.
           IF AVAIL stax
             THEN
              ASSIGN 
                v-tax-rate = stax.tax-rate[1] +
                             stax.tax-rate[2] + stax.tax-rate[3]
                v-tax-code[1] = stax.tax-code[1]
                v-tax-code[2] = stax.tax-code[2]
                v-tax-code[3] = stax.tax-code[3]
                v-tx-rate[1]  = stax.tax-rate[1]
                v-tx-rate[2]  = stax.tax-rate[2]
                v-tx-rate[3]  = stax.tax-rate[3].

        ASSIGN v-tot-pallets = 0.

        FOR EACH xinv-line NO-LOCK 
          WHERE xinv-line.r-no EQ inv-head.r-no
            BREAK BY xinv-line.i-no:
            
            DO i = 1 TO 3:
              IF xinv-line.sman[i] NE "" THEN DO:
                CREATE w-sman.
                ASSIGN w-sman.sman = xinv-line.sman[i].
              END.
            END.

            ASSIGN 
              v-tot-qty = v-tot-qty + xinv-line.ship-qty
              v-t-weight = v-t-weight + (round(xinv-line.t-weight /
                           xinv-line.qty, 2) * xinv-line.inv-qty)
              v-tot-pallets = 0
              v-pc = "C". /* complete*/

            FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ xinv-line.b-no:

                ASSIGN v-pc = "P". /* partial*/ 

                FOR EACH oe-boll NO-LOCK 
                  WHERE oe-boll.company EQ oe-bolh.company 
                    AND oe-boll.b-no    EQ oe-bolh.b-no 
                    AND oe-boll.i-no    EQ xinv-line.i-no 
                    AND oe-boll.ord-no  EQ xinv-line.ord-no:


                   /** Bill Of Lading TOTAL CASES **/
                    ASSIGN 
                      v-bol-cases = v-bol-cases + oe-boll.cases
                     v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (IF oe-boll.partial GT 0 
                                        THEN 1 ELSE 0).

                    IF oe-boll.p-c THEN v-pc = "C". /*complete*/
                END.  /* each oe-boll */

                ASSIGN v-date-ship = oe-bolh.bol-date.
            END.  /* each oe-bolh */

            IF LAST-OF(xinv-line.i-no) THEN DO:
                IF xinv-line.est-no NE "" THEN DO:

                  FIND FIRST eb NO-LOCK 
                    WHERE eb.company  EQ xinv-line.company 
                      AND eb.est-no   EQ xinv-line.est-no 
                      AND eb.e-num    EQ xinv-line.e-num 
                      AND eb.form-no  EQ xinv-line.form-no 
                      AND eb.blank-no EQ xinv-line.blank-no NO-ERROR.

                  IF xinv-line.form-no EQ 0 AND 
                     xinv-line.est-type EQ 2 
                    THEN DO:

                     FOR EACH fg-set FIELDS(part-qty) NO-LOCK 
                       WHERE fg-set.company EQ xinv-line.company
                         AND fg-set.set-no  EQ xinv-line.i-no:

                         ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
                     END.

                     IF v-set-qty = 0 THEN
                        ASSIGN v-set-qty = 1.

                     FOR EACH eb NO-LOCK 
                       WHERE eb.company EQ xinv-line.company 
                         AND eb.est-no  EQ xinv-line.est-no 
                         AND eb.e-num   EQ xinv-line.e-num 
                         AND eb.form-no NE 0:

                         FIND fg-set NO-LOCK 
                           WHERE fg-set.company EQ xinv-line.company 
                             AND fg-set.set-no  EQ xinv-line.i-no  
                             AND fg-set.part-no EQ eb.stock-no NO-ERROR.
                         IF AVAIL fg-set AND 
                            fg-set.part-qty NE 0 
                           THEN 
                            ASSIGN v-part-qty = fg-set.part-qty / v-set-qty. 
                           ELSE
                            ASSIGN v-part-qty = 1 / v-set-qty.

                         IF eb.cas-cnt EQ 0 
                           THEN
                            ASSIGN 
                              v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                          eb.cas-wt, 2).
                           ELSE
                            ASSIGN 
                              v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                          eb.cas-cnt, 2).

                         IF v-bol-cases NE 0 
                           THEN ASSIGN v-tot-cas = v-bol-cases.
                     END.  /* each eb */
                  END. /* IF xinv-line.form-no EQ 0 & inv-line.est-type do*/
                  ELSE
                  IF AVAIL eb THEN DO:
                    IF eb.cas-cnt = 0 
                      THEN ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                      ELSE ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).

                     IF v-bol-cases NE 0 
                       THEN ASSIGN v-tot-cas = v-bol-cases.
                  END. /* IF AVAIL eb */

                END. /* IF xinv-line.est-no NE "" */

                ASSIGN
                  v-t-weight = 0
                  v-tot-cas  = 0
                  v-tot-qty  = 0.

            END.  /* last-of i-no */
        END.  /* each xinv-line */

        /** Build Salesman Id String **/
        ASSIGN v-salesman = "".
        FOR EACH w-sman BREAK BY w-sman.sman:
           IF FIRST-OF(w-sman.sman) 
             THEN ASSIGN v-salesman = v-salesman + w-sman.sman.
           DELETE w-sman.
        END.

        FIND FIRST oe-bolh WHERE oe-bolh.company = inv-head.company 
                             AND oe-bolh.bol-no = inv-head.bol-no
                           USE-INDEX bol-no NO-LOCK NO-ERROR.
        IF AVAIL oe-bolh 
          THEN ASSIGN v-rel-po-no = oe-bolh.po-no.

        FIND FIRST inv-line WHERE inv-line.r-no = inv-head.r-no 
                            NO-LOCK NO-ERROR.
        IF AVAIL inv-line THEN DO:
           ASSIGN v-price-head = inv-line.pr-uom.
           
           FIND FIRST oe-ord WHERE oe-ord.company = cocode 
                               AND oe-ord.ord-no = inv-line.ord-no
                             NO-LOCK NO-ERROR.
           IF AVAIL oe-ord THEN DO:
          
             ASSIGN 
                 v-po-no = oe-ord.po-no
                 v-bill-i = oe-ord.bill-i[1]  
                 v-ord-no = oe-ord.ord-no        
                 v-ord-date = oe-ord.ord-date.
             
             FIND FIRST soldto WHERE soldto.company EQ cocode
                             AND soldto.cust-no EQ oe-ord.cust-no
                             AND soldto.sold-id EQ oe-ord.sold-id
                             USE-INDEX sold-id NO-LOCK NO-ERROR.
             IF AVAIL soldto THEN
                ASSIGN  
                v-soldto-name = soldto.sold-name
                v-soldto-addr[1] = soldto.sold-addr[1]
                v-soldto-addr[2] = soldto.sold-addr[2]
                v-addr3      = soldto.sold-city + ", " + 
                               soldto.sold-state + "  " + soldto.sold-zip.
             ELSE
             DO:
                FIND FIRST soldto WHERE
                     soldto.company EQ cocode AND
                     soldto.cust-no EQ inv-head.cust-no AND
                     soldto.sold-id EQ inv-head.cust-no
                     NO-LOCK NO-ERROR.
          
                IF AVAIL soldto THEN
                   ASSIGN
                    v-soldto-name = soldto.sold-name
                    v-soldto-addr[1] = soldto.sold-addr[1]
                    v-soldto-addr[2] = soldto.sold-addr[2]
                    v-addr3      = soldto.sold-city + ", " + 
                                   soldto.sold-state + "  " + soldto.sold-zip.
                ELSE
                   ASSIGN
                      v-soldto-name = inv-head.cust-name
                      v-soldto-addr[1] = inv-head.addr[1]
                      v-soldto-addr[2] = inv-head.addr[2]
                      v-addr3          = inv-head.city + ", " + 
                                         inv-head.state + "  " + inv-head.zip.
             END.
           END.
           ELSE
           DO:
              FIND FIRST soldto WHERE
                   soldto.company EQ cocode AND
                   soldto.cust-no EQ inv-head.cust-no AND
                   soldto.sold-id EQ inv-head.cust-no
                   NO-LOCK NO-ERROR.
          
              IF AVAIL soldto THEN
                 ASSIGN
                    v-soldto-name = soldto.sold-name
                    v-soldto-addr[1] = soldto.sold-addr[1]
                    v-soldto-addr[2] = soldto.sold-addr[2]
                    v-addr3      = soldto.sold-city + ", " + 
                                   soldto.sold-state + "  " + soldto.sold-zip.
              ELSE
                 ASSIGN
                    v-soldto-name = inv-head.cust-name
                    v-soldto-addr[1] = inv-head.addr[1]
                    v-soldto-addr[2] = inv-head.addr[2]
                    v-addr3          = inv-head.city + ", " + 
                                       inv-head.state + "  " + inv-head.zip.
          
              ASSIGN v-price-head = inv-line.pr-uom.
           END.
        END.
        ELSE
        DO:
           FIND FIRST soldto WHERE
                soldto.company EQ cocode AND
                soldto.cust-no EQ inv-head.cust-no AND
                soldto.sold-id EQ inv-head.cust-no
                NO-LOCK NO-ERROR.
          
           IF AVAIL soldto THEN
              ASSIGN
                 v-soldto-name = soldto.sold-name
                 v-soldto-addr[1] = soldto.sold-addr[1]
                 v-soldto-addr[2] = soldto.sold-addr[2]
                 v-addr3      = soldto.sold-city + ", " + 
                                soldto.sold-state + "  " + soldto.sold-zip.
           ELSE
              ASSIGN
                 v-soldto-name = inv-head.cust-name
                 v-soldto-addr[1] = inv-head.addr[1]
                 v-soldto-addr[2] = inv-head.addr[2]
                 v-addr3          = inv-head.city + ", " + 
                                    inv-head.state + "  " + inv-head.zip.
        END.


        {oe/rep/invprstl.i}  /* xprint form */
        
        ASSIGN v-printline    = 29
               v-subtot-lines = 0
               v-t-tax        = 0.

        FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no:

          ASSIGN 
              lv-print-bsf-item = NO
              ld-bsf = 1
              v-case-line = "" 
              v-part-line = "" 
              v-case-cnt = ""
              v-pc = "P". /* partial*/ 
          
          IF lv-print-bsf THEN
            DO:
                FIND FIRST itemfg 
                    WHERE itemfg.company EQ cocode  
                        AND itemfg.i-no = inv-line.i-no NO-LOCK NO-ERROR.
                IF AVAIL itemfg THEN
                DO:
                    IF itemfg.procat = "PADS" THEN 
                    DO:
                        ASSIGN 
                            lv-print-bsf-item = YES
                            ld-bsf = itemfg.t-sqin / 144.
                        /*convert price to price per M*/
                        RUN sys/ref/convptom.p (inv-line.pr-uom, inv-line.price, inv-line.qty, inv-line.cas-cnt, OUTPUT ld-price-per-m ).
                    END. /* if itemfg.procat = "PADS"*/
                END. /*avail itemfg*/
            END. /*if lv-print-bsf*/


          FOR EACH oe-boll NO-LOCK 
            WHERE oe-boll.company = inv-line.company
              AND oe-boll.bol-no = inv-head.bol-no
              /*and oe-boll.b-no = inv-line.b-no*/
              AND oe-boll.i-no = inv-line.i-no 
            USE-INDEX bol-no:

             /** Build Case Count Display Lines **/
             IF oe-boll.cases NE 0 AND 
                oe-boll.qty-case ne 0 
               THEN ASSIGN v-case-line = STRING(oe-boll.cases) + " @ " +
                                         STRING(oe-boll.qty-case).
               else ASSIGN v-case-line = "".

             IF oe-boll.partial NE 0 
               THEN ASSIGN v-part-line = "1" + " @ " + STRING(oe-boll.partial).
               ELSE assign v-part-line = "".

             IF oe-boll.p-c THEN v-pc = "C". /*complete*/

             DO i = 1 TO 5:

               IF (80 - LENGTH(v-case-cnt[i])) > LENGTH(v-case-line) AND
                   v-case-line NE "" 
                 THEN
                  ASSIGN 
                    v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                    v-case-line = "".

               IF (80 - LENGTH(v-case-cnt[i])) > LENGTH(v-part-line) AND
                   v-part-line NE "" 
                 THEN
                  ASSIGN 
                    v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                    v-part-line = "".
             END.  /* 1 to 5 */
          END. /* each oe-boll */

          FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company = cocode 
              AND oe-ordl.ord-no = inv-line.ord-no 
              AND oe-ordl.i-no = inv-line.i-no NO-ERROR.
          IF AVAIL oe-ordl 
           THEN
            ASSIGN v-bo-qty = IF (inv-line.qty - inv-line.ship-qty -
                                  oe-ordl.t-ship-qty) < 0 
                                THEN 0 
                                ELSE (inv-line.qty - inv-line.ship-qty -
                                      oe-ordl.t-ship-qty).
           ELSE
            ASSIGN v-bo-qty = IF (inv-line.qty - inv-line.ship-qty ) < 0
                                THEN 0 
                                ELSE inv-line.qty - inv-line.ship-qty.

          ASSIGN 
            v-inv-qty = inv-line.inv-qty
            v-ship-qty = inv-line.ship-qty
            v-i-no = inv-line.i-no
            v-i-dscr = inv-line.i-name
            v-t-price = inv-line.t-price
            v-subtot-lines = v-subtot-lines + inv-line.t-price.
          
           v-price = IF NOT lv-print-bsf-item THEN inv-line.price * (1 - (inv-line.disc / 100))  
                        ELSE (ld-price-per-m / ld-bsf ) * (1 - (inv-line.disc / 100)). /*override price if BSF for item */
          
          IF inv-line.tax AND 
             AVAIL stax 
            THEN
              DO i = 1 TO 3:

                IF stax.tax-code[i] NE "" THEN DO:
                  CREATE w-tax.
                  ASSIGN
                    w-dsc  = stax.tax-dscr[i]
                    w-tax  = ROUND((IF stax.company EQ "yes" 
                                      THEN v-t-price
                                      ELSE inv-line.t-price) * 
                                           stax.tax-rate[i] / 100,2)
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
                END.
          END.

          IF v-t-price NE inv-line.t-price THEN DO:
            CREATE w-tax.
            ASSIGN
              w-dsc     = "******ITEM TOTAL:"
              w-tax     = v-t-price
              v-lines   = v-lines + 1.
          END.

          ASSIGN 
            v-po-no  = inv-line.po-no
            v-ord-no = inv-line.ord-no.
            v-price-head = IF NOT lv-print-bsf-item THEN  inv-line.pr-uom
                        ELSE "BSF". /*override the UOM in case of LoylangBSF*/

          
          
          

          IF v-printline GE 63 THEN DO:             
            PAGE.
            {oe/rep/invprstl.i}
            v-printline = 29.
          END.

          PUT             
            SPACE(1)
             v-po-no           FORMAT "x(15)" SPACE(1)
             inv-line.part-no  FORMAT "x(30)"
            SPACE(2)
             v-ship-qty        FORMAT "->>>>>9"              
            SPACE(1)
             v-inv-qty         FORMAT "->>>>>9" 
            SPACE(1)            
            SPACE(6)
             v-price           FORMAT "->>>,>>9.9999" 
            SPACE(0)               
             inv-line.t-price  FORMAT "->>>,>>9.99"                
           SKIP
            SPACE(1)  v-ord-no 
            SPACE(10) v-i-dscr FORMAT "x(30)"
            SPACE(2) inv-line.qty FORMAT "->>>>>9"
            SPACE(13) v-pc  
            SPACE(1)  v-price-head 
            SPACE(1) 
           SKIP
            SPACE(1)
             inv-line.lot-no FORMAT "x(15)" 
            SPACE(1)
             inv-line.part-dscr1 FORMAT "x(30)" 
            SPACE(1)
           SKIP
            SPACE(17) inv-line.part-dscr2 FORMAT "x(30)" 
           SKIP .
          v-printline = v-printline + 5.

          IF AVAIL oe-ordl AND oe-ordl.part-dscr3 NE "" THEN do:
              PUT SPACE(17)  oe-ordl.part-dscr3 FORMAT "x(30)" SKIP .           /*Task# 02191403*/     
              v-printline = v-printline + 1.
          END.


          IF v-printline GE 63 THEN DO:             
             PAGE.
             {oe/rep/invprstl.i}
             v-printline = 29.
          END.

          v-printline = v-printline + 1.

        END.  /* each inv-line */

        PUT SKIP(1). 
        ASSIGN v-printline = v-printline + 2.

        FOR EACH inv-misc NO-LOCK WHERE inv-misc.company = inv-head.company 
                                    AND inv-misc.r-no = inv-head.r-no 
                                    AND inv-misc.bill = "Y" 
                                  BREAK BY inv-misc.ord-no:

            IF FIRST(inv-misc.ord-no) THEN DO:
                PUT "** Miscellaneous Items **" AT 20 
                    SKIP.
                ASSIGN v-printline = v-printline + 2.
            END.                 

            IF v-printline GE 66 THEN do:                                               
                PAGE.                
                {oe/rep/invprstl.i}                
                v-printline = 29.
            END.

            PUT 
              SPACE(1)
              inv-misc.po-no  FORMAT "x(16)"
              inv-misc.dscr   FORMAT "x(30)"
              inv-misc.amt    AT 85
             SKIP.

            ASSIGN 
              v-subtot-lines = v-subtot-lines + inv-misc.amt
              v-printline = v-printline + 1.

            IF inv-misc.tax AND 
               AVAIL stax 
              THEN
               DO i = 1 TO 3:
                IF stax.tax-code[i] NE "" THEN DO:
                  CREATE w-tax.
                  ASSIGN
                    w-dsc      = stax.tax-dscr[i]
                    w-tax      = IF stax.company EQ "yes" 
                                   THEN v-t-price
                                   ELSE inv-misc.amt
                    w-tax      = ROUND(w-tax * (1 + (stax.tax-rate[i] / 
                                                     100)),2) - w-tax
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
                END.
            END.

            IF v-printline GE 66 THEN do:                                
                PAGE.                
                {oe/rep/invprstl.i}                
                v-printline = 29.
            END.

            IF v-t-price NE inv-misc.amt THEN DO:
              CREATE w-tax.
              ASSIGN
                w-dsc     = "******ITEM TOTAL:"
                w-tax     = v-t-price
                v-lines   = v-lines + 1.
            END.
        END. /* each inv-misc */

        IF v-prntinst THEN DO:

          IF TRIM(inv-head.bill-i[1]) NE "" OR
             TRIM(inv-head.bill-i[2]) NE "" OR
             TRIM(inv-head.bill-i[3]) NE "" OR
             TRIM(inv-head.bill-i[4]) NE "" 
            THEN DO:
              PUT SKIP(1).
              ASSIGN v-printline = v-printline + 1.
          END.
         
          DO i = 1 TO 4:
           IF inv-head.bill-i[i] NE "" THEN DO:
             IF v-printline GE 66 THEN do:                                
                PAGE.                
                {oe/rep/invprstl.i}                
                v-printline = 29.
             END.

             PUT inv-head.bill-i[i] AT 18
              SKIP.
             ASSIGN v-printline = v-printline + 1.
           END.
          END.  /* 1 to 4 */
        END.

        ASSIGN v-frt-tax = inv-head.t-inv-freight.

        IF inv-head.tax-gr NE "" AND
           inv-head.f-bill AND 
           inv-head.t-inv-freight <> 0 AND 
           AVAIL stax
          THEN
           DO i = 1 TO 3:
             IF stax.tax-code[i] NE "" THEN DO:
               CREATE w-tax.
               ASSIGN
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = ROUND((IF stax.company EQ "yes" 
                                       THEN v-frt-tax
                                       ELSE inv-head.t-inv-freight) *
                                             stax.tax-rate[i] / 100,2)
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        END.

        ASSIGN
          inv-head.printed = yes
          inv-head.stat = "X".

      /*END.   DO TRANSACTION avail inv-head */

      DO i = 1 TO 3:
        ASSIGN
          v-bot-lab[i] = IF v-t-tax[i] NE 0 
                           THEN
                             ((IF AVAIL stax 
                                 THEN STRING(CAPS(stax.tax-code[i]),"x(5)") 
                                 ELSE FILL(" ",5) ) +
                               FILL(" ",6) + ":" + 
                               STRING(v-t-tax[i],"->>>>>9.99")) 
                           ELSE "".
      END.

      ASSIGN
        v-inv-freight = IF inv-head.f-bill 
                          THEN inv-head.t-inv-freight ELSE 0.
                          /*inv-head.t-inv-freight*/.

      ASSIGN v-inv-total = v-subtot-lines + inv-head.t-inv-tax + v-inv-freight.

      PUT 
        "<R59><C60><#8><B>" 
        "<=8><R59>  Sub Tot:" v-subtot-lines     FORMAT "->>,>>9.99"
        "<=8><R60>  Sals Tx:" inv-head.t-inv-tax FORMAT "->>,>>9.99"
        "<=8><R61>  Freight:" v-inv-freight      FORMAT "->>,>>9.99"
        "<=8><R62>  Tot Inv:" v-inv-total        FORMAT "->>,>>9.99".
  
    ASSIGN v-printline = v-printline + 6.

   END.

    IF v-printline <= 66 THEN PAGE. 
    /*PUT SKIP(74 - v-printline). */
    
END. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
