/* ------------------------------------------ oe/rep/invloyln.p 11200902 GDM */
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
DEF VAR v-inv-date      AS DATE FORMAT "99/99/9999" NO-UNDO.
def shared var v-fr-tax AS LOG INIT no NO-UNDO.
DEF VAR v-tax-rate      AS DEC FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tax-code      LIKE stax.tax-code NO-UNDO.
DEF VAR v-tx-rate       LIKE stax.tax-rate NO-UNDO.
DEF VAR v-ans           AS LOG INIT no NO-UNDO.
DEF VAR v-date-ship     AS DATE INIT 01/01/1900 NO-UNDO.
DEF VAR v-del-no        AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-bol-cases     LIKE oe-boll.cases NO-UNDO.
DEF VAR v-set-qty       AS DECIMAL NO-UNDO.
DEF VAR v-part-qty      AS DEC FORMAT "999.9999" NO-UNDO.
DEF VAR v-net           LIKE inv-head.t-inv-rev NO-UNDO.
DEF VAR v-case-cnt      AS CHAR FORMAT "x(80)" extent 5 NO-UNDO.
DEF VAR v-case-line     AS CHAR NO-UNDO.
DEF VAR v-part-line     AS CHAR NO-UNDO.
DEF VAR v-pc            AS CHAR NO-UNDO. /* partial or complete */

DEF buffer xinv-head FOR inv-head .
DEF buffer xinv-line FOR inv-line .
DEF BUFFER b-inv-head FOR inv-head.

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
DEF VARIABLE cBillNotes LIKE inv-head.bill-i NO-UNDO.

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
DEF VAR ls-image2    AS CHAR NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS CHAR FORMAT "x(200)" NO-UNDO.
ASSIGN ls-image1 = "images\protinv.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
ASSIGN ls-image2 = "images\protinvfoot.jpg"
       FILE-INFO:FILE-NAME = ls-image2
       ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(30)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORMAT "x(30)" NO-UNDO.

DEF VAR v-custno      LIKE cust.cust-no NO-UNDO.

DEF VAR lv-page AS INT INIT 0 NO-UNDO.
DEF VAR lv-currency AS CHAR NO-UNDO.
DEF VAR lv-taxable AS CHAR INIT "Y" NO-UNDO.
DEF VAR lv-line LIKE oe-ordl.e-num NO-UNDO.
DEF VAR lv-bolno LIKE oe-bolh.bol-no NO-UNDO.
DEF BUFFER b-inv-h FOR inv-head.
DEF VAR lv-carrier LIKE inv-head.carrier NO-UNDO.
DEF VAR lv-fob LIKE inv-head.fob NO-UNDO.
DEF VAR lv-terms LIKE inv-head.terms-d NO-UNDO.
{custom/formtext.i NEW}

DEFINE TEMP-TABLE tt-inv-line NO-UNDO
    FIELD po-no AS CHAR FORMAT "x(15)"
    FIELD line-no LIKE oe-ordl.e-num
    FIELD part-no LIKE inv-line.part-no
    FIELD inv-qty AS INT FORMAT "99999"
    FIELD ship-qty AS INT FORMAT "99999"
    FIELD price AS DEC FORMAT "->>>>9.9999"
    FIELD total-price LIKE inv-line.t-price
    FIELD taxable AS CHAR FORMAT "X"
    FIELD ord-no LIKE oe-ord.ord-no 
    FIELD dscr1 AS CHAR FORMAT "X(30)"
    FIELD price-head AS CHAR FORMAT "X(5)"
    FIELD bol-no LIKE oe-bolh.bol-no
    FIELD dscr2 LIKE inv-line.part-dscr1
    FIELD qty LIKE inv-line.qty
    FIELD dscr3  LIKE inv-line.part-dscr2.
                
                
FUNCTION formatDate RETURNS CHAR
  ( INPUT ip-date AS DATE)  FORWARD.
                         
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
  
    v-contact = xinv-head.contact.
    lv-carrier = xinv-head.carrier.
    lv-fob = xinv-head.fob.
    lv-terms = xinv-head.terms-d.

    IF xinv-head.multi-invoice THEN DO:

        FOR EACH b-inv-h WHERE b-inv-h.company = xinv-head.company
            AND b-inv-h.inv-date = xinv-head.inv-date
            AND b-inv-h.inv-no = xinv-head.inv-no
            AND b-inv-h.cust-no = xinv-head.cust-no
            AND NOT b-inv-h.multi-invoice
            AND b-inv-h.stat NE "H"
            NO-LOCK
            BREAK BY b-inv-h.inv-date DESC:



            IF FIRST-OF(b-inv-h.inv-date) AND b-inv-h.inv-date NE ? THEN DO:
            
                ASSIGN 
                    v-shipto-name = b-inv-h.sold-name
                    v-shipto-addr[1] = b-inv-h.sold-addr[1]
                    v-shipto-addr[2] = b-inv-h.sold-addr[2]
                    v-shipto-city = b-inv-h.sold-city
                    v-shipto-state = b-inv-h.sold-state
                    v-shipto-zip = b-inv-h.sold-zip
                    v-inv-date = b-inv-h.inv-date.
          
            END. /* First-of inv-date */


            IF b-inv-h.contact NE v-contact 
                OR b-inv-h.carrier NE lv-carrier 
                OR b-inv-h.fob NE lv-fob 
                OR b-inv-h.terms-d NE lv-terms THEN DO:
                ASSIGN 
                    v-contact = b-inv-h.contact
                    lv-carrier = b-inv-h.carrier
                    lv-fob = b-inv-h.fob
                    lv-terms = b-inv-h.terms-d.
                LEAVE.
            END. /* If b-inv-h.contact ne v-contact */  
        END. /* each b-inv-h */


    END. /* If multi-invoice */
    ELSE
        ASSIGN
            v-shipto-name = xinv-head.sold-name
            v-shipto-addr[1] = xinv-head.sold-addr[1]
            v-shipto-addr[2] = xinv-head.sold-addr[2]
            v-shipto-city = xinv-head.sold-city
            v-shipto-state = xinv-head.sold-state
            v-shipto-zip = xinv-head.sold-zip.
    ASSIGN  
      v-custno      = cust.cust-no.
    

    FIND FIRST currency WHERE currency.company = cust.company 
        AND currency.c-code = cust.curr-code.
    IF AVAIL currency THEN
        lv-currency = currency.c-desc.

      ASSIGN v-del-no = 0.
    
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
          THEN DO:
/*             ASSIGN */
/*              v-shipto-name = shipto.ship-name       */
/*              v-shipto-addr[1] = shipto.ship-addr[1] */
/*              v-shipto-addr[2] = shipto.ship-addr[2] */
/*              v-shipto-city = shipto.ship-city       */
/*              v-shipto-state = shipto.ship-state     */
/*              v-shipto-zip = shipto.ship-zip.        */
            IF shipto.tax-code = "0TX" THEN
                lv-taxable = "N".
            ELSE
                lv-taxable = "Y".
         END.
      END. /* avail oe-bolh */
      

      IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
         RUN oe/get-inv#.p (ROWID(xinv-head)).

      DO TRANSACTION:
        FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).
        
        IF v-inv-date EQ ? AND inv-head.inv-date NE ? 
            THEN v-inv-date = inv-head.inv-date.
            ELSE IF v-inv-date eq ? THEN v-inv-date = TODAY.

        IF lv-fob BEGINS "ORIG" 
          THEN ASSIGN v-fob = "Origin".
          ELSE ASSIGN v-fob = "Destination".

        Find FIRST carrier WHERE carrier.company EQ inv-head.company 
                            AND  carrier.carrier = lv-carrier 
                           NO-LOCK NO-ERROR.
        IF AVAIL carrier 
          THEN ASSIGN v-shipvia = carrier.dscr.
          ELSE ASSIGN v-shipvia = "".

        ASSIGN
          v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                         "  " + v-shipto-zip
          v-line = 1 v-printline = 0.

         IF trim(v-shipto-addr[2]) EQ '' THEN
             ASSIGN 
                v-shipto-addr[2] = v-sold-addr3
                v-sold-addr3 = ''.


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

                IF oe-bolh.bol-date > v-date-ship THEN ASSIGN v-date-ship = oe-bolh.bol-date.
                
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

                         ASSIGN v-set-qty = v-set-qty + fg-set.QtyPerSet.
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
                            fg-set.QtyPerSet NE 0 
                           THEN 
                            ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty. 
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
        IF v-salesman = "" THEN DO: /*misc charge only invoices*/
            FIND FIRST inv-misc WHERE inv-misc.company = inv-head.company 
                                    AND inv-misc.r-no = inv-head.r-no 
                                    AND inv-misc.bill = "Y" NO-LOCK NO-ERROR. 
                IF AVAIL inv-misc THEN ASSIGN v-salesman = inv-misc.s-man[1].
         END.
        /** end Build Salesman ID String **/

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
                             AND soldto.cust-no EQ inv-head.cust-no 
                             AND soldto.sold-id EQ inv-head.bill-to
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
                     soldto.sold-id EQ inv-head.bill-to
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
                   soldto.sold-id EQ inv-head.bill-to
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
        IF trim(v-soldto-addr[2]) EQ '' THEN
            ASSIGN 
                v-soldto-addr[2] = v-addr3
                v-addr3 = ''.

        IF v-date-ship = 01/01/1900 THEN v-date-ship = inv-head.inv-date .
        {oe/rep/invprot.i}  /* xprint form */
        
        RUN printNotes ("IT",1).

        ASSIGN v-printline    = 29
               v-subtot-lines = 0
               v-t-tax        = 0.

        FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no:

          ASSIGN 
              v-case-line = "" 
              v-part-line = "" 
              v-case-cnt = ""
              v-pc = "P". /* partial*/ 
  

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
           THEN  ASSIGN v-bo-qty = IF (inv-line.qty - inv-line.ship-qty -
                                  oe-ordl.t-ship-qty) < 0 
                                THEN 0 
                                ELSE (inv-line.qty - inv-line.ship-qty -
                                      oe-ordl.t-ship-qty)
                        lv-line = oe-ordl.e-num.
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
          
           v-price = inv-line.price * (1 - (inv-line.disc / 100)) .
          
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
            v-price-head = inv-line.pr-uom.
          
          FIND FIRST oe-bolh WHERE oe-bolh.company = inv-line.company AND
                              oe-bolh.b-no = inv-line.b-no NO-LOCK NO-ERROR.
          IF AVAIL oe-bolh THEN lv-bolno = oe-bolh.bol-no.

          
          IF v-printline GE 62 THEN DO:             
            PAGE.
            {oe/rep/invprot.i}
            v-printline = 29.
          END.

          IF (AVAIL oe-ordl AND NOT oe-ordl.is-a-component) OR NOT AVAIL oe-ordl THEN DO:
              FIND FIRST tt-inv-line WHERE 
                  tt-inv-line.part-no EQ inv-line.part-no 
                  AND tt-inv-line.dscr1 = inv-line.i-name
                  AND tt-inv-line.ord-no EQ v-ord-no
                  AND tt-inv-line.po-no EQ v-po-no
                  NO-LOCK NO-ERROR.
              IF AVAIL tt-inv-line THEN DO:
                    tt-inv-line.inv-qty = tt-inv-line.inv-qty + v-inv-qty.
                    tt-inv-line.ship-qty = tt-inv-line.ship-qty + v-ship-qty.
                    tt-inv-line.total-price = tt-inv-line.total-price + inv-line.t-price.
                    IF lv-bolno > tt-inv-line.bol-no THEN
                        tt-inv-line.bol-no = lv-bolno.
              END.
              ELSE DO:
                  CREATE tt-inv-line.
                  ASSIGN 
                      tt-inv-line.po-no = v-po-no
                      tt-inv-line.line-no = lv-line
                      tt-inv-line.part-no = inv-line.part-no
                      tt-inv-line.inv-qty = v-inv-qty
                      tt-inv-line.price = v-price
                      tt-inv-line.total-price = inv-line.t-price
                      tt-inv-line.taxable = lv-taxable
                      tt-inv-line.ord-no = v-ord-no
                      tt-inv-line.dscr1 = v-i-dscr
                      tt-inv-line.ship-qty = v-ship-qty
                      tt-inv-line.price-head = v-price-head
                      tt-inv-line.bol-no = lv-bolno
                      tt-inv-line.dscr2 = inv-line.part-dscr1
                      tt-inv-line.qty = inv-line.qty
                      tt-inv-line.dscr3 = inv-line.part-dscr2.
              END.

/*               PUT                                              */
/*                 "<C2>" v-po-no            FORMAT "x(15)"       */
/*                 "<C13>" lv-line           FORMAT ">>9"         */
/*                 "<C17>" inv-line.part-no  FORMAT "x(30)"       */
/*                 "<C51>" v-inv-qty         FORMAT "->>>>>9"     */
/*                 "<C58>" v-price           FORMAT "->>,>>9.99"  */
/*                 "<C67>" inv-line.t-price  FORMAT "->>>,>>9.99" */
/*                 "<C79>" lv-taxable        FORMAT "X"           */
/*                 SKIP                                           */
/*                 "<C2>"  trim(string(v-ord-no), ">>>>>>>")      */
/*                 "<C17>" v-i-dscr FORMAT "x(30)"                */
/*                 "<C51>" v-ship-qty        FORMAT "->>>>>9"     */
/*                 "<C63>" v-price-head                           */
/*                SKIP                                            */
/*                 "<C2>" trim(string(lv-bolno,">>>>>>>9"))       */
/*                 "<C17>" inv-line.part-dscr1 FORMAT "x(30)"     */
/*                 "<C51>" inv-line.qty FORMAT "->>>>>9"          */
/*                SKIP                                            */
/*                 "<C17>" inv-line.part-dscr2 FORMAT "x(30)"     */
/*                SKIP.                                           */
/*                                                                */
/*               v-printline = v-printline + 5.                   */
/*                                                                */
/*               IF v-printline GE 63 THEN DO:                    */
/*                  PAGE.                                         */
/*                  {oe/rep/invprot.i}                            */
/*                  v-printline = 29.                             */
/*               END.                                             */
/*                                                                */
/*               v-printline = v-printline + 1.                   */
          END. /*if avail*/
        END.  /* each inv-line */
        FOR EACH tt-inv-line:
            PUT             
                "<C2>" tt-inv-line.po-no            FORMAT "x(15)".
            IF tt-inv-line.line-no NE 0 THEN
                PUT
                "<C13>" tt-inv-line.line-no           FORMAT ">>9" .
            PUT
                "<C17>" tt-inv-line.part-no  FORMAT "x(30)"
                "<C51>" tt-inv-line.inv-qty         FORMAT "->>>>>9"                
                "<C57>" tt-inv-line.price           FORMAT "->,>>>,>>9.99"
                "<C67>" tt-inv-line.total-price  FORMAT "->,>>>,>>9.99"
                "<C79>" tt-inv-line.taxable        FORMAT "X"
                SKIP
                "<C2>"  trim(string(tt-inv-line.ord-no), ">>>>>>>")
                "<C17>" tt-inv-line.dscr1 FORMAT "x(30)"
                "<C51>" tt-inv-line.ship-qty        FORMAT "->>>>>9"              
                "<C63>" tt-inv-line.price-head 
               SKIP
                "<C2>" trim(string(tt-inv-line.bol-no,">>>>>>>9"))
                "<C17>" tt-inv-line.dscr2 FORMAT "x(30)" 
                "<C51>" tt-inv-line.qty FORMAT "->>>>>9"
               SKIP
                "<C17>" tt-inv-line.dscr3 FORMAT "x(30)" 
               SKIP.
    
              v-printline = v-printline + 5.
    
              IF v-printline GE 62 THEN DO:             
                 PAGE.
                 {oe/rep/invprot.i}
                 v-printline = 29.
              END.
    
              v-printline = v-printline + 1.
              DELETE tt-inv-line.
        END.
        PUT SKIP. 
        ASSIGN v-printline = v-printline + 2.

        FOR EACH inv-misc NO-LOCK WHERE inv-misc.company = inv-head.company 
                                    AND inv-misc.r-no = inv-head.r-no 
                                    AND inv-misc.bill = "Y" 
                                  BREAK BY inv-misc.ord-no:

            IF FIRST(inv-misc.ord-no) THEN DO:
                PUT "<C20>** Miscellaneous Items **" 
                    SKIP.
                ASSIGN v-printline = v-printline + 2.
            END.                 

            IF v-printline GE 62 THEN do:                                               
                PAGE.                
                {oe/rep/invprot.i}                
                v-printline = 29.
            END.

            FIND FIRST oe-ordm WHERE oe-ordm.company EQ inv-misc.company        /*10011302*/
                AND oe-ordm.ord-no EQ inv-misc.ord-no 
                AND oe-ordm.charge EQ inv-misc.charge NO-LOCK NO-ERROR.


            PUT  "<C2>" inv-misc.po-no  FORMAT "x(16)".
            IF AVAIL oe-ordm AND oe-ordm.spare-int-1 NE 0 THEN                   /*10011302*/
                PUT "<C13>" oe-ordm.spare-int-1 FORMAT ">>9".
            IF inv-misc.dscr NE "" THEN
               PUT "<C17>" inv-misc.dscr   FORMAT "x(30)".
            ELSE
               PUT "<C17>" inv-misc.charge   FORMAT "x(30)".

            PUT  "<C67>" inv-misc.amt FORMAT "->,>>>,>>9.99"
                 "<C79>" inv-misc.tax FORMAT "Y/N" SKIP
                 "<C2>" inv-misc.ord-no SKIP.

            ASSIGN 
              v-subtot-lines = v-subtot-lines + inv-misc.amt
              v-printline = v-printline + 2.
            

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

            IF v-printline GE 62 THEN do:                                
                PAGE.                
                {oe/rep/invprot.i}                
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
        PUT SKIP(1).
        v-printline = v-printline + 1.
        RUN printNotes ("IN",4).
        IF v-prntinst THEN DO:
          
            ASSIGN 
            cBillNotes[1] = ""
            cBillNotes[2] = ""
            cBillNotes[3] = ""
            cBillNotes[4] = "".
            
        IF inv-head.multi-invoice THEN
            FOR EACH b-inv-head WHERE b-inv-head.company = inv-head.company
              AND b-inv-head.inv-no = inv-head.inv-no
              /*AND b-inv-head.bol-no EQ inv-head.bol-no*/

              AND b-inv-head.cust-no = inv-head.cust-no  
              AND b-inv-head.stat NE "H" 
              AND NOT b-inv-head.multi-invoice NO-LOCK
              BREAK BY b-inv-head.inv-date DESC:
                ASSIGN 
                    cBillNotes[1] = b-inv-head.bill-i[1]
                    cBillNotes[2] = b-inv-head.bill-i[2]
                    cBillNotes[3] = b-inv-head.bill-i[3]
                    cBillNotes[4] = b-inv-head.bill-i[4]
                    .
                LEAVE.
        END.
        ELSE 
            ASSIGN
                cBillNotes[1] = inv-head.bill-i[1]
                cBillNotes[2] = inv-head.bill-i[2]
                cBillNotes[3] = inv-head.bill-i[3]
                cBillNotes[4] = inv-head.bill-i[4]
                .


          IF TRIM(cBillNotes[1]) NE "" OR
             TRIM(cBillNotes[2]) NE "" OR
             TRIM(cBillNotes[3]) NE "" OR
             TRIM(cBillNotes[4]) NE "" 
            THEN DO:
              PUT SKIP(1).
              ASSIGN v-printline = v-printline + 1.
          END.
          DO i = 1 TO 4:
           IF cBillNotes[i] NE "" THEN DO:
             IF v-printline GE 63 THEN do:                                
                PAGE.                
                {oe/rep/invprot.i}                
                v-printline = 29.
             END.

             PUT cBillNotes[i] FORMAT "x(100)" AT 18
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
/*        "<=1>" SKIP */
        "<P10><|10><R59><C54><FROM><R64><C80.75><RECT>"
        "<R59><C55><#8><B>" 
        "<=8><R59>    Sub Total:" v-subtot-lines      FORMAT "->,>>>,>>9.99"
        "<=8><R60>      Freight:" v-inv-freight       FORMAT "->,>>>,>>9.99"
        "<=8><R61>    Sales Tax:" inv-head.t-inv-tax  FORMAT "->,>>>,>>9.99"
        "<=8><R62>Total Invoice:" v-inv-total         FORMAT "->,>>>,>>9.99"
        "<=8><R63><C69><P8><FGCOLOR=RED>" lv-currency FORMAT "X(20)"
        "<FGCOLOR=BLACK>".
  
    ASSIGN v-printline = v-printline + 6.

   END.

    IF v-printline <= 66 THEN PAGE. 
    /*PUT SKIP(74 - v-printline). */
   
END. /* each xinv-head */

PROCEDURE printNotes:
    DEF INPUT PARAMETER ipcTypeList AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiLines AS INT NO-UNDO.
    
    DEF VAR li AS INT.
    DEF VAR li-width AS INT.
    DEF VAR lc-text AS CHAR.
    DEF VAR li-inv-no LIKE inv-head.inv-no NO-UNDO.
    DEF BUFFER b-inv-head FOR inv-head.

    ASSIGN
    li-width = 40
    lc-text = "".
    IF NOT v-reprint THEN li-inv-no = 0.
        ELSE li-inv-no = inv-head.inv-no.
  IF inv-head.multi-invoice THEN
    FOR EACH b-inv-head WHERE b-inv-head.company = inv-head.company
        AND b-inv-head.inv-no = li-inv-no
        AND b-inv-head.cust-no = inv-head.cust-no 
        AND b-inv-head.stat NE "H" 
        AND NOT b-inv-head.multi-invoice NO-LOCK:
      FIND FIRST notes
        WHERE notes.rec_key = b-inv-head.rec_key
        AND LOOKUP(notes.note_code,ipcTypeList) GT 0
        NO-LOCK NO-ERROR.
            IF AVAIL notes THEN DO:
                lc-text = lc-text + " " + notes.note_text + CHR(10).
                LEAVE.
            END.
    END.
   ELSE
     FOR EACH notes WHERE notes.rec_key = inv-head.rec_key
         AND LOOKUP(notes.note_code,ipcTypeList) GT 0
         NO-LOCK:
            lc-text = lc-text + " " + notes.note_text + CHR(10).
     END.
  
  IF lc-text NE "" THEN DO:
      DO li = 1 TO ipiLines:
        CREATE tt-formtext.
        ASSIGN
        tt-line-no = li
        tt-length  = li-width.
      END.
      RUN custom/formtext.p (lc-text).
      i = 0.
      FOR EACH tt-formtext:
        i = i + 1.
        IF i <= ipiLines AND tt-formtext.tt-text NE "" THEN DO:
            IF v-printline GE 62 THEN do:                                
                PAGE.                
                {oe/rep/invprot.i}                
                v-printline = 29.
            END.
            PUT "<C17>" tt-formtext.tt-text FORMAT "X(100)" SKIP.
            v-printline = v-printline + 1.
        END.
        DELETE tt-formtext.
       END.
  END.
  PUT SKIP(1).
  v-printline = v-printline + 1.
END PROCEDURE.

FUNCTION formatDate RETURNS CHAR
  ( INPUT ip-date AS DATE) :
  /*------------------------------------------------------------------------------
  Purpose:
  Notes:
  ------------------------------------------------------------------------------*/
  DEFINE VAR out-date AS CHAR NO-UNDO.
  DEFINE VAR cMonth AS CHAR EXTENT 12 NO-UNDO INIT
    [ "January",    "February",     "March", 
      "April",      "May",          "June", 
      "July",       "August",       "September",
      "October",    "November",     "December" ]. 
    
    out-date = cmonth[MONTH(ip-date)] + " " + STRING(DAY(ip-date)) + " " + STRING(YEAR(ip-date)).
    RETURN out-date.
END FUNCTION.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
