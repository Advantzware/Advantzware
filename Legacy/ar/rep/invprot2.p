/* ------------------------------------------- ar/rep/invprot2.p BPV */
/* PRINT INVOICE   Xprint form for Protagon2                                    */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}

DEF VAR v-salesman     AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR v-salesname    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fob          AS CHAR FORMAT "x(27)" NO-UNDO.
DEF VAR v-shipvia      LIKE carrier.dscr NO-UNDO.
DEF VAR v-addr2        AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-addr3        AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sold-addr3   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-name  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-addr  AS CHAR FORMAT "x(30)" extent 2 NO-UNDO.
DEF VAR v-shipto-city  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-shipto-state AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR v-shipto-zip   AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-soldto-name   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-soldto-addr   AS CHAR FORMAT "x(30)" extent 2 NO-UNDO.
DEF VAR v-line         AS INT NO-UNDO.
DEF VAR v-printline    AS INT NO-UNDO.
DEF VAR v-t-weight      LIKE ar-invl.t-weight NO-UNDO.
DEF VAR v-inv-no        AS INT NO-UNDO.
DEF VAR v-tot-cas       AS DEC FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tot-pallets   AS INT NO-UNDO.
DEF VAR v-tot-qty       AS INT NO-UNDO.
DEF VAR v-inv-date      AS DATE FORMAT "99/99/9999" NO-UNDO.
def shared var v-fr-tax AS LOG INIT NO NO-UNDO.
DEF VAR v-tax-rate      AS DEC FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tax-code      LIKE stax.tax-code NO-UNDO.
DEF VAR v-tx-rate       LIKE stax.tax-rate NO-UNDO.
DEF VAR v-ans           AS LOG initial no NO-UNDO.
DEF VAR v-date-ship     AS DATE initial today NO-UNDO.
DEF VAR v-del-no        AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-bol-cases     LIKE oe-boll.cases NO-UNDO.
DEF VAR v-set-qty       AS DECIMAL NO-UNDO.
DEF VAR v-part-qty      AS DEC FORMAT "999.9999" NO-UNDO.
DEF VAR v-net           LIKE ar-inv.gross NO-UNDO.
DEF VAR v-case-cnt      AS CHAR FORMAT "x(80)" extent 5 NO-UNDO.
DEF VAR v-case-line     AS CHAR NO-UNDO.
DEF VAR v-part-line     AS CHAR NO-UNDO.
DEF VAR v-pc            AS CHAR NO-UNDO. /* partial or complete */
DEF VAR v-i-dscr2       AS CHAR FORMAT "x(30)" NO-UNDO.

DEF BUFFER xar-inv for ar-inv .

DEF TEMP-TABLE w-sman NO-UNDO
  field sman AS CHAR FORMAT "x(4)".

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
DEF VAR v-po-no        LIKE ar-invl.po-no NO-UNDO.
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
DEF VAR v-inv-freight LIKE ar-inv.freight NO-UNDO.
DEF VAR v-frt-tax     AS DEC NO-UNDO.
DEF VAR v-tmp-lines   AS DEC NO-UNDO.
DEF VAR v-notes       AS CHAR EXTENT 4 FORMAT "x(80)" NO-UNDO.
DEF VAR v-notes-line  AS INT NO-UNDO.
DEF VAR v-inv-total   AS DEC NO-UNDO.
DEF VAR v-custno      LIKE cust.cust-no NO-UNDO.

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
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR lv-bol-no LIKE ar-invl.bol-no NO-UNDO.

DEF VAR lv-page AS INT INIT 0 NO-UNDO.
DEF VAR lv-currency AS CHAR NO-UNDO.
DEF VAR lv-taxable AS CHAR INIT "Y" NO-UNDO.
DEF VAR lv-line LIKE oe-ordl.e-num NO-UNDO.
DEF VAR lv-bolno LIKE oe-bolh.bol-no NO-UNDO.
DEF BUFFER b-inv-h FOR ar-inv.
DEF BUFFER b-save-line FOR reftable.
DEF BUFFER b-inv-head FOR ar-inv.
DEF VAR lv-bol-list AS CHAR NO-UNDO.
DEF VAR lv-bol-list-1 AS CHAR NO-UNDO.
DEF VAR lv-bol-list-2 AS CHAR NO-UNDO.
DEF VAR vShipID LIKE oe-bolh.ship-id NO-UNDO.
DEF VAR vCustNo LIKE oe-bolh.cust-no NO-UNDO.

DEF TEMP-TABLE tt-bols NO-UNDO
    FIELD bolno LIKE inv-head.bol-no.

DEF TEMP-TABLE tt-inv-line NO-UNDO
    FIELD shipnotes AS CHAR FORMAT "X(60)" EXTENT 4
    FIELD ship-id LIKE oe-bolh.ship-id.

DEF TEMP-TABLE tt-inv-line-item NO-UNDO
    FIELD ship-id LIKE oe-bolh.ship-id
    FIELD i-no LIKE inv-line.i-no
    FIELD i-name LIKE inv-line.i-name
    FIELD qty LIKE inv-line.inv-qty
    FIELD price LIKE inv-line.t-price
    FIELD amount LIKE inv-line.t-price
    FIELD ord-no LIKE inv-line.ord-no.

DEF TEMP-TABLE tt-inv-line-misc NO-UNDO
    FIELD ship-id LIKE oe-bolh.ship-id
    FIELD charge LIKE inv-misc.charge
    FIELD dscr LIKE inv-misc.dscr
    FIELD amt LIKE inv-misc.amt.

{custom/formtext.i NEW}

FUNCTION formatDate RETURNS CHAR
  ( INPUT ip-date AS DATE)  FORWARD.

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

/******************************************************************************
     ASSIGN v-comp-add1 = company.addr[1]
           v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
           v-comp-add3 = "Phone: 604.533.2545" 
           v-comp-add4 = "Fax  : 604.533.2633".
  
    ASSIGN v-comp-add1 = "MIDWEST FIBRE PRODUCTS, INC."
           v-comp-add2 = "2819 95TH AVENUE - PO BOX 397 - VIOLA ILLINOIS 61486"
           v-comp-add3 = "          (309)596-2955   FAX (309)596-2901"
           v-comp-add4 = "CORRUGATED CARTONS - FOLDING CARTONS - SET-UP BOXES"
           .
******************************************************************************/

FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR each report 
  WHERE report.term-id EQ v-term-id NO-LOCK,
  FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id NO-LOCK
  BREAK BY report.key-01
        BY report.key-02:

    FIND FIRST cust WHERE cust.company = ar-inv.company
                      AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.
    ASSIGN v-custno = cust.cust-no.

    v-contact = ar-inv.contact.
    FOR EACH b-inv-h WHERE b-inv-h.company = ar-inv.company
            AND b-inv-h.inv-no = ar-inv.inv-no
            AND b-inv-h.cust-no = ar-inv.cust-no 
            NO-LOCK
            BREAK BY b-inv-h.inv-date DESC:
            IF FIRST-OF(b-inv-h.inv-date) AND b-inv-h.inv-date NE ? THEN 
                v-inv-date = b-inv-h.inv-date.
            IF b-inv-h.contact NE v-contact  THEN DO:
                v-contact = b-inv-h.contact.
                LEAVE.
            END.
        END.
    
    FIND FIRST currency WHERE currency.company = cust.company 
        AND currency.c-code = cust.curr-code.
    IF AVAIL currency THEN
        lv-currency = currency.c-desc.

    IF ar-inv.sold-name <> "" 
      THEN
        ASSIGN  
         v-shipto-name = ar-inv.sold-name
         v-shipto-addr[1] = ar-inv.sold-addr[1]
         v-shipto-addr[2] = ar-inv.sold-addr[2]
         v-shipto-city = ar-inv.sold-city
         v-shipto-state = ar-inv.sold-state
         v-shipto-zip = ar-inv.sold-zip.
      ELSE DO:
       FIND FIRST shipto WHERE shipto.company = ar-inv.company
                           AND shipto.cust-no = ar-inv.cust-no
                           AND shipto.ship-id = ar-inv.ship-id NO-LOCK NO-ERROR.
       IF AVAIL shipto 
         THEN DO:
            ASSIGN  
            v-shipto-name = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city = shipto.ship-city
            v-shipto-state = shipto.ship-state
            v-shipto-zip = shipto.ship-zip.
            IF shipto.tax-code = "0TX" THEN
                lv-taxable = "N".
            ELSE
                lv-taxable = "Y".
         END.
     END.

     FIND FIRST oe-ord 
         WHERE oe-ord.company EQ cocode
           AND oe-ord.ord-no  EQ ar-inv.ord-no NO-LOCK NO-ERROR.
       IF AVAIL oe-ord THEN DO:
          
          IF AVAIL sman THEN v-salesname = sman.sname.
          FIND FIRST soldto WHERE soldto.company EQ cocode
                             AND soldto.cust-no EQ ar-inv.cust-no
                             AND soldto.sold-id EQ ar-inv.sold-id
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
                     soldto.cust-no EQ ar-inv.cust-no AND
                     soldto.sold-id EQ ar-inv.bill-to
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
                      v-soldto-name = ar-inv.cust-name
                      v-soldto-addr[1] = ar-inv.addr[1]
                      v-soldto-addr[2] = ar-inv.addr[2]
                      v-addr3          = ar-inv.city + ", " + 
                                         ar-inv.state + "  " + ar-inv.zip.
             END.
       END.
       ELSE
       DO:
          FIND FIRST soldto WHERE
               soldto.company EQ cocode AND
               soldto.cust-no EQ ar-inv.cust-no AND
               soldto.sold-id EQ ar-inv.BILL-TO
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
                v-soldto-name = ar-inv.cust-name
                v-soldto-addr[1] = ar-inv.addr[1]
                v-soldto-addr[2] = ar-inv.addr[2]
                v-addr3          = ar-inv.city + ", " + 
                                   ar-inv.state + "  " + ar-inv.zip.
       END.

     ASSIGN v-del-no = 0.

    
     IF v-inv-date EQ ? AND ar-inv.inv-date NE ? 
         THEN v-inv-date = ar-inv.inv-date.
         ELSE IF v-inv-date eq ? THEN v-inv-date = TODAY.
     v-date-ship = v-inv-date.
     
     IF ar-inv.fob-code BEGINS "ORIG" 
       THEN ASSIGN v-fob = "Origin".
       ELSE ASSIGN v-fob = "Destination".

     FIND FIRST carrier WHERE carrier.company = ar-inv.company 
                          AND carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.
     IF AVAIL carrier 
       THEN ASSIGN v-shipvia = carrier.dscr.
       ELSE ASSIGN v-shipvia = "".

     ASSIGN
       v-sold-addr3 = v-shipto-city /*+ ", " + v-shipto-state +
                      "  " + v-shipto-zip*/
       v-line = 1 v-printline = 0.

     FIND FIRST stax
       {sys/ref/stax1W.i}
       AND {sys/ref/taxgroup.i stax} eq ar-inv.tax-code no-lock no-error.
     IF NOT AVAIL stax 
       THEN
        FIND FIRST stax WHERE stax.tax-group EQ ar-inv.tax-code NO-LOCK NO-ERROR.
        IF AVAIL stax 
          THEN
           ASSIGN 
            v-tax-rate    = stax.tax-rate[1] +
                            stax.tax-rate[2] + stax.tax-rate[3]
            v-tax-code[1] = stax.tax-code[1]
            v-tax-code[2] = stax.tax-code[2]
            v-tax-code[3] = stax.tax-code[3]
            v-tx-rate[1]  = stax.tax-rate[1]
            v-tx-rate[2]  = stax.tax-rate[2]
            v-tx-rate[3]  = stax.tax-rate[3].

     ASSIGN v-tot-pallets = 0.

     FOR EACH ar-invl NO-LOCK 
         WHERE ar-invl.x-no EQ ar-inv.x-no  
         BREAK BY ar-invl.i-no:

         DO i = 1 TO 3:
             IF ar-invl.sman[i] ne "" THEN DO:
               CREATE w-sman.
               ASSIGN w-sman.sman = ar-invl.sman[i].
             END.
         END.

         ASSIGN 
           v-tot-qty = v-tot-qty + ar-invl.ship-qty
           v-t-weight = v-t-weight + (round(ar-invl.t-weight / ar-invl.qty, 2) 
                                      * ar-invl.inv-qty)
           v-tot-pallets = 0
           v-pc = "C". 

         IF LAST-OF(ar-invl.i-no) THEN DO:

           IF ar-invl.est-no NE "" THEN DO:

             FIND FIRST eb 
               WHERE eb.company = ar-invl.company 
                 AND eb.est-no = ar-invl.est-no 
                 AND eb.e-num = ar-invl.e-num 
                 AND eb.form-no = ar-invl.form-no 
                 AND eb.blank-no = ar-invl.blank-no NO-LOCK NO-ERROR.
             IF ar-invl.form-no = 0 AND 
                ar-invl.est-type = 2 
               THEN DO:

                FOR EACH fg-set NO-LOCK 
                  WHERE fg-set.company = ar-invl.company
                    AND fg-set.set-no = ar-invl.i-no:
                  ASSIGN v-set-qty = v-set-qty + fg-set.qtyPerSet.
                END.

                IF v-set-qty = 0 
                  THEN ASSIGN v-set-qty = 1.

                FOR EACH eb NO-LOCK 
                  WHERE eb.company = ar-invl.company 
                    AND eb.est-no = ar-invl.est-no 
                    AND eb.e-num = ar-invl.e-num 
                    AND eb.form-no NE 0:

                  FIND fg-set
                    WHERE fg-set.company = ar-invl.company 
                      AND fg-set.set-no = ar-invl.i-no  
                      AND fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.
                  IF AVAIL fg-set AND 
                     fg-set.qtyPerSet NE 0 
                    THEN ASSIGN v-part-qty = fg-set.qtyPerSet / v-set-qty.
                    ELSE ASSIGN v-part-qty = 1 / v-set-qty.

                  IF eb.cas-cnt = 0 
                    THEN ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                            eb.cas-wt, 2).
                    ELSE ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                            eb.cas-cnt, 2).
                  IF v-bol-cases ne 0
                    THEN ASSIGN v-tot-cas = v-bol-cases.
                END.  /* each eb */
             END. /* do */
             ELSE
             IF AVAIL eb THEN DO:
               IF eb.cas-cnt = 0 
                 THEN ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                 ELSE ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).

               IF v-bol-cases NE 0 
                 THEN ASSIGN v-tot-cas = v-bol-cases.
             END. /* do */
           END. /* est-no ne "" */

           ASSIGN 
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         END. /* last-of i-no */
     END.  /* each ar-invl */

    
     /** Build Salesman Id String **/
     ASSIGN v-salesman = "".
     FOR EACH w-sman BREAK BY w-sman.sman:
         
         IF FIRST-OF(w-sman.sman) 
           THEN ASSIGN v-salesman = v-salesman + w-sman.sman.

         DELETE w-sman.
     END. /* Salesman */

     ASSIGN 
       v-po-no = ar-inv.po-no
       v-bill-i = ar-inv.bill-i[1]
       v-ord-no = ar-inv.ord-no
       v-ord-date = ar-inv.ord-date.

     IF v-salesman EQ "" THEN DO:
      
       FIND FIRST oe-ord 
         WHERE oe-ord.company EQ cocode
           AND oe-ord.ord-no  EQ ar-inv.ord-no NO-LOCK NO-ERROR.
       IF AVAIL oe-ord THEN DO:
         
         FIND FIRST sman 
           WHERE sman.company = cocode 
             AND sman.sman = oe-ord.sman[1] NO-LOCK NO-ERROR.

         ASSIGN v-salesman = oe-ord.sman[1].

         IF AVAIL sman THEN v-salesname = sman.sname.
         
       END. /* IF AVAIL oe-ord */

       IF v-salesman EQ "" THEN DO:
         
         ASSIGN v-salesman = cust.sman.

         FIND FIRST sman 
           WHERE sman.company = cocode 
             AND sman.sman = cust.sman NO-LOCK NO-ERROR.             
         IF AVAIL sman THEN v-salesname = sman.sname.
       END. /* IF v-salesman = "" */
     END. /* IF v-salesman EQ "" */

     FIND FIRST ar-invl 
         WHERE ar-invl.x-no = ar-inv.x-no NO-LOCK NO-ERROR.
     IF AVAIL ar-invl THEN DO:
       ASSIGN 
         v-price-head = ar-invl.pr-uom
         v-po-no = ar-invl.po-no                  
         v-ord-no = ar-invl.ord-no.    

       FIND FIRST reftable NO-LOCK
         WHERE reftable.reftable EQ "brokerbol" 
           AND reftable.CODE EQ STRING(ar-invl.inv-no) NO-ERROR.
       IF AVAIL reftable AND 
          reftable.code2 NE "" 
         THEN DO:
           ASSIGN lv-bol-no = INT(reftable.code2).
           RELEASE reftable.
       END.
       ELSE ASSIGN lv-bol-no = ar-invl.bol-no.

     END.

     
/*      {ar/rep/invprot2.i}  /* xprint form */ */
/*      RUN printNotes ("IT",1).               */

     ASSIGN 
       v-printline = 29
       v-subtot-lines = 0
       v-t-tax = 0.

     FOR EACH ar-invl NO-LOCK 
         WHERE ar-invl.x-no = ar-inv.x-no:

         ASSIGN 
           v-case-line = "" 
           v-part-line = "" 
           v-case-cnt = "".          
         
/*          IF v-printline GE 57 THEN DO: */
/*            PAGE.                       */
/*            {ar/rep/invprot2.i}         */
/*            v-printline = 29.           */
/*          END.                          */

         FIND first oe-ordl 
           WHERE oe-ordl.company = cocode 
             AND oe-ordl.ord-no = ar-invl.ord-no 
             AND oe-ordl.i-no = ar-invl.i-no 
             AND oe-ordl.LINE = ar-invl.LINE NO-LOCK NO-ERROR.
         IF AVAIL oe-ordl 
           THEN
            ASSIGN 
              v-bo-qty = IF (ar-invl.qty - ar-invl.ship-qty -
                             oe-ordl.t-ship-qty) < 0 
                            THEN 0 
                            ELSE (ar-invl.qty - ar-invl.ship-qty -
                                  oe-ordl.t-ship-qty)
              lv-line = oe-ordl.e-num.
           ELSE 
            ASSIGN 
              v-bo-qty = IF ( ar-invl.qty - ar-invl.ship-qty ) < 0
                            THEN 0 
                            ELSE ar-invl.qty - ar-invl.ship-qty.

         ASSIGN 
           v-inv-qty = ar-invl.inv-qty
           v-ship-qty = ar-invl.ship-qty
           v-i-no = ar-invl.i-no
           v-i-dscr = ar-invl.i-name
           v-t-price = ar-invl.amt
           v-subtot-lines = v-subtot-lines + ar-invl.amt.
         
         v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100)).

         IF v-t-price NE ar-invl.amt THEN DO:
           CREATE w-tax.
           ASSIGN
             w-dsc     = "******ITEM TOTAL:"
             w-tax     = v-t-price
             v-lines   = v-lines + 1.
         END.

         ASSIGN 
           v-po-no  = ar-invl.po-no
           v-ord-no = ar-invl.ord-no
           v-i-dscr2 = ar-invl.part-dscr1.
          
         v-price-head = ar-invl.pr-uom.
         FIND FIRST oe-bolh WHERE oe-bolh.company = ar-invl.company AND
                              oe-bolh.b-no = ar-invl.b-no NO-LOCK NO-ERROR.
          IF AVAIL oe-bolh THEN 
              ASSIGN 
                lv-bolno = oe-bolh.bol-no
                vShipID  = oe-bolh.ship-id
                vCustNo  = oe-bolh.cust-no.
          ELSE
              ASSIGN
                vShipID = ar-inv.ship-id
                vCustNo = ar-inv.cust-no.
          FIND FIRST tt-inv-line WHERE tt-inv-line.ship-id = vShipId NO-ERROR.
            IF NOT AVAIL tt-inv-line THEN
            DO:
                FIND FIRST shipto WHERE shipto.company = ar-inv.company
                    AND shipto.cust-no = vCustNo
                    AND shipto.ship-id = vShipID NO-LOCK NO-ERROR.
                IF AVAIL shipto THEN 
                DO:
                    CREATE tt-inv-line.
                    ASSIGN 
                        tt-inv-line.ship-id = vShipID
                        tt-inv-line.shipnotes[1] = shipto.notes[1]
                        tt-inv-line.shipnotes[2] = shipto.notes[2]
                        tt-inv-line.shipnotes[3] = shipto.notes[3]
                        tt-inv-line.shipnotes[4] = shipto.notes[4].
                END.
                
            END.
            IF NOT ar-invl.misc THEN 
            DO:
                FIND FIRST tt-inv-line-item WHERE 
                    tt-inv-line-item.ship-id = vShipID
                    AND tt-inv-line-item.i-no = ar-invl.i-no NO-ERROR.
                IF AVAIL tt-inv-line-item THEN
                DO:
                    ASSIGN 
                        tt-inv-line-item.qty = tt-inv-line-item.qty + v-inv-qty
                        tt-inv-line-item.amount = tt-inv-line-item.amount + ar-invl.amt.
                END.
                ELSE DO:
                CREATE tt-inv-line-item.
                ASSIGN 
                        tt-inv-line-item.ship-id = vShipID
                        tt-inv-line-item.i-no = ar-invl.i-no
                        tt-inv-line-item.i-name =  ar-invl.i-name
                        tt-inv-line-item.qty = v-inv-qty
                        tt-inv-line-item.price = v-price
                        tt-inv-line-item.ord-no = ar-invl.ord-no
                        tt-inv-line-item.amount = ar-invl.amt.
                END.
            END. /*not misc*/
            ELSE DO:
                CREATE tt-inv-line-misc.
                ASSIGN 
                        tt-inv-line-misc.ship-id = vShipID
                        tt-inv-line-misc.charge = IF ar-invl.i-dscr NE "" THEN ar-invl.i-dscr ELSE ar-invl.i-name
                        tt-inv-line-misc.amt = ar-invl.amt.
            END. /*misc*/
           /*BUILD BOL LIST*/
            FIND FIRST tt-bols WHERE tt-bols.bolno = lv-bolno NO-LOCK NO-ERROR.
            IF NOT AVAIL tt-bols THEN
            DO:
                CREATE tt-bols.
                ASSIGN tt-bols.bolno = lv-bolno.
            END. /*AVAIL *TT-BOLS*/

         IF v-i-dscr2 = "" THEN v-i-dscr2 = ar-invl.i-dscr.
        
         IF v-ord-no = 0 AND v-ship-qty = 0 THEN v-ship-qty = v-inv-qty.
         
/*          IF AVAIL oe-ordl AND NOT oe-ordl.is-a-component THEN DO: */
/*              PUT                                                  */
/*                "<C2>" v-po-no            FORMAT "x(15)"           */
/*                "<C13>" lv-line           FORMAT ">>9"             */
/*                "<C17>" ar-invl.part-no   FORMAT "x(30)"           */
/*                "<C51>" v-inv-qty         FORMAT "->>>>>9"         */
/*                "<C58>" v-price           FORMAT "->>,>>9.99"      */
/*                "<C67>" ar-invl.amt       FORMAT "->>>,>>9.99"     */
/*                "<C79>" lv-taxable        FORMAT "X"               */
/*                SKIP                                               */
/*                "<C2>"  trim(string(v-ord-no), ">>>>>>>")          */
/*                "<C17>" v-i-dscr FORMAT "x(30)"                    */
/*                "<C51>" v-ship-qty        FORMAT "->>>>>9"         */
/*                "<C63>" v-price-head                               */
/*                SKIP                                               */
/*                "<C2>" trim(string(lv-bolno,">>>>>>>9"))           */
/*                "<C17>" ar-invl.part-dscr1 FORMAT "x(30)"          */
/*                "<C51>" ar-invl.qty FORMAT "->>>>>9"               */
/*                SKIP                                               */
/*                "<C17>" ar-invl.part-dscr2 FORMAT "x(30)"          */
/*                SKIP.                                              */
/*                                                                   */
/*              ASSIGN v-printline = v-printline + 4.                */
/*          END.                                                     */
     END. /* each ar-invl */
    
/*      RUN printNotes ("IN",4). */
     
/*      IF v-printline GE 66 THEN do: */
/*        PAGE.                       */
/*        {ar/rep/invprot2.i}         */
/*         v-printline = 29.          */
/*      END.                          */

     ASSIGN v-notes = "" v-notes-line = 0.

     NOTES:
      FOR EACH notes WHERE notes.rec_key = ar-inv.rec_key NO-LOCK:
          v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
          {SYS/INC/ROUNDUP.I v-tmp-lines}

          IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
            ASSIGN v-notes-line = v-notes-line + 1
                   v-notes[v-notes-line] = SUBSTR(NOTES.NOTE_TEXT,(1 + 80 * 
                                                  (i - 1)), 80).              
            IF v-notes-line >= 4  THEN LEAVE notes.        
          END.
     END.

     ASSIGN
       v-frt-tax     = ar-inv.freight
       v-inv-freight = IF NOT(ar-inv.freight eq 0 or not ar-inv.f-bill) THEN
                          ar-inv.freight 
                       ELSE 0.

     IF ar-inv.tax-code <> "" AND
        ar-inv.f-bill AND 
        ar-inv.freight <> 0 AND 
        AVAIL stax 
       THEN DO i = 1 TO 3:

         IF stax.tax-code[i] NE "" THEN DO:

           CREATE w-tax.
           ASSIGN
             w-dsc      = stax.tax-dscr[i]
             w-tax      = ROUND((IF stax.company EQ "yes" 
                                   THEN v-frt-tax
                                   ELSE ar-inv.freight) *
                                stax.tax-rate[i] / 100,2)
             v-frt-tax  = v-frt-tax + w-tax
             v-t-tax[i] = v-t-tax[i] + w-tax
             v-lines    = v-lines + 1.
         END.
     END.

     DO i = 1 TO 3:

       ASSIGN 
         v-bot-lab[i] = IF v-t-tax[i] NE 0 
                          THEN ((IF AVAIL stax 
                                   THEN STRING(CAPS(stax.tax-code[i]),"x(5)") 
                                   ELSE FILL(" ",5) ) +
                                 FILL(" ",6) + ":" + 
                                 STRING(v-t-tax[i],"->>>>>9.99")) 
                          ELSE "".
     END.
     
     ASSIGN
       v-inv-total = v-subtot-lines + ar-inv.tax-amt + v-inv-freight.

/*           PUT                                                              */
/*                                                                            */
/*         "<|10><R59><C53><FROM><R64><C80.75><RECT>"                         */
/*         "<R59><C56><#8><B>"                                                */
/*         "<=8><R59>    Sub Total:" v-subtot-lines      FORMAT "->>>,>>9.99" */
/*         "<=8><R60>      Freight:" v-inv-freight       FORMAT "->>>,>>9.99" */
/*         "<=8><R61>    Sales Tax:" ar-inv.tax-amt      FORMAT "->>>,>>9.99" */
/*         "<=8><R62>Total Invoice:" v-inv-total         FORMAT "->>>,>>9.99" */
/*         "<=8><R63><C69><P8><FGCOLOR=RED>" lv-currency FORMAT "X(20)".      */
/*                                                                            */
/*     ASSIGN v-printline = v-printline + 6.                                  */
/*                                                                            */
/*                                                                      */
/*     IF v-printline <= 66 THEN page. /*PUT SKIP(74 - v-printline). */ */
     
    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.
              xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 

    lv-bol-list = "".
    FOR EACH tt-bols BY tt-bols.bolno:
        lv-bol-list = lv-bol-list + string(tt-bols.bolno) + ",".
    END.
    RUN parseBOLList (lv-bol-list, 27, OUTPUT lv-bol-list-1, OUTPUT lv-bol-list-2).
    RUN checkPage(YES).
    PUT "<P9>".
    RUN printNotes ("IT",1).
    FOR EACH tt-inv-line:
       RUN printShipNotes (tt-inv-line.shipnotes).
       FOR EACH tt-inv-line-item WHERE tt-inv-line-item.ship-id = tt-inv-line.ship-id:
           PUT             
                "<C2>" tt-inv-line-item.qty         FORMAT "->>>>>9"
                "<C10>" tt-inv-line-item.i-name       FORMAT "x(30)"
                "<C54>" tt-inv-line-item.price    FORMAT "->,>>>,>>9.99"
                "<C64>" tt-inv-line-item.amount   FORMAT "->,>>>,>>9.99"
                "<C75>" trim(string(tt-inv-line-item.ord-no,">>>>>>>"))
                SKIP.
            v-printline = v-printline + 1.
            RUN checkPage(NO).
       END.
       FOR EACH tt-inv-line-misc WHERE tt-inv-line-misc.ship-id = tt-inv-line.ship-id:
            PUT             
                "<C2>" 1                            FORMAT "->>>>>9"
                "<C10>" tt-inv-line-misc.charge     FORMAT "x(30)"
/*                 "<C55.5>" tt-inv-line-misc.amt      FORMAT "->>,>>9.99" */
                "<C64>" tt-inv-line-misc.amt      FORMAT "->,>>>,>>9.99"
                SKIP.
            v-printline = v-printline + 1.
            RUN checkPage(NO).
       END.
       PUT SKIP(1).
       v-printline = v-printline + 1.
       RUN checkPage(NO).
    END.
    PUT SKIP(1).
    v-printline = v-printline + 1.
    RUN checkPage(NO).
    RUN printNotes ("IN",4).
    RUN checkPage(NO).
    
    PUT     "<P10><|10><R59><C53><FROM><R64><C80.75><RECT>"
            "<R59><C55><#8><B>" 
            "<=8><R60>    Sub Total:" v-subtot-lines      FORMAT "->,>>>,>>9.99"
            "<=8><R61>    Sales Tax:" ar-inv.tax-amt  FORMAT "->,>>>,>>9.99"
            "<=8><R62>Total Invoice:" v-inv-total         FORMAT "->,>>>,>>9.99"
            "<=8><R63><C69><P8><FGCOLOR=RED>" lv-currency FORMAT "X(20)"
            "<FGCOLOR=BLACK>".
/*     ASSIGN v-printline = v-printline + 6. */
/*     IF v-printline <= 66 THEN             */
    PAGE.
    FOR EACH tt-inv-line:
        DELETE tt-inv-line.
    END.
    FOR EACH tt-inv-line-item:
        DELETE tt-inv-line-item.
    END.
 
    end. /* each ar-inv */

PROCEDURE checkPage:
    DEF INPUT PARAMETER ipl-force-header AS LOG NO-UNDO.

    IF NOT ipl-force-header THEN DO:
        IF v-printline GE 66 THEN 
        DO: 
            PAGE.                
            {ar/rep/invprot2.i}                
            v-printline = 29. 
        END.
    END.
    ELSE
    DO:
        {ar/rep/invprot2.i}                
        v-printline = 29. 
    END.
    
END PROCEDURE.

PROCEDURE printShipNotes:
    DEF INPUT PARAMETER ipc-notes AS CHAR EXTENT 4 NO-UNDO.
    DEF VAR li AS INT NO-UNDO.
    
    DO i = 1 TO 4:
        IF i = 1 OR ipc-notes[i] NE "" THEN
        DO:
            PUT "<C10>" ipc-notes[i] FORMAT "X(53)" SKIP.
            v-printline = v-printline + 1.
        END.
    END.
    PUT SKIP.
    v-printline = v-printline + 1.
END PROCEDURE.

PROCEDURE parseBOLList:
    DEF INPUT PARAMETER ipc-bol-list AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipi-max-length AS INT NO-UNDO.
    DEF OUTPUT PARAMETER opc-bol-list-1 AS CHAR NO-UNDO.
    DEF OUTPUT PARAMETER opc-bol-list-2 AS CHAR NO-UNDO.

    DEF VAR li-comma-index AS INT NO-UNDO.

    IF LENGTH(ipc-bol-list) > ipi-max-length THEN
    DO:
        li-comma-index = INDEX(ipc-bol-list, ",", ipi-max-length - 5).
        ASSIGN
            opc-bol-list-1 = SUBSTRING(ipc-bol-list,1,li-comma-index)
            opc-bol-list-2 = TRIM(SUBSTRING(ipc-bol-list,li-comma-index + 1,LENGTH(ipc-bol-list)),",").
    END.
    ELSE
        ASSIGN
            opc-bol-list-1 = TRIM(ipc-bol-list,",")
            opc-bol-list-2 = "".

END PROCEDURE.

PROCEDURE printNotes:
    DEF INPUT PARAMETER ipcTypeList AS CHAR NO-UNDO.
    DEF INPUT PARAMETER ipiLines AS INT NO-UNDO.
    
    DEF VAR li AS INT.
    DEF VAR li-width AS INT.
    DEF VAR lc-text AS CHAR.
    DEF BUFFER b-ar-inv FOR ar-inv.

    ASSIGN
    li-width = 40
    lc-text = "".
  
  FOR EACH b-ar-inv WHERE b-ar-inv.company = ar-inv.company
      AND b-ar-inv.inv-no = ar-inv.inv-no
      AND b-ar-inv.cust-no = ar-inv.cust-no NO-LOCK:
      FIND FIRST notes
        WHERE notes.rec_key = b-ar-inv.rec_key
        AND LOOKUP(notes.note_code,ipcTypeList) GT 0
        NO-LOCK NO-ERROR.
            IF AVAIL notes THEN do:
                lc-text = lc-text + " " + notes.note_text + CHR(10).
                LEAVE.
            END.
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
            PUT "<C10>" tt-formtext.tt-text FORMAT "X(100)" SKIP.
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

