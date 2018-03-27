/* ------------------------------------------------------- oe/rep/invptreelot.p */
/* PRINT INVOICE   Xprint form for PEACHTREE with FG Lot                       */ 
/* -------------------------------------------------------------------------- */
DEF INPUT PARAM ip-copy-title AS cha NO-UNDO.
{sys/inc/var.i shared}

{oe/rep/invoice.i}

DEF SHARED VAR v-fr-tax AS LOGICAL INITIAL NO NO-UNDO.

DEF VAR v-salesman     AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR v-salesname    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fob          AS CHAR FORMAT "x(27)" NO-UNDO.
DEF VAR v-addr3        AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sold-addr3   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-name  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-id    AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-shipto-addr  AS CHAR FORMAT "x(30)" NO-UNDO EXTENT 2.
DEF VAR v-shipto-city  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-shipto-state AS CHAR FORMAT "x(2)"  NO-UNDO.
DEF VAR v-shipto-zip   AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-case-cnt     AS CHAR FORMAT "x(80)" NO-UNDO EXTENT 5.
DEF VAR disp-frt       AS CHAR FORMAT "x(8)"  NO-UNDO INIT "Freight:".
DEF VAR v-pc           AS CHAR                NO-UNDO. /* partial or complete */
DEF VAR v-ord-del-hdr  AS CHAR FORMAT "x(3)"  NO-UNDO INIT "Del".
DEF VAR v-part-info    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-i-no         AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-i-dscr       AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR v-bill-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ship-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-price-head   AS CHAR FORMAT "x(5)"  NO-UNDO.
DEF VAR ls-image1      AS CHAR                NO-UNDO.
DEF VAR ls-full-img1   AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR v-comp-add1    AS CHAR FORM "x(30)"   NO-UNDO.
DEF VAR v-comp-add2    AS CHAR FORM "x(30)"   NO-UNDO.
DEF VAR v-comp-add3    AS CHAR FORM "x(30)"   NO-UNDO.
DEF VAR v-comp-add4    AS CHAR FORM "x(30)"   NO-UNDO.

DEF VAR v-shipvia     LIKE carrier.dscr           NO-UNDO.
DEF VAR v-t-weight    LIKE inv-line.t-weight      NO-UNDO.
DEF VAR v-tax-code    LIKE stax.tax-code          NO-UNDO.
DEF VAR v-tx-rate     LIKE stax.tax-rate          NO-UNDO.
DEF VAR v-bol-cases   LIKE oe-boll.cases          NO-UNDO.
DEF VAR v-net         LIKE inv-head.t-inv-rev     NO-UNDO.
DEF VAR v-po-no       LIKE inv-line.po-no         NO-UNDO.
DEF VAR v-ord-no      LIKE oe-ord.ord-no          NO-UNDO.
DEF VAR v-ord-date    LIKE oe-ord.ord-date        NO-UNDO.
DEF VAR v-rel-po-no   LIKE oe-rel.po-no           NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-net2        LIKE inv-head.t-inv-rev     NO-UNDO.

DEF VAR v-line         AS INT                 NO-UNDO.
DEF VAR v-printline    AS INT                 NO-UNDO.
DEF VAR v-tot-pallets  AS INT                 NO-UNDO.
DEF VAR v-tot-qty      AS INT                 NO-UNDO.
DEF VAR v-set-qty      AS INT                 NO-UNDO.
DEF VAR v              as INT                 NO-UNDO.
DEF VAR v-bo-qty       AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-inv-qty      AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-ord-qty      AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-ship-qty     AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-lines        AS INT                 NO-UNDO.

DEF VAR v-tot-cas      AS DEC FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tax-rate     AS DEC FORMAT "->>>.99"    NO-UNDO.
DEF VAR v-part-qty     AS DEC FORMAT "999.9999"   NO-UNDO.
DEF VAR tmp1           AS DEC                     NO-UNDO.
DEF VAR v-price        AS DEC FORMAT ">>>>9.9999" NO-UNDO.
DEF VAR v-t-price      AS DEC FORMAT ">>>>>>9.99" NO-UNDO.
DEF VAR v-subtot-lines AS DEC                     NO-UNDO.         
DEF VAR v-t-tax        AS DEC                     NO-UNDO extent 3.
DEF VAR v-frt-tax      AS DEC                     NO-UNDO.         

DEF VAR v-inv-date  AS DATE INITIAL TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR v-date-ship AS DATE INITIAL TODAY                     NO-UNDO.
DEF VAR tmp2        AS DATE                                   NO-UNDO.

DEF VAR v-tailgate     AS CHAR NO-UNDO FORM "x(30)".
DEF VAR v-tail-price   AS CHAR NO-UNDO FORM "x(10)".
DEF VAR v-ship-qty1    AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR v-ship-qty1i   AS INT  NO-UNDO.
DEF VAR v-price2       AS DECI NO-UNDO.
DEF VAR v-price-head2 AS CHAR FORMAT "X(5)" NO-UNDO.
DEF VAR v-soldto-name   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-soldto-addr   AS CHAR FORMAT "x(30)" extent 2 NO-UNDO.

DEF BUFFER xinv-head FOR inv-head.
DEF BUFFER xinv-line FOR inv-line.

DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER ref-sell-price FOR reftable.
DEF VAR v-qty  AS CHAR  NO-UNDO.
DEF VAR v-qty2 AS CHAR  NO-UNDO.
DEF VAR v-rel  AS CHAR FORMAT "x(1)" NO-UNDO.


DEF TEMP-TABLE w-sman NO-UNDO
  FIELD sman AS CHAR FORMAT "x(4)".

FIND FIRST inv-head NO-LOCK NO-ERROR.

ASSIGN ls-image1 = "images\Peachtree_logo_2018.png"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
   FIRST xinv-head WHERE RECID(xinv-head) EQ report.rec-id NO-LOCK
     BREAK BY report.key-01
           BY report.key-02:

    FIND FIRST cust WHERE cust.company EQ xinv-head.company
                      AND cust.cust-no EQ xinv-head.cust-no NO-LOCK NO-ERROR.

    ASSIGN v-shipto-name    = xinv-head.sold-name
           v-shipto-addr[1] = xinv-head.sold-addr[1]
           v-shipto-addr[2] = xinv-head.sold-addr[2]
           v-shipto-city    = xinv-head.sold-city
           v-shipto-state   = xinv-head.sold-state
           v-shipto-zip     = xinv-head.sold-zip
           v-shipto-id      = xinv-head.sold-no.

    FIND FIRST oe-bolh NO-LOCK 
      WHERE oe-bolh.company = xinv-head.company 
        AND oe-bolh.bol-no = xinv-head.bol-no use-index bol-no NO-ERROR.
    IF AVAIL oe-bolh THEN DO:

      FIND FIRST shipto NO-LOCK
        WHERE shipto.company EQ oe-bolh.company 
          AND shipto.cust-no EQ oe-bolh.cust-no 
          AND shipto.ship-id EQ oe-bolh.ship-id NO-ERROR.
      IF AVAIL shipto 
        THEN assign  v-shipto-name = shipto.ship-name
                     v-shipto-addr[1] = shipto.ship-addr[1]
                     v-shipto-addr[2] = shipto.ship-addr[2]
                     v-shipto-city = shipto.ship-city
                     v-shipto-state = shipto.ship-state
                     v-shipto-zip = shipto.ship-zip
                     v-shipto-id = shipto.ship-id.
    END. /* avail oe-bolh */

    IF NOT v-reprint OR xinv-head.inv-no EQ 0 
      THEN RUN oe/get-inv#.p (ROWID(xinv-head)).


    DO TRANSACTION:

      FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).
      
      IF inv-head.inv-date NE ? THEN
         v-inv-date = inv-head.inv-date.
      
      IF inv-head.fob-code BEGINS "ORIG" THEN
         v-fob = "Origin".
      ELSE
         v-fob = "Destination".

      FIND FIRST carrier NO-LOCK 
        WHERE carrier.company EQ inv-head.company 
          AND carrier.carrier EQ inv-head.carrier NO-ERROR.
      IF AVAIL carrier THEN
         ASSIGN v-shipvia = carrier.dscr.
      ELSE
         ASSIGN v-shipvia = "".

      ASSIGN v-sold-addr3 = v-shipto-city + ", " + 
                            v-shipto-state + "  " + v-shipto-zip
             v-line       = 1 
             v-printline  = 0.

      FIND FIRST stax NO-LOCK 
        {sys/ref/stax1W.i}
          and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr NO-ERROR.
      IF NOT AVAIL stax THEN 
        FIND FIRST stax WHERE stax.tax-group EQ inv-head.tax-gr NO-LOCK NO-ERROR.

      IF AVAIL stax 
        THEN  assign v-tax-rate    = stax.tax-rate[1] +
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
         break by xinv-line.i-no:

         DO i = 1 TO 3:

           IF xinv-line.sman[i] NE "" THEN DO:

              CREATE w-sman.
              ASSIGN w-sman.sman = xinv-line.sman[i].
           END.
         END.

         ASSIGN v-tot-qty     = v-tot-qty + xinv-line.ship-qty
                v-t-weight    = v-t-weight + (ROUND(xinv-line.t-weight /
                                xinv-line.qty, 2) * xinv-line.inv-qty)
                v-tot-pallets = 0
                v-pc          = "C". /* complete*/

         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no:
           
          ASSIGN v-pc = "P". /* partial*/ 

          FOR EACH oe-boll NO-LOCK 
            WHERE oe-boll.company EQ oe-bolh.company 
              AND oe-boll.b-no    EQ oe-bolh.b-no 
              AND oe-boll.i-no    EQ xinv-line.i-no 
              AND oe-boll.ord-no  EQ xinv-line.ord-no:

            /** Bill Of Lading TOTAL CASES **/
            ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases
                   v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                   (IF oe-boll.partial GT 0 THEN 1 ELSE 0)
                   v-rel = oe-boll.s-code.
             
            IF oe-boll.p-c THEN v-pc = "C". /*complete*/
          END. /* each oe-boll */

          ASSIGN v-date-ship = oe-bolh.bol-date.
                

         END. /* each oe-bolh */

         IF LAST-OF(xinv-line.i-no) THEN DO:

           IF xinv-line.est-no NE "" THEN DO:

             FIND FIRST eb NO-LOCK 
               WHERE eb.company = xinv-line.company 
                 AND eb.est-no = xinv-line.est-no 
                 AND eb.e-num = xinv-line.e-num 
                 AND eb.form-no = xinv-line.form-no 
                 AND eb.blank-no = xinv-line.blank-no NO-ERROR.
             IF xinv-line.form-no = 0 AND xinv-line.est-type = 2 
               THEN DO:

               FOR EACH fg-set NO-LOCK WHERE fg-set.company = xinv-line.company
                  AND fg-set.set-no = xinv-line.i-no:
                 ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
               END.

               IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.

               FOR EACH eb NO-LOCK 
                 WHERE eb.company EQ xinv-line.company 
                   AND eb.est-no  EQ xinv-line.est-no 
                   AND eb.e-num   EQ xinv-line.e-num 
                   AND eb.form-no NE 0:

                 FIND fg-set NO-LOCK
                   WHERE fg-set.company EQ xinv-line.company 
                     AND fg-set.set-no  EQ xinv-line.i-no  
                     AND fg-set.part-no EQ eb.stock-no NO-ERROR.
                 IF AVAIL fg-set AND fg-set.part-qty NE 0 
                   THEN ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
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
                  THEN assign v-tot-cas = v-bol-cases.
             END.  /* do */
           END. /* est-no ne "" */

           ASSIGN v-t-weight = 0
                  v-tot-cas  = 0
                  v-tot-qty  = 0.
         END. /* last-of i-no */
      END. /* each xinv-line */

      /** Build Salesman Id String **/
      ASSIGN v-salesman = "".
      FOR EACH w-sman BREAK BY w-sman.sman:
        if first-of(w-sman.sman) 
          THEN ASSIGN v-salesman = v-salesman + w-sman.sman.
          DELETE w-sman.
      END.

      FIND FIRST oe-bolh NO-LOCK
        where oe-bolh.company EQ inv-head.company 
          AND oe-bolh.bol-no  EQ inv-head.bol-no USE-INDEX bol-no NO-ERROR.
      IF AVAIL oe-bolh 
        THEN ASSIGN v-rel-po-no = oe-bolh.po-no.

      FIND FIRST inv-line NO-LOCK
        WHERE inv-line.r-no EQ inv-head.r-no NO-ERROR.
      IF AVAIL inv-line THEN DO:

        ASSIGN
           v-price-head = inv-line.pr-uom
           v-price-head2 = inv-line.pr-uom.

        FIND FIRST oe-ord WHERE
             oe-ord.company EQ cocode AND
             oe-ord.ord-no  EQ inv-line.ord-no
             NO-LOCK NO-ERROR.

        IF avail oe-ord THEN
        DO:
           ASSIGN
              v-po-no    = oe-ord.po-no
              v-bill-i   = oe-ord.bill-i[1]
              v-ord-no   = oe-ord.ord-no
              v-ord-date = oe-ord.ord-date.

           FIND FIRST soldto WHERE
                soldto.company EQ cocode AND
                soldto.cust-no EQ oe-ord.cust-no AND
                soldto.sold-id EQ oe-ord.sold-id
                NO-LOCK NO-ERROR.

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
           ASSIGN
              v-price-head = inv-line.pr-uom
              v-price-head2 = inv-line.pr-uom.

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
      END.
      
      /* br task 12081001 */
      FOR EACH inv-misc NO-LOCK 
        WHERE inv-misc.company = inv-head.company 
          AND inv-misc.r-no = inv-head.r-no 
          AND inv-misc.bill = "Y" 
         BREAK BY ord-no :
          IF FIRST(inv-misc.ord-no) THEN DO:
             IF v-salesman = "" THEN 
                 ASSIGN 
                 v-salesman = inv-misc.s-man[1].
          END.
      END.
      
      {oe/rep/invptreelot.i}  /* xprint form */

      ASSIGN v-subtot-lines = 0
             v-t-tax = 0
             v-net2 = 0.

      FOR EACH inv-line NO-LOCK 
        WHERE inv-line.r-no = inv-head.r-no:

        ASSIGN v-case-cnt = ""
               v-pc       = "P" /* partial*/ 
               i          = 0.
       
        IF inv-line.inv-qty EQ 0 AND inv-line.ship-qty EQ 0 THEN NEXT .

        
        FOR EACH oe-boll NO-LOCK 
          WHERE oe-boll.company EQ inv-line.company
            AND oe-boll.bol-no  EQ inv-head.bol-no
            and oe-boll.i-no    EQ inv-line.i-no USE-INDEX bol-no:

           /** Build Case Count Display Lines **/
           IF oe-boll.cases NE 0 AND oe-boll.qty-case NE 0 
             THEN 
              IF oe-boll.p-c THEN v-pc = "C". /*complete*/

        END. /* each oe-boll */

        v-case-cnt[1] = v-case-cnt[1] + 
                               FILL(" ",32 - LENGTH(v-case-cnt[1])) 
                                .

        IF v-printline > 62 THEN DO:
          PAGE.
          /* br task 12081001 */
          {oe/rep/invptreelot.i}
          v-printline = 21.
        END.

        assign v-line = v-line + 1
               v-printline = v-printline + 2.

        FIND FIRST oe-ordl NO-LOCK
          WHERE oe-ordl.company = cocode 
            AND oe-ordl.ord-no = inv-line.ord-no 
            AND oe-ordl.i-no = inv-line.i-no NO-ERROR.
        IF AVAIL oe-ordl 
          THEN ASSIGN v-bo-qty = IF (inv-line.qty - inv-line.ship-qty -
                                     oe-ordl.t-ship-qty) < 0 
                                   THEN 0 
                                   ELSE (inv-line.qty - inv-line.ship-qty -
                                         oe-ordl.t-ship-qty).
          ELSE assign v-bo-qty = IF (inv-line.qty - inv-line.ship-qty ) < 0
                                   THEN 0 
                                   ELSE inv-line.qty - inv-line.ship-qty.


        ASSIGN v-ord-qty      = inv-line.qty
               v-inv-qty      = inv-line.inv-qty
               v-ship-qty     = inv-line.ship-qty
               v-i-no         = inv-line.i-no
               v-i-dscr       = inv-line.i-name
               v-price        = inv-line.price * (1 - (inv-line.disc / 100))
               v-t-price      = inv-line.t-price
               v-subtot-lines = v-subtot-lines + inv-line.t-price.

        /*IF inv-line.tax AND AVAIL stax 
          THEN DO i = 1 to 3:

           IF stax.tax-code[i] NE "" THEN DO:

             CREATE w-tax.
             ASSIGN w-dsc = stax.tax-dscr[i]
                    w-tax = ROUND((IF stax.company EQ "yes" 
                                     THEN v-t-price
                                     ELSE inv-line.t-price) *
                                  stax.tax-rate[i] / 100,2)
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
           END.
        END.

        IF v-t-price NE inv-line.t-price THEN do:

          CREATE w-tax.
          ASSIGN w-dsc     = "******ITEM TOTAL:"
                 w-tax     = v-t-price
                 v-lines   = v-lines + 1.
        END.*/

        ASSIGN v-po-no  = inv-line.po-no
               v-ord-no = inv-line.ord-no
               v-price-head2 = inv-line.pr-uom
               v-price-head = fill(" ",5) + inv-line.pr-uom
               v-tail-price = ""                         
               v-tailgate   = ""
               /*v-ship-qty1  = ""
               v-ship-qty1i = 0*/
               v-price2     = 0
               v-qty = ""
               v-qty2 = "".
       
        IF v-ship-qty EQ v-inv-qty  THEN
            v-qty = string(v-ship-qty).
        ELSE IF v-ship-qty > 0 AND v-inv-qty EQ 0 AND v-rel EQ "S"  THEN
            v-qty = string(v-ship-qty).
        ELSE IF v-ship-qty EQ 0 AND v-inv-qty > 0 AND v-rel EQ "I" THEN
            v-qty2 = string(v-inv-qty).
        ELSE IF v-ship-qty NE v-inv-qty AND v-ship-qty > 0 AND v-inv-qty > 0 THEN
            ASSIGN 
                v-qty = STRING(v-ship-qty)
                v-qty2 = string(v-inv-qty).
        ELSE ASSIGN 
                v-qty = STRING(v-ship-qty)
                v-qty2 = string(v-inv-qty).
        
        /*FIND FIRST itemfg NO-LOCK WHERE itemfg.company = inv-line.company AND 
                                        itemfg.i-no    = inv-line.i-no NO-ERROR.
        IF AVAIL itemfg THEN
            FOR EACH fg-rcpth NO-LOCK OF itemfg 
                WHERE (IF AVAIL oe-ordl THEN fg-rcpth.job-no = oe-ordl.job-no ELSE TRUE) 
                  AND (IF AVAIL oe-ordl THEN fg-rcpth.job-no2 = oe-ordl.job-no2 ELSE TRUE),
                EACH fg-rdtlh  WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no      
                 AND fg-rdtlh.rita-code            EQ fg-rcpth.rita-code 
                 AND fg-rdtlh.stack-code BEGINS "TAIL"
                NO-LOCK:

                   ASSIGN v-tailgate = fg-rdtlh.stack-code.

                   FOR EACH oe-boll NO-LOCK 
                      WHERE oe-boll.company EQ inv-line.company
                        AND oe-boll.bol-no  EQ inv-head.bol-no
                        AND oe-boll.i-no    EQ inv-line.i-no USE-INDEX bol-no:

                       FIND FIRST b-oe-rell  
                         WHERE b-oe-rell.company  EQ oe-boll.company  
                           AND b-oe-rell.ord-no   EQ oe-boll.ord-no    
                           AND b-oe-rell.i-no     EQ oe-boll.i-no      
                           AND b-oe-rell.LINE     EQ oe-boll.line      
                           AND b-oe-rell.r-no     EQ oe-boll.r-no      
                           AND b-oe-rell.rel-no   EQ oe-boll.rel-no    
                           AND b-oe-rell.b-ord-no EQ oe-boll.b-ord-no  
                           AND b-oe-rell.po-no    EQ oe-boll.po-no     
                         USE-INDEX ord-no NO-LOCK NO-ERROR.
                       IF AVAIL b-oe-rell THEN DO:
                          FIND b-oe-rel NO-LOCK WHERE 
                               b-oe-rel.r-no EQ b-oe-rell.link-no
                               USE-INDEX seq-no NO-ERROR.
                          IF NOT AVAIL b-oe-rel THEN
                            FIND FIRST b-oe-rel NO-LOCK
                                WHERE b-oe-rel.company  EQ b-oe-rell.company
                                  AND b-oe-rel.link-no  EQ b-oe-rell.r-no
                                  AND b-oe-rel.ord-no   EQ b-oe-rell.ord-no
                                  AND b-oe-rel.i-no     EQ b-oe-rell.i-no
                                  AND b-oe-rel.line     EQ b-oe-rell.line
                                  AND b-oe-rel.rel-no   EQ b-oe-rell.rel-no
                                  AND b-oe-rel.b-ord-no EQ b-oe-rell.b-ord-no
                                  AND b-oe-rel.po-no    EQ b-oe-rell.po-no
                                USE-INDEX link NO-ERROR.
                            IF NOT AVAIL b-oe-rel THEN
                            FIND FIRST b-oe-rel NO-LOCK
                                WHERE b-oe-rel.company  EQ b-oe-rell.company
                                  AND b-oe-rel.ord-no   EQ b-oe-rell.ord-no
                                  AND b-oe-rel.i-no     EQ b-oe-rell.i-no
                                  AND b-oe-rel.line     EQ b-oe-rell.line
                                  AND b-oe-rel.rel-no   EQ b-oe-rell.rel-no
                                  AND b-oe-rel.b-ord-no EQ b-oe-rell.b-ord-no
                                  AND b-oe-rel.po-no    EQ b-oe-rell.po-no
                                USE-INDEX ord-item NO-ERROR.
                          IF AVAIL b-oe-rel THEN DO:
                             FIND FIRST ref-sell-price WHERE
                                        ref-sell-price.reftable EQ "oe-rel.sell-price" AND
                                        ref-sell-price.company  EQ STRING(b-oe-rel.r-no,"9999999999")
                             NO-LOCK NO-ERROR.
                             IF AVAIL ref-sell-price THEN
                                ASSIGN v-price-head = string(ref-sell-price.val[1],">>>9.9999")
                                       v-tail-price = string((ref-sell-price.val[1] * fg-rdtlh.qty),">>,>>9.99").
                             ELSE
                                ASSIGN v-price-head = string(0,">>>9.9999")
                                       v-tail-price = string(0,">>,>>9.99").
                             
                             RELEASE ref-sell-price.
        
                             ASSIGN v-ship-qty1 = STRING(fg-rdtlh.qty,">>>>>>>9")
                                    v-ship-qty1i = fg-rdtlh.qty.
                          END. /* avail b-oe-rel */
                       END. /* avail b-oe-rell */
                       LEAVE.
                   END. /* each oe-boll */
        END. /* avail itemfg */ 

        IF v-ship-qty1i > 0 THEN DO:
            RUN compute-ext-price (recid(inv-line), v-ship-qty1i, OUTPUT v-price2).
            ASSIGN v-net2 = v-net2 + v-price2. 
        END.
        ELSE*/
            ASSIGN v-net2 = v-net2 + inv-line.t-price. 


        PUT SPACE(1)
             v-po-no 
             inv-line.part-no  
            SPACE(1)
             v-i-dscr    FORMAT "x(30)" 
             (IF v-qty NE "" THEN v-qty ELSE " ") /*FORMAT "->>>>>>9" */
            SPACE(2)
             v-price     FORMAT ">>>,>>9.9999"                
             (IF v-ship-qty1i > 0 THEN v-price2 ELSE inv-line.t-price)  FORMAT "->>>,>>9.99"                
            SKIP
             v-ord-no 
            SPACE(10) 
             inv-line.i-no 
            SPACE(1)
             (IF v-tailgate <> "" THEN v-i-dscr ELSE "") FORM "x(30)"
             /*v-ship-qty1 */
            SPACE(1) 
            (IF v-qty2 NE "" THEN v-qty2 ELSE " ") /*FORMAT "->>>>>9"*/
            SPACE(3)
             v-pc        FORMAT "x"   
             v-price-head FORM "x(9)" SPACE(2)
             v-tail-price FORM "x(10)"
            SKIP.

        ASSIGN v-printline = v-printline + 2.

        IF v-tailgate <> "" THEN DO:
          PUT SPACE(40) v-tailgate SPACE(10) v-price-head2
            SKIP.
          ASSIGN v-printline = v-printline + 1.
        END.

        DO i = 1 TO 5:

          IF v-case-cnt[i] NE "" THEN DO:
            PUT v-case-cnt[i] SKIP.
            ASSIGN v-printline = v-printline + 1.
          END.
        END.

        PUT SKIP(1).

        ASSIGN v-printline = v-printline + 1.
      END. /* each inv-line */

      FOR EACH inv-misc NO-LOCK 
        WHERE inv-misc.company = inv-head.company 
          AND inv-misc.r-no = inv-head.r-no 
          AND inv-misc.bill = "Y" 
         BREAK BY ord-no WITH FRAME detailm:

         IF v-printline > 62 THEN DO:
           PAGE.  
           /* br task 12081001 */
           {oe/rep/invptreelot.i}
            ASSIGN v-printline = 21.
         END.

         IF FIRST(inv-misc.ord-no) THEN DO:
             IF v-salesman = "" THEN 
                 ASSIGN 
                 v-salesman = inv-misc.s-man[1].
             
           PUT "** Miscellaneous Items ** " AT 23  SKIP(1).

           ASSIGN v-printline = v-printline + 2.
         END.


         PUT inv-misc.charge AT 17
             inv-misc.dscr 
             inv-misc.amt    AT 85 
            SKIP.
         

         ASSIGN v-subtot-lines = v-subtot-lines + inv-misc.amt
                v-net2         = v-net2 + inv-misc.amt
                v-printline = v-printline + 1.

         /*IF inv-misc.tax AND AVAIL stax THEN
            DO i = 1 TO 3:

            IF stax.tax-code[i] NE "" THEN DO:

              CREATE w-tax.
              ASSIGN w-dsc      = stax.tax-dscr[i]
                     w-tax      = IF stax.company EQ "yes" 
                                    THEN v-t-price
                                    ELSE inv-misc.amt
                     w-tax      = ROUND(w-tax * (1 + (stax.tax-rate[i] / 100)),2)
                                        - w-tax
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
            END.
         END.

         IF v-t-price NE inv-misc.amt THEN DO :

           CREATE w-tax.
           ASSIGN w-dsc     = "******ITEM TOTAL:"
                  w-tax     = v-t-price
                  v-lines   = v-lines + 1.
         END.*/
      END. /* each inv-misc */

      IF v-prntinst THEN do:

        DO i = 1 TO 4:

          IF inv-head.bill-i[i] NE "" THEN DO:

            PUT inv-head.bill-i[i] AT 10 SKIP.
            ASSIGN v-printline = v-printline + 1.
          END.
        END. /* 1 to 4 */
      END.

      /* T O T A L S */
      ASSIGN tmp1  = 0
             tmp2  = ?
             v-net = inv-head.t-inv-rev - inv-head.t-inv-tax.


      IF v-net2 <> inv-head.t-inv-rev THEN DO:
          v-net = v-net2. /* - inv-head.t-inv-tax.*/
      END.
         
/*       IF inv-head.f-bill THEN v-net = v-net - inv-head.t-inv-freight. */

      RELEASE terms.
      FIND FIRST terms WHERE terms.t-code EQ inv-head.terms NO-LOCK NO-ERROR.
      IF AVAIL terms 
        THEN ASSIGN tmp1 = v-net * (ROUND(terms.disc-rate / 100, 2))
                    tmp2 = /*TODAY*/ inv-head.inv-date + terms.disc-days.

      ASSIGN v-frt-tax = inv-head.t-inv-freight.

      /*IF inv-head.tax-gr <> ""       AND
         inv-head.f-bill             AND 
         inv-head.t-inv-freight <> 0 AND 
         AVAIL stax 
        THEN DO i = 1 TO 3:

         IF stax.tax-code[i] NE "" THEN DO:

           CREATE w-tax.
           ASSIGN w-dsc      = stax.tax-dscr[i]
                  w-tax      = ROUND((IF stax.company EQ "yes" 
                                        THEN v-frt-tax
                                        ELSE inv-head.t-inv-freight) * 
                                             stax.tax-rate[i] / 100,2)                 
                  v-frt-tax  = v-frt-tax + w-tax
                  v-t-tax[i] = v-t-tax[i] + w-tax
                  v-lines    = v-lines + 1.
         END.
      END.*/
    END. /* DO TRANSACTION */

    /*DO i = 1 TO 3:

      ASSIGN v-bot-lab[i] = IF v-t-tax[i] NE 0 
                             THEN ((IF AVAIL stax 
                                      THEN string(CAPS(stax.tax-code[i]),"x(5)")
                                      ELSE FILL(" ",5) ) + FILL(" ",6) + ":" +
                                           STRING(v-t-tax[i],"->>>>>9.99")) 
                             ELSE "".
    END. */

    ASSIGN v-inv-freight = IF inv-head.f-bill THEN inv-head.t-inv-freight 
                           ELSE 0
           v-net2 = v-net2 + inv-head.t-inv-tax.

    PUT "<R57><C1><#7><FROM><C+80><LINE>"
        "<=7><C21><FROM><R+2.4><LINE>"
        "<=7><C31><FROM><R+2.4><LINE>"
        "<=7><C40><FROM><R+2.4><LINE>"
        "<=7><C50><FROM><R+2.4><LINE>"
        "<=7><C60><FROM><R+2.4><LINE>"
        "<=7><C70><FROM><R+2.4><LINE>"
        "<=7><C81><FROM><R+2.4><LINE>"
        "<=7><C21>" v-inv-freight FORMAT ">>,>>9.99" "<C30.5>" inv-head.t-inv-tax FORMAT "->>,>>9.99" v-net FORMAT "->,>>>,>>9.99" " " tmp1 "    " tmp2 " " ( /*IF v-net2 <> inv-head.t-inv-rev THEN v-net2 ELSE*/ inv-head.t-inv-rev) FORMAT "->>>>,>>9.99"
        "<=7><R+1.2><C21><FROM><C+60><LINE>"        
        "<=7><R+1.2><C24><P6>FREIGHT<C34>SALES TAX<C40> NET AMOUNT SUBJECT    CASH DISCOUNT        IF PAID BY         INVOICE AMOUNT"
        "<=7><R+1.7><C40><P6> TO CASH DISCOUNT          AMOUNT            THIS DATE  "
        "<=7><R+2.4><C21><FROM><C+60><LINE>".

    PUT "<FArial><R61><C63><P12><B> THANK YOU. </B> <P9> " SKIP.

    ASSIGN v-printline = v-printline + 6
           v-net2 = 0.

    IF v-printline <= 66 THEN PAGE. 

END. /* each xinv-head */


/*PROCEDURE compute-ext-price:
    DEFINE INPUT PARAM in-recid AS RECID.
    DEFINE INPUT PARAM in-qty AS INTE NO-UNDO.
    DEFINE OUTPUT PARAM out-price AS DECI NO-UNDO.

    DEF BUFFER bf-inv-line FOR inv-line.

    FIND bf-inv-line WHERE RECID(bf-inv-line) = in-recid NO-LOCK NO-ERROR.
    IF AVAIL bf-inv-line THEN DO:

      ASSIGN out-price = (bf-inv-line.ship-qty - in-qty) / 1000 * bf-inv-line.price.

      IF bf-inv-line.pr-uom BEGINS "L" AND bf-inv-line.pr-uom NE "LB" THEN
         out-price = bf-inv-line.price *
                     IF (bf-inv-line.ship-qty - in-qty) LT 0 THEN -1 ELSE 1.
      ELSE IF bf-inv-line.pr-uom EQ "CS" THEN
         out-price = (bf-inv-line.ship-qty - in-qty) /
                     (IF bf-inv-line.cas-cnt NE 0 THEN
                         bf-inv-line.cas-cnt
                      ELSE
                      IF itemfg.case-count NE 0 THEN
                         itemfg.case-count ELSE 1) *
                      bf-inv-line.price.
      ELSE IF LOOKUP(bf-inv-line.pr-uom,fg-uom-list) GT 0 THEN
         out-price = (bf-inv-line.ship-qty - in-qty) * bf-inv-line.price.
      ELSE
      FOR EACH uom
         WHERE uom.uom  EQ bf-inv-line.pr-uom
           AND uom.mult NE 0
         NO-LOCK:
         out-price = (bf-inv-line.ship-qty - in-qty) / uom.mult * bf-inv-line.price.
         LEAVE.
      END.
      out-price = ROUND(out-price,2).

      IF bf-inv-line.disc NE 0 THEN
        out-price = ROUND(out-price * (1 - (bf-inv-line.disc / 100)),2).
    END.
    ELSE
        out-price = inv-line.t-price.

END PROCEDURE. */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
