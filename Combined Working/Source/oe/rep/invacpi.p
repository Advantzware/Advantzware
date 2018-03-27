/* ------------------------------------------ oe/rep/invacpi.p 06050916 GDM  */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = ACPI                          */
/* ------------------------------------------------------------------------- */

DEF INPUT PARAM ip-copy-title AS cha NO-UNDO.

{sys/inc/var.i shared}

{oe/rep/invoice.i}

DEF SHARED VAR v-fr-tax AS LOGICAL INITIAL NO NO-UNDO.

DEF VAR v-salesman     AS CHAR FORMAT "x(14)"          NO-UNDO.
DEF VAR v-salesname    AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-fob          AS CHAR FORMAT "x(27)"          NO-UNDO.
DEF VAR v-addr3        AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-sold-addr3   AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-shipto-name  AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-shipto-addr  AS CHAR FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEF VAR v-shipto-city  AS CHAR FORMAT "x(15)"          NO-UNDO.
DEF VAR v-shipto-state AS CHAR FORMAT "x(2)"           NO-UNDO.
DEF VAR v-shipto-zip   AS CHAR FORMAT "x(10)"          NO-UNDO.
DEF VAR v-invhead      AS CHAR FORMAT "x(13)"          NO-UNDO 
     INITIAL "I N V O I C E".
DEF VAR v-case-cnt     AS CHAR FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR v-case-line    AS CHAR                         NO-UNDO.
DEF VAR v-part-line    AS CHAR                         NO-UNDO.
DEF VAR disp-frt       AS CHAR FORMAT "x(8)"           NO-UNDO
    INITIAL "Freight:".
/* partial or complete */
DEF VAR v-pc           AS CHAR                         NO-UNDO.
DEF VAR v-ord-del-hdr  AS CHAR FORMAT "x(3)"  NO-UNDO
    INITIAL "Del".
DEF VAR v-part-info    AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-i-no         AS CHAR FORMAT "x(15)"          NO-UNDO.
DEF VAR v-i-dscr       AS CHAR FORMAT "x(18)"          NO-UNDO.
DEF VAR v-bill-i       AS CHAR FORMAT "x(25)"          NO-UNDO.
DEF VAR v-ship-i       AS CHAR FORMAT "x(25)"          NO-UNDO.
DEF VAR v-price-head   AS CHAR FORMAT "x(5)"           NO-UNDO.
DEF VAR v-bot-lab      AS CHAR FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEF VAR ls-image1      AS CHAR                         NO-UNDO.
DEF VAR ls-image2      AS CHAR                         NO-UNDO.
DEF VAR ls-full-img1   AS CHAR FORMAT "x(200)"          NO-UNDO.
DEF VAR ls-full-img2   AS CHAR FORMAT "x(200)"          NO-UNDO.
DEF VAR v-tel          AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-fax          AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-contact      AS CHAR FORMAT "x(20)"          NO-UNDO.
DEF VAR v-comp-add1    AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add2    AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add3    AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add4    AS CHAR FORMAT "x(30)"          NO-UNDO.
DEF VAR v-comp-add5    AS CHAR FORMAT "x(30)"          NO-UNDO.


DEF VAR v-shipvia      LIKE carrier.dscr               NO-UNDO.
DEF VAR v-pitch        LIKE asi.printer.pitch          NO-UNDO.
DEF VAR v-hldpitch     LIKE asi.printer.pitch          NO-UNDO.
DEF VAR v-t-weight     LIKE inv-line.t-weight          NO-UNDO.
DEF VAR v-tax-code     LIKE stax.tax-code              NO-UNDO.
DEF VAR v-tx-rate      LIKE stax.tax-rate              NO-UNDO.
DEF VAR v-bol-cases    LIKE oe-boll.cases              NO-UNDO.
DEF VAR v-net          LIKE inv-head.t-inv-rev         NO-UNDO.
DEF VAR v-po-no        LIKE inv-line.po-no             NO-UNDO.
DEF VAR v-ord-no       LIKE oe-ord.ord-no              NO-UNDO.
DEF VAR v-ord-date     LIKE oe-ord.ord-date            NO-UNDO.
DEF VAR v-rel-po-no    LIKE oe-rel.po-no               NO-UNDO.
DEF VAR v-inv-freight  LIKE inv-head.t-inv-freight     NO-UNDO.


DEF VAR v-line         AS INT                 NO-UNDO.
DEF VAR v-printline    AS INT                 NO-UNDO.
DEF VAR v-len          AS INT                 NO-UNDO.
DEF VAR v-tot-pallets  AS INT                 NO-UNDO.
DEF VAR v-tot-qty      AS INT                 NO-UNDO.
DEF VAR v-del-no       AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-set-qty      AS INT                 NO-UNDO.
DEF VAR cnt            AS INT                 NO-UNDO.
DEF VAR minus-ship     AS INT                 NO-UNDO.
DEF VAR v-beeler-lines AS INT                 NO-UNDO.
DEF VAR v              AS INT                 NO-UNDO.
DEF VAR v-bo-qty       AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-inv-qty      AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-ship-qty     AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-lines        AS INT                 NO-UNDO.


DEF VAR v-tot-cas      AS DEC FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tax-rate     AS DEC FORMAT "->>>.99"    NO-UNDO.
DEF VAR v-part-qty     AS DEC FORMAT "999.9999"   NO-UNDO.
DEF VAR tmp1           AS DEC                     NO-UNDO.
DEF VAR net1           AS DEC                     NO-UNDO.
DEF VAR net2           AS DEC                     NO-UNDO.
DEF VAR net3           AS DEC                     NO-UNDO.
DEF VAR v-price        AS DEC FORMAT ">>>>9.9999" NO-UNDO.
DEF VAR v-t-price      AS DEC FORMAT ">>>>>>9.99" NO-UNDO.
DEF VAR v-subtot-lines AS DEC                     NO-UNDO.
DEF VAR v-t-tax        AS DEC EXTENT 3            NO-UNDO.
DEF VAR v-frt-tax      AS DEC                     NO-UNDO.


DEF VAR v-inv-date     AS DATE INITIAL TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR v-date-ship    AS DATE INITIAL TODAY                     NO-UNDO.
DEF VAR tmp2           AS DATE                                   NO-UNDO.


DEF VAR v-ans          AS LOG  INITIAL NO                        NO-UNDO.


DEF BUFFER xinv-head FOR inv-head.
DEF BUFFER xinv-line FOR inv-line.

DEF TEMP-TABLE w-sman
    FIELD sman AS CHAR FORMAT "x(4)".

DEF TEMP-TABLE w-tax
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.


FIND FIRST inv-head NO-LOCK NO-ERROR.

ASSIGN ls-image1 = "images\AmPoly.jpg"
       ls-image2 = "images\AmPoly.jpg".

ASSIGN
  FILE-INFO:FILE-NAME = ls-image1.
  ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".
  FILE-INFO:FILE-NAME = ls-image2.
  ls-full-img2 = FILE-INFO:FULL-PATHNAME + ">".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

FIND FIRST cust 
  WHERE cust.company = cocode 
    AND cust.active = "X" NO-LOCK NO-ERROR.

ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = "".

IF AVAIL cust 
  THEN
   ASSIGN v-comp-add1 = cust.addr[1]
          v-comp-add2 = cust.addr[2]
          v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
          v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + 
                                     STRING(cust.phone,"999-9999") 
          v-comp-add5 = "Fax  :  " + STRING(cust.fax,"(999)999-9999").

IF TRIM(v-comp-add1) EQ "" 
  THEN ASSIGN v-comp-add1 = v-comp-add2
              v-comp-add2 = v-comp-add3
              v-comp-add3 = "".

IF TRIM(v-comp-add2) EQ "" 
  THEN ASSIGN v-comp-add2 = v-comp-add3
              v-comp-add3 = "".

FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
  FIRST xinv-head 
   WHERE RECID(xinv-head) EQ report.rec-id NO-LOCK
    BREAK BY report.key-01
          BY report.key-02:

    FIND FIRST cust 
      WHERE cust.company = xinv-head.company
        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

    ASSIGN  v-shipto-name    = xinv-head.sold-name
            v-shipto-addr[1] = xinv-head.sold-addr[1]
            v-shipto-addr[2] = xinv-head.sold-addr[2]
            v-shipto-city    = xinv-head.sold-city
            v-shipto-state   = xinv-head.sold-state
            v-shipto-zip     = xinv-head.sold-zip
            v-del-no         = 0.

    FIND FIRST oe-bolh 
      WHERE oe-bolh.company EQ xinv-head.company 
        AND oe-bolh.bol-no  EQ xinv-head.bol-no 
        USE-INDEX bol-no NO-LOCK NO-ERROR.
    IF AVAIL oe-bolh THEN DO:

      FIND FIRST shipto 
        WHERE shipto.company EQ oe-bolh.company 
          AND shipto.cust-no EQ oe-bolh.cust-no 
          AND shipto.ship-id EQ oe-bolh.ship-id NO-LOCK NO-ERROR.
      IF AVAIL shipto 
        THEN ASSIGN  v-shipto-name = shipto.ship-name
                     v-shipto-addr[1] = shipto.ship-addr[1]
                     v-shipto-addr[2] = shipto.ship-addr[2]
                     v-shipto-city = shipto.ship-city
                     v-shipto-state = shipto.ship-state
                     v-shipto-zip = shipto.ship-zip.
    END. /* avail oe-bolh */


    IF NOT v-reprint OR 
       xinv-head.inv-no EQ 0 
      THEN RUN oe/get-inv#.p (ROWID(xinv-head)).

    DO TRANSACTION:


       FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).
       
       IF inv-head.inv-date NE ? 
         THEN ASSIGN v-inv-date = inv-head.inv-date.

       IF inv-head.fob-code BEGINS "ORIG" 
         THEN ASSIGN v-fob = "Origin".
         ELSE ASSIGN v-fob = "Destination".

       FIND FIRST carrier 
         WHERE carrier.company EQ inv-head.company 
           AND carrier.carrier EQ inv-head.carrier NO-LOCK NO-ERROR.
       IF AVAIL carrier 
         THEN ASSIGN v-shipvia = carrier.dscr.
         ELSE ASSIGN v-shipvia = "".

       ASSIGN v-addr3      = inv-head.city + ", " + inv-head.state + "  " + 
                             inv-head.zip
              v-sold-addr3 = v-shipto-city + ", " + v-shipto-state + "  " + 
                             v-shipto-zip
              v-line       = 1
              v-printline   = 0.

       FIND FIRST stax 
         {sys/ref/stax1W.i}
         AND {sys/ref/taxgroup.i stax} EQ inv-head.tax-gr NO-LOCK NO-ERROR.
       IF NOT AVAIL stax 
         THEN
          FIND FIRST stax 
            WHERE stax.tax-group eq inv-head.tax-gr NO-LOCK NO-ERROR.

       IF AVAIL stax 
         THEN ASSIGN v-tax-rate    = stax.tax-rate[1] +
                                     stax.tax-rate[2] + stax.tax-rate[3]
                     v-tax-code[1] = stax.tax-code[1]
                     v-tax-code[2] = stax.tax-code[2]
                     v-tax-code[3] = stax.tax-code[3]
                     v-tx-rate[1]  = stax.tax-rate[1]
                     v-tx-rate[2]  = stax.tax-rate[2]
                     v-tx-rate[3]  = stax.tax-rate[3].

       ASSIGN v-tot-pallets = 0.

       FOR EACH xinv-line NO-LOCK 
         WHERE xinv-line.r-no = inv-head.r-no 
           BREAK BY xinv-line.i-no:

         DO i = 1 TO 3:

           IF xinv-line.sman[i] NE "" THEN DO:

             CREATE w-sman.
             ASSIGN w-sman.sman = xinv-line.sman[i].
           END.
         END. /* DO i */

         ASSIGN v-tot-qty  = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (ROUND(xinv-line.t-weight /
                                                 xinv-line.qty, 2) 
                             * xinv-line.inv-qty).
                v-tot-pallets = 0.

         ASSIGN v-pc = "C". /* complete*/

         FOR EACH oe-bolh NO-LOCK 
           WHERE oe-bolh.b-no = xinv-line.b-no 
            /*oe-bolh.ord-no = xinv-line.ord-no*/ :

           ASSIGN v-pc = "P". /* partial*/ 


           FOR EACH oe-boll NO-LOCK 
             WHERE oe-boll.company = oe-bolh.company 
               AND oe-boll.b-no = oe-bolh.b-no 
               AND oe-boll.i-no = xinv-line.i-no 
               AND oe-boll.ord-no = xinv-line.ord-no:

               /** Bill Of Lading TOTAL CASES **/
               ASSIGN v-bol-cases   = v-bol-cases + oe-boll.cases
                      v-tot-pallets = v-tot-pallets + oe-boll.cases +
                                     (IF oe-boll.partial GT 0 
                                        THEN 1 
                                        ELSE 0).

              IF oe-boll.p-c THEN v-pc = "C". /*complete*/

           END.  /* each oe-boll */

           ASSIGN v-date-ship = oe-bolh.bol-date
                 /* v-tot-pallets = v-tot-pallets + v-bol-cases +
                                  (if oe-boll.partial gt 0 THEN 1 else 0) */.
         END. /* each oe-bolh */

         IF LAST-OF(xinv-line.i-no) THEN DO:

           IF xinv-line.est-no NE "" THEN DO:

             FIND FIRST eb 
               WHERE eb.company = xinv-line.company 
                 AND eb.est-no = xinv-line.est-no 
                 AND eb.e-num = xinv-line.e-num 
                 AND eb.form-no = xinv-line.form-no 
                 AND eb.blank-no = xinv-line.blank-no NO-LOCK NO-ERROR.
             IF xinv-line.form-no = 0 AND 
                xinv-line.est-type = 2 
               THEN DO:

                FOR EACH fg-set NO-LOCK 
                  WHERE fg-set.company = xinv-line.company
                    AND fg-set.set-no = xinv-line.i-no:

                   ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
                END.

               IF v-set-qty = 0 
                 THEN ASSIGN v-set-qty = 1.

               FOR EACH eb NO-LOCK 
                 WHERE eb.company = xinv-line.company 
                   AND eb.est-no = xinv-line.est-no 
                   AND eb.e-num = xinv-line.e-num 
                   AND eb.form-no NE 0:


                 FIND fg-set 
                   WHERE fg-set.company = xinv-line.company 
                     AND fg-set.set-no = xinv-line.i-no  
                     AND fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.
                 IF AVAIL fg-set AND 
                    fg-set.part-qty NE 0 
                   THEN ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
                   ELSE ASSIGN v-part-qty = 1 / v-set-qty.

                 IF eb.cas-cnt = 0 
                   THEN ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                           eb.cas-wt, 2).
                   ELSE ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                           eb.cas-cnt, 2).

                 IF v-bol-cases ne 0 
                   THEN ASSIGN v-tot-cas = v-bol-cases.

                 /***
                 ASSIGN v-tot-pallets = v-tot-pallets +
                      ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
                 ***/
               END.  /* each eb */

             END. /* do */
             ELSE
             IF AVAIL eb THEN DO:

               IF eb.cas-cnt = 0 
                 THEN ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                 ELSE ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).

               IF v-bol-cases NE 0 
                 THEN ASSIGN v-tot-cas = v-bol-cases.

               /***
               ASSIGN v-tot-pallets = v-tot-pallets +
                   ROUND((v-tot-cas  / eb.cas-pal) + .49, 0).
               ***/
             END. /* ELSE do */
           END. /* est-no NE "" */

           ASSIGN v-t-weight = 0
                  v-tot-cas = 0 
                  v-tot-qty = 0.

         END. /* last-of i-no */
       END. /* each xinv-line */

       
       /** Build Salesman Id String **/
       ASSIGN  v-salesman = "".

       FOR EACH w-sman BREAK BY w-sman.sman:

         IF FIRST-OF(w-sman.sman) 
           THEN ASSIGN v-salesman = v-salesman + w-sman.sman.

         DELETE w-sman.
       END.

       FIND FIRST oe-bolh 
         WHERE oe-bolh.company EQ inv-head.company 
           AND oe-bolh.bol-no  EQ inv-head.bol-no
          USE-INDEX bol-no NO-LOCK NO-ERROR.
       IF AVAIL oe-bolh 
         THEN ASSIGN v-rel-po-no = oe-bolh.po-no.

       FIND FIRST inv-line 
         WHERE inv-line.r-no EQ inv-head.r-no NO-LOCK NO-ERROR.
       IF AVAIL inv-line THEN DO:

         ASSIGN v-price-head = inv-line.pr-uom.

         FIND FIRST oe-ord 
           WHERE oe-ord.company EQ cocode 
             AND oe-ord.ord-no  EQ inv-line.ord-no NO-LOCK NO-ERROR.
         IF AVAIL oe-ord THEN DO:

           ASSIGN v-po-no  = oe-ord.po-no
                  v-bill-i = oe-ord.bill-i[1]
                  v-ord-no = oe-ord.ord-no
                  v-ord-date = oe-ord.ord-date.
         END.
         ELSE ASSIGN v-price-head = inv-line.pr-uom.
       END. 

       /* 
       display heder info view frame invhead-comp.  /* Print headers */
       */

       {oe/rep/invacpi.i}  /* HEADER */

       ASSIGN v-subtot-lines = 0
              v-t-tax        = 0.


       FOR EACH inv-line NO-LOCK 
         WHERE inv-line.r-no = inv-head.r-no:

          ASSIGN v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = "".

          ASSIGN v-pc = "P". /* partial*/ 

          FOR EACH oe-boll NO-LOCK 
            WHERE oe-boll.company EQ inv-line.company
              AND oe-boll.bol-no  EQ inv-head.bol-no
              /*and oe-boll.b-no  eq inv-line.b-no*/
              AND oe-boll.i-no    EQ inv-line.i-no 
             USE-INDEX bol-no:


             /** Build Case Count Display Lines **/
             IF oe-boll.cases NE 0 AND 
                oe-boll.qty-case NE 0 
               THEN ASSIGN v-case-line = STRING(oe-boll.cases) + " @ " +
                                         STRING(oe-boll.qty-case).
               ELSE ASSIGN v-case-line = "".

             IF oe-boll.partial NE 0 
               THEN ASSIGN v-part-line = "1" + " @ " + STRING(oe-boll.partial).
               ELSE ASSIGN v-part-line = "".

             IF oe-boll.p-c THEN v-pc = "C". /*complete*/

             DO i = 1 TO 5:

               IF (80 - LENGTH(v-case-cnt[i])) > LENGTH(v-case-line) AND
                   v-case-line NE "" 
                 THEN ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                                             v-case-line = "".

               IF (80 - LENGTH(v-case-cnt[i])) > LENGTH(v-part-line) AND
                  v-part-line NE "" 
                 THEN ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                                             v-part-line = "".
             END.  /* 1 to 5 */

          END. /* each oe-boll */

          
          IF v-printline > 62 THEN do:           
                PAGE.
                {oe/rep/invacpi.i}
                v-printline = 21.
          END.

          ASSIGN v-line = v-line + 1
                 v-printline = v-printline + 2.

          FIND FIRST oe-ordl 
            WHERE oe-ordl.company EQ cocode 
              AND oe-ordl.ord-no  EQ inv-line.ord-no 
              AND oe-ordl.i-no    EQ inv-line.i-no NO-LOCK NO-ERROR.
          IF AVAIL oe-ordl 
            THEN ASSIGN v-bo-qty = IF (inv-line.qty - inv-line.ship-qty -
                                       oe-ordl.t-ship-qty) < 0 
                                     THEN 0 
                                     ELSE (inv-line.qty - inv-line.ship-qty -
                                             oe-ordl.t-ship-qty).
            ELSE ASSIGN v-bo-qty = IF (inv-line.qty - inv-line.ship-qty ) < 0
                                     THEN 0 
                                     ELSE inv-line.qty - inv-line.ship-qty.

          ASSIGN v-inv-qty = inv-line.inv-qty  /* task 04091404*/
                 v-ship-qty = inv-line.ship-qty
                 v-i-no = inv-line.i-no
                 v-i-dscr = inv-line.i-name
                 v-price = inv-line.price * (1 - (inv-line.disc / 100))
                 v-t-price = inv-line.t-price
                 v-subtot-lines = v-subtot-lines + inv-line.t-price.

          IF inv-line.tax AND 
             AVAIL stax 
            THEN
             DO i = 1 TO 3:

               IF stax.tax-code[i] NE "" THEN DO:

                 CREATE w-tax.
                 ASSIGN w-dsc      = stax.tax-dscr[i]
                        w-tax      = ROUND((IF stax.company EQ "yes" 
                                             THEN v-t-price
                                             ELSE inv-line.t-price) *
                                      stax.tax-rate[i] / 100,2)
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
               END.
          END.

          IF v-t-price ne inv-line.t-price THEN DO:

            CREATE w-tax.
            ASSIGN w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
          END.


          ASSIGN v-po-no  = inv-line.po-no
                 v-ord-no = inv-line.ord-no
                 v-price-head = inv-line.pr-uom.

          PUT SPACE(1)
              v-po-no 
              /* v-i-dscr  FORMAT "x(25)" SPACE(1)
                 v-inv-qty FORMAT "->>>>>9" SPACE(1) */ 
              inv-line.part-no  
             SPACE(1)
              v-i-dscr    FORMAT "x(30)" 
              v-inv-qty  FORMAT "->>>>>9" 
            SPACE(3)
              /* v-bo-qty  FORMAT "->>>>>9" SPACE(1)
                 v-i-no  FORMAT "x(15)" SPACE(1) */ 
              v-price  FORMAT ">>>,>>9.9999"                
              inv-line.t-price  FORMAT "->>>,>>9.99"                
            SKIP
              v-ord-no 
            SPACE(10)
              inv-line.i-no 
            SPACE(1)
              inv-line.part-dscr1 FORMAT "x(30)" 
              v-ship-qty FORMAT "->>>>>9"
            SPACE(4)
              v-pc  FORMAT "x" 
            SPACE(7)
              v-price-head 
            SKIP
              /*   space(16) inv-line.part-dscr2  */
              inv-line.part-dscr2 AT 33 
            SKIP .

          ASSIGN v-printline = v-printline + 3.

          PUT SKIP(1).

          ASSIGN v-printline = v-printline + 1.

       END.  /* each inv-line */

       FOR EACH inv-misc NO-LOCK 
         WHERE inv-misc.company = inv-head.company 
           AND inv-misc.r-no = inv-head.r-no 
           AND inv-misc.bill = "Y" 
          BREAK BY ord-no WITH FRAME detailm:

          IF v-printline > 62 THEN do:
              
                PAGE.                
                {oe/rep/invacpi.i}
                v-printline = 21.
          END.

          IF FIRST(inv-misc.ord-no) THEN DO:

             PUT "** Miscellaneous Items **" AT 23 
                 SKIP(1).

             ASSIGN v-printline = v-printline + 2.
          END.
          /*
            if v-printline gt 29 THEN
            do:
              put skip(30 - v-printline) "* CONTINUED *" at 72.
              ASSIGN v-printline = 0.
              page.

            end.
          */

          PUT 
              inv-misc.charge FORMAT "x(15)"   AT 17 
              inv-misc.dscr   FORMAT "x(30)"   AT 33
              inv-misc.amt       AT 85   
             SKIP.
          ASSIGN v-subtot-lines = v-subtot-lines + inv-misc.amt
                 v-printline = v-printline + 1.

          IF inv-misc.tax AND 
             AVAIL stax 
            THEN 
              DO i = 1 to 3:

              if stax.tax-code[i] NE "" THEN do:
                create w-tax.
                ASSIGN
                 w-dsc      = stax.tax-dscr[i]
                 w-tax      = if stax.company eq "yes" THEN v-t-price
                              else inv-misc.amt
                 w-tax      = round(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - w-tax
                 v-t-price  = v-t-price + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
              end.
            end.

            IF v-t-price ne inv-misc.amt THEN DO:

              CREATE w-tax.
              ASSIGN w-dsc     = "******ITEM TOTAL:"
                     w-tax     = v-t-price
                     v-lines   = v-lines + 1.
            END.
       END.  /* each inv-misc */

       IF v-prntinst THEN DO:

         DO i = 1 TO 4:

           IF inv-head.bill-i[i] NE "" THEN DO:

             PUT inv-head.bill-i[i] AT 10 SKIP.

             ASSIGN v-printline = v-printline + 1.
           END.
         END. /* 1 to 4 */
       END.



       ASSIGN v-frt-tax = inv-head.t-inv-freight.

       IF inv-head.tax-gr <> "" AND 
          inv-head.f-bill       AND 
          inv-head.t-inv-freight <> 0 AND 
          AVAIL stax 
         THEN
          DO i = 1 TO 3:

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
       END.
    END. /* DO TRANSACTION */

    DO i = 1 TO 3:

      ASSIGN v-bot-lab[i] = IF v-t-tax[i] ne 0 
                              THEN ((IF AVAIL stax 
                                       THEN STRING(CAPS(
                                                   stax.tax-code[i]),"x(5)") 
                                       ELSE FILL(" ",5) ) +
                                    FILL(" ",6) + ":" +
                                    STRING(v-t-tax[i],"->>>>>9.99")) 
                              ELSE "".
    END.


    ASSIGN v-inv-freight = IF inv-head.f-bill 
                             THEN inv-head.t-inv-freight 
                             ELSE 0. /*inv-head.t-inv-freight*/.

    PUT "<R58><C58><#8><FROM><R+5><C+22><RECT> </B>" 
        "<=8> Sub Total    :" v-subtot-lines FORMAT "->>,>>9.99"
        "<=8><R+1> Freight      :" v-inv-freight
        /*"<=8><R+2> ""  " v-bot-lab[1]  */
        "<=8><R+2> Sales Tax    :" inv-head.t-inv-tax FORMAT "->>,>>9.99"
                  /*  v-bot-lab[2] */
        "<=8><R+3>" "" 
        "<=8><R+4><B> Total Invoice:" inv-head.t-inv-rev FORMAT "->>,>>9.99"
        "</B>".

    
    PUT 
        "<FArial><#9><R58><C1><P12><B> THANK YOU. YOUR BUSINESS IS GREATLY APPRECIATED !</B> <P9> " SKIP        
        "<|10><R59.5><C1><FROM><R63><C57.5><RECT>" SKIP 
        "<=9><R60><C1.5>Please pay by invoice - No statements will be sent." SKIP
        "<=9><R61><C1.5>A finance charge of 1.5% per month (18% APR) may be charged after 30 days from date of invoice." SKIP
        "<=9><R62><C1.5>Collection and Attorney fees incurred to collect past due balances will be paid by the customer." SKIP
        .

    v-printline = v-printline + 5.

    IF v-printline <= 66 THEN page. /*PUT SKIP(74 - v-printline). */
    
 
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
