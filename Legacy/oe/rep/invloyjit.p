/* ------------------------------------------ oe/rep/invloyjit.p */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = LoylangJIT                    */
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
DEF VAR v-line          AS INT NO-UNDO.
DEF VAR v-printline     AS INT NO-UNDO.
DEF VAR v-inv-date      AS DATE INIT TODAY FORMAT "99/99/9999" NO-UNDO.
def shared var v-fr-tax AS LOG INIT no NO-UNDO.
DEF VAR v-tax-rate      AS DEC FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tax-code      LIKE stax.tax-code NO-UNDO.
DEF VAR v-tx-rate       LIKE stax.tax-rate NO-UNDO.
DEF VAR v-date-ship     AS DATE INIT today NO-UNDO.
DEF VAR v-del-no        AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-pc            AS CHAR NO-UNDO. /* partial or complete */
DEF VAR v-t-price-2 LIKE inv-line.t-price NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.

DEF buffer xinv-head FOR inv-head.
DEF buffer xinv-line FOR inv-line.
DEF BUFFER b-inv-head FOR inv-head.
DEF BUFFER b-oe-bolh FOR oe-bolh.

DEF TEMP-TABLE w-sman
  FIELD sman AS CHAR FORMAT "x(4)".

DEF TEMP-TABLE tt-bol
    FIELD bol-no AS INT
    FIELD ship-qty AS INT
    FIELD ship-date AS DATE.

DEF VAR v-ord-del-hdr  AS CHAR FORMAT "x(3)" init "Del".
DEF VAR v-beeler-lines AS INT.
DEF VAR v-part-info    AS CHAR FORMAT "x(30)".
DEF VAR v              AS INT.
DEF VAR v-inv-qty      AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-ship-qty     AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-i-no         AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-i-dscr       AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR v-price        AS DEC FORMAT ">>>>9.9999" NO-UNDO.
DEF VAR v-t-price      AS DEC FORMAT ">>>>>>9.99" NO-UNDO.
DEF VAR v-po-no        LIKE inv-line.po-no NO-UNDO.
DEF VAR v-bill-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEF VAR v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEF VAR v-ship-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-rel-po-no    LIKE oe-rel.po-no NO-UNDO.
DEF VAR v-price-head   AS CHAR FORMAT "x(5)" NO-UNDO.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
DEF VAR v-ship-qty-2   AS INT NO-UNDO.

DEF TEMP-TABLE w-tax
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.

DEF VAR v-t-tax       AS DEC extent 3 NO-UNDO.
/*DEF VAR v-bot-lab     AS CHAR FORMAT "x(63)" extent 3 NO-UNDO.*/
DEF VAR v-lines       AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax     AS DEC NO-UNDO.

DEF VAR v-bol-no AS INT FORMAT ">>>>>>>9" NO-UNDO.
DEF VAR v-inv-total AS DECI NO-UNDO.
DEF VAR v-str AS CHAR FORMAT "X(100)" NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.

/* === with xprint ====*/
DEF VAR ls-image1    AS CHAR NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(150)" NO-UNDO.

ASSIGN
 ls-image1 = "images\LoyINV.jpg"
 FILE-INFO:FILE-NAME = ls-image1
 ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORMAT "x(30)" NO-UNDO.

DEF VAR v-custno      LIKE cust.cust-no NO-UNDO.

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
      v-shipto-zip = xinv-head.sold-zip
      v-del-no = 0.

    FIND FIRST oe-bolh WHERE oe-bolh.company = xinv-head.company 
                         AND oe-bolh.bol-no = xinv-head.bol-no 
                       USE-INDEX bol-no 
                       NO-LOCK NO-ERROR.
    IF AVAIL oe-bolh THEN DO:

       FIND FIRST shipto WHERE shipto.company EQ cocode
                           AND shipto.cust-no EQ oe-bolh.cust-no
                           AND shipto.ship-id EQ oe-bolh.ship-id
                           USE-INDEX ship-id NO-LOCK NO-ERROR.
       IF AVAIL shipto THEN
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
      
       IF inv-head.fob-code BEGINS "ORIG" THEN
          ASSIGN v-fob = "Origin".
       ELSE
          ASSIGN v-fob = "Destination".
      
       Find FIRST carrier WHERE carrier.company EQ inv-head.company 
                           AND  carrier.carrier = inv-head.carrier 
                          NO-LOCK NO-ERROR.
       IF AVAIL carrier THEN
          ASSIGN v-shipvia = carrier.dscr.
       ELSE
          ASSIGN v-shipvia = "".
      
       ASSIGN
         v-addr3      = inv-head.city + ", " + 
                        inv-head.state + "  " + inv-head.zip
         v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                        "  " + v-shipto-zip
         v-line = 1 v-printline = 0.
      
       FIND FIRST stax
         {sys/ref/stax1W.i}
         AND {sys/ref/taxgroup.i stax} EQ inv-head.tax-gr NO-LOCK NO-ERROR.
       IF NOT AVAIL stax THEN
          FIND FIRST stax 
            WHERE stax.tax-group EQ inv-head.tax-gr NO-LOCK NO-ERROR.
          IF AVAIL stax THEN
             ASSIGN 
               v-tax-rate = stax.tax-rate[1] +
                            stax.tax-rate[2] + stax.tax-rate[3]
               v-tax-code[1] = stax.tax-code[1]
               v-tax-code[2] = stax.tax-code[2]
               v-tax-code[3] = stax.tax-code[3]
               v-tx-rate[1]  = stax.tax-rate[1]
               v-tx-rate[2]  = stax.tax-rate[2]
               v-tx-rate[3]  = stax.tax-rate[3].
      
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
             v-pc = "C". /* complete*/
             v-date-ship = ?.
      
           FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ xinv-line.b-no:
      
               IF v-date-ship EQ ? OR
                  oe-bolh.bol-date GT v-date-ship THEN
                  ASSIGN v-date-ship = oe-bolh.bol-date.
           END.  /* each oe-bolh */
           
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
       IF AVAIL oe-bolh THEN
          ASSIGN v-rel-po-no = oe-bolh.po-no.
      
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
          END.
          ELSE ASSIGN v-price-head = inv-line.pr-uom.
       END. 
      
       {oe/rep/invloyjit.i}
       
       ASSIGN v-printline    = 29
              v-subtot-lines = 0
              v-t-tax        = 0.
      
       FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no
           BREAK BY inv-line.i-no
                 BY inv-line.po-no
                 BY inv-line.price:
      
         IF FIRST-OF(inv-line.price) THEN
         DO:
            EMPTY TEMP-TABLE tt-bol.
      
            ASSIGN
               v-ship-qty = 0
               v-inv-qty = 0
               v-qty = 0
               v-t-price-2 = 0
               v-pc = "P".
           
            FOR EACH b-inv-head FIELDS(bol-no stat inv-no company) WHERE
                b-inv-head.company EQ inv-head.company AND
                b-inv-head.cust-no EQ inv-head.cust-no AND
                b-inv-head.multi-invoice EQ NO
                NO-LOCK,
                FIRST b-oe-bolh fields(bol-date) WHERE
                      b-oe-bolh.company EQ b-inv-head.company AND
                      b-oe-bolh.bol-no EQ b-inv-head.bol-no AND
                      b-oe-bolh.bol-no NE 0
                      NO-LOCK:

                /*trigger is delayed so invoice numbers not populated until after this is called*/
                IF NOT((v-reprint EQ YES AND b-inv-head.inv-no EQ inv-head.inv-no AND
                   b-inv-head.stat NE "H") OR
                   (v-reprint EQ NO AND b-inv-head.inv-no EQ 0 AND
                   b-inv-head.stat NE "H")) THEN
                  NEXT.

                v-ship-qty-2 = 0.
      
                FOR EACH oe-boll FIELDS(qty) WHERE
                    oe-boll.company = inv-line.company AND
                    oe-boll.bol-no = b-inv-head.bol-no AND
                    oe-boll.i-no = inv-line.i-no 
                    NO-LOCK:
      
                    v-ship-qty-2 = v-ship-qty-2 + oe-boll.qty.
                END.
      
                FIND FIRST tt-bol WHERE
                     tt-bol.bol-no EQ b-inv-head.bol-no
                     NO-ERROR.
      
                IF NOT AVAIL tt-bol THEN
                DO:
                   CREATE tt-bol.
                   ASSIGN
                      tt-bol.bol-no   = b-inv-head.bol-no
                      tt-bol.ship-date = b-oe-bolh.bol-date
                      tt-bol.ship-qty = tt-bol.ship-qty + v-ship-qty-2.
                END.
      
                RELEASE tt-bol.
            END.
         END.
      
         FOR EACH oe-boll FIELDS(p-c) NO-LOCK 
           WHERE oe-boll.company = inv-line.company
             AND oe-boll.bol-no = inv-head.bol-no
             AND oe-boll.i-no = inv-line.i-no 
           USE-INDEX bol-no:
      
            IF oe-boll.p-c THEN v-pc = "C". /*complete*/
            
         END. /* each oe-boll */

         FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company = cocode 
              AND oe-ordl.ord-no = inv-line.ord-no 
              AND oe-ordl.i-no = inv-line.i-no NO-ERROR.
      
         ASSIGN 
           v-inv-qty = v-inv-qty + inv-line.inv-qty
           v-ship-qty = v-ship-qty + inv-line.ship-qty
           v-i-no = inv-line.i-no
           v-i-dscr = inv-line.i-name
           v-price = inv-line.price * (1 - (inv-line.disc / 100))
           v-t-price = inv-line.t-price
           v-t-price-2 = v-t-price-2 + inv-line.t-price
           v-qty = v-qty + inv-line.qty
           v-subtot-lines = v-subtot-lines + inv-line.t-price.
      
         IF inv-line.tax AND AVAIL stax THEN
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
      
         IF LAST-OF(inv-line.price) THEN
         DO:
            ASSIGN 
              v-po-no  = inv-line.po-no
              v-ord-no = inv-line.ord-no
              v-price-head = inv-line.pr-uom.
           

           
            IF v-printline GE 60 THEN DO:
               PAGE.
               {oe/rep/invloyjit.i}
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
               
               v-t-price-2  FORMAT "->>>,>>9.99"                
               SKIP
               SPACE(1)  v-ord-no 
               SPACE(10) v-i-dscr          FORMAT "x(30)"
               SPACE(2) v-qty FORMAT "->>>>>9"
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
               SKIP.
           
            v-printline = v-printline + 5.

            IF AVAIL oe-ordl AND oe-ordl.part-dscr3 NE "" THEN do:
              PUT SPACE(17)  oe-ordl.part-dscr3 FORMAT "x(30)" SKIP .       /*Task# 02191403*/     
              v-printline = v-printline + 1.
            END.

            IF v-printline GE 60 THEN DO:
           
               PAGE.
               {oe/rep/invloyjit.i}
               v-printline = 29.
            END.
           
            FOR EACH tt-bol:
           
                ASSIGN v-str = FILL(" ",17)
                             + "BOL: "
                             + TRIM(STRING(tt-bol.bol-no))
                             + " Ship Date: "
                             + TRIM(STRING(tt-bol.ship-date))
                             + FILL(" ",2)
                             + STRING(tt-bol.ship-qty,"->>>>>>9").
           
                PUT v-str SKIP.
                v-printline = v-printline + 1.
           
                IF v-printline GE 60 THEN DO:
           
                   PAGE.
                   {oe/rep/invloyjit.i}
                   v-printline = 29.
                END.
            END.
           
            IF v-printline GE 60 THEN DO:
           
               PAGE.
               {oe/rep/invloyjit.i}
               v-printline = 29.
            END.
         END.
       END.  /* each inv-line */
      
       PUT SKIP(1). 
       ASSIGN v-printline = v-printline + 2.
      
       FOR EACH inv-misc NO-LOCK WHERE inv-misc.company = inv-head.company 
                                   AND inv-misc.r-no = inv-head.r-no 
                                   AND inv-misc.bill = "Y" 
                                 BREAK BY inv-misc.ord-no:
      
           IF FIRST(inv-misc.ord-no) THEN DO:
              PUT "** Miscellaneous Items **" AT 20 SKIP.
              ASSIGN v-printline = v-printline + 2.
           END.                 
      
           IF v-printline GE 60 THEN do:

               PAGE.                
               {oe/rep/invloyjit.i}                
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
      
           IF inv-misc.tax AND AVAIL stax THEN
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
      
           IF v-printline GE 60 THEN do:

              PAGE.                
              {oe/rep/invloyjit.i}                
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
             TRIM(inv-head.bill-i[4]) NE "" THEN DO:
             PUT SKIP(1).
             ASSIGN v-printline = v-printline + 1.
          END.
         
          DO i = 1 TO 4:
             IF inv-head.bill-i[i] NE "" THEN DO:
                IF v-printline GE 60 THEN do:

                   PAGE.                
                   {oe/rep/invloyjit.i}                
                    v-printline = 29.
                END.
         
                PUT inv-head.bill-i[i] AT 18 SKIP.
                ASSIGN v-printline = v-printline + 1.
             END.
          END.  /* 1 to 4 */
       END.
      
       ASSIGN v-frt-tax = inv-head.t-inv-freight.
      
       IF inv-head.tax-gr NE "" AND
          inv-head.f-bill AND 
          inv-head.t-inv-freight <> 0 AND 
          AVAIL stax THEN
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

    ASSIGN
      v-inv-freight = IF inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0
      v-inv-total = v-subtot-lines + inv-head.t-inv-tax + v-inv-freight
      v-printline = v-printline + 6.

    PUT 
      "<R59><C60><#8><B>" 
      "<=8><R59>  Sub Tot:" v-subtot-lines     FORMAT "->>,>>9.99"
      "<=8><R60>  Sals Tx:" inv-head.t-inv-tax FORMAT "->>,>>9.99"
      "<=8><R61>  Freight:" v-inv-freight      FORMAT "->>,>>9.99"
      "<=8><R62>  Tot Inv:" v-inv-total        FORMAT "->>,>>9.99".
   END.

   IF v-printline <= 60 THEN PAGE.
    
END. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
