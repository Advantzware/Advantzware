/* ------------------------------------------- ar/rep/invloyjit.p  */
/* PRINT INVOICE   Xprint form for LoylangJIT                      */
/* --------------------------------------------------------------- */

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
DEF VAR v-line         AS INT NO-UNDO.
DEF VAR v-printline    AS INT NO-UNDO.
DEF VAR v-inv-no        AS INT NO-UNDO.
DEF VAR v-inv-date      AS DATE initial TODAY FORMAT "99/99/9999" NO-UNDO.
def shared var v-fr-tax AS LOG INIT NO NO-UNDO.
DEF VAR v-tax-rate      AS DEC FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tax-code      LIKE stax.tax-code NO-UNDO.
DEF VAR v-tx-rate       LIKE stax.tax-rate NO-UNDO.
DEF VAR v-date-ship     AS DATE initial today NO-UNDO.
DEF VAR v-del-no        AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-case-cnt      AS CHAR FORMAT "x(80)" extent 5 NO-UNDO.
DEF VAR v-pc            AS CHAR INIT "C" NO-UNDO. /* partial or complete */
DEF VAR v-i-dscr2       AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-str AS CHAR FORMAT "X(100)" NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.

DEF BUFFER b-oe-bolh    FOR oe-bolh.
DEF BUFFER b-ar-invl    FOR ar-invl.

DEF BUFFER xar-inv for ar-inv .

DEF TEMP-TABLE w-sman
  field sman AS CHAR FORMAT "x(4)".

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
DEF VAR v-po-no        LIKE ar-invl.po-no NO-UNDO.
DEF VAR v-bill-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEF VAR v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEF VAR v-ship-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-rel-po-no    LIKE oe-rel.po-no NO-UNDO.
DEF VAR v-price-head   AS CHAR FORMAT "x(5)" NO-UNDO.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
DEF VAR v-t-price-2 LIKE inv-line.t-price NO-UNDO.

DEF TEMP-TABLE w-tax
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.

DEF VAR v-t-tax       AS DEC extent 3 NO-UNDO.
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
DEF VAR ls-full-img1 AS CHAR FORMAT "x(50)" NO-UNDO.

ASSIGN
   ls-image1 = "images\LoyINV.jpg"
   FILE-INFO:FILE-NAME = ls-image1
   ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR lv-bol-no LIKE ar-invl.bol-no NO-UNDO.
DEF VAR v-ship-qty-2   AS INT NO-UNDO.

DEF TEMP-TABLE tt-bol
    FIELD bol-no AS INT
    FIELD ship-qty AS INT
    FIELD ship-date AS DATE.


FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR each report 
  WHERE report.term-id EQ v-term-id NO-LOCK,
  FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id NO-LOCK
  BREAK BY report.key-01
        BY report.key-02:

    FIND FIRST cust WHERE cust.company = ar-inv.company
                      AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.
    ASSIGN v-custno = cust.cust-no.
    IF ar-inv.sold-name <> "" THEN
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
       IF AVAIL shipto THEN
          ASSIGN  
           v-shipto-name = shipto.ship-name
           v-shipto-addr[1] = shipto.ship-addr[1]
           v-shipto-addr[2] = shipto.ship-addr[2]
           v-shipto-city = shipto.ship-city
           v-shipto-state = shipto.ship-state
           v-shipto-zip = shipto.ship-zip.
    END.

    ASSIGN v-del-no = 0.

    IF ar-inv.inv-date NE ? THEN 
       ASSIGN 
         v-inv-date = ar-inv.inv-date
         v-date-ship = ar-inv.inv-date.
    
    IF ar-inv.fob-code BEGINS "ORIG" THEN
       ASSIGN v-fob = "Origin".
    ELSE
       ASSIGN v-fob = "Destination".

    FIND FIRST carrier WHERE carrier.company = ar-inv.company 
                         AND carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.
    IF AVAIL carrier THEN
       ASSIGN v-shipvia = carrier.dscr.
    ELSE
       ASSIGN v-shipvia = "".

    ASSIGN
      v-addr3      = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
      v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                     "  " + v-shipto-zip
      v-line = 1 v-printline = 0.

    FIND FIRST stax
      {sys/ref/stax1W.i}
      AND {sys/ref/taxgroup.i stax} eq ar-inv.tax-code no-lock no-error.
    IF NOT AVAIL stax THEN
       FIND FIRST stax WHERE stax.tax-group EQ ar-inv.tax-code NO-LOCK NO-ERROR.
       IF AVAIL stax THEN
          ASSIGN 
           v-tax-rate    = stax.tax-rate[1] +
                           stax.tax-rate[2] + stax.tax-rate[3]
           v-tax-code[1] = stax.tax-code[1]
           v-tax-code[2] = stax.tax-code[2]
           v-tax-code[3] = stax.tax-code[3]
           v-tx-rate[1]  = stax.tax-rate[1]
           v-tx-rate[2]  = stax.tax-rate[2]
           v-tx-rate[3]  = stax.tax-rate[3].

    FOR EACH ar-invl NO-LOCK 
        WHERE ar-invl.x-no EQ ar-inv.x-no  
        BREAK BY ar-invl.i-no:

        DO i = 1 TO 3:
            IF ar-invl.sman[i] ne "" THEN DO:
              CREATE w-sman.
              ASSIGN w-sman.sman = ar-invl.sman[i].
            END.
        END.
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
          reftable.code2 NE "" THEN
          DO:
             ASSIGN lv-bol-no = INT(reftable.code2).
             RELEASE reftable.
          END.
       ELSE
          ASSIGN lv-bol-no = ar-invl.bol-no.
     END.
     
     {ar/rep/invloyjit.i}  /* xprint form */
     ASSIGN 
       v-printline = 29
       v-subtot-lines = 0
       v-t-tax = 0.

     FOR EACH ar-invl NO-LOCK 
         WHERE ar-invl.x-no = ar-inv.x-no
         BREAK BY ar-invl.i-no
               BY ar-invl.po-no
               BY ar-invl.unit-pr:

         IF FIRST-OF(ar-invl.unit-pr) THEN
         DO:
            EMPTY TEMP-TABLE tt-bol.
            
            ASSIGN
               v-ship-qty = 0
               v-inv-qty = 0
               v-qty = 0
               v-t-price-2 = 0.

            FOR EACH b-ar-invl FIELDS(i-no company bol-no) WHERE
                b-ar-invl.x-no EQ ar-invl.x-no AND
                b-ar-invl.i-no EQ ar-invl.i-no 
                NO-LOCK,
                FIRST b-oe-bolh fields(bol-date) WHERE
                      b-oe-bolh.company EQ b-ar-invl.company AND
                      b-oe-bolh.bol-no EQ b-ar-invl.bol-no AND
                      b-oe-bolh.bol-no NE 0
                      NO-LOCK:
           
                v-ship-qty-2 = 0.
           
                FOR EACH oe-boll FIELDS(qty) WHERE
                    oe-boll.company = b-ar-invl.company AND
                    oe-boll.bol-no = b-ar-invl.bol-no AND
                    oe-boll.i-no = b-ar-invl.i-no 
                    NO-LOCK:
           
                    v-ship-qty-2 = v-ship-qty-2 + oe-boll.qty.
                END.
           
                FIND FIRST tt-bol WHERE
                     tt-bol.bol-no EQ b-ar-invl.bol-no
                     NO-ERROR.
           
                IF NOT AVAIL tt-bol THEN
                DO:
                   CREATE tt-bol.
                   ASSIGN
                      tt-bol.bol-no   = b-ar-invl.bol-no
                      tt-bol.ship-date = b-oe-bolh.bol-date
                      tt-bol.ship-qty = tt-bol.ship-qty + v-ship-qty-2.
                END.
           
                RELEASE tt-bol.
            END.
         END.

         FIND first oe-ordl 
           WHERE oe-ordl.company = cocode 
             AND oe-ordl.ord-no = ar-invl.ord-no 
             AND oe-ordl.i-no = ar-invl.i-no 
             AND oe-ordl.LINE = ar-invl.LINE NO-LOCK NO-ERROR.

         ASSIGN 
           v-inv-qty = v-inv-qty + ar-invl.inv-qty
           v-ship-qty = v-ship-qty + ar-invl.ship-qty
           v-i-no = ar-invl.i-no
           v-i-dscr = ar-invl.i-name
           v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
           v-t-price = ar-invl.amt
           v-t-price-2 = v-t-price-2 + ar-invl.amt
           v-qty = v-qty + ar-invl.qty
           v-subtot-lines = v-subtot-lines + ar-invl.amt.

         IF v-t-price NE ar-invl.amt THEN DO:
           CREATE w-tax.
           ASSIGN
             w-dsc     = "******ITEM TOTAL:"
             w-tax     = v-t-price
             v-lines   = v-lines + 1.
         END.

         IF LAST-OF(ar-invl.unit-pr) THEN
         DO:
            ASSIGN 
              v-po-no  = ar-invl.po-no
              v-ord-no = ar-invl.ord-no
              v-price-head = ar-invl.pr-uom.
              v-i-dscr2 = ar-invl.part-dscr1.
           
            IF v-i-dscr2 = "" THEN v-i-dscr2 = ar-invl.i-dscr.
           
            IF v-ord-no = 0 AND v-ship-qty = 0 THEN v-ship-qty = v-inv-qty.
            
            IF v-printline GE 60 THEN DO: 
               PAGE.
               {ar/rep/invloyjit.i}
               v-printline = 29.
            END.
           
            PUT 
              SPACE(1)
                v-po-no         FORMAT "x(15)" 
              SPACE(1)
                ar-invl.part-no FORMAT "x(30)" 
              SPACE(1)
                v-ship-qty  FORMAT "->>>>>>9"              
              SPACE(1)
                v-inv-qty   FORMAT "->>>>>9" 
              SPACE(1)
              SPACE(6)                               
                v-price    FORMAT "->>>,>>9.9999" 
                v-t-price-2  FORMAT "->>>,>>9.99"                
             SKIP
              SPACE(1)
                v-ord-no    FORMAT ">>>>>>" 
              SPACE(10) v-i-dscr    FORMAT "x(30)"
              SPACE(2) v-qty FORMAT "->>>>>9"
              SPACE(13) v-pc  
              SPACE(1)  v-price-head 
              SPACE(1) 
             SKIP
              SPACE(1)
                ar-invl.lot-no FORMAT "x(15)" 
               SPACE(1) v-i-dscr2  FORMAT "x(30)"
             SKIP
              SPACE(17) ar-invl.part-dscr2 FORMAT "x(30)"
             SKIP.
           
            ASSIGN v-printline = v-printline + 4.

            IF AVAIL oe-ordl AND oe-ordl.part-dscr3 NE "" THEN do:
              PUT SPACE(17)  oe-ordl.part-dscr3 FORMAT "x(30)" SKIP .       /*Task# 02191403*/     
              v-printline = v-printline + 1.
            END.
           
            FOR EACH tt-bol:
           
               ASSIGN v-str = FILL(" ",17)
                            + "BOL: "
                            + TRIM(STRING(tt-bol.bol-no))
                            + " Ship Date: "
                            + TRIM(STRING(tt-bol.ship-date))
                            + FILL(" ",3)
                            + STRING(tt-bol.ship-qty,">>>>>>9").
           
               PUT v-str SKIP.
               v-printline = v-printline + 1.
            END.
           
            IF v-printline GE 60 THEN do:           
               PAGE.
               {ar/rep/invloyjit.i}
               v-printline = 29.
            END.
         END.
     END. /* each ar-invl */

     IF v-printline GE 60 THEN do:           
       PAGE.
       {ar/rep/invloyjit.i}
        v-printline = 29.
     END.

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
        AVAIL stax THEN
        DO i = 1 TO 3:

           IF stax.tax-code[i] NE "" THEN DO:
           
              CREATE w-tax.
              ASSIGN
                w-dsc      = stax.tax-dscr[i]
                w-tax      = ROUND((IF stax.company EQ "yes" THEN v-frt-tax
                                    ELSE ar-inv.freight) *
                                   stax.tax-rate[i] / 100,2)
                v-frt-tax  = v-frt-tax + w-tax
                v-t-tax[i] = v-t-tax[i] + w-tax
                v-lines    = v-lines + 1.
           END.
        END.

     ASSIGN
       v-inv-total = v-subtot-lines + ar-inv.tax-amt + v-inv-freight.

    PUT "<R59><C63><#8><B>"
        "<=8><R59> Sub Tot:" v-subtot-lines FORMAT "->>,>>9.99"
        "<=8><R60> Sals Tx:" ar-inv.tax-amt FORMAT "->>,>>9.99"                  
        "<=8><R61> Freight:" v-inv-freight FORMAT "->>,>>9.99" 
        "<=8><R62> Tot Inv:" v-inv-total    FORMAT "->>,>>9.99" "</B>". 

    PUT "<R60><C13><P8></B> CLAIMS MUST BE MADE ON RECEIPT OF SHIPMENTS OTHERWISE NOT ALLOWED." .

    PUT "<FArial><R61><C1><#9><FROM><R63><C61><RECT> " 
        "<R61><C34.5><FROM><R63><C34.5><LINE>"         
        "<R61><C61><FROM><R63><C61><LINE>" 
        SKIP .
 
    v-printline = v-printline + 8.
  
    IF v-printline <= 60 THEN page.
     
    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.
              xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 
 
    end. /* each ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
