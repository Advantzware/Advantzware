/* ------------------------------------------ oe/rep/invcapcin.p 01281102 GDM */
/* INVOICE PRINT  Program for N-K-1-INVPRINT = CapCityIN                        */
/* ------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}
{custom/notesdef.i}

{custom/formtext.i NEW} 

DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF TEMP-TABLE tt-inv-line NO-UNDO LIKE inv-line 
    FIELD enum  LIKE oe-ordl.e-num .

def var v-salesman as char format "x(25)" NO-UNDO EXTENT 2.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-line as int NO-UNDO.
def var v-printline as INT NO-UNDO.
def var v-t-weight like inv-line.t-weight NO-UNDO.
def var v-tot-cas as dec format "->>>9.9999" NO-UNDO.
def var v-tot-pallets as int NO-UNDO.
def var v-tot-qty as INT NO-UNDO.
def var v-inv-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
def shared var v-fr-tax as logical initial no NO-UNDO.
def var v-date-ship as date initial today NO-UNDO.
def var v-bol-cases LIKE oe-boll.cases NO-UNDO.
def var v-set-qty AS DECIMAL NO-UNDO.
def var v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
def var v-net like inv-head.t-inv-rev NO-UNDO.
def var v-case-cnt as char format "x(80)" extent 5 NO-UNDO.
def var v-case-line as char NO-UNDO.
def var v-part-line as char NO-UNDO.
DEF VAR v-int AS DEC NO-UNDO.

def buffer xinv-head for inv-head .
def buffer xinv-line for inv-line .

def TEMP-TABLE w-sman NO-UNDO
  field sman as char format "x(4)".

def var v-beeler-lines as INT NO-UNDO.
def var v-part-info as char format "x(30)" NO-UNDO.
def var v as INT NO-UNDO.
def var v-bo-qty as int format "99999" no-undo.
def var v-inv-qty as int format "99999" no-undo.
def var v-ship-qty as int format "99999" no-undo.
def var v-i-no as char format "x(15)" no-undo.
def var v-i-dscr as char format "x(18)" no-undo.
def var v-price as dec format ">>>>9.9999" no-undo.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
DEF VAR v-ordqty LIKE oe-ordl.qty NO-UNDO.
DEF VAR v-ord-ln LIKE oe-ordl.e-num format ">>9" NO-UNDO.
def var v-price-head as char format "x(5)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
def TEMP-TABLE w-tax NO-UNDO
    field w-dsc as   char
    field w-tax as   dec.
def var v-t-tax      as   dec extent 3 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight FORMAT "->>,>>9.99" NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR lv-inv-list AS CHAR NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.

DEF VAR ls-image1    AS CHAR NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.

ASSIGN 
    ls-image1 = "images\capcity.jpg"
    FILE-INFO:FILE-NAME = ls-image1
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">" .

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.

DEF VAR v-blt-addr1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-blt-addr2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-blt-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR v-sh-addr1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sh-addr2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sh-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(48)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.
DEF VAR v-p-c AS CHAR NO-UNDO.
DEF VAR v-addr3-printed AS LOG INIT NO.
DEF VAR v-sold-addr3-printed AS LOG INIT NO.

DEF VAR v-icnt AS INT NO-UNDO.

DEF VAR v-text          AS CHAR                         NO-UNDO.
DEF VAR v-licnt         AS INT                          NO-UNDO.
DEF VAR v-notes         AS CHAR FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR note-count      AS INT                          NO-UNDO. 
DEF VAR v-text1         AS CHAR FORMAT "x(170)" EXTENT 10 NO-UNDO.

DEF BUFFER bf-inv-head FOR inv-head .
EMPTY TEMP-TABLE tt-inv-line.

FIND FIRST sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "INVPRINT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK. 

IF lv-display-comp THEN DO:
 FIND FIRST cust WHERE cust.company = cocode AND
                       cust.active = "X" NO-LOCK NO-ERROR.
 IF AVAIL cust THEN
   ASSIGN v-comp-add1 = cust.addr[1]
          v-comp-add2 = cust.addr[2]
          v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
          v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
          v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
          lv-email    = "Email:  " + cust.email 
          lv-comp-name = cust.NAME.
END.
    
FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
  FIRST xinv-head 
   WHERE RECID(xinv-head) EQ report.rec-id no-lock
    BREAK BY report.key-01
          BY report.key-02:

    FIND FIRST cust WHERE cust.company = xinv-head.company
                      AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

    ASSIGN  v-shipto-name = xinv-head.sold-name
            v-shipto-addr[1] = xinv-head.sold-addr[1]
            v-shipto-addr[2] = xinv-head.sold-addr[2]
            v-shipto-city = xinv-head.sold-city
            v-shipto-state = xinv-head.sold-state
            v-shipto-zip = xinv-head.sold-zip.

    FIND FIRST oe-bolh 
      WHERE oe-bolh.company = xinv-head.company 
        AND oe-bolh.bol-no = xinv-head.bol-no use-index bol-no NO-LOCK NO-ERROR.
    IF AVAIL oe-bolh THEN DO:
     
     FIND FIRST shipto 
       WHERE shipto.company  = oe-bolh.company 
         AND shipto.cust-no = oe-bolh.cust-no 
         AND shipto.ship-id = oe-bolh.ship-id NO-LOCK NO-ERROR.
     IF AVAIL shipto THEN
       ASSIGN  v-shipto-name = shipto.ship-name
               v-shipto-addr[1] = shipto.ship-addr[1]
               v-shipto-addr[2] = shipto.ship-addr[2]
               v-shipto-city = shipto.ship-city
               v-shipto-state = shipto.ship-state
               v-shipto-zip = shipto.ship-zip.

    END. /* avail oe-bolh */

    /* gdm - 09230909 */
    EMPTY TEMP-TABLE w-sman.
    ASSIGN v-salesman = "".
    /* gdm - 09230909 */

    IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
       RUN oe/get-inv#.p (ROWID(xinv-head)).

    DO TRANSACTION:

      FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).

      IF inv-head.inv-date NE ? THEN v-inv-date = inv-head.inv-date.

      IF inv-head.fob-code BEGINS "ORIG" 
        THEN ASSIGN v-fob = "Origin".
        ELSE ASSIGN v-fob = "Destination".

      FIND FIRST carrier 
        WHERE carrier.company = inv-head.company 
          AND carrier.carrier = inv-head.carrier NO-LOCK NO-ERROR.
      IF AVAIL carrier 
        THEN ASSIGN v-shipvia = carrier.dscr.
        ELSE ASSIGN v-shipvia = "".

      ASSIGN
        v-addr3      = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
        v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                        "  " + v-shipto-zip
        v-line       = 1
        v-printline  = 0
        v-addr3      = IF TRIM(v-addr3) EQ ","   
                         THEN ""
                         ELSE TRIM(v-addr3)
        v-sold-addr3 = IF TRIM(v-sold-addr3) EQ ","  
                         THEN ""
                         ELSE TRIM(v-sold-addr3)
        v-tot-pallets = 0.
        
      FOR EACH xinv-line NO-LOCK WHERE xinv-line.r-no EQ inv-head.r-no
         BREAK BY xinv-line.po-no
               BY xinv-line.i-no:
        
         DO i = 1 TO 3:
           IF xinv-line.sman[i] NE "" THEN DO:
             CREATE w-sman.
             ASSIGN w-sman.sman = xinv-line.sman[i].
           END.
         END.

         ASSIGN v-tot-qty  = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (ROUND(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).

         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no:

           FOR EACH oe-boll NO-LOCK 
             WHERE oe-boll.company = oe-bolh.company 
               AND oe-boll.b-no = oe-bolh.b-no 
               AND oe-boll.i-no = xinv-line.i-no 
               AND oe-boll.ord-no = xinv-line.ord-no:

               /** Bill Of Lading TOTAL CASES **/

             ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases.

             RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).
             
             v-tot-pallets = v-tot-pallets + v-int.

           END. /* each oe-boll */

           ASSIGN v-date-ship = oe-bolh.bol-date.

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
                xinv-line.est-type = 2 THEN DO:

               FOR EACH fg-set NO-LOCK 
                 WHERE fg-set.company = xinv-line.company
                   AND fg-set.set-no = xinv-line.i-no:

                 ASSIGN v-set-qty = v-set-qty + fg-set.QtyPerSet.
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
                    fg-set.QtyPerSet NE 0 
                   THEN ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
                   ELSE ASSIGN v-part-qty = 1 / v-set-qty.

                 IF eb.cas-cnt = 0 
                   THEN ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                           eb.cas-wt, 2).
                   ELSE ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                           eb.cas-cnt, 2).
                 IF v-bol-cases ne 0 
                   THEN ASSIGN v-tot-cas = v-bol-cases.

               END.  /* each eb */
             END.  /* IF xinv-line.form-no */
             ELSE
             IF AVAIL eb THEN DO:

               IF eb.cas-cnt = 0 
                 THEN ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                 ELSE ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).

               IF v-bol-cases NE 0 
                 THEN ASSIGN v-tot-cas = v-bol-cases.

             END. /* IF AVAIL eb */
           END. /* IF xinv-line.est-no NE "" */

           ASSIGN v-t-weight = 0
                  v-tot-cas = 0
                  v-tot-qty = 0.

         END. /* IF LAST-OF(xinv-line.i-no) */
      END.  /* FOR EACH xinv-line */


      /** Build Salesman Id String **/
      ASSIGN i = 0 v-salesman = "".

      FOR EACH w-sman BREAK BY w-sman.sman:

         ASSIGN i = i + 1.

         IF STRING(w-sman.sman) NE "" 
           THEN ASSIGN v-salesman[i] = w-sman.sman.

         DELETE w-sman.

         IF i = 2 THEN LEAVE.
      END.

      ASSIGN i = 0.

      FIND FIRST oe-bolh 
        WHERE oe-bolh.company = inv-head.company 
          AND oe-bolh.bol-no = inv-head.bol-no
         USE-INDEX bol-no NO-LOCK NO-ERROR.
      
      /* gdm - 01280914 */
      FIND FIRST inv-line
        WHERE inv-line.r-no  EQ inv-head.r-no
          AND inv-line.po-no NE "" NO-LOCK NO-ERROR.
      IF AVAIL inv-line 
        THEN ASSIGN v-po-no = inv-line.po-no.
        ELSE DO:
         FIND FIRST inv-misc NO-LOCK 
           WHERE inv-misc.r-no EQ xinv-head.r-no NO-ERROR.
         IF AVAIL inv-misc 
           THEN ASSIGN v-po-no = inv-misc.po-no.
           ELSE ASSIGN v-po-no = "".

        END.
     
     {oe/rep/invcapcin.i}

     ASSIGN
        v-subtot-lines = 0
        v-t-tax = 0.

     FOR EACH inv-line no-lock where inv-line.r-no = inv-head.r-no BY inv-line.po-no :
        CREATE tt-inv-line .
        buffer-copy inv-line to tt-inv-line.
        FIND FIRST oe-ordl 
          WHERE oe-ordl.company = cocode 
            AND oe-ordl.ord-no = inv-line.ord-no 
            AND oe-ordl.i-no = inv-line.i-no NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl AND oe-ordl.e-num NE 0 THEN
           tt-inv-line.enum = oe-ordl.e-num.
        ELSE
           tt-inv-line.enum = 9999.
     END.

     FOR EACH tt-inv-line no-lock where tt-inv-line.r-no = inv-head.r-no
         BY tt-inv-line.po-no BY tt-inv-line.enum BY tt-inv-line.i-no:

        ASSIGN v-case-line = ""
               v-part-line = ""
               v-case-cnt = "".

        IF v-printline > 65 THEN DO:
           PAGE.
           v-printline = 0.
           {oe/rep/invcapcin.i}
        END.

        ASSIGN v-p-c = "P".

        FOR EACH oe-boll
          WHERE oe-boll.company EQ tt-inv-line.company
            AND oe-boll.ord-no  EQ tt-inv-line.ord-no
            AND oe-boll.b-no    EQ tt-inv-line.b-no
            AND oe-boll.i-no    EQ tt-inv-line.i-no
            AND oe-boll.line    EQ tt-inv-line.line
            AND oe-boll.po-no   EQ tt-inv-line.po-no USE-INDEX bol-no 
            NO-LOCK:


          /** Build Case Count Display Lines **/
          IF oe-boll.cases NE 0 AND 
             oe-boll.qty-case NE 0 
            THEN ASSIGN v-case-line = STRING(oe-boll.cases) + " @ " +
                                      STRING(oe-boll.qty-case).
            ELSE ASSIGN v-case-line = "".

          IF oe-boll.partial ne 0 
            THEN ASSIGN v-part-line = "1" + " @ " + STRING(oe-boll.partial).
            ELSE ASSIGN v-part-line = "".

          IF oe-boll.p-c THEN v-p-c = "C".

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

        ASSIGN v-line = v-line + 1.

        ASSIGN lv-inv-list = ""
               v-ship-qty  = IF tt-inv-line.ord-no EQ 0 
                               THEN tt-inv-line.inv-qty
                               ELSE tt-inv-line.ship-qty.

        FIND FIRST oe-ordl 
          WHERE oe-ordl.company = cocode 
            AND oe-ordl.ord-no = tt-inv-line.ord-no 
            AND oe-ordl.i-no = tt-inv-line.i-no NO-LOCK NO-ERROR.
        IF AVAIL oe-ordl THEN DO:
         ASSIGN
          v-ordqty = oe-ordl.qty
          v-ord-ln = oe-ordl.e-num
          v-bo-qty = IF (tt-inv-line.qty - v-ship-qty -
                         oe-ordl.t-ship-qty) < 0 
                       THEN 0 
                       ELSE (tt-inv-line.qty - v-ship-qty -
                             oe-ordl.t-ship-qty).

          IF NOT CAN-FIND(FIRST oe-boll
                           WHERE oe-boll.company EQ tt-inv-line.company
                             AND oe-boll.b-no    EQ tt-inv-line.b-no
                             AND oe-boll.po-no   EQ tt-inv-line.po-no
                             AND oe-boll.ord-no  EQ oe-ordl.ord-no
                             AND oe-boll.i-no    EQ oe-ordl.i-no
                             AND oe-boll.line    EQ oe-ordl.line
                             AND oe-boll.s-code  EQ "I"
                             USE-INDEX b-no) 
            THEN
             FOR EACH ar-invl
               WHERE ar-invl.company  EQ oe-ordl.company
                 AND ar-invl.ord-no   EQ oe-ordl.ord-no
                 AND ar-invl.i-no     EQ oe-ordl.i-no
                 AND CAN-FIND(FIRST oe-boll
                               WHERE oe-boll.company EQ ar-invl.company
                                 AND oe-boll.b-no    EQ ar-invl.b-no
                                 AND oe-boll.po-no   EQ ar-invl.po-no
                                 AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                 AND oe-boll.i-no    EQ oe-ordl.i-no
                                 AND oe-boll.line    EQ oe-ordl.line
                                 AND oe-boll.s-code  EQ "I"
                               USE-INDEX b-no)
                NO-LOCK:

                lv-inv-list = lv-inv-list + 
                              TRIM(STRING(ar-invl.inv-no,">>>>>>>>>>")) + " ".
             END. /* FOR EACH ar-invl*/
        END. /* IF AVAIL oe-ordl*/
        ELSE ASSIGN v-bo-qty = IF ( tt-inv-line.qty - v-ship-qty ) < 0
                                 THEN 0 
                                 ELSE tt-inv-line.qty - v-ship-qty.

        v-beeler-lines = 0.

        DO v = 1 TO 3:

          v-part-info = IF v EQ 1 
                          THEN tt-inv-line.part-dscr1
                          ELSE
                           if v EQ 2 
                             THEN tt-inv-line.part-dscr2
                             ELSE trim(lv-inv-list).

          IF v-part-info NE "" OR 
            (v = 1 AND tt-inv-line.part-no <> "") 
            THEN v-beeler-lines = v-beeler-lines + 1.
        END. /* DO v  */

        v-printline = v-printline + v-beeler-lines.

        ASSIGN v-inv-qty = tt-inv-line.inv-qty /* task 03170618 inv-qty*/
               v-i-no = tt-inv-line.i-no
               v-i-dscr = tt-inv-line.i-name
               v-price = tt-inv-line.price * (1 - (tt-inv-line.disc / 100))
               v-t-price = tt-inv-line.t-price
               v-subtot-lines = v-subtot-lines + tt-inv-line.t-price.

        IF tt-inv-line.tax AND 
           AVAIL stax 
          THEN DO i = 1 TO 3:

            IF stax.tax-code[i] ne "" THEN DO:

              CREATE w-tax.
              ASSIGN w-dsc      = stax.tax-dscr[i]
                     w-tax      = ROUND((IF stax.company EQ "yes" 
                                           THEN v-t-price
                                           ELSE tt-inv-line.t-price) *
                                         stax.tax-rate[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
            END.
        END. /* THEN DO i */

        IF v-t-price NE tt-inv-line.t-price THEN DO:

          CREATE w-tax.
          ASSIGN w-dsc     = "******ITEM TOTAL:"
                 w-tax     = v-t-price
                 v-lines   = v-lines + 1.
        END. /* IF v-t-price*/

        v-price-head = tt-inv-line.pr-uom.    
        
      FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ tt-inv-line.b-no NO-LOCK NO-ERROR. 

      ASSIGN tt-inv-line.bol-no = IF AVAIL oe-bolh THEN oe-bolh.bol-no ELSE inv-head.bol-no .

        IF v-ship-qty GT 999999 OR v-inv-qty GT 999999 THEN
           PUT 
            v-ordqty         FORMAT "->,>>>,>>9"  SPACE(1)
            v-inv-qty        FORMAT "->,>>>,>>9"   SPACE(1)  
            tt-inv-line.bol-no           SPACE(1)
            tt-inv-line.part-no FORMAT "x(15)"     SPACE(1) 
            /*v-i-dscr         FORMAT "x(30)"      SPACE(1)  */
            tt-inv-line.po-no   FORMAT "x(15)"     SPACE(8)
            v-ord-ln         FORMAT ">>9"          SPACE(1)

            "<C60.2>"v-price FORMAT "->,>>9.99<<"  SPACE(1)
            v-price-head                           SPACE(1)
            "<C71>" 
            tt-inv-line.t-price FORMAT "->>>,>>9.99"                     
            SKIP.

        ELSE
        PUT 
            v-ordqty         FORMAT "->,>>>,>>9"    SPACE(1)
            v-inv-qty        FORMAT "->>,>>9"     SPACE(1)  
            tt-inv-line.bol-no                  SPACE(1)
            tt-inv-line.part-no FORMAT "x(15)"        SPACE(1)   
            /*v-i-dscr         FORMAT "x(30)"      SPACE(1)  */
            tt-inv-line.po-no   FORMAT "x(15)"        SPACE(8)
            v-ord-ln         FORMAT ">>9"          SPACE(1)
            "<C60.2>"v-price FORMAT "->,>>9.99<<" SPACE(1)
            v-price-head                           SPACE(1)
            "<C71>" 
            tt-inv-line.t-price FORMAT "->>>,>>9.99"                     
            SKIP.

        v-printline = v-printline + 1.
      
        
        DO v = 1 TO 4:

          v-part-info = IF v EQ 1 
                          THEN  v-i-dscr
                          ELSE 
                          IF v EQ 2 
                          THEN tt-inv-line.part-dscr1
                          ELSE
                          IF v EQ 3 
                             THEN tt-inv-line.part-dscr2
                             ELSE TRIM(lv-inv-list).

          IF v-part-info NE "" OR 
             (v = 1 AND tt-inv-line.part-no <> "") or
             (v = 2 and tt-inv-line.po-no <> "")
            THEN  DO:
               FIND FIRST oe-bolh WHERE oe-bolh.company = tt-inv-line.company AND
                              oe-bolh.b-no = tt-inv-line.b-no NO-LOCK NO-ERROR.
             IF v = 1 
               THEN  PUT  tt-inv-line.ord-no  FORMAT "->>>>>>>9"      SPACE(1)
                          v-ship-qty       FORMAT "->>>,>>9"     SPACE(10)
                          v-i-no           FORMAT "x(15)"        SPACE(1)  
                          v-part-info  SKIP.
               ELSE 
                IF v = 2 
                  THEN PUT SPACE(44) /*inv-line.po-no SPACE*/
                                     v-part-info    SKIP.
                  ELSE
                     IF v = 3 THEN 
                     PUT SPACE(44) "" SKIP.
                     ELSE
                     PUT SPACE(22) "Previous Invoice(s): " v-part-info SKIP.

             v-printline = v-printline + 1.
          END.
        END.
        IF NOT v-print-dept THEN PUT skip(1).
        ASSIGN v-printline = v-printline + 1.

         IF v-print-dept AND AVAIL oe-ordl THEN
         DO:
            FOR EACH notes WHERE
                notes.rec_key EQ oe-ordl.rec_key AND
                CAN-DO(v-depts,notes.note_code)
                NO-LOCK
                BY notes.note_code:
               
                v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
            
                DO i = 1 TO v-tmp-lines:
                   IF v-printline > 65 THEN DO:
                      PAGE.
                      v-printline = 0.
                      {oe/rep/invcapcin.i}
                   END.
            
                   PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                       v-printline = v-printline + 1.
                END.
            END.
            PUT skip(1).
        END. /* IF v-prin t-dept AND AVAIL oe-ordl */
     END.  /* each inv-line */

     FOR EACH inv-misc NO-LOCK 
       WHERE inv-misc.company = inv-head.company 
         AND inv-misc.r-no = inv-head.r-no 
         AND inv-misc.bill = "Y" 
        BREAK BY inv-misc.ord-no with frame detailm:

        IF FIRST(inv-misc.ord-no) THEN DO:

          IF v-printline > 65 THEN DO:
             PAGE.
             v-printline = 0.
             {oe/rep/invcapcin.i}
          END.

          PUT 
           SKIP(3)
            "** Miscellaneous Items **" at 23 
           SKIP(1).

          ASSIGN v-printline = v-printline + 2.
        END.
          
          PUT inv-misc.ord-no AT 3 inv-misc.charge AT 12  inv-misc.dscr inv-misc.po-no inv-misc.amt SKIP.

          ASSIGN v-subtot-lines = v-subtot-lines + inv-misc.amt
                 v-printline = v-printline + 1.

          IF inv-misc.tax and avail stax THEN  
            DO i = 1 to 3:
              IF stax.tax-code[i] NE "" THEN DO:
                 CREATE w-tax.
                 ASSIGN
                   w-dsc      = stax.tax-dscr[i]
                   w-tax      = IF stax.company EQ "yes" 
                                 THEN v-t-price
                                 ELSE inv-misc.amt
                   w-tax      = ROUND(w-tax * (1 + (stax.tax-rate[i] / 100)),2) - 
                                      w-tax
                   v-t-price  = v-t-price + w-tax
                   v-t-tax[i] = v-t-tax[i] + w-tax
                   v-lines    = v-lines + 1.
              END.
          END.

          IF v-t-price NE inv-misc.amt THEN DO:
            CREATE w-tax.
            ASSIGN w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
          END.
          IF v-printline > 65 THEN DO:
             PAGE.
             v-printline = 0.
             {oe/rep/invcapcin.i}
          END.
     END.  /* each inv-misc */

     IF v-print-dept THEN DO:

      FOR FIRST bf-inv-head NO-LOCK
         WHERE bf-inv-head.company EQ  inv-head.company
           AND bf-inv-head.inv-no  EQ inv-head.inv-no
           AND bf-inv-head.rec_key NE "":          

         v-printline = v-printline + 1.
         ASSIGN v-depts = REPLACE(v-depts," ","").

         IF CAN-FIND(FIRST notes
           WHERE notes.rec_key EQ bf-inv-head.rec_key
             AND CAN-DO(v-depts,notes.note_code)) THEN DO:

           FOR EACH notes NO-LOCK 
             WHERE notes.rec_key = bf-inv-head.rec_key
               AND CAN-DO(v-depts,notes.note_code) BY notes.note_code:           

             EMPTY TEMP-TABLE tt-formtext.

             ASSIGN v-text = ""
                    v-text = v-text + " " + notes.note_text.
             
             DO v-licnt = 1 TO 5:
                CREATE tt-formtext.
                ASSIGN tt-line-no = v-licnt
                       tt-length  = 100. 
             END.

             RUN custom/formtext.p (v-text).

             ASSIGN i = 0 v-notes = "" note-count = 0.

             FOR EACH tt-formtext:

               ASSIGN i = i + 1.

               IF i <= 5 THEN
                  ASSIGN v-notes[i] = tt-formtext.tt-text.

               IF v-notes[i] <> "" THEN note-count = i.
             END.

             DO i = 1 TO note-count:
               
              IF v-printline > 65 THEN DO:
                PAGE.
                v-printline = 0.
                {oe/rep/invcapcin.i}
              END.

              IF v-notes[i] NE "" THEN
                 PUT v-notes[i] FORM "x(80)" SKIP.

              ASSIGN v-printline = v-printline + 1.
                            
              IF v-printline > 65 THEN DO:
                 PAGE.
                 v-printline = 0.
                 {oe/rep/invcapcin.i}
              END.
             END.
           END. /* FOR EACH NOTES */
         END. /* FIND FIRST NOTES */
       END. /* for each bf-inv-head  */    
     END. /* IF v-print-dept */  

     IF v-prntinst THEN DO:

       IF v-print-dept = NO THEN
          PUT SKIP(3).
       ELSE
          PUT SKIP(2).

       DO i = 1 TO 4:

         IF inv-head.bill-i[i] <> "" THEN DO:
           IF v-printline > 65 THEN DO:
              PAGE.
              v-printline = 0.
              {oe/rep/invcapcin.i}
           END.

           PUT inv-head.bill-i[i] SKIP.

           v-printline = v-printline + 1.

         END.
       END.
     END. /* IF v-prntinst*/

     v-frt-tax = inv-head.t-inv-freight.

     IF inv-head.tax-gr <> "" AND
        inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAIL stax 
       THEN 
       DO i = 1 TO 3:

        IF stax.tax-code[i] NE "" AND 
           stax.tax-frt[i] EQ YES 
          THEN DO:

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

     ASSIGN inv-head.printed = yes
            inv-head.stat = "X".

    END. /* DO TRANSACTION avail inv-head */

    PUT "<FArial><R57.5><C1><#5><FROM><R+6><C+36><RECT>"
         "<=5><R57.5><C3>Please Remit To:"
         "<=5><R59.5><C6>     Capitol City Container Corp.".
    PUT "<=5><R60.5><C6>     PO Box 68531" .
    PUT "<=5><R61.5><C6>     Indianapolis, Indiana  46268".

    v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.

    PUT "<FLucida Console><R57.5><C58><#8><FROM><R+7><C+22><RECT> "
        "<=8><R+0.2> Sub Total" "<C69>:" v-subtot-lines FORMAT "->>>,>>9.99"
        "<=8><R+1> Freight"   "<C69>:" v-inv-freight  FORMAT "->>>,>>9.99"
        "<=8><R+2> IN Sales Tax"   "<C69>:" inv-head.t-inv-tax  FORMAT "->>>,>>9.99".

    PUT "<=8><R+6> Grand Total" "<C69>:"  inv-head.t-inv-rev  FORMAT "->>>,>>9.99".
    PUT "<FArial>".

    ASSIGN
       v-printline = v-printline + 6
       v-page-num = PAGE-NUM.
    page.
 
    end. /* each xinv-head */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
