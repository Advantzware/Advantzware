/* ------------------------------------------- ar/rep/invrfcx.p 11200902 GDM */
/* PRINT INVOICE   Xprint form for Loylang                                    */
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
DEF VAR v-inv-date      AS DATE initial TODAY FORMAT "99/99/9999" NO-UNDO.
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
DEF VAR v-net2        LIKE inv-head.t-inv-rev     NO-UNDO.

DEF VAR tmp1           AS DEC NO-UNDO.
DEF VAR tmp2        AS DATE NO-UNDO.
DEF VAR v-tailgate     AS CHAR NO-UNDO FORM "x(30)".
DEF VAR v-tail-price   AS CHAR NO-UNDO FORM "x(10)".
DEF VAR v-ship-qty1    AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR v-ship-qty1i   AS INT  NO-UNDO.
DEF VAR fg-uom-list    AS CHAR NO-UNDO.
DEF VAR v-price2       AS DECI NO-UNDO.
DEF VAR v-price-head2 AS CHAR FORMAT "X(5)" NO-UNDO.

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

DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER ref-sell-price FOR reftable.
DEF BUFFER b-cust FOR cust.

DEF TEMP-TABLE w-tax NO-UNDO
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.

DEF VAR v-t-tax       AS DEC extent 3 NO-UNDO.
DEF VAR v-bot-lab     AS CHAR FORMAT "x(63)" extent 3 NO-UNDO.
DEF VAR v-lines       AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax     AS DEC NO-UNDO.

DEF VAR v-cust-add1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-cust-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-cust-add3  AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-cust-phn  AS CHAR FORM "x(30)" NO-UNDO.
DEF VAR v-fax-arcd  AS CHAR FORM "x(30)" NO-UNDO.


DEF VAR v-tmp-lines   AS DEC NO-UNDO.
DEF VAR v-notes       AS CHAR EXTENT 4 FORMAT "x(80)" NO-UNDO.
DEF VAR v-notes-line  AS INT NO-UNDO.
DEF VAR v-inv-total   AS DEC NO-UNDO.
DEF VAR v-custno      LIKE cust.cust-no NO-UNDO.

/* === with xprint ====*/
DEF VAR ls-image1    AS CHAR NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.

DEF VAR v-tel     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax     AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR lv-bol-no LIKE ar-invl.bol-no NO-UNDO.

DEF VAR lv-print-bsf AS LOG INIT NO NO-UNDO.  /* controls the switch between Loylang and LoylangBSF*/
DEF VAR lv-print-bsf-item AS LOG INIT NO NO-UNDO. /*controls BSF printing per item*/
DEF VAR ld-bsf LIKE eb.t-sqin INIT 1 NO-UNDO. /* store bsf for price/bsf calculation */
DEF VAR ld-price-per-m AS DEC INIT 1 NO-UNDO. /*for calculating BSF*/
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdFileSysProcs AS HANDLE    NO-UNDO.

RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.
FIND FIRST sys-ctrl where sys-ctrl.company = cocode
                      and sys-ctrl.NAME = "INVPRINT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.char-fld = "LoylangBSF" THEN lv-print-bsf = YES.
    ELSE lv-print-bsf = NO.  /* controls the switch between Loylang and LoylangBSF at main level*/

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound AND cRtnChar NE "" THEN DO:
    cRtnChar = DYNAMIC-FUNCTION (
                   "fFormatFilePath" IN hdFileSysProcs,
                   cRtnChar
                   ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile IN hdFileSysProcs (
        INPUT  cRtnChar,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.

    IF NOT lValid THEN DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

ASSIGN ls-full-img1 = cRtnChar + ">" .

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


    FIND FIRST b-cust WHERE b-cust.company = cocode AND
                         b-cust.active = "X" NO-LOCK NO-ERROR.
         
    IF AVAIL b-cust THEN
             ASSIGN
             v-cust-add1 = b-cust.addr[1]                                                                 
             v-cust-add2 = b-cust.addr[2]
             v-cust-add3 = b-cust.city + ", " + b-cust.state + "  " + b-cust.zip 
             v-cust-phn  = "Phone: " + string(b-cust.area-code,"(999)") + string(b-cust.phone,"999-9999")                                
             v-fax-arcd  = "Fax: " + string(b-cust.fax,"(999)999-9999")                             .

    FIND FIRST cust WHERE cust.company = ar-inv.company
                      AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.
    ASSIGN v-custno = cust.cust-no.
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
         THEN
           ASSIGN  
            v-shipto-name = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city = shipto.ship-city
            v-shipto-state = shipto.ship-state
            v-shipto-zip = shipto.ship-zip.
     END.

     FIND FIRST oe-ord 
         WHERE oe-ord.company EQ cocode
           AND oe-ord.ord-no  EQ ar-inv.ord-no NO-LOCK NO-ERROR.
       IF AVAIL oe-ord THEN DO:
          
          IF AVAIL sman THEN v-salesname = sman.sname.
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
                     soldto.cust-no EQ ar-inv.cust-no AND
                     soldto.sold-id EQ ar-inv.cust-no
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
               soldto.sold-id EQ ar-inv.cust-no
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
     IF ar-inv.inv-date NE ? THEN 
        ASSIGN 
          v-inv-date = ar-inv.inv-date
          v-date-ship = ar-inv.inv-date.
     
     IF ar-inv.fob-code BEGINS "ORIG" 
       THEN ASSIGN v-fob = "Origin".
       ELSE ASSIGN v-fob = "Destination".

     FIND FIRST carrier WHERE carrier.company = ar-inv.company 
                          AND carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.
     IF AVAIL carrier 
       THEN ASSIGN v-shipvia = carrier.dscr.
       ELSE ASSIGN v-shipvia = "".

     ASSIGN
       v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                      "  " + v-shipto-zip
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
      /* v-po-no = ar-inv.po-no*/
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
         /*v-po-no = ar-invl.po-no                  */
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

     
     {ar/rep/invrfcx.i}  /* xprint form */
         
     ASSIGN 
       v-printline = 29
       v-subtot-lines = 0
         v-net2 = 0
       v-t-tax = 0.

     FOR EACH ar-invl NO-LOCK 
         WHERE ar-invl.x-no = ar-inv.x-no:

         ASSIGN 
           lv-print-bsf-item = NO
           ld-bsf = 1
           v-case-line = "" 
           v-part-line = "" 
           v-case-cnt = "".          
         
         /*start of block for LoylangBSF*/
         IF lv-print-bsf THEN
            DO:
                FIND FIRST itemfg 
                    WHERE itemfg.company EQ cocode 
                        AND itemfg.i-no = ar-invl.i-no NO-LOCK NO-ERROR.
                IF AVAIL itemfg THEN
                DO:
                    IF itemfg.procat = "PADS" THEN 
                    DO:
                        ASSIGN 
                            lv-print-bsf-item = YES
                            ld-bsf = itemfg.t-sqin / 144.
                        /*convert price to price per M*/
                        RUN sys/ref/convptom.p (ar-invl.pr-uom, ar-invl.unit-pr, ar-invl.qty, ar-invl.cas-cnt, OUTPUT ld-price-per-m ).
                    END. /* if itemfg.procat = "PADS"*/
                END. /*avail itemfg*/
            END. /*if lv-print-bsf*/
        
         
         IF v-printline GE 57 THEN DO: 
             PAGE.
           {oe/rep/rfcx_tc.i}
            PAGE. 
           {ar/rep/invrfcx.i}
           v-printline = 29.
         END.

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
              v-po-no = oe-ordl.po-no .
           ELSE 
            ASSIGN 
              v-bo-qty = IF ( ar-invl.qty - ar-invl.ship-qty ) < 0
                            THEN 0 
                            ELSE ar-invl.qty - ar-invl.ship-qty .

          IF v-po-no EQ "" AND AVAIL oe-ord THEN
              ASSIGN v-po-no = oe-ord.po-no .

         ASSIGN 
           v-inv-qty = ar-invl.inv-qty
           v-ship-qty = ar-invl.ship-qty
           v-i-no = ar-invl.i-no
           v-i-dscr = ar-invl.i-name
           v-t-price = ar-invl.amt
           v-subtot-lines = v-subtot-lines + ar-invl.amt.
         
         v-price = IF NOT lv-print-bsf-item THEN ar-invl.unit-pr * (1 - (ar-invl.disc / 100))  
                    ELSE (ld-price-per-m / ld-bsf ) * (1 - (ar-invl.disc / 100)). /*override price if BSF for item */

         IF v-t-price NE ar-invl.amt THEN DO:
           CREATE w-tax.
           ASSIGN
             w-dsc     = "******ITEM TOTAL:"
             w-tax     = v-t-price
             v-lines   = v-lines + 1.
         END.

         ASSIGN 
           /*v-po-no  = ar-invl.po-no*/
           v-ord-no = ar-invl.ord-no
           v-i-dscr2 = ar-invl.part-dscr1.
          
         v-price-head = IF NOT lv-print-bsf-item THEN ar-invl.pr-uom
                      ELSE "BSF". /*override the UOM in case of LoylangBSF*/

                           FIND FIRST itemfg WHERE
                 itemfg.company = ar-invl.company AND 
                 itemfg.i-no    = ar-invl.i-no
                 NO-LOCK NO-ERROR.

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
                   WHERE oe-boll.company EQ ar-invl.company
                     AND oe-boll.bol-no  EQ ar-invl.bol-no
                     AND oe-boll.i-no    EQ ar-invl.i-no USE-INDEX bol-no:

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
                          
                             ASSIGN v-price-head = string(b-oe-rel.sell-price,">>>9.9999")
                                    v-tail-price = string((b-oe-rel.sell-price * fg-rdtlh.qty),">>,>>9.99").
                         
                          
                             ASSIGN v-price-head = string(0,">>>9.9999")
                                    v-tail-price = string(0,">>,>>9.99").

                          
        
                          ASSIGN v-ship-qty1 = STRING(fg-rdtlh.qty,">>>>>>>9")
                                 v-ship-qty1i = fg-rdtlh.qty.
                       END. /* avail b-oe-rel */
                    END. /* avail b-oe-rell */
                    LEAVE.
                END. /* each oe-boll */
            END. /* avail itemfg */ 

            IF v-ship-qty1i > 0 THEN DO:
               RUN compute-ext-price (recid(ar-invl), v-ship-qty1i, OUTPUT v-price2).

               ASSIGN v-net2 = v-net2 + v-price2. 
            END.
            ELSE
               ASSIGN v-net2 = v-net2 + ar-invl.amt.
         
         IF v-i-dscr2 = "" THEN v-i-dscr2 = ar-invl.i-dscr.

         IF v-ord-no = 0 AND v-ship-qty = 0 THEN v-ship-qty = v-inv-qty.
         
         PUT 
           SPACE(1)
             ar-invl.po-no         FORMAT "x(15)" 
           SPACE(1)
             ar-invl.part-no FORMAT "x(30)" 
           SPACE(1)
           /*  v-ship-qty  FORMAT "->>>>>>9"              */
           SPACE(9)
             v-inv-qty   FORMAT "->>>>>9" 
           SPACE(3)                               
             v-price     FORMAT "->>>,>>9.9999" 
           SPACE(4)
             ar-invl.amt FORMAT "->>>,>>9.99"                
          SKIP
           SPACE(1)
             v-ord-no    FORMAT ">>>>>>" 
           SPACE(10) v-i-dscr    FORMAT "x(30)"
           SPACE(2) /*ar-invl.qty FORMAT "->>>>>9"*/
         /*  SPACE(13) v-pc  */
           SPACE(27)  v-price-head 
           SPACE(1) 
          SKIP
           /*SPACE(1)
             ar-invl.lot-no FORMAT "x(15)" */
            SPACE(17) v-i-dscr2  FORMAT "x(30)"
          SKIP
           SPACE(17) ar-invl.part-dscr2 FORMAT "x(30)"
          SKIP.

         ASSIGN v-printline = v-printline + 4.

     END. /* each ar-invl */

     IF v-printline GE 66 THEN do:           
       PAGE.
       {oe/rep/rfcx_tc.i}
             PAGE.
       {ar/rep/invrfcx.i}
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

     /* Bill Notes Task# 12131308*/
     IF TRIM(ar-inv.bill-i[1]) NE "" OR
         TRIM(ar-inv.bill-i[2]) NE "" OR
         TRIM(ar-inv.bill-i[3]) NE "" OR
         TRIM(ar-inv.bill-i[4]) NE "" 
         THEN DO:
         PUT SKIP.
        /* ASSIGN v-printline = v-printline + 1.*/
     END. 
     DO i = 1 TO 4:
         IF ar-inv.bill-i[i] NE "" THEN DO:
             IF v-printline GE 66 THEN do:           
                 PAGE.
                 {oe/rep/rfcx_tc.i}
                     PAGE.
                 {ar/rep/invrfcx.i}
                     v-printline = 29.
             END.
             PUT ar-inv.bill-i[i] AT 18
                 SKIP.
             ASSIGN v-printline = v-printline + 1.
        END.
     END.  /* 1 to 4 */
     /*Bill Notes End */

       

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
       v-inv-total = v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight
       tmp1 = 0 
       tmp2 = ?
       v-net = ar-inv.net.

    IF v-net2 <> ar-inv.net THEN
       v-net = v-net2.

    FIND FIRST terms NO-LOCK 
         WHERE terms.company EQ cocode 
           AND terms.t-code  EQ ar-inv.terms 
         NO-ERROR.
    if avail terms then
       assign
        tmp1 = v-net * (round(terms.disc-rate / 100, 2))
        tmp2 = v-inv-date + terms.disc-days.        /*Task# 01151403*/
     
     ASSIGN
       v-inv-total = v-subtot-lines + ar-inv.tax-amt + v-inv-freight.

     PUT 
       "<=5><R60.3><C1><P12> " v-net FORMAT "->,>>>,>>9.99" "<C19>" tmp1 "<C34.3>" tmp2 "<C46>" (IF v-net2 + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight <> v-inv-total THEN v-net2 + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight ELSE v-inv-total)  FORM "->>>,>>9.99"
        "<=5><R61.8><C1><FROM><C61><LINE>"       
        "<=5><R62><C3><P8>NET AMOUNT SUBJECT   <C19.5> CASH DISCOUNT  <C31>      IF PAID BY    <C45>     INVOICE AMOUNT"
        "<=5><R62.7><C3><P8> TO CASH DISCOUNT  <C20>    AMOUNT      <C31.5>      THIS DATE  " .

    PUT 
        "<R59><P9><C63><#8><B>"
        "<=8><R59> Sub Tot:" v-subtot-lines FORMAT "->>>,>>9.99"
        "<=8><R60> Sals Tx:" ar-inv.tax-amt FORMAT "->>>,>>9.99"                  
        "<=8><R61> Freight:" v-inv-freight FORMAT "->>>,>>9.99" 
        "<=8><R62> Tot Inv:" v-inv-total    FORMAT "->>>,>>9.99" "</B>". 

    /*PUT "<R60><C13><P8></B> CLAIMS MUST BE MADE ON RECEIPT OF SHIPMENTS OTHERWISE NOT ALLOWED." .*/

  /*  PUT "<FArial><R61><C1><#9><FROM><R63><C61><RECT> " 
        "<R61><C34.5><FROM><R63><C34.5><LINE>"         
        "<R61><C61><FROM><R63><C61><LINE>" 
        SKIP .*/
 
    v-printline = v-printline + 8.
  
    IF v-printline <= 66 THEN do:
         PAGE.
         {oe/rep/rfcx_tc.i}
    END.
    ELSE do:
        PAGE.
        {oe/rep/rfcx_tc.i} .
    END.
     
    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = yes.
              xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 

    
 
    end. /* each ar-inv */

IF VALID-HANDLE(hdFileSysProcs) THEN
    DELETE PROCEDURE hdFileSysProcs.
    
PROCEDURE compute-ext-price.
    DEFINE INPUT PARAM in-recid AS RECID.
    DEFINE INPUT PARAM in-qty AS INTE NO-UNDO.
    DEFINE OUTPUT PARAM out-price AS DECI NO-UNDO.

    DEF BUFFER bf-ar-invl FOR ar-invl.

    FIND bf-ar-invl WHERE RECID(bf-ar-invl) = in-recid NO-LOCK NO-ERROR.
    IF AVAIL bf-ar-invl THEN DO:

      ASSIGN out-price = (bf-ar-invl.ship-qty - in-qty) / 1000 * bf-ar-invl.unit-pr.

      IF bf-ar-invl.pr-uom BEGINS "L" AND bf-ar-invl.pr-uom NE "LB" THEN
         out-price = bf-ar-invl.unit-pr *
                     IF (bf-ar-invl.ship-qty - in-qty) LT 0 THEN -1 ELSE 1.
      ELSE IF bf-ar-invl.pr-uom EQ "CS" THEN
         out-price = (bf-ar-invl.ship-qty - in-qty) /
                     (IF bf-ar-invl.cas-cnt NE 0 THEN
                         bf-ar-invl.cas-cnt
                      ELSE
                      IF itemfg.case-count NE 0 THEN
                         itemfg.case-count ELSE 1) *
                      bf-ar-invl.unit-pr.
      ELSE IF LOOKUP(bf-ar-invl.pr-uom,fg-uom-list) GT 0 THEN
         out-price = (bf-ar-invl.ship-qty - in-qty) * bf-ar-invl.unit-pr.
      ELSE
      FOR EACH uom
         WHERE uom.uom  EQ bf-ar-invl.pr-uom
           AND uom.mult NE 0
         NO-LOCK:
         out-price = (bf-ar-invl.ship-qty - in-qty) / uom.mult * bf-ar-invl.unit-pr.
         LEAVE.
      END.
      out-price = ROUND(out-price,2).

      IF bf-ar-invl.disc NE 0 THEN
        out-price = ROUND(out-price * (1 - (bf-ar-invl.disc / 100)),2).
    END.
    ELSE
        out-price = ar-invl.amt.

END PROCEDURE.


/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
