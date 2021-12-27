/* ---------------------------------------------- ar/rep/invhenry.p   */
/* PRINT INVOICE   Henry form                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}

DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR v-salesman AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR v-fob AS CHAR FORMAT "x(27)" NO-UNDO.
DEF VAR v-shipvia LIKE carrier.dscr NO-UNDO.
DEF VAR v-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-sold-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-name AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-addr AS CHAR FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEF VAR v-shipto-city AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-shipto-state AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR v-shipto-zip AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-line AS INT NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-t-weight LIKE ar-invl.t-weight NO-UNDO.
DEF VAR v-inv-no AS INT NO-UNDO.
DEF VAR v-tot-cas AS DEC FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tot-pallets AS INT NO-UNDO.
DEF VAR v-tot-qty AS INT NO-UNDO.
DEF VAR v-inv-date AS DATE INITIAL TODAY FORM "99/99/9999" NO-UNDO.
DEF SHARED VAR v-fr-tax AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR v-tax-rate AS DEC FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tax-code LIKE stax.tax-code NO-UNDO.
DEF VAR v-tx-rate LIKE stax.tax-rate NO-UNDO.
DEF VAR v-ans AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR v-date-ship AS DATE INITIAL TODAY NO-UNDO.
DEF VAR v-del-no AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-bol-cases LIKE oe-boll.cases NO-UNDO.
DEF VAR v-set-qty AS DECIMAL NO-UNDO.
DEF VAR v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
DEF VAR v-net LIKE inv-head.t-inv-rev NO-UNDO.
DEF VAR v-case-cnt AS CHAR FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR v-case-line AS CHAR NO-UNDO.
DEF VAR v-part-line AS CHAR NO-UNDO.
DEF VAR tmp1 AS DEC NO-UNDO.
DEF VAR tmp2 AS DATE NO-UNDO.
DEF VAR net1 AS DEC NO-UNDO.
DEF VAR net2 AS DEC NO-UNDO.
DEF VAR net3 AS DEC NO-UNDO.
DEF VAR cnt AS INT NO-UNDO.
DEF VAR disp-frt AS CHAR INIT "Freight:" FORMAT "x(8)" NO-UNDO.
DEF VAR minus-ship AS INT NO-UNDO.
DEF VAR lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.

DEF BUFFER xar-inv FOR ar-inv.
DEF BUFFER xar-invl FOR ar-invl.

DEF TEMP-TABLE w-sman
  FIELD sman AS CHAR FORMAT "x(4)".

DEF VAR v-ord-del-hdr AS CHAR FORMAT "x(3)" INIT "Del".
DEF VAR v-beeler-lines AS INT.
DEF VAR v-part-info AS CHAR FORMAT "x(30)".
DEF VAR v AS INT.
DEF VAR v-bo-qty AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-inv-qty AS DEC NO-UNDO.
DEF VAR v-ship-qty AS INT FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-ord-qty AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-i-no AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-i-dscr AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR v-price AS DEC FORMAT ">>>>9.9999" NO-UNDO.
DEF VAR v-t-price AS DEC FORMAT ">>>>>>9.99" NO-UNDO.
DEF VAR v-po-no LIKE ar-invl.po-no NO-UNDO.
DEF VAR v-bill-i AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR v-ord-date LIKE oe-ord.ord-date NO-UNDO.
DEF VAR v-ship-i AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ord-po-no LIKE oe-ord.po-no NO-UNDO.
DEF VAR v-price-head AS CHAR FORMAT "x(5)" NO-UNDO.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
DEF TEMP-TABLE w-tax
    FIELD w-dsc AS   CHAR
    FIELD w-tax AS   DEC.
DEF VAR v-t-tax      AS   DEC EXTENT 5 NO-UNDO.
DEF VAR v-bot-lab    AS   CHAR FORMAT "x(63)" EXTENT 5 NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR lv-inv-list AS CHAR NO-UNDO.
DEF VAR v-int AS DEC NO-UNDO.
DEF VAR v-tot-tax AS DEC NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(48)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE vRelPo LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE iPoCheck AS LOGICAL NO-UNDO.
DEFINE VARIABLE cPo-No AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
DEF VAR cStockNotes AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
DEFINE SHARED VARIABLE lPrintQtyAll  AS LOGICAL NO-UNDO .
DEF BUFFER bf-cust FOR cust .
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE dTotalAmount   AS DECIMAL NO-UNDO.
DEFINE VARIABLE dExchangeRate  AS DECIMAL EXTENT 3 NO-UNDO.
DEFINE VARIABLE cCurrency      AS CHARACTER NO-UNDO.

{methods/pPrintImageOnBack.i v-print-fmt "first"}
/* v-print-fmt => ".\custfiles\Images\<FormatName>BackImage.pdf" */
/* After which Page- Image will print  (First, All) */

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound AND cRtnChar NE "" THEN DO:
    cRtnChar = DYNAMIC-FUNCTION (
                   "fFormatFilePath",
                   cRtnChar
                   ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile(
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

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "INVPRINT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                      AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".
   
    FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

     ASSIGN v-comp-add1 = ""
        v-comp-add2 = "" 
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = "".

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
           lv-comp-name = cust.NAME   
           .
 END.
    

    FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
        FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id NO-LOCK,
        FIRST cust WHERE cust.company = ar-inv.company
                     AND cust.cust-no = ar-inv.cust-no NO-LOCK 

        BREAK BY (IF v-print-fmt EQ "ASIXprnt" THEN "" ELSE ar-inv.cust-no)
              BY ar-inv.inv-no:
     
        FIND FIRST carrier WHERE carrier.company EQ cocode
             AND carrier.carrier EQ ar-inv.carrier NO-LOCK NO-ERROR.
        IF AVAIL carrier THEN ASSIGN v-shipvia = carrier.dscr.
        ELSE ASSIGN v-shipvia = "".

        FIND FIRST shipto WHERE shipto.company EQ cocode
             AND shipto.cust-no EQ ar-inv.cust-no
             AND shipto.ship-id EQ ar-inv.ship-id NO-LOCK NO-ERROR.

        IF AVAIL shipto THEN 
           ASSIGN  v-shipto-name = shipto.ship-name
                   v-shipto-addr[1] = shipto.ship-addr[1]
                   v-shipto-addr[2] = shipto.ship-addr[2]
                   v-shipto-city = shipto.ship-city
                   v-shipto-state = shipto.ship-state
                   v-shipto-zip = shipto.ship-zip
                   v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
                   .
         v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip .


        IF ar-inv.fob-code BEGINS "ORIG" THEN
         ASSIGN v-fob = "Origin".
        ELSE
         ASSIGN v-fob = "Destination".

        ASSIGN
          
          v-line = 1
          v-printline = 0.
    
        FIND FIRST stax
            {sys/ref/stax1W.i}
              AND {sys/ref/taxgroup.i stax} EQ ar-inv.tax-code
            NO-LOCK NO-ERROR.
        IF NOT AVAIL stax THEN
           FIND FIRST stax WHERE stax.company = ar-inv.company AND stax.tax-group EQ ar-inv.tax-code
               NO-LOCK NO-ERROR.
        IF NOT AVAIL stax THEN
           FIND FIRST stax WHERE stax.tax-group EQ ar-inv.tax-code NO-LOCK NO-ERROR.

        IF AVAIL stax THEN
          ASSIGN v-tax-rate = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                 v-tax-code[1] = stax.tax-code[1]
                 v-tax-code[2] = stax.tax-code[2]
                 v-tax-code[3] = stax.tax-code[3]
                 v-tx-rate[1]  = stax.tax-rate[1]
                 v-tx-rate[2]  = stax.tax-rate[2]
                 v-tx-rate[3]  = stax.tax-rate[3].

        ASSIGN v-tot-pallets = 0
               v-date-ship   = ar-inv.inv-date.
        cPo-No = "".
        
       FOR EACH ar-invl NO-LOCK
           WHERE ar-invl.x-no  EQ ar-inv.x-no  
             AND (ar-invl.misc EQ NO OR ar-invl.billable)
           BREAK BY ar-invl.i-no:
         DO i = 1 TO 3:
          IF ar-invl.sman[i] NE "" THEN DO:
            CREATE w-sman.
            ASSIGN w-sman.sman = ar-invl.sman[i].
          END.
         END.
         ASSIGN v-tot-qty = v-tot-qty + ar-invl.ship-qty
                v-t-weight = v-t-weight + (ROUND(ar-invl.t-weight /
                            ar-invl.qty, 2) * ar-invl.inv-qty).

         IF ar-invl.po-no NE "" THEN DO:
                 cPo-No = cPo-No + ar-invl.po-no + ",". 
         END.
        
         FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = ar-invl.b-no AND
             oe-bolh.ord-no = ar-invl.ord-no:
           FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
              oe-boll.b-no = oe-bolh.b-no AND
              oe-boll.i-no = ar-invl.i-no:

                                      /** Bill Of Lading TOTAL CASES **/
              ASSIGN v-bol-cases = v-bol-cases + oe-boll.cases.
              RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).
              v-tot-pallets = v-tot-pallets + v-int.
           END. /* each oe-boll */
           ASSIGN v-date-ship = oe-bolh.bol-date.

         END. /* each oe-bolh */
         
         FIND FIRST oe-bolh WHERE oe-bolh.b-no = ar-invl.b-no NO-LOCK NO-ERROR.
         IF AVAIL oe-bolh THEN v-date-ship = oe-bolh.bol-date.

         IF LAST-OF(ar-invl.i-no) THEN DO:
           IF ar-invl.est-no NE "" THEN
           DO:
             FIND FIRST eb WHERE eb.company = ar-invl.company AND
               eb.est-no = ar-invl.est-no AND
               eb.form-no = ar-invl.form-no AND
               eb.blank-no = ar-invl.blank-no NO-LOCK NO-ERROR.

             IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN
             DO:
               FOR EACH fg-set NO-LOCK WHERE fg-set.company = ar-invl.company
                                       AND fg-set.set-no = ar-invl.i-no:
                    ASSIGN v-set-qty = v-set-qty + fg-set.qtyPerSet.
               END.
               IF v-set-qty = 0 THEN ASSIGN v-set-qty = 1.
               FOR EACH eb NO-LOCK WHERE eb.company = ar-invl.company AND
                              eb.est-no = ar-invl.est-no AND
                              eb.form-no NE 0:
                 FIND fg-set WHERE fg-set.company = ar-invl.company AND
                    fg-set.set-no = ar-invl.i-no  AND
                    fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                 IF AVAIL fg-set AND fg-set.qtyPerSet NE 0 THEN
                   ASSIGN v-part-qty = fg-set.qtyPerSet / v-set-qty.
                 ELSE
                   ASSIGN v-part-qty = 1 / v-set-qty.


                IF eb.cas-cnt = 0 THEN
                   ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                 ELSE
                   ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                 IF v-bol-cases NE 0 THEN
                   ASSIGN v-tot-cas = v-bol-cases.
               END. /* each eb */
             END. /* do */
             ELSE
             IF AVAIL eb THEN
             DO:
               IF eb.cas-cnt = 0 THEN
                 ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
               ELSE
                 ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
               IF v-bol-cases NE 0 THEN
                 ASSIGN v-tot-cas = v-bol-cases.
             END. /* do */
           END. /* est-no ne "" */
          ASSIGN
             v-t-weight = 0
             v-tot-cas = 0
             v-tot-qty = 0.
         END. /* last-of i-no */
        END. /* each ar-invl */
        DO iCount = 1 TO NUM-ENTRIES(cPo-No):
           IF ENTRY(1,cPo-No) NE ENTRY(iCount,cPo-No) AND ENTRY(iCount,cPo-No) NE ""  THEN iPoCheck = YES. 
       END.
    
                        /** Build Salesman Id String **/
        v-salesman = "".
        FOR EACH w-sman BREAK BY w-sman.sman:
          IF FIRST-OF(w-sman.sman) THEN
            ASSIGN v-salesman = v-salesman + w-sman.sman.
          DELETE w-sman.
        END.

        ASSIGN v-po-no = ar-inv.po-no
               v-bill-i = ar-inv.bill-i[1]
               v-ord-no = ar-inv.ord-no
               v-ord-date = ar-inv.ord-date
               v-ord-po-no = ""
               .
        
        FIND FIRST ar-invl
             WHERE ar-invl.x-no  EQ ar-inv.x-no  
               AND (ar-invl.misc EQ NO OR ar-invl.billable)
            NO-LOCK NO-ERROR.
        IF AVAIL ar-invl THEN
        DO:
           ASSIGN v-price-head = ar-invl.pr-uom
                  v-po-no = IF ar-invl.po-no <> "" THEN ar-invl.po-no ELSE ar-inv.po-no
                  v-ord-no = ar-invl.ord-no
                  lv-bol-no = ar-invl.bol-no
                  v-ord-po-no = IF iPoCheck EQ YES THEN "See below" ELSE ar-invl.po-no.
         /*find first oe-ord where oe-ord.company = cocode and
                                  oe-ord.ord-no = ar-invl.ord-no
                                  no-lock no-error.   
          if avail oe-ord then
          do:
            ASSIGN v-ord-po-no = IF iPoCheck EQ YES THEN "See below" ELSE oe-ord.po-no.
          end.*/
        END.      

        /* display heder info 
         view frame invhead-comp.  /* Print headers */  */
        IF v-salesman = "" THEN v-salesman = cust.sman.
        v-inv-date = ar-inv.inv-date.
        
        {ar/rep/invhenry.i}

        ASSIGN
           v-subtot-lines = 0
           v-t-tax = 0.

        FOR EACH ar-invl NO-LOCK
             WHERE ar-invl.x-no  EQ ar-inv.x-no  
               AND (ar-invl.misc EQ NO OR ar-invl.billable) 
              BY ar-invl.misc  BY ar-invl.i-no:
          ASSIGN v-case-line = ""
                 v-part-line = ""
                 v-case-cnt = ""
                 v-line = v-line + 1
                 v-beeler-lines = 0
                 lv-inv-list = ""
                 v-ship-qty  = IF ar-invl.ord-no EQ 0 THEN ar-invl.qty
                               ELSE ar-invl.ship-qty.

            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode AND
                                     oe-ordl.ord-no = ar-invl.ord-no AND
                                     oe-ordl.i-no = ar-invl.i-no
                                     NO-LOCK NO-ERROR.
            
            IF AVAIL oe-ordl THEN DO:
              ASSIGN v-bo-qty = IF (ar-invl.qty - v-ship-qty -
                                    oe-ordl.t-ship-qty) < 0 then 0 else
                                   (ar-invl.qty - v-ship-qty -
                                    oe-ordl.t-ship-qty).
            /* ASSIGN vRelPo = "".
             FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company = cocode
                AND oe-rel.ord-no = oe-ordl.ord-no
                AND oe-rel.i-no = oe-ordl.i-no
                AND oe-rel.LINE = oe-ordl.LINE :

                IF oe-rel.po-no NE "" THEN DO:
                  vRelPo = oe-rel.po-no.  
                  LEAVE.
                END.
             END.*/

              IF NOT CAN-FIND(FIRST oe-boll
                              WHERE oe-boll.company EQ ar-invl.company
                                AND oe-boll.b-no    EQ ar-invl.b-no
                                AND oe-boll.po-no   EQ ar-invl.po-no
                                AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                AND oe-boll.i-no    EQ oe-ordl.i-no
                                AND oe-boll.line    EQ oe-ordl.line
                                AND oe-boll.s-code  EQ "I"
                              USE-INDEX b-no) THEN
              FOR EACH xar-invl
                  WHERE xar-invl.company EQ oe-ordl.company
                    AND xar-invl.ord-no  EQ oe-ordl.ord-no
                    AND xar-invl.i-no    EQ oe-ordl.i-no
                    AND ROWID(xar-invl)  NE ROWID(ar-invl)
                    AND CAN-FIND(FIRST oe-boll
                                 WHERE oe-boll.company EQ xar-invl.company
                                   AND oe-boll.b-no    EQ xar-invl.b-no
                                   AND oe-boll.po-no   EQ xar-invl.po-no
                                   AND oe-boll.ord-no  EQ oe-ordl.ord-no
                                   AND oe-boll.i-no    EQ oe-ordl.i-no
                                   AND oe-boll.line    EQ oe-ordl.line
                                   AND oe-boll.s-code  EQ "I"
                                 USE-INDEX b-no)
                    
                  NO-LOCK:
                lv-inv-list = lv-inv-list + TRIM(STRING(xar-invl.inv-no,">>>>>>>>>>")) + " ".
              END.
            END.

            ELSE
              ASSIGN v-bo-qty = IF ( ar-invl.qty - v-ship-qty ) < 0
                                  then 0 else ar-invl.qty - v-ship-qty.

            ASSIGN v-inv-qty = ar-invl.qty
                   v-i-no = ar-invl.i-no
                   v-i-dscr = ar-invl.i-name
                   v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
                   v-t-price = ar-invl.amt
                   v-subtot-lines = v-subtot-lines + ar-invl.amt
                   v-ord-qty = ar-invl.qty .
                
              IF lPrintQtyAll THEN
                   v-inv-qty = ar-invl.inv-qty .

                IF ar-invl.tax AND AVAIL stax THEN
                DO i = 1 TO 5:
                  IF stax.tax-code1[i] NE "" THEN DO:
                    CREATE w-tax.
                    ASSIGN
                     w-dsc      = stax.tax-dscr1[i]
                     w-tax      = ROUND((IF stax.company EQ "yes" THEN v-t-price
                                                                  ELSE ar-invl.amt) *
                                        stax.tax-rate1[i] / 100,2)
                     v-t-price  = v-t-price + w-tax
                     v-t-tax[i] = v-t-tax[i] + w-tax
                     v-lines    = v-lines + 1.
                  END.
                END.

                IF v-t-price NE ar-invl.amt THEN DO:
                  CREATE w-tax.
                  ASSIGN
                   w-dsc     = "******ITEM TOTAL:"
                   w-tax     = v-t-price
                   v-lines   = v-lines + 1.
                END.
            
          IF NOT lPrintQtyAll THEN DO:  
            PUT SPACE(1)        /*"->>>>9.9<"*/
                v-inv-qty FORMAT  "->>>>>>9" SPACE(1)
                v-ship-qty  FORMAT "->>>>>>9" SPACE(1)
                /*v-bo-qty  format "->>>>>9" SPACE(1) */
                ar-invl.ord-no FORM ">>>>>>9" SPACE(1)
                v-i-no  FORMAT "x(15)" SPACE(3)
                v-i-dscr  FORMAT "x(25)" SPACE(3)
                v-price  FORMAT "$->>>,>>9.99" /*"$->,>>9.99<<"*/ SPACE(1)
                v-price-head SPACE(1)
                ar-invl.amt  FORMAT "$->>>>,>>9.99" /*"$->>>,>>9.99" */               
                SKIP.
          END.
          ELSE DO:
              PUT SPACE(1)        /*"->>>>9.9<"*/
                v-ord-qty FORMAT  "->>>>>>9" SPACE(1)
                v-inv-qty  FORMAT "->>>>>>9" SPACE(1)
                /*v-bo-qty  format "->>>>>9" SPACE(1) */
                ar-invl.ord-no FORM ">>>>>>9" SPACE(1)
                v-i-no  FORMAT "x(15)" SPACE(3)
                v-i-dscr  FORMAT "x(25)" SPACE(3)
                v-price  FORMAT "$->>>,>>9.99" /*"$->,>>9.99<<"*/ SPACE(1)
                v-price-head SPACE(1)
                ar-invl.amt  FORMAT "$->>>>,>>9.99" /*"$->,>>9.99<<"*/                
                SKIP.

          END.    /* else do */
             v-printline = v-printline + 1.
      
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company = ar-invl.company 
                AND itemfg.i-no = ar-invl.i-no NO-ERROR.

            DO v = 1 TO 4:
              CASE v:
                WHEN 1 THEN ASSIGN v-part-info = IF ar-invl.part-dscr1 <> "" THEN ar-invl.part-dscr1 ELSE ar-invl.i-dscr.
                WHEN 2 THEN ASSIGN v-part-info = IF ar-invl.part-dscr2 NE "" THEN ar-invl.part-dscr2 ELSE
                                                 IF AVAIL itemfg THEN itemfg.part-dscr2 ELSE "".
                WHEN 3 THEN ASSIGN v-part-info = IF AVAIL itemfg THEN itemfg.part-dscr3 ELSE "".
                WHEN 4 THEN ASSIGN v-part-info = "".
              END CASE.

              IF v-part-info NE "" OR (v = 1 AND ar-invl.part-no <> "") THEN DO:
                 IF v = 1 THEN DO:

                     IF lPrintQtyAll THEN DO:
                      PUT SPACE(1) v-ship-qty FORMAT "->>>>>>9" .

                         IF LENGTH(ar-invl.po-no) LE 8 THEN DO:
                             PUT SPACE(9) ar-invl.po-no FORMAT "x(8)" SPACE(1)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.
                         ELSE DO: 
                             PUT  SPACE(1) ar-invl.po-no FORMAT "x(15)" SPACE(2)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.
                     END. /* lPrintQtyAll*/
                     ELSE DO:
                         IF LENGTH(ar-invl.po-no) LE 8 THEN DO:
                             PUT  SPACE(18) ar-invl.po-no FORMAT "x(8)" SPACE(1)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.
                         ELSE DO: 
                             PUT SPACE(11) ar-invl.po-no FORMAT "x(15)" SPACE(1)   ar-invl.part-no SPACE(3) v-part-info SKIP.
                         END.

                     END.    /* else do */
                 END.
                 ELSE 
                 IF v = 2 THEN PUT SPACE(45) v-part-info SKIP.
                 ELSE          PUT SPACE(45) v-part-info SKIP.
                 v-printline = v-printline + 1.
              END.
            END.
            PUT SKIP(1).
            v-printline = v-printline + 1.
            
            IF v-print-dept AND AVAIL oe-ordl THEN
            DO:
               FIND FIRST job-hdr WHERE
                    job-hdr.company EQ cocode AND
                    job-hdr.job-no  EQ oe-ordl.job-no AND
                    job-hdr.job-no2 EQ oe-ordl.job-no2
                    NO-LOCK NO-ERROR.

               IF AVAIL job-hdr THEN
               DO:
                  FIND FIRST job WHERE
                       job.company EQ cocode AND
                       job.job     EQ job-hdr.job AND
                       job.job-no  EQ job-hdr.job-no AND
                       job.job-no2 EQ job-hdr.job-no2
                       NO-LOCK NO-ERROR.
                 
                  IF AVAIL job THEN
                  DO:
                     FOR EACH notes WHERE
                         notes.rec_key EQ job.rec_key AND /*for capitol task 06180806*/
                         CAN-DO(v-depts,notes.note_code)
                         NO-LOCK
                         BY notes.note_code:
                        
                         v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                         {SYS/INC/ROUNDUP.I v-tmp-lines}
                    
                         IF notes.note_text <> "" THEN
                            DO i = 1 TO v-tmp-lines:
                           
                               IF v-printline > 50 THEN DO:
                                  RUN pPrintImageOnBack.
                                  PAGE.
                                  v-printline = 0.
                                  {ar/rep/invhenry.i}
                               END.
                           
                               PUT SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                               v-printline = v-printline + 1.
                            END.
                     
                     END.
                 
                     RELEASE job.
                  END.
                 
                  RELEASE job-hdr.
               END.
            END.
            
            IF v-printline > 50 THEN DO:
               RUN pPrintImageOnBack.
               PAGE.
               v-printline = 0.
               {ar/rep/invhenry.i}
            END.

        END. /* each ar-invl */

        lv-line-chars = 80.

        IF v-prntinst THEN DO:
           {custom/notesprtA.i ar-inv v-inst 4}
           DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN DO:                
                   IF v-printline > 50 THEN DO:
                      RUN pPrintImageOnBack.
                      PAGE.
                      v-printline = 0.
                      {ar/rep/invhenry.i}
                   END.
                   PUT v-inst[i] SKIP.
                   v-printline = v-printline + 1.
                END.            
           END.
          
           DO i = 1 TO 4:
              IF ar-inv.bill-i[i] <> "" THEN DO:
                 IF v-printline > 50 THEN DO:
                    RUN pPrintImageOnBack.
                    PAGE.
                    v-printline = 0.
                    {ar/rep/invhenry.i}
                 END.
                 PUT ar-inv.bill-i[i] SKIP.
                 v-printline = v-printline + 1.
              END.
           END.
        END.

        IF v-printline > 50 THEN DO:
           RUN pPrintImageOnBack.
           PAGE.
           v-printline = 0.
           {ar/rep/invhenry.i}
        END.

        v-frt-tax = ar-inv.freight.        
        IF ar-inv.tax-code <> "" AND
           (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
           AND ar-inv.freight <> 0
           AND AVAIL stax THEN
        DO i = 1 TO 5:

           IF stax.tax-code1[i] NE "" AND stax.tax-frt1[i] EQ YES THEN DO:
                CREATE w-tax.
                ASSIGN
                 w-dsc      = stax.tax-dscr1[i]
                 w-tax      = ROUND((IF stax.company EQ "yes" THEN v-frt-tax
                                                         ELSE ar-inv.freight) *
                                        stax.tax-rate1[i] / 100,2)                 
                 v-frt-tax  = v-frt-tax + w-tax
                 v-t-tax[i] = v-t-tax[i] + w-tax
                 v-lines    = v-lines + 1.
           END.
        END. 

    IF ar-inv.tax-amt EQ 0 THEN v-t-tax = 0.

    ELSE DO:
      v-tot-tax = 0.
      DO i = 1 TO 5:
        v-tot-tax = v-tot-tax + v-t-tax[i].
      END.
      IF v-tot-tax EQ 0 THEN
        ASSIGN
         v-t-tax    = 0
         v-t-tax[1] = ar-inv.tax-amt.

      ELSE DO:
        IF v-tot-tax NE ar-inv.tax-amt THEN
        DO i = 1 TO 5:
          v-t-tax[i] = ROUND(v-t-tax[i] * (ar-inv.tax-amt / v-tot-tax),2).
        END.
        v-tot-tax = 0.
        DO i = 1 TO 5:
          v-tot-tax = v-tot-tax + v-t-tax[i].
        END.
        IF v-tot-tax NE ar-inv.tax-amt THEN
          v-t-tax[1] = v-t-tax[1] + (ar-inv.tax-amt - v-tot-tax).
      END.
    END.
    
    IF cust.fax-country NE "USA" AND cust.fax-country NE "" THEN
    DO:
        DO i = 1 TO 3:
         IF i EQ 1 THEN 
         cCurrency = "EUR" .
         ELSE IF i EQ 2 THEN
         cCurrency = "GBP".
         RUN spCommon_CurrencyExcRate( INPUT cust.company,
                                       INPUT cCurrency,
                                       INPUT "USD",
                                       INPUT ar-inv.inv-date,
                                       OUTPUT dExchangeRate[i]).   
        END.     
    END.

    DO i = 1 TO 5:
       v-bot-lab[i] = IF v-t-tax[i] NE 0 THEN                      
                        ((IF AVAIL stax THEN STRING(CAPS(stax.tax-code1[i] + " TAX"),"x(7)") 
                           ELSE FILL(" ",7) ) +
                       fill(" ",4) + ":" + ( IF cust.fax-country EQ "USA" OR cust.fax-country EQ ""  THEN
                       string(v-t-tax[i],"->>>>,>>9.99") ELSE 
                       STRING(fill(" ",2) + string(v-t-tax[i] * dExchangeRate[1],"->>>>,>>9.99") + fill(" ",2) + string(v-t-tax[i] * dExchangeRate[2] ,"->>>>,>>9.99") 
                       + fill(" ",2) + string(v-t-tax[i],"->>>>,>>9.99")))) ELSE "".
    END.
    v-inv-freight = IF (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
                    THEN ar-inv.freight ELSE 0.    
                    /*ar-inv.t-inv-freight*/.

    /* #101624 */
    IF ltb_print-message THEN DO:
        PUT "<p8><R58><C2>" cInvMessage[1] FORMAT "x(60)" SKIP
                "<R59><C2>" cInvMessage[2] FORMAT "x(60)" SKIP
                "<R60><C2>" cInvMessage[3] FORMAT "x(60)" SKIP
                "<R61><C2>" cInvMessage[4] FORMAT "x(60)" SKIP
                "<R62><C2>" cInvMessage[5] FORMAT "x(60)" SKIP
                "<p10>".
    END.
    dTotalAmount = v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-t-tax[4] + v-t-tax[5] + v-inv-freight .
    
    
IF v-bot-lab[4] <> "" THEN do:
    IF cust.fax-country EQ "USA" OR cust.fax-country EQ "" THEN
    PUT "<R58><C59><#8><FROM><R+8><C+21><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "$->>>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight FORM "->>>>,>>9.99"
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2]
        "<=8><R+4> " v-bot-lab[3]
        "<=8><R+5> " v-bot-lab[4]
        "<=8><R+6> " v-bot-lab[5]
        "<=8><R+7> Grand Total:" dTotalAmount FORM "$->>,>>9.99" .
     ELSE
     PUT "<R56><p9><C42><#8><FROM><R+10><C+41.4><RECT> " 
            "<=8> Sales      :          EUR           GBP           USD"
            "<=8><R+1> Conversion :" dExchangeRate[1] FORMAT "->>,>>9.99" " USD" dExchangeRate[2] FORMAT "->>,>>9.99" " USD" "      1.00 USD"
            "<=8><R+2> Sub Total  :" (v-subtot-lines * dExchangeRate[1]) FORM "�->,>>>,>>9.99"  (v-subtot-lines * dExchangeRate[2]) FORM "�->,>>>,>>9.99" v-subtot-lines FORM "$->,>>>,>>9.99"
            "<=8><R+3> Freight    :" (v-inv-freight * dExchangeRate[1])  FORMAT "->>,>>>,>>9.99" (v-inv-freight * dExchangeRate[2])  FORMAT "->>,>>>,>>9.99" v-inv-freight FORMAT "->>,>>>,>>9.99"
            "<=8><R+4> " v-bot-lab[1] 
            "<=8><R+5> " v-bot-lab[2] 
            "<=8><R+6> " v-bot-lab[3] 
            "<=8><R+7> " v-bot-lab[4] 
            "<=8><R+8> " v-bot-lab[5] 
            "<=8><R+9> Grand Total:" (dTotalAmount * dExchangeRate[1]) FORM "�->,>>>,>>9.99" (dTotalAmount * dExchangeRate[2]) FORM "�->,>>>,>>9.99" dTotalAmount FORM "$->>>,>>9.99" "<P10>" .
     
END.        
ELSE do:
    IF cust.fax-country EQ "USA" OR cust.fax-country EQ "" THEN
    PUT "<R58><C59><#8><FROM><R+6><C+21><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "$->>>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight FORM "->>>>,>>9.99"
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2]
        "<=8><R+4> " v-bot-lab[3]
        "<=8><R+5> Grand Total:" dTotalAmount FORM "$->>>,>>9.99" .
    ELSE
    PUT "<R56><p9><C42><#8><FROM><R+8><C+41.4><RECT> " 
        "<=8> Sales      :           EUR           GBP           USD"
        "<=8><R+1> Conversion :" dExchangeRate[1] FORMAT "->>,>>9.99" " USD" dExchangeRate[2] FORMAT "->>,>>9.99" " USD" "      1.00 USD"
        "<=8><R+2> Sub Total  :" (v-subtot-lines * dExchangeRate[1]) FORM "�->,>>>,>>9.99"  (v-subtot-lines * dExchangeRate[2]) FORM "�->,>>>,>>9.99" v-subtot-lines FORM "$->,>>>,>>9.99"
        "<=8><R+3> Freight    :" (v-inv-freight * dExchangeRate[1])  FORMAT "->>,>>>,>>9.99" (v-inv-freight * dExchangeRate[2])  FORMAT "->>,>>>,>>9.99" v-inv-freight FORMAT "->>,>>>,>>9.99"
        "<=8><R+4> " v-bot-lab[1] 
        "<=8><R+5> " v-bot-lab[2] 
        "<=8><R+6> " v-bot-lab[3]
        "<=8><R+7> Grand Total:" (dTotalAmount * dExchangeRate[1]) FORM "�->,>>>,>>9.99" (dTotalAmount * dExchangeRate[2]) FORM "�->,>>>,>>9.99" dTotalAmount FORM "$->>>,>>9.99" "<P10>" .
    
END.
    ASSIGN
       v-printline = v-printline + 6
       v-page-num = PAGE-NUM.

    RUN pPrintImageOnBack.
    /*IF v-printline < 50 THEN PUT SKIP(60 - v-printline). */
    PAGE. 

    DO TRANSACTION:
       FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
       ASSIGN xar-inv.printed = YES.

    END. /* DO TRANSACTION avail ar-inv */ 
 
END. /* each report, ar-inv */

PROCEDURE pNotes:

DEFINE INPUT PARAMETER reckey LIKE cust.rec_key NO-UNDO.
DEFINE OUTPUT PARAMETER cNotes AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
    
ASSIGN 
       v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

FOR EACH notes WHERE notes.rec_key = reckey 
     AND notes.note_type = "G"
     AND notes.note_group = "BN" NO-LOCK:

    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent +*/ k.
    DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                 lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / lv-line-chars.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return +
               IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.

           IF k > 0 AND k <= 6 THEN cNotes[k] = cNotes[k] + 
                                  IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                                  THEN SUBSTRING(notes.note_text,i,1)
                                  ELSE "" .              
           
           IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
           THEN DO:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.
    END.
    ASSIGN v-prev-note-rec = RECID(notes)
           j = 0
           lv-got-return = 0.
    
    IF k > 6 THEN LEAVE.
END.

END PROCEDURE.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
