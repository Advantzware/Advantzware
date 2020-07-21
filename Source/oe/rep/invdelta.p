/* ---------------------------------------------- oe/rep/invdelta.p  */
/* PRINT INVOICE   delta             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{oe/rep/invoice.i}
{custom/notesdef.i}
DEFINE        VARIABLE v-inst         AS cha       FORM "x(80)" EXTENT 4 NO-UNDO.
DEFINE        VARIABLE cStockNotes    AS cha       FORM "x(80)" EXTENT 6 NO-UNDO.

DEFINE        VARIABLE v-salesman     AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE        VARIABLE v-fob          AS CHARACTER FORMAT "x(27)" NO-UNDO.
DEFINE        VARIABLE v-shipvia      LIKE carrier.dscr NO-UNDO.
DEFINE        VARIABLE v-addr3        AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-sold-addr3   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-name  AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE        VARIABLE v-shipto-addr  AS CHARACTER FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE        VARIABLE v-shipto-city  AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE        VARIABLE v-shipto-state AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE        VARIABLE v-shipto-zip   AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE        VARIABLE v-line         AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-printline    AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-invhead      AS CHARACTER FORMAT "x(13)" INITIAL
    "I N V O I C E".
DEFINE        VARIABLE v-pitch        LIKE asi.printer.pitch NO-UNDO.
DEFINE        VARIABLE v-len          AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-hldpitch     LIKE asi.printer.pitch NO-UNDO.
DEFINE        VARIABLE v-t-weight     LIKE inv-line.t-weight NO-UNDO.
DEFINE        VARIABLE v-tot-cas      AS DECIMAL   FORMAT "->>>9.9999" NO-UNDO.
DEFINE        VARIABLE v-tot-pallets  AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-tot-qty      AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-inv-date     AS DATE      INITIAL TODAY FORM "99/99/9999" NO-UNDO.
DEFINE SHARED VARIABLE v-fr-tax       AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE        VARIABLE v-tax-rate     AS DECIMAL   FORMAT "->>>.99" NO-UNDO.
DEFINE        VARIABLE v-tax-code     LIKE stax.tax-code NO-UNDO.
DEFINE        VARIABLE v-tx-rate      LIKE stax.tax-rate NO-UNDO.
DEFINE        VARIABLE v-ans          AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE        VARIABLE v-date-ship    AS DATE      INITIAL TODAY NO-UNDO.
DEFINE        VARIABLE v-del-no       AS INTEGER   FORMAT ">>>>>>" NO-UNDO.
DEFINE        VARIABLE v-bol-cases    LIKE oe-boll.cases NO-UNDO.
DEFINE        VARIABLE v-set-qty      AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-part-qty     AS DECIMAL   FORMAT "999.9999" NO-UNDO.
DEFINE        VARIABLE v-net          LIKE inv-head.t-inv-rev NO-UNDO.
DEFINE        VARIABLE v-case-cnt     AS CHARACTER FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE        VARIABLE v-case-line    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-part-line    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE tmp1           AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE tmp2           AS DATE      NO-UNDO.
DEFINE        VARIABLE net1           AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE net2           AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE net3           AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE cnt            AS INTEGER   NO-UNDO.
DEFINE        VARIABLE disp-frt       AS CHARACTER INIT "Freight:" FORMAT "x(8)" NO-UNDO.
DEFINE        VARIABLE minus-ship     AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-int          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE cBillNotes     LIKE inv-head.bill-i NO-UNDO.

DEFINE BUFFER bf-inv-head FOR inv-head.

DEFINE BUFFER xinv-head   FOR inv-head .
DEFINE BUFFER xinv-line   FOR inv-line .

DEFINE WORKFILE w-sman
    FIELD sman AS CHARACTER FORMAT "x(4)".

DEFINE VARIABLE v-ord-del-hdr  AS CHARACTER FORMAT "x(3)" INIT "Del".
DEFINE VARIABLE v-beeler-lines AS INTEGER.
DEFINE VARIABLE v-part-info    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v              AS INTEGER.
DEFINE VARIABLE v-bo-qty       AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qty      AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-ship-qty     AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-ord-qty      AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-i-no         AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-i-dscr       AS CHARACTER FORMAT "x(18)" NO-UNDO.
DEFINE VARIABLE v-price        AS DECIMAL   FORMAT ">>>>9.9999" NO-UNDO.
DEFINE VARIABLE v-t-price      AS DECIMAL   FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE VARIABLE v-po-no        LIKE inv-line.po-no NO-UNDO.
DEFINE VARIABLE v-bill-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEFINE VARIABLE v-ship-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-po-no    LIKE oe-ord.po-no NO-UNDO.
DEFINE VARIABLE v-price-head   AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL   NO-UNDO.
DEFINE WORKFILE w-tax
    FIELD w-dsc AS   CHARACTER
    FIELD w-tax AS   DECIMAL.
DEFINE VARIABLE v-t-tax       AS DECIMAL   EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-bot-lab     AS CHARACTER FORMAT "x(63)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-lines       AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEFINE VARIABLE v-frt-tax     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-inv-list   AS CHARACTER NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.

DEFINE VARIABLE v-tel           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact       AS cha       FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-display-comp AS LOG       NO-UNDO.
DEFINE VARIABLE lv-email        AS cha       FORM "x(48)" NO-UNDO.
DEFINE VARIABLE lv-comp-name    AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-comp-color   AS cha       NO-UNDO.
DEFINE VARIABLE lv-other-color  AS cha       INIT "BLACK" NO-UNDO.
DEFINE VARIABLE v-page-num      AS INTEGER   NO-UNDO.
DEFINE VARIABLE vRelPo          LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE iPoCheck        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPo-No          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.
DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1   AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound AND cRtnChar NE "" THEN 
DO:
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

    IF NOT lValid THEN 
    DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company EQ cocode NO-LOCK. 

IF lv-display-comp THEN 
DO:
    FIND FIRST cust WHERE cust.company = cocode AND
        cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN
        ASSIGN v-comp-add1  = cust.addr[1]
            v-comp-add2  = cust.addr[2]
            v-comp-add3  = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email     = "Email:  " + cust.email 
            lv-comp-name = cust.NAME   
            .
END.
    
FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST xinv-head WHERE RECID(xinv-head) EQ report.rec-id NO-LOCK
    BREAK BY report.key-01
    BY report.key-02:

    FIND FIRST cust WHERE cust.company = xinv-head.company
        AND cust.cust-no = xinv-head.cust-no NO-LOCK NO-ERROR.

    ASSIGN  
        v-shipto-name    = xinv-head.sold-name
        v-shipto-addr[1] = xinv-head.sold-addr[1]
        v-shipto-addr[2] = xinv-head.sold-addr[2]
        v-shipto-city    = xinv-head.sold-city
        v-shipto-state   = xinv-head.sold-state
        v-shipto-zip     = xinv-head.sold-zip.

    v-del-no = 0.

    FIND FIRST oe-bolh WHERE oe-bolh.company = xinv-head.company AND
        oe-bolh.bol-no = xinv-head.bol-no USE-INDEX bol-no NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN 
    DO:
        /*find first oe-relh where oe-relh.company = oe-bolh.company and
                   oe-relh.r-no = oe-bolh.r-no no-lock no-error.
        if avail oe-relh then */
        FIND FIRST shipto WHERE shipto.company  = oe-bolh.company AND
            shipto.cust-no = oe-bolh.cust-no AND
            shipto.ship-id = oe-bolh.ship-id NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN
            ASSIGN  v-shipto-name    = shipto.ship-name
                v-shipto-addr[1] = shipto.ship-addr[1]
                v-shipto-addr[2] = shipto.ship-addr[2]
                v-shipto-city    = shipto.ship-city
                v-shipto-state   = shipto.ship-state
                v-shipto-zip     = shipto.ship-zip.

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
        
        ASSIGN 
            cBillNotes[1] = ""
            cBillNotes[2] = ""
            cBillNotes[3] = ""
            cBillNotes[4] = "".
        IF xinv-head.multi-invoice THEN
            FOR EACH bf-inv-head 
                WHERE bf-inv-head.company EQ xinv-head.company
                AND bf-inv-head.bol-no EQ xinv-head.bol-no
                AND bf-inv-head.cust-no EQ xinv-head.cust-no
                AND NOT bf-inv-head.multi-invoice
                AND bf-inv-head.stat NE "H"
                NO-LOCK
                BREAK BY bf-inv-head.inv-date DESCENDING:
                ASSIGN 
                    cBillNotes[1] = bf-inv-head.bill-i[1]
                    cBillNotes[2] = bf-inv-head.bill-i[2]
                    cBillNotes[3] = bf-inv-head.bill-i[3]
                    cBillNotes[4] = bf-inv-head.bill-i[4]
                    .
                LEAVE.
            END.
        ELSE 
            ASSIGN
                cBillNotes[1] = xinv-head.bill-i[1]
                cBillNotes[2] = xinv-head.bill-i[2]
                cBillNotes[3] = xinv-head.bill-i[3]
                cBillNotes[4] = xinv-head.bill-i[4]
                .

        FIND FIRST carrier WHERE carrier.company = inv-head.company AND
            carrier.carrier = inv-head.carrier NO-LOCK NO-ERROR.
        IF AVAILABLE carrier THEN
            ASSIGN v-shipvia = carrier.dscr.
        ELSE
            ASSIGN v-shipvia = "".
        ASSIGN
            v-addr3      = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
            v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
            v-line       = 1
            v-printline  = 0.
    
        FIND FIRST stax WHERE stax.company = cocode AND
            stax.tax-group = inv-head.tax-gr NO-LOCK NO-ERROR.
        /*find first stax
            {sys/ref/stax1W.i}
              and {sys/ref/taxgroup.i stax} eq inv-head.tax-gr
            no-lock no-error. */
        IF NOT AVAILABLE stax THEN
            FIND FIRST stax WHERE stax.tax-group EQ inv-head.tax-gr
                NO-LOCK NO-ERROR.
   
        IF AVAILABLE stax THEN
            ASSIGN v-tax-rate    = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
                v-tax-code[1] = stax.tax-code[1]
                v-tax-code[2] = stax.tax-code[2]
                v-tax-code[3] = stax.tax-code[3]
                v-tx-rate[1]  = stax.tax-rate[1]
                v-tx-rate[2]  = stax.tax-rate[2]
                v-tx-rate[3]  = stax.tax-rate[3].

        ASSIGN 
            v-tot-pallets = 0.
        cPo-No = "".

        
        FOR EACH xinv-line NO-LOCK WHERE xinv-line.r-no = inv-head.r-no
            BREAK BY xinv-line.i-no:
        
            DO i = 1 TO 3:
                IF xinv-line.sman[i] NE "" THEN 
                DO:
                    CREATE w-sman.
                    ASSIGN 
                        w-sman.sman = xinv-line.sman[i].
                END.
            END.
            ASSIGN 
                v-tot-qty  = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (ROUND(xinv-line.t-weight /
                            xinv-line.qty, 2) * xinv-line.inv-qty).

            IF xinv-line.po-no NE "" THEN 
            DO:
                cPo-No = cPo-No + xinv-line.po-no + ",". 
            END.
         
            FOR EACH oe-bolh NO-LOCK WHERE oe-bolh.b-no = xinv-line.b-no:
                FOR EACH oe-boll NO-LOCK WHERE oe-boll.company = oe-bolh.company AND
                    oe-boll.b-no = oe-bolh.b-no AND
                    oe-boll.i-no = xinv-line.i-no AND
                    oe-boll.ord-no = xinv-line.ord-no:

                    /** Bill Of Lading TOTAL CASES **/
                    ASSIGN 
                        v-bol-cases = v-bol-cases + oe-boll.cases.
                    RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).
                    v-tot-pallets = v-tot-pallets + v-int.
                END. /* each oe-boll */
                ASSIGN 
                    v-date-ship = oe-bolh.bol-date.

            END. /* each oe-bolh */
            IF LAST-OF(xinv-line.i-no) THEN 
            DO:
                IF xinv-line.est-no NE "" THEN
                DO:
                    FIND FIRST eb WHERE eb.company = xinv-line.company AND
                        eb.est-no = xinv-line.est-no AND
                        eb.e-num = xinv-line.e-num AND
                        eb.form-no = xinv-line.form-no AND
                        eb.blank-no = xinv-line.blank-no NO-LOCK NO-ERROR.

                    IF xinv-line.form-no = 0 AND xinv-line.est-type = 2 THEN
                    DO:
                        FOR EACH fg-set NO-LOCK WHERE fg-set.company = xinv-line.company
                            AND fg-set.set-no = xinv-line.i-no:
                            ASSIGN 
                                v-set-qty = v-set-qty + fg-set.QtyPerSet.
                        END.
                        IF v-set-qty = 0 THEN
                            ASSIGN v-set-qty = 1.
                        FOR EACH eb NO-LOCK WHERE eb.company = xinv-line.company AND
                            eb.est-no = xinv-line.est-no AND
                            eb.e-num = xinv-line.e-num AND
                            eb.form-no NE 0:
                            FIND fg-set WHERE fg-set.company = xinv-line.company AND
                                fg-set.set-no = xinv-line.i-no  AND
                                fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                            IF AVAILABLE fg-set AND fg-set.QtyPerSet NE 0 THEN
                                ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
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
                        IF AVAILABLE eb THEN
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
                    v-tot-cas  = 0
                    v-tot-qty  = 0.
            END. /* last-of i-no */
        END. /* each xinv-line */

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

        FIND FIRST oe-bolh WHERE oe-bolh.company = inv-head.company AND
            oe-bolh.bol-no = inv-head.bol-no
            USE-INDEX bol-no NO-LOCK NO-ERROR.
        /*  if avail oe-bolh then
            assign v-rel-po-no = oe-bolh.po-no.*/

        FIND FIRST inv-line WHERE inv-line.r-no = inv-head.r-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE inv-line THEN
        DO:
            ASSIGN 
                v-price-head = inv-line.pr-uom.
            FIND FIRST oe-ord WHERE oe-ord.company = cocode AND
                oe-ord.ord-no = inv-line.ord-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE oe-ord THEN
            DO:
                ASSIGN 
                    v-bill-i   = oe-ord.bill-i[1]
                    v-ord-no   = oe-ord.ord-no
                    v-ord-date = oe-ord.ord-date
                    .
            END.
            ELSE
                ASSIGN v-price-head = inv-line.pr-uom.
        END.

        /* gdm - 01280914 */
        FIND FIRST inv-line
            WHERE inv-line.r-no  EQ inv-head.r-no
            AND inv-line.po-no NE ""
            NO-LOCK NO-ERROR.
        IF AVAILABLE inv-line THEN 
            ASSIGN v-ord-po-no = IF iPoCheck EQ YES THEN "See below" ELSE inv-line.po-no.
        /* ELSE DO:
            FIND FIRST inv-misc NO-LOCK 
                WHERE inv-misc.r-no EQ xinv-head.r-no NO-ERROR.
            IF AVAIL inv-misc 
              THEN ASSIGN v-po-no = inv-misc.po-no.
              ELSE ASSIGN v-po-no = "".

         END.*/

        /* display heder info 
         view frame invhead-comp.  /* Print headers */
                */
        {oe/rep/invdelta.i}

        v-subtot-lines = 0.
        v-t-tax = 0.
        FOR EACH inv-line NO-LOCK WHERE inv-line.r-no = inv-head.r-no:
            ASSIGN 
                v-case-line = ""
                v-part-line = ""
                v-case-cnt  = "".

            IF v-printline > 50 THEN 
            DO:
                PAGE.
                v-printline = 0.
                {oe/rep/invdelta.i}
            END.

            FOR EACH oe-boll
                WHERE oe-boll.company EQ inv-line.company
                AND oe-boll.ord-no  EQ inv-line.ord-no
                AND oe-boll.b-no    EQ inv-line.b-no
                AND oe-boll.i-no    EQ inv-line.i-no
                AND oe-boll.line    EQ inv-line.line
                AND oe-boll.po-no   EQ inv-line.po-no
                USE-INDEX bol-no NO-LOCK:

                /** Build Case Count Display Lines **/
                IF oe-boll.cases NE 0 AND oe-boll.qty-case NE 0 THEN
                    ASSIGN v-case-line = STRING(oe-boll.cases) + " @ " +
                                     string(oe-boll.qty-case).
                ELSE ASSIGN v-case-line = "".
                IF oe-boll.partial NE 0 THEN
                    ASSIGN v-part-line = "1" + " @ " + string(oe-boll.partial).
                ELSE ASSIGN v-part-line = "".

                DO i = 1 TO 5:
                    IF (80 - length(v-case-cnt[i])) > length(v-case-line) AND
                        v-case-line NE "" THEN
                        ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                            v-case-line   = "".
                    IF (80 - length(v-case-cnt[i])) > length(v-part-line) AND
                        v-part-line NE "" THEN
                        ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                            v-part-line   = "".
                END. /* 1 to 5 */
            END. /* each oe-boll */

            ASSIGN 
                v-line = v-line + 1
                .

            ASSIGN
                lv-inv-list = ""
                v-ship-qty  = IF inv-line.ord-no EQ 0 THEN inv-line.inv-qty
                                                   ELSE inv-line.ship-qty.
             

            FIND FIRST oe-ordl WHERE oe-ordl.company = cocode AND
                oe-ordl.ord-no = inv-line.ord-no AND
                oe-ordl.i-no = inv-line.i-no
                NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ordl THEN 
            DO:
                v-bo-qty = IF (inv-line.qty - v-ship-qty -
                    oe-ordl.t-ship-qty) < 0 then 0 else
                             (inv-line.qty - v-ship-qty -
                              oe-ordl.t-ship-qty).

                /*  ASSIGN vRelPo = "".
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
                    WHERE oe-boll.company EQ inv-line.company
                    AND oe-boll.b-no    EQ inv-line.b-no
                    AND oe-boll.po-no   EQ inv-line.po-no
                    AND oe-boll.ord-no  EQ oe-ordl.ord-no
                    AND oe-boll.i-no    EQ oe-ordl.i-no
                    AND oe-boll.line    EQ oe-ordl.line
                    AND oe-boll.s-code  EQ "I"
                    USE-INDEX b-no) THEN
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
                        lv-inv-list = lv-inv-list + TRIM(STRING(ar-invl.inv-no,">>>>>>>>>>")) + " ".
                    END.
            END.
            ELSE
                ASSIGN v-bo-qty = IF ( inv-line.qty - v-ship-qty ) < 0
                                  then 0 else inv-line.qty - v-ship-qty.

            v-beeler-lines = 0.
            DO v = 1 TO 3:
                v-part-info = IF v EQ 1 THEN inv-line.part-dscr1
                ELSE
                    IF v EQ 2 THEN inv-line.part-dscr2
                    ELSE           TRIM(lv-inv-list).

                IF v-part-info NE "" OR (v = 1 AND inv-line.part-no <> "") THEN
                    v-beeler-lines = v-beeler-lines + 1.
            END.
           
            /*v-printline = v-printline + v-beeler-lines.*/
 
            ASSIGN 
                v-inv-qty      = inv-line.qty /* task 03170618 inv-qty*/
                v-i-no         = inv-line.i-no
                v-i-dscr       = inv-line.i-name
                v-price        = inv-line.price * (1 - (inv-line.disc / 100))
                v-t-price      = inv-line.t-price
                v-subtot-lines = v-subtot-lines + inv-line.t-price
                v-ord-qty      = inv-line.qty .
                        

            IF inv-line.tax AND AVAILABLE stax THEN
            DO i = 1 TO 5:
                IF stax.tax-code1[i] NE "" THEN 
                DO:
                    CREATE w-tax.
                    ASSIGN
                        w-dsc      = stax.tax-dscr1[i]
                        w-tax      = ROUND((IF stax.company EQ "yes" THEN v-t-price
                                                              ELSE inv-line.t-price) *
                                    stax.tax-rate1[i] / 100,2)
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
                END.
            END.

            IF v-t-price NE inv-line.t-price THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc   = "******ITEM TOTAL:"
                    w-tax   = v-t-price
                    v-lines = v-lines + 1.
            END.
            
            v-price-head = inv-line.pr-uom.
  
          
            PUT SPACE(1) v-ship-qty FORMAT "->>>>>>9" SPACE(1)
                inv-line.inv-qty  FORMAT "->>>>>>9" SPACE(1)
                inv-line.ord-no FORMAT ">>>>>>9" SPACE(2)
                v-i-no  FORMAT "x(15)" SPACE(3)
                v-i-dscr  FORMAT "x(25)" SPACE(3)
                v-price  FORMAT "$->>>,>>9.99" /*"$->>,>>9.99<<"*/ SPACE(1)
                v-price-head 
                inv-line.t-price  FORMAT "$->>>>,>>9.99" /*"$->>>,>>9.99"*/                     
                SKIP.
          

            v-printline = v-printline + 1.
      

            DO v = 1 TO 3:
                v-part-info = IF v EQ 1 THEN inv-line.part-dscr1
                ELSE
                    IF v EQ 2 THEN inv-line.part-dscr2
                    ELSE           TRIM(lv-inv-list).
                IF v-part-info NE "" OR  (v = 1 AND inv-line.part-no <> "") THEN 
                DO:
                    IF v = 1 THEN 
                    DO:
                        IF LENGTH(inv-line.po-no) LE 8 THEN 
                        DO:
                            PUT  SPACE(19) inv-line.po-no FORMAT "x(8)" SPACE(1)   inv-line.part-no SPACE(3) v-part-info SKIP.
                        END.
                        ELSE 
                        DO: 
                            PUT  SPACE(10) inv-line.po-no FORMAT "x(15)" SPACE(3)   inv-line.part-no SPACE(3) v-part-info SKIP.
                        END.                   

                    END.
                    ELSE
                        IF v = 2 THEN  PUT /*SPACE(10)  v-po-no FORMAT "x(15)"*/ SPACE(47) v-part-info SKIP.
                        ELSE          PUT SPACE(26) "Previous Invoice(s): " v-part-info SKIP.
                    v-printline = v-printline + 1.
                END.
            END.
            PUT SKIP(1).
            v-printline = v-printline + 1.
       
            IF v-print-dept AND AVAILABLE oe-ordl THEN
            DO:
                FIND FIRST job-hdr WHERE
                    job-hdr.company EQ cocode AND
                    job-hdr.job-no  EQ oe-ordl.job-no AND
                    job-hdr.job-no2 EQ oe-ordl.job-no2
                    NO-LOCK NO-ERROR.

                IF AVAILABLE job-hdr THEN
                DO:
                    FIND FIRST job WHERE
                        job.company EQ cocode AND
                        job.job     EQ job-hdr.job AND
                        job.job-no  EQ job-hdr.job-no AND
                        job.job-no2 EQ job-hdr.job-no2
                        NO-LOCK NO-ERROR.
                 
                    IF AVAILABLE job THEN
                    DO:
                        FOR EACH notes WHERE
                            notes.rec_key EQ job.rec_key AND /*for capitol task 06180806*/
                            CAN-DO(v-depts,notes.note_code)
                            NO-LOCK
                            BY notes.note_code:
                        
                            v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                            {SYS/INC/ROUNDUP.I v-tmp-lines}
                    
                            DO i = 1 TO v-tmp-lines:
                                IF v-printline > 50 THEN 
                                DO:
                                    PAGE.
                                    v-printline = 0.
                                    {oe/rep/invdelta.i}
                                END.
                    
                                PUT SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                                v-printline = v-printline + 1.
                            END.
                        END.

                        /* custom/notesprt.i {1} = table name 
                        {2} = note variable name with extent 
                        {3} =  variable's extent */

                        RELEASE job.
                    END.

                    RELEASE job-hdr.
                END.
            END.

        END. /* each inv-line */

        FOR EACH inv-misc NO-LOCK WHERE inv-misc.company = inv-head.company AND
            inv-misc.r-no = inv-head.r-no AND
            inv-misc.bill = "Y" BREAK BY inv-misc.ord-no WITH FRAME detailm:
            IF FIRST(inv-misc.ord-no) THEN
            DO:
                IF v-printline > 50 THEN 
                DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/invdelta.i}
                END.
                PUT "** Miscellaneous Items **" AT 23 SKIP(1).
                ASSIGN 
                    v-printline = v-printline + 2.
            END.
            
            PUT inv-misc.charge AT 10 inv-misc.dscr inv-misc.amt FORMAT "$->>,>>9.99"  SKIP.
            ASSIGN
                v-subtot-lines = v-subtot-lines + inv-misc.amt
                v-printline    = v-printline + 1.
            IF inv-misc.tax AND AVAILABLE stax THEN
            DO i = 1 TO 5:
                IF stax.tax-code1[i] NE "" THEN 
                DO:
                    CREATE w-tax.
                    ASSIGN
                        w-dsc      = stax.tax-dscr1[i]
                        w-tax      = IF stax.company EQ "yes" THEN v-t-price
                            ELSE inv-misc.amt
                        w-tax      = ROUND(w-tax * (1 + (stax.tax-rate1[i] / 100)),2) - w-tax
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
                END.
            END.

            IF v-t-price NE inv-misc.amt THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc   = "******ITEM TOTAL:"
                    w-tax   = v-t-price
                    v-lines = v-lines + 1.
            END.
            IF v-printline > 53 THEN 
            DO:
                PAGE.
                v-printline = 0.
                {oe/rep/invdelta.i}
            END.

        END. /* each inv-misc */

        IF v-prntinst THEN 
        DO:

            {custom/notesprt.i inv-head v-inst 4}
            DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN 
                DO:                
                    IF v-printline > 50 THEN 
                    DO:
                        PAGE.
                        v-printline = 0.
                        {oe/rep/invdelta.i}
                    END.
                    PUT v-inst[i] SKIP.
                    v-printline = v-printline + 1.
                END.
            END.

            DO i = 1 TO 4:
                IF cBillNotes[i] <> "" THEN 
                DO:
                    IF v-printline > 50 THEN 
                    DO:
                        PAGE.
                        v-printline = 0.
                        {oe/rep/invdelta.i}
                    END.
                    PUT cBillNotes[i] SKIP.
                    v-printline = v-printline + 1.
                END.
            END.
        END.
        
        v-frt-tax = inv-head.t-inv-freight.
        
        IF inv-head.tax-gr <> "" AND
            inv-head.f-bill AND inv-head.t-inv-freight <> 0 AND AVAILABLE stax THEN
        DO i = 1 TO 5:

            IF stax.tax-code1[i] NE "" AND stax.tax-frt1[i] EQ YES THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr1[i]
                    w-tax      = ROUND((IF stax.company EQ "yes" THEN v-frt-tax
                                                         ELSE inv-head.t-inv-freight) *
                                        stax.tax-rate1[i] / 100,2)
                    v-frt-tax  = v-frt-tax + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
            END.
        END. 

        ASSIGN
            inv-head.printed = YES
            inv-head.stat    = "X".
    END. /* DO TRANSACTION avail inv-head */

    DO i = 1 TO 5:
        v-bot-lab[i] = IF v-t-tax[i] NE 0 THEN
            ((IF AVAILABLE stax THEN STRING(CAPS(stax.tax-code1[i] + " TAX"),"x(7)") 
            ELSE FILL(" ",7) ) +
            fill(" ",4) + ":" +
            string(v-t-tax[i],"->>,>>>,>>9.99")) ELSE "".
    END.
    v-inv-freight = IF inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
    FOR EACH bf-cust NO-LOCK
        WHERE bf-cust.company EQ cocode
        AND bf-cust.ACTIVE EQ "X":

        RUN pNotes(INPUT bf-cust.rec_key, OUTPUT cStockNotes).
        
        PUT "<p8><R56><C3>" cStockNotes[1] SKIP
            "<R57><C3>" cStockNotes[2] SKIP
            "<R58><C3>" cStockNotes[3] SKIP
            "<R59><C3>" cStockNotes[4] SKIP
            "<p10>".
        
    END.

    IF v-bot-lab[4] <> "" THEN
        PUT "<R56><C59><#8><FROM><R+8><C+23><RECT> " 
            "<=8> Sub Total  :" v-subtot-lines FORM "$->,>>>,>>9.99"
            "<=8><R+1> Freight    :" v-inv-freight FORMAT "->>,>>>,>>9.99"
            "<=8><R+2> " v-bot-lab[1] 
            "<=8><R+3> " v-bot-lab[2] 
            "<=8><R+4> " v-bot-lab[3] 
            "<=8><R+5> " v-bot-lab[4] 
            "<=8><R+6> " v-bot-lab[5] 
            "<=8><R+7> Grand Total:" inv-head.t-inv-rev FORM "$->,>>>,>>9.99" .
    ELSE
        PUT "<R56><C59><#8><FROM><R+6><C+23><RECT> " 
            "<=8> Sub Total  :" v-subtot-lines FORM "$->,>>>,>>9.99"
            "<=8><R+1> Freight    :" v-inv-freight FORMAT "->>,>>>,>>9.99"
            "<=8><R+2> " v-bot-lab[1] 
            "<=8><R+3> " v-bot-lab[2] 
            "<=8><R+4> " v-bot-lab[3] 
            "<=8><R+5> Grand Total:" inv-head.t-inv-rev FORM "$->,>>>,>>9.99" .

    ASSIGN
        v-printline = v-printline + 6
        v-page-num  = PAGE-NUMBER.
    PAGE.
 
END. /* each xinv-head */
    
PROCEDURE pNotes:

    DEFINE INPUT PARAMETER reckey LIKE cust.rec_key NO-UNDO.
    DEFINE OUTPUT PARAMETER cNotes AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
    
    ASSIGN 
        v-tmp-lines   = 0
        j             = 0
        K             = 0
        lv-got-return = 0.

    FOR EACH notes WHERE notes.rec_key = reckey 
        AND notes.note_type = "G"
        AND notes.note_group = "BN" NO-LOCK:

        IF v-prev-note-rec <> ? AND
            v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent +*/ k.
        DO i = 1 TO LENGTH(notes.note_text) :        
            IF i - j >= lv-line-chars THEN ASSIGN j             = i
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
                THEN 
            DO:
                lv-got-return = lv-got-return + 1.
                j = i.
            END.
        END.
        ASSIGN 
            v-prev-note-rec = RECID(notes)
            j               = 0
            lv-got-return   = 0.
    
        IF k > 6 THEN LEAVE.
    END.

END PROCEDURE.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
