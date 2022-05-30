/* ---------------------------------------------- oe/rep/invHarwell.p  */
/* PRINT INVOICE   Xprint Standard Form             */
/* -------------------------------------------------------------------------- */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */
{sys/inc/var.i shared}

{oe/rep/invoice.i}
{custom/notesdef.i}

DEF BUFFER bf-inv-head FOR inv-head.
DEF BUFFER xinv-head   FOR inv-head .
DEF BUFFER xinv-line   FOR inv-line .

DEF WORKFILE w-sman
    FIELD sman AS CHAR FORMAT "x(4)".
DEF WORKFILE w-tax
    FIELD w-dsc AS   CHAR
    FIELD w-tax AS   DEC.

DEF SHARED VAR v-fr-tax AS LOGICAL INITIAL NO NO-UNDO.

DEF VAR cAddr4 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEF VAR cBillNotes LIKE inv-head.bill-i NO-UNDO.
DEF VAR cMessage AS CHARACTER NO-UNDO.
DEF VAR cnt AS INT NO-UNDO.
DEF VAR cRtnChar AS CHARACTER NO-UNDO.
DEF VAR cShipAddr4 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEF VAR disp-frt AS CHAR INIT "Freight:" FORMAT "x(8)" NO-UNDO.
DEF VAR lRecFound AS LOGICAL NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR ls-full-img2 AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR ls-image1 AS CHAR NO-UNDO.
DEF VAR ls-image2 AS CHAR NO-UNDO.
DEF VAR lv-comp-color AS CHAR NO-UNDO.
DEF VAR lv-comp-name AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR lv-currency AS CHAR NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS CHAR FORMAT "x(48)" NO-UNDO.
DEF VAR lv-inv-list AS CHAR NO-UNDO.
DEF VAR lv-other-color AS CHAR INIT "BLACK" NO-UNDO.
DEF VAR lValid AS LOGICAL NO-UNDO.
DEF VAR minus-ship AS INT NO-UNDO.
DEF VAR net1 AS DEC NO-UNDO.
DEF VAR net2 AS DEC NO-UNDO.
DEF VAR net3 AS DEC NO-UNDO.
DEF VAR tmp1 AS DEC NO-UNDO.
DEF VAR tmp2 AS DATE NO-UNDO.
DEF VAR v AS INT.
DEF VAR v-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-ans AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR v-beeler-lines AS INT.
DEF VAR v-bill-i AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-bo-qty AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-bol-cases LIKE oe-boll.cases NO-UNDO.
DEF VAR v-bot-lab AS CHAR FORMAT "x(63)" EXTENT 5 NO-UNDO.
DEF VAR v-case-cnt AS CHAR FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR v-case-line AS CHAR NO-UNDO.
DEF VAR v-comp-add1 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact AS CHAR FORMAT "x(20)" NO-UNDO .
DEF VAR v-date-ship AS DATE INITIAL TODAY NO-UNDO.
DEF VAR v-del-no AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-fax AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fob AS CHAR FORMAT "x(27)" NO-UNDO.
DEF VAR v-frt-tax AS DEC NO-UNDO.
DEF VAR v-hldpitch LIKE asi.printer.pitch NO-UNDO.
DEF VAR v-i-dscr AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR v-i-no AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-inst AS CHAR FORMAT "x(80)" EXTENT 4 NO-UNDO.
DEF VAR v-int AS DEC NO-UNDO.
DEF VAR v-inv-date AS DATE INITIAL TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-inv-qty AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-invhead AS CHAR FORMAT "x(13)" INITIAL "I N V O I C E".
DEF VAR v-len AS INT NO-UNDO.
DEF VAR v-line AS INT NO-UNDO.
DEF VAR v-lines AS INT NO-UNDO.
DEF VAR v-net LIKE inv-head.t-inv-rev NO-UNDO.
DEF VAR v-ord-date LIKE oe-ord.ord-date NO-UNDO.
DEF VAR v-ord-del-hdr AS CHAR FORMAT "x(3)" INIT "Del".
DEF VAR v-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.
DEF VAR v-part-info AS CHAR FORMAT "x(30)".
DEF VAR v-part-line AS CHAR NO-UNDO.
DEF VAR v-part-qty AS DEC FORMAT "999.9999" NO-UNDO.
DEF VAR v-pitch LIKE asi.printer.pitch NO-UNDO.
DEF VAR v-po-no LIKE inv-line.po-no NO-UNDO.
DEF VAR v-price AS DEC FORMAT ">>>>9.9999" NO-UNDO.
DEF VAR v-price-head AS CHAR FORMAT "x(5)" NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-rel-po-no LIKE oe-rel.po-no NO-UNDO.
DEF VAR v-salesman AS CHAR FORMAT "x(14)" NO-UNDO.
DEF VAR v-set-qty AS DECIMAL NO-UNDO.
DEF VAR v-ship-i AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ship-qty AS INT FORMAT "99999" NO-UNDO.
DEF VAR v-shipto-addr AS CHAR FORMAT "x(45)" EXTENT 2 NO-UNDO.
DEF VAR v-shipto-city AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-shipto-name AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-state AS CHAR FORMAT "x(2)" NO-UNDO.
DEF VAR v-shipto-zip AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-shipvia LIKE carrier.dscr NO-UNDO.
DEF VAR v-sold-addr3 AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
DEF VAR v-t-price AS DEC FORMAT ">>>>>>9.99" NO-UNDO.
DEF VAR v-t-tax AS DEC EXTENT 5 NO-UNDO.
DEF VAR v-t-weight LIKE inv-line.t-weight NO-UNDO.
DEF VAR v-tax-code LIKE stax.tax-code NO-UNDO.
DEF VAR v-tax-rate AS DEC FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tel AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-tot-cas AS DEC FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tot-pallets AS INT NO-UNDO.
DEF VAR v-tot-qty AS INT NO-UNDO.
DEF VAR v-tot-sqft      AS DEC     FORMAT "->>>>>>>9.99".
DEF VAR v-tot-tax       AS DEC     NO-UNDO.
DEF VAR v-tot-wght      AS DEC     FORMAT "->>>>>>>9.99".
DEF VAR v-tx-rate LIKE stax.tax-rate NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.

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

    IF NOT lValid THEN 
    DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .

FIND FIRST sys-ctrl NO-LOCK WHERE 
    sys-ctrl.company EQ cocode AND 
    sys-ctrl.name    EQ "INVPRINT" 
    NO-ERROR.
IF AVAIL sys-ctrl 
AND sys-ctrl.log-fld THEN 
    lv-display-comp = YES.
ELSE 
    lv-display-comp = NO.

FIND FIRST sys-ctrl NO-LOCK WHERE 
    sys-ctrl.company EQ cocode AND 
    sys-ctrl.name    EQ "LOGOCOLR" 
    NO-ERROR.
IF AVAIL sys-ctrl THEN 
    lv-comp-color = sys-ctrl.char-fld.
ELSE 
    lv-comp-color = "BLACK".

FIND FIRST company NO-LOCK WHERE 
    company.company EQ cocode. 

IF lv-display-comp THEN DO:
    FIND FIRST cust NO-LOCK WHERE 
        cust.company = cocode AND
        cust.active = "X" 
        NO-ERROR.
    IF AVAIL cust THEN ASSIGN 
        v-comp-add1  = cust.addr[1]
        v-comp-add2  = cust.addr[2]
        v-comp-add3  = cust.city + ", " + cust.state + "  " + cust.zip
        v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
        v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
        lv-email     = "Email:  " + cust.email 
        lv-comp-name = cust.NAME   
        .
END.
    
FIND FIRST oe-ctrl NO-LOCK WHERE 
    oe-ctrl.company = cocode 
    NO-ERROR.

FOR EACH report NO-LOCK WHERE 
    report.term-id EQ v-term-id,
    FIRST xinv-head NO-LOCK WHERE 
        RECID(xinv-head) EQ report.rec-id
    BREAK BY report.key-01
    BY report.key-02:

    FIND FIRST cust NO-LOCK WHERE  
        cust.company = xinv-head.company AND 
        cust.cust-no = xinv-head.cust-no 
        NO-ERROR.

    ASSIGN  
        v-shipto-name    = xinv-head.sold-name
        v-shipto-addr[1] = xinv-head.sold-addr[1]
        v-shipto-addr[2] = xinv-head.sold-addr[2]
        v-shipto-city    = xinv-head.sold-city
        v-shipto-state   = xinv-head.sold-state
        v-shipto-zip     = xinv-head.sold-zip.

    FIND FIRST currency NO-LOCK WHERE 
        currency.company = cust.company AND 
        currency.c-code = cust.curr-code
        NO-ERROR.
    IF AVAIL currency THEN
        lv-currency = currency.c-desc.

    v-del-no = 0.
    cAddr4 = IF AVAIL cust THEN cust.contact ELSE "" .
    
    FIND FIRST oe-bolh NO-LOCK WHERE 
        oe-bolh.company = xinv-head.company AND
        oe-bolh.bol-no = xinv-head.bol-no 
        USE-INDEX bol-no 
        NO-ERROR.
    IF AVAIL oe-bolh THEN DO:
        FIND FIRST shipto NO-LOCK WHERE 
            shipto.company  = oe-bolh.company AND
            shipto.cust-no = oe-bolh.cust-no AND
            shipto.ship-id = oe-bolh.ship-id 
            NO-ERROR.
        IF AVAIL shipto THEN ASSIGN  
            v-shipto-name    = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city    = shipto.ship-city
            v-shipto-state   = shipto.ship-state
            v-shipto-zip     = shipto.ship-zip
            cShipAddr4       = shipto.contact.
    END. /* avail oe-bolh */

    IF NOT v-reprint 
    OR xinv-head.inv-no EQ 0 THEN
        RUN oe/get-inv#.p (ROWID(xinv-head)).

    DO TRANSACTION:
        FIND inv-head EXCLUSIVE WHERE 
            ROWID(inv-head) EQ ROWID(xinv-head).

        IF inv-head.inv-date NE ? THEN ASSIGN 
            v-inv-date = inv-head.inv-date.
        
        IF inv-head.fob-code BEGINS "ORIG" THEN ASSIGN 
            v-fob = "Origin".
        ELSE ASSIGN 
            v-fob = "Destination".
        
        ASSIGN 
            cBillNotes[1] = ""
            cBillNotes[2] = ""
            cBillNotes[3] = ""
            cBillNotes[4] = "".
        IF xinv-head.multi-invoice THEN
            FOR EACH bf-inv-head NO-LOCK WHERE 
                bf-inv-head.company EQ xinv-head.company AND 
                bf-inv-head.bol-no EQ xinv-head.bol-no AND 
                bf-inv-head.cust-no EQ xinv-head.cust-no AND 
                NOT bf-inv-head.multi-invoice AND 
                bf-inv-head.stat NE "H"
                BREAK BY bf-inv-head.inv-date DESC:
                ASSIGN 
                    cBillNotes[1] = bf-inv-head.bill-i[1]
                    cBillNotes[2] = bf-inv-head.bill-i[2]
                    cBillNotes[3] = bf-inv-head.bill-i[3]
                    cBillNotes[4] = bf-inv-head.bill-i[4]
                    .
                LEAVE.
            END.
        ELSE ASSIGN
            cBillNotes[1] = xinv-head.bill-i[1]
            cBillNotes[2] = xinv-head.bill-i[2]
            cBillNotes[3] = xinv-head.bill-i[3]
            cBillNotes[4] = xinv-head.bill-i[4]
            .

        FIND FIRST carrier NO-LOCK WHERE 
            carrier.company = inv-head.company AND
            carrier.carrier = inv-head.carrier 
            NO-ERROR.
        IF AVAIL carrier THEN ASSIGN 
            v-shipvia = carrier.dscr.
        ELSE ASSIGN 
            v-shipvia = "".
        
        ASSIGN
            v-addr3      = cust.city + ", " + cust.state + "  " + cust.zip
            v-sold-addr3 = v-shipto-city + ", " + v-shipto-state + "  " + v-shipto-zip
            v-line       = 1
            v-printline  = 0.
    
        FIND FIRST stax NO-LOCK WHERE 
            stax.company = cocode AND
            stax.tax-group = inv-head.tax-gr 
            NO-ERROR.
        IF NOT AVAIL stax THEN FIND FIRST stax NO-LOCK WHERE 
            stax.tax-group EQ inv-head.tax-gr
            NO-ERROR.
   
        IF AVAIL stax THEN ASSIGN 
            v-tax-rate    = stax.tax-rate[1] + stax.tax-rate[2] + stax.tax-rate[3]
            v-tax-code[1] = stax.tax-code[1]
            v-tax-code[2] = stax.tax-code[2]
            v-tax-code[3] = stax.tax-code[3]
            v-tx-rate[1]  = stax.tax-rate[1]
            v-tx-rate[2]  = stax.tax-rate[2]
            v-tx-rate[3]  = stax.tax-rate[3].

        ASSIGN 
            v-tot-pallets = 0.

        FOR EACH xinv-line NO-LOCK WHERE 
            xinv-line.r-no = inv-head.r-no
            BREAK BY xinv-line.i-no:
        
            DO i = 1 TO 3:
                IF xinv-line.sman[i] NE "" THEN DO:
                    CREATE w-sman.
                    ASSIGN 
                        w-sman.sman = xinv-line.sman[i].
                END.
            END.
            
            ASSIGN 
                v-tot-qty  = v-tot-qty + xinv-line.ship-qty
                v-t-weight = v-t-weight + (ROUND(xinv-line.t-weight / xinv-line.qty, 2) * xinv-line.inv-qty).
         
            FOR EACH oe-bolh NO-LOCK WHERE 
                oe-bolh.b-no = xinv-line.b-no:
                FOR EACH oe-boll NO-LOCK WHERE 
                    oe-boll.company = oe-bolh.company AND
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

            IF LAST-OF(xinv-line.i-no) THEN DO:
                IF xinv-line.est-no NE "" THEN DO:
                    FIND FIRST eb NO-LOCK WHERE 
                        eb.company = xinv-line.company AND
                        eb.est-no = xinv-line.est-no AND
                        eb.e-num = xinv-line.e-num AND
                        eb.form-no = xinv-line.form-no AND
                        eb.blank-no = xinv-line.blank-no 
                        NO-ERROR.

                    IF xinv-line.form-no = 0 
                    AND xinv-line.est-type = 2 THEN DO:
                        FOR EACH fg-set NO-LOCK WHERE 
                            fg-set.company = xinv-line.company AND 
                            fg-set.set-no = xinv-line.i-no:
                            ASSIGN 
                                v-set-qty = v-set-qty + fg-set.QtyPerSet.
                        END.
                        IF v-set-qty = 0 THEN ASSIGN 
                            v-set-qty = 1.
                        FOR EACH eb NO-LOCK WHERE 
                            eb.company = xinv-line.company AND
                            eb.est-no = xinv-line.est-no AND
                            eb.e-num = xinv-line.e-num AND
                            eb.form-no NE 0:
                            FIND fg-set NO-LOCK WHERE 
                                fg-set.company = xinv-line.company AND
                                fg-set.set-no = xinv-line.i-no  AND
                                fg-set.part-no = eb.stock-no 
                                NO-ERROR.

                            IF AVAIL fg-set 
                            AND fg-set.QtyPerSet NE 0 THEN ASSIGN 
                                v-part-qty = fg-set.QtyPerSet / v-set-qty.
                            ELSE ASSIGN 
                                v-part-qty = 1 / v-set-qty.

                            IF eb.cas-cnt EQ 0 THEN ASSIGN 
                                v-tot-cas = ROUND((v-t-weight * v-part-qty) / eb.cas-wt, 2).
                            ELSE ASSIGN 
                                v-tot-cas = ROUND((v-tot-qty * v-part-qty) / eb.cas-cnt, 2).
                            IF v-bol-cases NE 0 THEN ASSIGN 
                                v-tot-cas = v-bol-cases.
                        END. /* each eb */
                    END. /* do */
                    ELSE IF AVAIL eb THEN DO:
                        IF eb.cas-cnt = 0 THEN ASSIGN 
                            v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                        ELSE ASSIGN 
                            v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
                        IF v-bol-cases NE 0 THEN ASSIGN 
                            v-tot-cas = v-bol-cases.
                    END. /* do */
                END. /* est-no ne "" */
                ASSIGN
                    v-t-weight = 0
                    v-tot-cas  = 0
                    v-tot-qty  = 0.
            END. /* last-of i-no */
        END. /* each xinv-line */
    
        /** Build Salesman Id String **/
        v-salesman = "".
        FOR EACH w-sman BREAK BY w-sman.sman:
            IF FIRST-OF(w-sman.sman) THEN ASSIGN 
                v-salesman = v-salesman + w-sman.sman.
            DELETE w-sman.
        END.

        FIND FIRST oe-bolh NO-LOCK WHERE 
            oe-bolh.company = inv-head.company AND
            oe-bolh.bol-no = inv-head.bol-no
            USE-INDEX bol-no 
            NO-ERROR.
        IF AVAIL oe-bolh THEN ASSIGN 
            v-rel-po-no = oe-bolh.po-no.

        FIND FIRST inv-line NO-LOCK WHERE 
            inv-line.r-no = inv-head.r-no
            NO-ERROR.
        IF AVAIL inv-line THEN DO:
            ASSIGN 
                v-price-head = inv-line.pr-uom.
            FIND FIRST oe-ord NO-LOCK WHERE 
                oe-ord.company = cocode AND
                oe-ord.ord-no = inv-line.ord-no
                NO-ERROR.
            IF AVAIL oe-ord THEN ASSIGN 
                v-bill-i   = oe-ord.bill-i[1]
                v-ord-no   = oe-ord.ord-no
                v-ord-date = oe-ord.ord-date.
            ELSE ASSIGN 
                v-price-head = inv-line.pr-uom.
        END.

        FIND FIRST inv-line NO-LOCK WHERE 
            inv-line.r-no  EQ inv-head.r-no AND 
            inv-line.po-no NE ""
            NO-ERROR.
        IF AVAIL inv-line THEN ASSIGN 
            v-po-no = inv-line.po-no.
        ELSE DO:
            FIND FIRST inv-misc NO-LOCK WHERE 
                inv-misc.r-no EQ xinv-head.r-no 
                NO-ERROR.
            IF AVAIL inv-misc THEN ASSIGN 
                v-po-no = inv-misc.po-no.
            ELSE ASSIGN 
                v-po-no = "".
        END.

        {oe/rep/invHarwell.i}

        ASSIGN 
            v-subtot-lines = 0
            v-t-tax = 0.
        
        FOR EACH inv-line NO-LOCK WHERE 
            inv-line.r-no = inv-head.r-no:
            ASSIGN 
                v-case-line = ""
                v-part-line = ""
                v-case-cnt  = "".

            IF v-printline > 50 THEN DO:
                PAGE.
                v-printline = 0.
                {oe/rep/invHarwell.i}
            END.

            FOR EACH oe-boll NO-LOCK WHERE 
                oe-boll.company EQ inv-line.company AND 
                oe-boll.ord-no  EQ inv-line.ord-no AND 
                oe-boll.b-no    EQ inv-line.b-no AND 
                oe-boll.i-no    EQ inv-line.i-no AND 
                oe-boll.line    EQ inv-line.line AND 
                oe-boll.po-no   EQ inv-line.po-no
                USE-INDEX bol-no:

                /** Build Case Count Display Lines **/
                IF oe-boll.cases NE 0 
                AND oe-boll.qty-case NE 0 THEN ASSIGN 
                    v-case-line = STRING(oe-boll.cases) + " @ " + STRING(oe-boll.qty-case).
                ELSE ASSIGN 
                    v-case-line = "".
                
                IF oe-boll.partial NE 0 THEN ASSIGN 
                    v-part-line = "1" + " @ " + string(oe-boll.partial).
                ELSE ASSIGN 
                    v-part-line = "".

                DO i = 1 TO 5:
                    IF (80 - LENGTH(v-case-cnt[i])) GT LENGTH(v-case-line) 
                    AND v-case-line NE "" THEN ASSIGN 
                        v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                        v-case-line   = "".
                    IF (80 - LENGTH(v-case-cnt[i])) GT LENGTH(v-part-line) 
                    AND v-part-line NE "" THEN ASSIGN 
                        v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                        v-part-line   = "".
                END. /* 1 to 5 */
            END. /* each oe-boll */

            ASSIGN 
                v-line = v-line + 1
                lv-inv-list = ""
                v-ship-qty  = IF inv-line.ord-no EQ 0 THEN inv-line.inv-qty ELSE inv-line.ship-qty.

            FIND FIRST oe-ordl NO-LOCK WHERE 
                oe-ordl.company = cocode AND
                oe-ordl.ord-no = inv-line.ord-no AND
                oe-ordl.i-no = inv-line.i-no
                NO-ERROR.
            IF AVAIL oe-ordl THEN DO:
                ASSIGN 
                    v-bo-qty = IF (inv-line.qty - v-ship-qty - oe-ordl.t-ship-qty) LT 0 THEN 0 ELSE 
                                (inv-line.qty - v-ship-qty - oe-ordl.t-ship-qty).

                IF NOT CAN-FIND(FIRST oe-boll WHERE 
                    oe-boll.company EQ inv-line.company AND 
                    oe-boll.b-no    EQ inv-line.b-no AND 
                    oe-boll.po-no   EQ inv-line.po-no AND 
                    oe-boll.ord-no  EQ oe-ordl.ord-no AND 
                    oe-boll.i-no    EQ oe-ordl.i-no AND 
                    oe-boll.line    EQ oe-ordl.line AND 
                    oe-boll.s-code  EQ "I"
                    USE-INDEX b-no) THEN FOR EACH ar-invl NO-LOCK WHERE 
                        ar-invl.company  EQ oe-ordl.company AND 
                        ar-invl.ord-no   EQ oe-ordl.ord-no AND 
                        ar-invl.i-no     EQ oe-ordl.i-no AND 
                        CAN-FIND(FIRST oe-boll WHERE 
                            oe-boll.company EQ ar-invl.company AND 
                            oe-boll.b-no    EQ ar-invl.b-no AND 
                            oe-boll.po-no   EQ ar-invl.po-no AND 
                            oe-boll.ord-no  EQ oe-ordl.ord-no AND 
                            oe-boll.i-no    EQ oe-ordl.i-no AND 
                            oe-boll.line    EQ oe-ordl.line AND 
                            oe-boll.s-code  EQ "I"
                            USE-INDEX b-no):
                    ASSIGN 
                        lv-inv-list = lv-inv-list + TRIM(STRING(ar-invl.inv-no,">>>>>>>>>>")) + " ".
                END.
            END.
            ELSE ASSIGN 
                v-bo-qty = IF ( inv-line.qty - v-ship-qty ) LT 0 THEN 0 ELSE inv-line.qty - v-ship-qty.

            v-beeler-lines = 0.
            DO v = 1 TO 3:
                ASSIGN 
                    v-part-info = IF v EQ 1 THEN inv-line.part-dscr1 ELSE
                                  IF v EQ 2 THEN inv-line.part-dscr2 ELSE           
                                  TRIM(lv-inv-list).

                IF v-part-info NE "" 
                OR (v = 1 AND inv-line.part-no <> "") THEN ASSIGN 
                    v-beeler-lines = v-beeler-lines + 1.
            END.
            ASSIGN 
                v-printline = v-printline + v-beeler-lines.

            ASSIGN 
                v-inv-qty      = inv-line.qty 
                v-i-no         = inv-line.i-no
                v-i-dscr       = inv-line.i-name
                v-price        = inv-line.price * (1 - (inv-line.disc / 100))
                v-t-price      = inv-line.t-price
                v-subtot-lines = v-subtot-lines + inv-line.t-price.

            ASSIGN 
                v-price-head = inv-line.pr-uom.

            /* Display first line of line block */
            PUT UNFORMATTED SKIP 
                v-inv-qty FORMAT "->>>>>9"          TO 9
                TRIM(STRING(inv-line.i-no))         AT 12
                inv-line.po-no                      AT 46
                v-ship-qty  FORMAT "->>>>>9"        TO 75
                IF v-ship-qty LT v-inv-qty THEN "P" ELSE "C" FORMAT "x" AT 78
                v-price  FORMAT "->,>>9.99<<"       TO 91
                inv-line.t-price  FORMAT "->>>,>>9.99" TO 105                     
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.

        /* Additional descriptive lines of line block */
        IF inv-line.i-name NE "" THEN DO:
            PUT UNFORMATTED 
                inv-line.i-name FORMAT "x(22)"       AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.                
        IF inv-line.i-dscr NE "" THEN DO:
            PUT UNFORMATTED 
                inv-line.i-dscr FORMAT "x(22)"       AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.
        IF inv-line.part-dscr1 NE "" THEN DO:
            PUT UNFORMATTED 
                inv-line.part-dscr1 FORMAT "x(22)"   AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.
        IF inv-line.part-dscr2 NE "" THEN DO:
            PUT UNFORMATTED 
                inv-line.part-dscr2 FORMAT "x(22)"   AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.

        IF inv-line.tax 
        AND AVAIL stax THEN DO i = 1 TO 5:
            IF stax.tax-code1[i] NE "" THEN DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr1[i]
                    w-tax      = ROUND((IF stax.company EQ "yes" THEN v-t-price ELSE inv-line.t-price) * stax.tax-rate1[i] / 100,2)
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
                PUT UNFORMATTED 
                    w-dsc                       AT 30
                    w-tax FORMAT "->>>>>>9.99"   TO 105
                    SKIP.
                ASSIGN 
                    v-printline = v-printline + 1.
            END.
        END.

        DO:
            CREATE w-tax.
            ASSIGN
                w-dsc   = "******ITEM TOTAL:"
                w-tax   = v-t-price
                v-lines = v-lines + 1.
            PUT UNFORMATTED 
                w-dsc                           AT 30
                v-t-price FORMAT "->>>>>>9.99"  TO 105 
                SKIP(1).
            ASSIGN 
                v-printline = v-printline + 2.
        END.
      
        PUT SKIP.
        ASSIGN 
            v-printline = v-printline + 1.

/*            DO v = 1 TO 3:                                                      */
/*                v-part-info = IF v EQ 1 THEN inv-line.part-dscr1 ELSE           */
/*                              IF v EQ 2 THEN inv-line.part-dscr2 ELSE           */
/*                              TRIM(lv-inv-list).                                */
/*                                                                                */
/*                IF v-part-info NE ""                                            */
/*                OR (v = 1 AND inv-line.part-no <> "") THEN DO:                  */
/*                    IF v = 1 THEN                                               */
/*                        PUT SPACE(26) inv-line.part-no SPACE v-part-info SKIP.  */
/*                    ELSE IF v = 2 THEN PUT SPACE(42) v-part-info SKIP.          */
/*                    ELSE PUT SPACE(20) "Previous Invoice(s): " v-part-info SKIP.*/
/*                    v-printline = v-printline + 1.                              */
/*                END.                                                            */
/*            END.                                                                */
/*            PUT SKIP(1).                                                        */
/*            v-printline = v-printline + 1.                                      */
       
            IF v-print-dept 
            AND AVAIL oe-ordl THEN DO:
                FIND FIRST job-hdr NO-LOCK WHERE
                    job-hdr.company EQ cocode AND
                    job-hdr.job-no  EQ oe-ordl.job-no AND
                    job-hdr.job-no2 EQ oe-ordl.job-no2
                    NO-ERROR.

                IF AVAIL job-hdr THEN DO:
                    FIND FIRST job NO-LOCK WHERE
                        job.company EQ cocode AND
                        job.job     EQ job-hdr.job AND
                        job.job-no  EQ job-hdr.job-no AND
                        job.job-no2 EQ job-hdr.job-no2
                        NO-ERROR.
                 
                    IF AVAIL job THEN DO:
                        FOR EACH notes NO-LOCK WHERE
                            notes.rec_key EQ job.rec_key AND 
                            CAN-DO(v-depts,notes.note_code)
                            BY notes.note_code:
                        
                            v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                            {SYS/INC/ROUNDUP.I v-tmp-lines}
                    
                            DO i = 1 TO v-tmp-lines:
                                IF v-printline > 50 THEN DO:
                                    PAGE.
                                    v-printline = 0.
                                    {oe/rep/invHarwell.i}
                                END.
                                PUT SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORMAT "x(80)" SKIP.              
                                v-printline = v-printline + 1.
                            END.
                        END.

                        RELEASE job.
                    END.
                    RELEASE job-hdr.
                END.
            END.
        END. /* each inv-line */

        FOR EACH inv-misc NO-LOCK WHERE 
            inv-misc.company = inv-head.company AND
            inv-misc.r-no = inv-head.r-no AND
            inv-misc.bill = "Y" 
            BREAK BY inv-misc.ord-no WITH FRAME detailm:
            IF FIRST(inv-misc.ord-no) THEN DO:
                IF v-printline > 50 THEN DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/invHarwell.i}
                END.
            END.
            ASSIGN 
                v-t-price = inv-misc.amt.
                
            /* Display first line of line block */
            PUT UNFORMATTED SKIP 
                1 FORMAT "->>>>>9"                  TO 9
                inv-misc.charge                     AT 12
                TRIM(STRING(inv-misc.po-no,"x(15)"))AT 46
                1  FORMAT "->>>>>9"                 TO 75
                inv-misc.amt  FORMAT "->,>>9.99<<"  TO 91
                inv-misc.amt  FORMAT "->,>>9.99"    TO 105                     
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
            
            ASSIGN
                v-subtot-lines = v-subtot-lines + inv-misc.amt.
            
            IF inv-misc.tax 
            AND AVAIL stax THEN DO i = 1 TO 5:
                IF stax.tax-code1[i] NE "" THEN DO:
                    CREATE w-tax.
                    ASSIGN
                        w-dsc      = stax.tax-dscr1[i]
                        w-tax      = ROUND((IF stax.company EQ "yes" THEN v-t-price ELSE ar-invl.amt) * stax.tax-rate1[i] / 100,2)
                        v-t-price  = v-t-price + w-tax
                        v-t-tax[i] = v-t-tax[i] + w-tax
                        v-lines    = v-lines + 1.
                    PUT UNFORMATTED 
                        w-dsc                       AT 30
                        w-tax FORMAT "->>>>>>9.99"   TO 105
                        SKIP.
                    ASSIGN 
                        v-printline = v-printline + 1.
                END.
            END.

            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc   = "******ITEM TOTAL:"
                    w-tax   = v-t-price
                    v-lines = v-lines + 1.
                PUT UNFORMATTED 
                    w-dsc                           AT 30
                    v-t-price FORMAT "->>>>>>9.99"  TO 105 
                    SKIP(1).
                ASSIGN 
                    v-printline = v-printline + 2.
            END.
            IF v-printline > 53 THEN DO:
                PAGE.
                v-printline = 0.
                {oe/rep/invHarwell.i}
            END.
        END. /* each inv-misc */

        IF v-prntinst THEN DO:
            {custom/notesprt.i inv-head v-inst 4}

            DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN DO:                
                    IF v-printline > 50 THEN DO:
                        PAGE.
                        v-printline = 0.
                        {oe/rep/invHarwell.i}
                    END.
                    PUT v-inst[i] SKIP.
                    v-printline = v-printline + 1.
                END.
            END.

            DO i = 1 TO 4:
                IF cBillNotes[i] <> "" THEN DO:
                    IF v-printline > 50 THEN DO:
                        PAGE.
                        v-printline = 0.
                        {oe/rep/invHarwell.i}
                    END.
                    PUT cBillNotes[i] SKIP.
                    v-printline = v-printline + 1.
                END.
            END.
        END.
        
        v-frt-tax = inv-head.t-inv-freight.
        IF inv-head.tax-gr NE "" 
        AND inv-head.f-bill 
        AND inv-head.t-inv-freight <> 0 
        AND AVAIL stax THEN DO i = 1 TO 5:
            IF stax.tax-code1[i] NE "" 
            AND stax.tax-frt[i] EQ YES THEN DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr1[i]
                    w-tax      = ROUND((IF stax.company EQ "yes" THEN v-frt-tax ELSE inv-head.t-inv-freight) * stax.tax-rate1[i] / 100,2)                 
                    v-frt-tax  = v-frt-tax + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
            END.
        END.      

        DO i = 1 TO 5:
            ASSIGN 
                v-bot-lab[i] = IF v-t-tax[i] NE 0 THEN ((IF AVAIL stax THEN STRING(CAPS(stax.tax-code1[i]),"x(5)") ELSE 
                               FILL(" ",5) ) + FILL(" ",7) + ":" + STRING(v-t-tax[i],"->>>>>9.99")) ELSE 
                               "".
        END.
        
        ASSIGN 
            v-inv-freight = IF inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0.
        
        /* Bottom section */
        /* First footer block - box */
        PUT 
            "<R58><C1><#4><FROM><R61.5><C80><RECT><||3>"  /* Rectangle */
            "<R58><C29><FROM><R61.5><C29><LINE><||3>"     /* Vert Line */
            "<R58><C41><FROM><R61.58><C41><LINE><||3>"    /* Vert Line */
            "<R58><C53><FROM><R61.5><C53><LINE><||3>"     /* Vert Line */
            .
        /* First footer block - labels and data */
        PUT 
            "<FArial><P12>"
            "<R58.5><C1.5>T.P.S. / G.S.T. R102293933RT"
            "<R60><C1.5>T.V.Q. / P.S.T. 1001259578TV001"
            "<FArial><P5>"
            "<R58.2><C29.2>SUPERF. TOTALE Pt2"
            "<R58.7><C29.2>TOTAL SQUARE FOOTAGE"
            "<R58.2><C41.2>POIDS TOTAL"
            "<R58.7><C41.2>TOTAL WEIGHT"
            "<FArial><P8>"
            "<R59><C55>PAYEZ CE MONTANT"
            "<R60><C53.5>PLEASE PAY THIS AMOUNT"
            "<FCourierNew><P10>"
            "<R60><C29.2>" v-tot-sqft 
            "<R60><C41.2>" v-tot-wght
            "<R59.1><C71.2><B>" v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight FORMAT "->>,>>9.99"
            "</B><FArial><P8>"
            "<R60.5><C71.2><FGCOLOR=RED>" lv-currency FORMAT "x(16)"
            "<FGCOLOR=BLACK>" 
            .
        /* Bottom margin text */
        PUT      
            "<FArial><P6>"
            "<R62><C1>TOUTE MATRICE EN ACIER ET MATRICE D'IMPRESSION NON-UTILISEE DEPUIS PLUS DE"
            "<R62.6><C1>DEUX (2) ANS SERA DETRUITE SANS PRE-AVIS"
            "<R62><C42>ANY STEEL DIE OR PRINT PLATE NOT USED FOR MORE THAN TO (2) YEARS WILL BE"
            "<R62.6><C42>DESTROYED WITHOUT PRIOR NOTICE"
            .
        
        ASSIGN
            v-printline = v-printline + 7
            v-page-num  = PAGE-NUM.
    
        PAGE. 

        ASSIGN
            inv-head.printed = YES
            inv-head.stat    = "X".
    END. /* DO TRANSACTION avail inv-head */
END. /* each xinv-head */

