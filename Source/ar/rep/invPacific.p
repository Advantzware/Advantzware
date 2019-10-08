/* ---------------------------------------------- ar/rep/invxprnt.p   */
/* PRINT INVOICE   Xprint form for Pacific PKG             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{ar/rep/invoice.i}
{custom/notesdef.i}

DEFINE BUFFER xar-inv FOR ar-inv.
DEFINE BUFFER xar-invl FOR ar-invl.

DEFINE TEMP-TABLE ttar-invl LIKE ar-invl.
DEFINE TEMP-TABLE w-sman
    FIELD sman AS CHARACTER FORMAT "x(4)".
DEFINE TEMP-TABLE w-tax
    FIELD w-dsc AS   CHARACTER
    FIELD w-tax AS   DECIMAL.

DEFINE SHARED VARIABLE v-fr-tax AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE cnt AS INTEGER NO-UNDO.
DEFINE VARIABLE disp-frt AS CHARACTER INIT "Freight:" FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE lv-bol-no LIKE oe-bolh.bol-no NO-UNDO.
DEFINE VARIABLE lv-comp-color AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-comp-name AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-display-comp AS LOG NO-UNDO.
DEFINE VARIABLE lv-email AS CHARACTER FORM "x(48)" NO-UNDO.
DEFINE VARIABLE lv-inv-list AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-other-color AS CHARACTER INIT "BLACK" NO-UNDO.
DEFINE VARIABLE minus-ship AS INTEGER NO-UNDO.
DEFINE VARIABLE net1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE net2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE net3 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tmp1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE tmp2 AS DATE NO-UNDO.
DEFINE VARIABLE v AS INTEGER.
DEFINE VARIABLE v-addr3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-ans AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE v-beeler-lines AS INTEGER.
DEFINE VARIABLE v-bill-i AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-bo-qty AS INTEGER FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-bol-cases LIKE oe-boll.cases NO-UNDO.
DEFINE VARIABLE v-bot-lab AS CHARACTER FORMAT "x(63)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-case-cnt AS CHARACTER FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-case-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-comp-add1 AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2 AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3 AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4 AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5 AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact AS CHARACTER FORM "x(20)" NO-UNDO .
DEFINE VARIABLE v-date-ship AS DATE INITIAL TODAY NO-UNDO.
DEFINE VARIABLE v-del-no AS INTEGER FORMAT ">>>>>>" NO-UNDO.
DEFINE VARIABLE v-fax AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fob AS CHARACTER FORMAT "x(27)" NO-UNDO.
DEFINE VARIABLE v-frt-tax AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-i-dscr AS CHARACTER FORMAT "x(18)" NO-UNDO.
DEFINE VARIABLE v-i-no AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-inst AS CHARACTER FORM "x(80)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-int AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-inv-date AS DATE INITIAL TODAY FORM "99/99/9999" NO-UNDO.
DEFINE VARIABLE v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEFINE VARIABLE v-inv-no AS INTEGER NO-UNDO.
DEFINE VARIABLE v-inv-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-line AS INTEGER NO-UNDO.
DEFINE VARIABLE v-lines AS INTEGER NO-UNDO.
DEFINE VARIABLE v-net LIKE inv-head.t-inv-rev NO-UNDO.
DEFINE VARIABLE v-ord-date LIKE oe-ord.ord-date NO-UNDO.
DEFINE VARIABLE v-ord-del-hdr AS CHARACTER FORMAT "x(3)" INIT "Del".
DEFINE VARIABLE v-ord-no LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE v-page-num AS INTEGER NO-UNDO.
DEFINE VARIABLE v-part-info AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v-part-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-part-qty AS DECIMAL FORMAT "999.9999" NO-UNDO.
DEFINE VARIABLE v-po-no LIKE ar-invl.po-no NO-UNDO.
DEFINE VARIABLE v-price AS DECIMAL FORMAT ">>>>9.9999" NO-UNDO.
DEFINE VARIABLE v-price-head AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-printline AS INTEGER NO-UNDO.
DEFINE VARIABLE v-rel-po-no LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE v-salesman AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE VARIABLE v-set-qty AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-ship-i AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ship-qty AS INTEGER FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-shipto-addr AS CHARACTER FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-shipto-city AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-shipto-name AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-shipto-state AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE VARIABLE v-shipto-zip AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-shipvia LIKE carrier.dscr NO-UNDO.
DEFINE VARIABLE v-sold-addr3 AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-t-price AS DECIMAL FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE VARIABLE v-t-tax AS DECIMAL EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-t-weight LIKE ar-invl.t-weight NO-UNDO.
DEFINE VARIABLE v-tax-code LIKE stax.tax-code NO-UNDO.
DEFINE VARIABLE v-tax-rate AS DECIMAL FORMAT "->>>.99" NO-UNDO.
DEFINE VARIABLE v-tel AS CHARACTER FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-tot-cas AS DECIMAL FORMAT "->>>9.9999" NO-UNDO.
DEFINE VARIABLE v-tot-pallets AS INTEGER NO-UNDO.
DEFINE VARIABLE v-tot-qty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-tot-tax AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-tx-rate LIKE stax.tax-rate NO-UNDO.

FIND FIRST sys-ctrl WHERE 
    sys-ctrl.company EQ cocode AND 
    sys-ctrl.name    EQ "INVPRINT" 
    NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl 
AND sys-ctrl.log-fld THEN ASSIGN 
    lv-display-comp = YES.
ELSE ASSIGN 
    lv-display-comp = NO.

FIND FIRST sys-ctrl WHERE 
    sys-ctrl.company EQ cocode AND 
    sys-ctrl.name    EQ "LOGOCOLR" 
    NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN ASSIGN 
    lv-comp-color = sys-ctrl.char-fld.
ELSE ASSIGN 
    lv-comp-color = "BLACK".
   
FIND FIRST company WHERE 
    company.company = cocode 
    NO-LOCK NO-ERROR.

ASSIGN 
    v-comp-add1 = ""
    v-comp-add2 = "" 
    v-comp-add3 = ""
    v-comp-add4 = ""
    v-comp-add5 = "".

IF lv-display-comp THEN DO:
    FIND FIRST cust WHERE 
        cust.company = cocode AND
        cust.active = "X" 
        NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN ASSIGN 
        v-comp-add1 = cust.addr[1]
        v-comp-add2 = cust.addr[2]
        v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
        v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + STRING(cust.phone,"999-9999") 
        v-comp-add5 = "Fax     :  " + STRING(cust.fax,"(999)999-9999") 
        lv-email    = "Email:  " + cust.email 
        lv-comp-name = cust.NAME   
        .
END.
    

FIND FIRST oe-ctrl WHERE 
    oe-ctrl.company = cocode 
    NO-LOCK NO-ERROR.

FOR EACH report WHERE 
    report.term-id EQ v-term-id NO-LOCK,
    FIRST ar-inv WHERE 
        RECID(ar-inv) EQ report.rec-id NO-LOCK,
    FIRST cust WHERE 
        cust.company = ar-inv.company AND 
        cust.cust-no = ar-inv.cust-no NO-LOCK 
    BREAK BY (IF v-print-fmt EQ "ASIXprnt" THEN "" ELSE ar-inv.cust-no)
    BY ar-inv.inv-no:
     
    FIND FIRST carrier WHERE 
        carrier.company EQ cocode AND 
        carrier.carrier EQ ar-inv.carrier 
        NO-LOCK NO-ERROR.
    ASSIGN
        v-shipvia = IF AVAILABLE carrier THEN carrier.dscr ELSE "".  

    FIND FIRST shipto WHERE 
        shipto.company EQ cocode AND 
        shipto.cust-no EQ ar-inv.cust-no AND 
        shipto.ship-id EQ ar-inv.ship-id 
        NO-LOCK NO-ERROR.

    IF AVAILABLE shipto THEN ASSIGN  
        v-shipto-name = shipto.ship-name
        v-shipto-addr[1] = shipto.ship-addr[1]
        v-shipto-addr[2] = shipto.ship-addr[2]
        v-shipto-city = shipto.ship-city
        v-shipto-state = shipto.ship-state
        v-shipto-zip = shipto.ship-zip
        v-addr3 = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
        .
    ASSIGN 
        v-sold-addr3 = v-shipto-city + ", " + v-shipto-state + "  " + v-shipto-zip 
        v-fob = IF ar-inv.fob-code BEGINS "ORIG" THEN "Origin" ELSE "Destination"
        v-line = 1
        v-printline = 0.
    
    FIND FIRST stax
        {sys/ref/stax1W.i} AND 
        {sys/ref/taxgroup.i stax} EQ ar-inv.tax-code
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE stax THEN FIND FIRST stax WHERE 
        stax.company = ar-inv.company AND 
        stax.tax-group EQ ar-inv.tax-code
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE stax THEN FIND FIRST stax WHERE 
        stax.tax-group EQ ar-inv.tax-code 
        NO-LOCK NO-ERROR.

    IF AVAILABLE stax THEN ASSIGN 
        v-tax-rate = stax.tax-rate[1] + stax.tax-rate[2] + stax.tax-rate[3]
        v-tax-code[1] = stax.tax-code[1]
        v-tax-code[2] = stax.tax-code[2]
        v-tax-code[3] = stax.tax-code[3]
        v-tx-rate[1]  = stax.tax-rate[1]
        v-tx-rate[2]  = stax.tax-rate[2]
        v-tx-rate[3]  = stax.tax-rate[3]
        .

    ASSIGN 
        v-tot-pallets = 0
        v-date-ship   = ar-inv.inv-date.
        
    FOR EACH ar-invl NO-LOCK WHERE 
        ar-invl.x-no  EQ ar-inv.x-no AND 
        (ar-invl.misc EQ NO OR ar-invl.billable)
        BREAK BY ar-invl.i-no:
        
        DO i = 1 TO 3:
            IF ar-invl.sman[i] NE "" THEN DO:
                CREATE w-sman.
                ASSIGN 
                    w-sman.sman = ar-invl.sman[i].
            END.
        END.
        ASSIGN 
            v-tot-qty = v-tot-qty + ar-invl.ship-qty
            v-t-weight = v-t-weight + (ROUND(ar-invl.t-weight / ar-invl.qty, 2) * ar-invl.inv-qty).
        
        FOR EACH oe-bolh NO-LOCK WHERE 
            oe-bolh.b-no = ar-invl.b-no AND
            oe-bolh.ord-no = ar-invl.ord-no:
            FOR EACH oe-boll NO-LOCK WHERE 
                oe-boll.company = oe-bolh.company AND
                oe-boll.b-no = oe-bolh.b-no AND
                oe-boll.i-no = ar-invl.i-no:

                ASSIGN 
                    v-bol-cases = v-bol-cases + oe-boll.cases.
                RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).
                ASSIGN 
                    v-tot-pallets = v-tot-pallets + v-int.
            END. /* each oe-boll */
            ASSIGN 
                v-date-ship = oe-bolh.bol-date.
        END. /* each oe-bolh */
         
        FIND FIRST oe-bolh WHERE 
            oe-bolh.b-no = ar-invl.b-no 
            NO-LOCK NO-ERROR.
        IF AVAILABLE oe-bolh THEN ASSIGN 
            v-date-ship = oe-bolh.bol-date.

        IF LAST-OF(ar-invl.i-no) THEN DO:
            IF ar-invl.est-no NE "" THEN DO:
                FIND FIRST eb WHERE 
                    eb.company = ar-invl.company AND
                    eb.est-no = ar-invl.est-no AND
                    eb.form-no = ar-invl.form-no AND
                    eb.blank-no = ar-invl.blank-no 
                    NO-LOCK NO-ERROR.

                IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 THEN DO:
                    FOR EACH fg-set NO-LOCK WHERE 
                        fg-set.company = ar-invl.company AND 
                        fg-set.set-no = ar-invl.i-no:
                        ASSIGN 
                            v-set-qty = v-set-qty + fg-set.qtyPerSet.
                    END.
                    IF v-set-qty = 0 THEN ASSIGN 
                        v-set-qty = 1.
                    
                    FOR EACH eb NO-LOCK WHERE 
                        eb.company = ar-invl.company AND
                        eb.est-no = ar-invl.est-no AND
                        eb.form-no NE 0:
                        FIND fg-set WHERE 
                            fg-set.company = ar-invl.company AND
                            fg-set.set-no = ar-invl.i-no  AND
                            fg-set.part-no = eb.stock-no 
                            NO-LOCK NO-ERROR.
                        ASSIGN 
                            v-part-qty = IF AVAILABLE fg-set AND fg-set.qtyPerSet NE 0 THEN fg-set.qtyPerSet / v-set-qty ELSE 1 / v-set-qty
                            v-tot-cas = IF eb.cas-cnt = 0 THEN ROUND((v-t-weight * v-part-qty) / eb.cas-wt, 2) ELSE ROUND((v-tot-qty * v-part-qty) / eb.cas-cnt, 2)
                            v-tot-cas = IF v-bol-cases NE 0 THEN v-bol-cases ELSE v-tot-cas. 
                    END. /* each eb */
                END. /* do */
                ELSE IF AVAILABLE eb THEN ASSIGN 
                    v-tot-cas = IF eb.cas-cnt EQ 0 THEN ROUND(v-t-weight / eb.cas-wt, 2) ELSE ROUND(v-tot-qty / eb.cas-cnt, 2)
                    v-tot-cas = IF v-bol-cases NE 0 THEN v-bol-cases ELSE v-tot-cas.
            END. /* est-no ne "" */
            ASSIGN
                v-t-weight = 0
                v-tot-cas = 0
                v-tot-qty = 0.
        END. /* last-of i-no */
    END. /* each ar-invl */
    
    /** Build Salesman Id String **/
    ASSIGN 
        v-salesman = "".
    FOR EACH w-sman 
        BREAK BY w-sman.sman:
        IF FIRST-OF(w-sman.sman) THEN ASSIGN 
            v-salesman = v-salesman + w-sman.sman.
        DELETE w-sman.
    END.

    ASSIGN 
        v-po-no = ar-inv.po-no
        v-bill-i = ar-inv.bill-i[1]
        v-ord-no = ar-inv.ord-no
        v-ord-date = ar-inv.ord-date.

    FIND FIRST ar-invl WHERE 
        ar-invl.x-no  EQ ar-inv.x-no AND 
        (ar-invl.misc EQ NO OR ar-invl.billable)
        NO-LOCK NO-ERROR.
    IF AVAILABLE ar-invl THEN ASSIGN 
        v-price-head = ar-invl.pr-uom
        v-po-no = IF ar-invl.po-no <> "" THEN ar-invl.po-no ELSE ar-inv.po-no
        v-ord-no = ar-invl.ord-no
        lv-bol-no = ar-invl.bol-no.

    /* display header info */
    IF v-salesman = "" THEN ASSIGN 
        v-salesman = cust.sman.
    ASSIGN 
        v-inv-date = ar-inv.inv-date.
        
    {ar/rep/invxprnt.i}

    ASSIGN
        v-subtot-lines = 0
        v-t-tax = 0.

    /* This section consolidates invoice lines by bol#, po# and i-no */
    FOR EACH ar-invl NO-LOCK WHERE 
        ar-invl.x-no = ar-inv.x-no:
        FIND ttar-invl WHERE 
            ttar-invl.x-no EQ ar-invl.x-no AND
            ttar-invl.b-no EQ ar-invl.b-no AND  
            ttar-invl.i-no EQ ar-invl.i-no AND 
            ttar-invl.po-no EQ ar-invl.po-no 
            NO-ERROR.
        IF NOT AVAILABLE ttar-invl THEN 
        DO:
            CREATE ttar-invl.
            BUFFER-COPY ar-invl TO ttar-invl.
        END. 
        ELSE ASSIGN 
                ttar-invl.inv-qty = ttar-invl.inv-qty + ar-invl.inv-qty
                ttar-invl.qty = ttar-invl.qty + ar-invl.qty
                ttar-invl.amt = ttar-invl.amt + ar-invl.amt.
    END.
    /* End of consolidation */

    FOR EACH ttar-invl NO-LOCK WHERE 
        ttar-invl.x-no  EQ ar-inv.x-no AND 
        (ttar-invl.misc EQ NO OR ttar-invl.billable) 
        BY ttar-invl.misc  BY ttar-invl.i-no:
        ASSIGN 
            v-case-line = ""
            v-part-line = ""
            v-case-cnt = ""
            v-line = v-line + 1
            v-beeler-lines = 0
            lv-inv-list = ""
            v-ship-qty  = IF ttar-invl.ord-no EQ 0 THEN ttar-invl.qty ELSE ttar-invl.ship-qty.

        FIND FIRST oe-ordl WHERE 
            oe-ordl.company = cocode AND
            oe-ordl.ord-no = ttar-invl.ord-no AND
            oe-ordl.i-no = ttar-invl.i-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE oe-ordl THEN DO:
            ASSIGN 
                v-bo-qty = IF (ttar-invl.qty - v-ship-qty - oe-ordl.t-ship-qty) < 0 THEN 0 ELSE (ttar-invl.qty - v-ship-qty - oe-ordl.t-ship-qty).

            IF NOT CAN-FIND(FIRST oe-boll WHERE 
                oe-boll.company EQ ar-invl.company AND 
                oe-boll.b-no    EQ ttar-invl.b-no AND 
                oe-boll.po-no   EQ ttar-invl.po-no AND 
                oe-boll.ord-no  EQ oe-ordl.ord-no AND 
                oe-boll.i-no    EQ oe-ordl.i-no AND 
                oe-boll.line    EQ oe-ordl.line AND 
                oe-boll.s-code  EQ "I"
                USE-INDEX b-no) THEN FOR EACH xar-invl WHERE 
                    xar-invl.company EQ oe-ordl.company AND 
                    xar-invl.ord-no  EQ oe-ordl.ord-no AND 
                    xar-invl.i-no    EQ oe-ordl.i-no AND 
                    ROWID(xar-invl)  NE ROWID(ar-invl) AND CAN-FIND(FIRST oe-boll WHERE 
                        oe-boll.company EQ xar-invl.company AND 
                        oe-boll.b-no    EQ xar-invl.b-no AND 
                        oe-boll.po-no   EQ xar-invl.po-no AND 
                        oe-boll.ord-no  EQ oe-ordl.ord-no AND 
                        oe-boll.i-no    EQ oe-ordl.i-no AND 
                        oe-boll.line    EQ oe-ordl.line AND 
                        oe-boll.s-code  EQ "I"
                        USE-INDEX b-no)
                    NO-LOCK:
                    ASSIGN 
                        lv-inv-list = lv-inv-list + TRIM(STRING(xar-invl.inv-no,">>>>>>>>>>")) + " ".
            END.
        END.
        ELSE ASSIGN 
            v-bo-qty = IF (ttar-invl.qty - v-ship-qty ) < 0 THEN 0 ELSE ttar-invl.qty - v-ship-qty.

        ASSIGN 
            v-inv-qty = ttar-invl.qty
            v-i-no = ttar-invl.i-no
            v-i-dscr = ttar-invl.i-name
            v-price = ttar-invl.unit-pr * (1 - (ttar-invl.disc / 100))
            v-t-price = ttar-invl.amt
            v-subtot-lines = v-subtot-lines + ttar-invl.amt.

        IF ttar-invl.tax AND AVAILABLE stax THEN DO i = 1 TO 5:
            IF stax.tax-code1[i] NE "" THEN DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr1[i]
                    w-tax      = ROUND((IF stax.company EQ "yes" THEN v-t-price ELSE ttar-invl.amt) * stax.tax-rate1[i] / 100,2)
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
            END.
        END.

        IF v-t-price NE ttar-invl.amt THEN DO:
            CREATE w-tax.
            ASSIGN
                w-dsc     = "******ITEM TOTAL:"
                w-tax     = v-t-price
                v-lines   = v-lines + 1.
        END.
            
        PUT SPACE(1)        /*"->>>>9.9<"*/
            v-inv-qty FORMAT "->>>>>>9" SPACE(1)
            v-ship-qty  FORMAT "->>>>>>9" SPACE(1)
            /*v-bo-qty  format "->>>>>9" SPACE(1) */
            ttar-invl.ord-no FORM ">>>>>>9" SPACE(1)
            v-i-no  FORMAT "x(15)" SPACE(2)
            v-i-dscr  FORMAT "x(25)" SPACE(4)
            v-price  FORMAT "->>>>,>>9.99" /*"->,>>9.99<<"*/ SPACE(1)
            v-price-head SPACE(1)
            ttar-invl.amt  FORMAT "->,>>>,>>9.99" /*"->>>,>>9.99"   */             
            SKIP.
        ASSIGN 
            v-printline = v-printline + 1.
      
        DO v = 1 TO 3:
            ASSIGN 
                v-part-info = IF v EQ 1 THEN (IF ttar-invl.part-dscr1 <> "" THEN ttar-invl.part-dscr1 ELSE ttar-invl.i-dscr)
            ELSE
                IF v EQ 2 THEN ttar-invl.part-dscr2
                ELSE           TRIM(lv-inv-list).

            IF v-part-info NE "" OR (v = 1 AND ttar-invl.part-no <> "") THEN DO:
                IF v = 1 THEN PUT SPACE(27) ttar-invl.part-no SPACE(2) v-part-info SKIP.
                ELSE
                    IF v = 2 THEN PUT SPACE(44) v-part-info SKIP.
                    ELSE          PUT SPACE(23) "Previous Invoice(s): " v-part-info SKIP.
                ASSIGN 
                    v-printline = v-printline + 1.
            END.
        END.
        PUT SKIP(1).
        ASSIGN 
            v-printline = v-printline + 1.

        IF v-print-dept AND AVAILABLE oe-ordl THEN DO:
            FIND FIRST job-hdr WHERE
                job-hdr.company EQ cocode AND
                job-hdr.job-no  EQ oe-ordl.job-no AND
                job-hdr.job-no2 EQ oe-ordl.job-no2
                NO-LOCK NO-ERROR.

            IF AVAILABLE job-hdr THEN DO:
                FIND FIRST job WHERE
                    job.company EQ cocode AND
                    job.job     EQ job-hdr.job AND
                    job.job-no  EQ job-hdr.job-no AND
                    job.job-no2 EQ job-hdr.job-no2
                    NO-LOCK NO-ERROR.
                 
                IF AVAILABLE job THEN DO:
                    FOR EACH notes WHERE
                        notes.rec_key EQ job.rec_key AND /*for capitol task 06180806*/
                        CAN-DO(v-depts,notes.note_code)
                        NO-LOCK
                        BY notes.note_code:
                        
                        ASSIGN 
                            v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                    
                        IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                            IF v-printline > 50 THEN DO:
                                PAGE.
                                ASSIGN 
                                    v-printline = 0.
                                {ar/rep/invxprnt.i}
                            END.
                           
                            PUT SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                            ASSIGN 
                                v-printline = v-printline + 1.
                        END.
                    END.
                    RELEASE job.
                END.
                RELEASE job-hdr.
            END.
        END.
            
        IF v-printline > 50 THEN DO:
            PAGE.
            ASSIGN 
                v-printline = 0.
            {ar/rep/invxprnt.i}
        END.
    END. /* each ar-invl */

    ASSIGN 
        lv-line-chars = 80.

    IF v-prntinst THEN DO:
        {custom/notesprtA.i ar-inv v-inst 4}
        DO i = 1 TO 4:
            IF v-inst[i] <> "" THEN DO:                
                IF v-printline > 50 THEN DO:
                    PAGE.
                    ASSIGN 
                        v-printline = 0.
                    {ar/rep/invxprnt.i}
                END.
                PUT v-inst[i] SKIP.
                ASSIGN 
                    v-printline = v-printline + 1.
            END.            
        END.
          
        DO i = 1 TO 4:
            IF ar-inv.bill-i[i] <> "" THEN DO:
                IF v-printline > 50 THEN DO:
                    PAGE.
                    ASSIGN 
                        v-printline = 0.
                    {ar/rep/invxprnt.i}
                END.
                PUT ar-inv.bill-i[i] SKIP.
                ASSIGN 
                    v-printline = v-printline + 1.
            END.
        END.
    END.

    IF v-printline > 50 THEN DO:
        PAGE.
        ASSIGN 
            v-printline = 0.
        {ar/rep/invxprnt.i}
    END.

    ASSIGN 
        v-frt-tax = ar-inv.freight.        
    IF ar-inv.tax-code <> "" 
    AND (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
    AND ar-inv.freight <> 0
    AND AVAILABLE stax THEN DO i = 1 TO 5:
        IF stax.tax-code1[i] NE "" AND stax.tax-frt1[i] EQ YES THEN DO:
            CREATE w-tax.
            ASSIGN
                w-dsc      = stax.tax-dscr1[i]
                w-tax      = ROUND((IF stax.company EQ "yes" THEN v-frt-tax ELSE ar-inv.freight) * stax.tax-rate1[i] / 100,2)                 
                v-frt-tax  = v-frt-tax + w-tax
                v-t-tax[i] = v-t-tax[i] + w-tax
                v-lines    = v-lines + 1.
        END.
    END. 

    IF ar-inv.tax-amt EQ 0 THEN ASSIGN 
        v-t-tax = 0.
    ELSE DO:
        ASSIGN 
            v-tot-tax = 0.
        DO i = 1 TO 5:
            ASSIGN 
                v-tot-tax = v-tot-tax + v-t-tax[i].
        END.
        IF v-tot-tax EQ 0 THEN ASSIGN
            v-t-tax    = 0
            v-t-tax[1] = ar-inv.tax-amt.
        ELSE DO:
            IF v-tot-tax NE ar-inv.tax-amt THEN DO i = 1 TO 5:
                ASSIGN 
                    v-t-tax[i] = ROUND(v-t-tax[i] * (ar-inv.tax-amt / v-tot-tax),2).
            END.
            ASSIGN 
                v-tot-tax = 0.
            DO i = 1 TO 5:
                ASSIGN 
                    v-tot-tax = v-tot-tax + v-t-tax[i].
            END.
            IF v-tot-tax NE ar-inv.tax-amt THEN ASSIGN 
                v-t-tax[1] = v-t-tax[1] + (ar-inv.tax-amt - v-tot-tax).
        END.
    END.

    DO i = 1 TO 5:
        ASSIGN 
            v-bot-lab[i] = IF v-t-tax[i] NE 0 THEN
            ((IF AVAILABLE stax THEN STRING(CAPS(stax.tax-code1[i]),"x(5)") 
            ELSE FILL(" ",5) ) +
            fill(" ",6) + ":" +
            string(v-t-tax[i],"->>>,>>9.99")) ELSE "".
    END.
    ASSIGN 
        v-inv-freight = IF (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0)) THEN ar-inv.freight ELSE 0.    
    /*ar-inv.t-inv-freight*/.

    IF v-bot-lab[4] <> "" THEN
        PUT "<P10><R58><C60><#8><FROM><R+8><C+20><RECT> " 
            "<=8> Sub Total  :" v-subtot-lines FORM "->>>,>>9.99"
            "<=8><R+1> Freight    :" v-inv-freight FORM "->>>,>>9.99"
            "<=8><R+2> " v-bot-lab[1] 
            "<=8><R+3> " v-bot-lab[2]
            "<=8><R+4> " v-bot-lab[3]
            "<=8><R+5> " v-bot-lab[4]
            "<=8><R+6> " v-bot-lab[5]
            "<=8><R+7> Grand Total:" v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-t-tax[4] + v-t-tax[5] + v-inv-freight FORM "->>>,>>9.99" .
    ELSE
        PUT "<R58><C60><#8><FROM><R+6><C+20><RECT> " 
            "<=8> Sub Total  :" v-subtot-lines FORM "->>>,>>9.99"
            "<=8><R+1> Freight    :" v-inv-freight FORM "->>>,>>9.99"
            "<=8><R+2> " v-bot-lab[1] 
            "<=8><R+3> " v-bot-lab[2]
            "<=8><R+4> " v-bot-lab[3]
            "<=8><R+5> Grand Total:" v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight FORM "->>>,>>9.99" .

    ASSIGN
        v-printline = v-printline + 6
        v-page-num = PAGE-NUMBER.

    /*IF v-printline < 50 THEN PUT SKIP(60 - v-printline). */
    PAGE. 

    DO TRANSACTION:
        FIND FIRST xar-inv WHERE 
            RECID(xar-inv) = RECID(ar-inv).
        ASSIGN 
            xar-inv.printed = YES.
    END. /* DO TRANSACTION avail ar-inv */ 
END. /* each report, ar-inv */

