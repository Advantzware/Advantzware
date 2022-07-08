/* ---------------------------------------------- ar/rep/invHarwell.p   */
/* PRINT INVOICE   Xprint form for Pacific PKG             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{ar/rep/invoice.i}
{custom/notesdef.i}

DEF BUFFER xar-inv  FOR ar-inv.
DEF BUFFER xar-invl FOR ar-invl.

DEF TEMP-TABLE w-sman
    FIELD sman AS CHAR FORMAT "x(4)".
DEF TEMP-TABLE w-tax
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.

DEF VAR cAddr4          AS CHAR    NO-UNDO .
DEF VAR cMessage        AS CHAR    NO-UNDO.
DEF VAR cnt             AS INT     NO-UNDO.
DEF VAR cRtnChar        AS CHAR    NO-UNDO.
DEF VAR cShipAddr4      AS CHAR    NO-UNDO .
DEF VAR disp-frt        AS CHAR    INIT "Freight:" FORMAT "x(8)" NO-UNDO.
DEF VAR lRecFound       AS LOGICAL NO-UNDO.
DEF VAR ls-full-img1    AS CHAR    FORMAT "x(200)" NO-UNDO.
DEF VAR ls-full-img2    AS CHAR    FORMAT "x(200)" NO-UNDO.
DEF VAR ls-image1       AS CHAR    NO-UNDO.
DEF VAR ls-image2       AS CHAR    NO-UNDO.
DEF VAR lv-bol-no       LIKE oe-bolh.bol-no NO-UNDO.
DEF VAR lv-comp-color   AS CHAR    NO-UNDO.
DEF VAR lv-comp-name    AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR lv-currency     AS CHAR    NO-UNDO.
DEF VAR lv-display-comp AS LOG     NO-UNDO.
DEF VAR lv-email        AS CHAR    FORMAT "x(48)" NO-UNDO.
DEF VAR lv-inv-list     AS CHAR    NO-UNDO.
DEF VAR lv-other-color  AS CHAR    INIT "BLACK" NO-UNDO.
DEF VAR lValid          AS LOGICAL NO-UNDO.
DEF VAR minus-ship      AS INT     NO-UNDO.
DEF VAR net1            AS DEC     NO-UNDO.
DEF VAR net2            AS DEC     NO-UNDO.
DEF VAR net3            AS DEC     NO-UNDO.
DEF VAR tmp1            AS DEC     NO-UNDO.
DEF VAR tmp2            AS DATE    NO-UNDO.
DEF VAR v               AS INT.
DEF VAR v-addr3         AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-ans           AS LOGICAL INITIAL NO NO-UNDO.
DEF VAR v-beeler-lines  AS INT.
DEF VAR v-bill-i        AS CHAR    FORMAT "x(25)" NO-UNDO.
DEF VAR v-bo-qty        AS INT     FORMAT "99999" NO-UNDO.
DEF VAR v-bol-cases     LIKE oe-boll.cases NO-UNDO.
DEF VAR v-bot-lab       AS CHAR    FORMAT "x(63)" EXTENT 5 NO-UNDO.
DEF VAR v-case-cnt      AS CHAR    FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEF VAR v-case-line     AS CHAR    NO-UNDO.
DEF VAR v-comp-add1     AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2     AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3     AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4     AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add5     AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact       AS CHAR    FORMAT "x(20)" NO-UNDO .
DEF VAR v-date-ship     AS DATE    INITIAL TODAY NO-UNDO.
DEF VAR v-del-no        AS INT     FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-fax           AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-fob           AS CHAR    FORMAT "x(27)" NO-UNDO.
DEF VAR v-frt-tax       AS DEC     NO-UNDO.
DEF VAR v-i-dscr        AS CHAR    FORMAT "x(18)" NO-UNDO.
DEF VAR v-i-no          AS CHAR    FORMAT "x(15)" NO-UNDO.
DEF VAR v-inst          AS CHAR    FORMAT "x(80)" EXTENT 4 NO-UNDO.
DEF VAR v-int           AS DEC     NO-UNDO.
DEF VAR v-inv-date      AS DATE    INITIAL TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR v-inv-freight   LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR v-inv-no        AS INT     NO-UNDO.
DEF VAR v-inv-qty       AS DEC     NO-UNDO.
DEF VAR v-line          AS INT     NO-UNDO.
DEF VAR v-lines         AS INT     NO-UNDO.
DEF VAR v-net           LIKE inv-head.t-inv-rev NO-UNDO.
DEF VAR v-ord-date      LIKE oe-ord.ord-date NO-UNDO.
DEF VAR v-ord-del-hdr   AS CHAR    FORMAT "x(3)" INIT "Del".
DEF VAR v-ord-no        LIKE oe-ord.ord-no NO-UNDO.
DEF VAR v-page-num      AS INT     NO-UNDO.
DEF VAR v-part-info     AS CHAR    FORMAT "x(30)".
DEF VAR v-part-line     AS CHAR    NO-UNDO.
DEF VAR v-part-qty      AS DEC     FORMAT "999.9999" NO-UNDO.
DEF VAR v-po-no         LIKE ar-invl.po-no NO-UNDO.
DEF VAR v-price         AS DEC     FORMAT ">>>>9.9999" NO-UNDO.
DEF VAR v-price-head    AS CHAR    FORMAT "x(5)" NO-UNDO.
DEF VAR v-printline     AS INT     NO-UNDO.
DEF VAR v-rel-po-no     LIKE oe-rel.po-no NO-UNDO.
DEF VAR v-salesman      AS CHAR    FORMAT "x(14)" NO-UNDO.
DEF VAR v-set-qty       AS DECIMAL NO-UNDO.
DEF VAR v-ship-i        AS CHAR    FORMAT "x(25)" NO-UNDO.
DEF VAR v-ship-qty      AS INT     FORMAT "99999" NO-UNDO.
DEF VAR v-shipto-addr   AS CHAR    FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEF VAR v-shipto-city   AS CHAR    FORMAT "x(15)" NO-UNDO.
DEF VAR v-shipto-name   AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-state  AS CHAR    FORMAT "x(2)" NO-UNDO.
DEF VAR v-shipto-zip    AS CHAR    FORMAT "x(10)" NO-UNDO.
DEF VAR v-shipvia       LIKE carrier.dscr NO-UNDO.
DEF VAR v-sold-addr3    AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-subtot-lines  AS DEC     NO-UNDO.
DEF VAR v-t-price       AS DEC     FORMAT ">>>>>>9.99" NO-UNDO.
DEF VAR v-t-tax         AS DEC     EXTENT 5 NO-UNDO.
DEF VAR v-t-weight      LIKE ar-invl.t-weight NO-UNDO.
DEF VAR v-tax-code      LIKE stax.tax-code NO-UNDO.
DEF VAR v-tax-rate      AS DEC     FORMAT "->>>.99" NO-UNDO.
DEF VAR v-tel           AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-tot-cas       AS DEC     FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tot-pallets   AS INT     NO-UNDO.
DEF VAR v-tot-qty       AS INT     NO-UNDO.
DEF VAR v-tot-sqft      AS DEC     FORMAT "->>>>>>>9.99".
DEF VAR v-tot-tax       AS DEC     NO-UNDO.
DEF VAR v-tot-wght      AS DEC     FORMAT "->>>>>>>9.99".
DEF VAR v-tx-rate       LIKE stax.tax-rate NO-UNDO.

FIND FIRST ar-inv NO-LOCK NO-ERROR.

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
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".
   
FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

ASSIGN 
    v-comp-add1 = ""
    v-comp-add2 = "" 
    v-comp-add3 = ""
    v-comp-add4 = ""
    v-comp-add5 = "".

IF lv-display-comp THEN 
DO:
    FIND FIRST cust WHERE cust.company = cocode AND
        cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAIL cust THEN
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
        ASSIGN  v-shipto-name    = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city    = shipto.ship-city
            v-shipto-state   = shipto.ship-state
            v-shipto-zip     = shipto.ship-zip
            cShipAddr4       = shipto.contact .
    v-addr3 = cust.city + ", " + cust.state + "  " + cust.zip .
    cAddr4 = cust.contact .
    v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
        "  " + v-shipto-zip .


    FIND FIRST currency NO-LOCK WHERE 
        currency.company = cust.company AND 
        currency.c-code = cust.curr-code 
        NO-ERROR.
    IF AVAIL currency THEN
        lv-currency = currency.c-desc.

    IF ar-inv.fob-code BEGINS "ORIG" THEN
        ASSIGN v-fob = "Origin".
    ELSE
        ASSIGN v-fob = "Destination".

    ASSIGN
          
        v-line      = 1
        v-printline = 0.
    
    FIND FIRST stax NO-LOCK WHERE 
        (stax.tax-group BEGINS cocode AND SUBSTRING(stax.tax-group,1,10) EQ cocode)
        AND SUBSTRING(stax.tax-group,11,LENGTH(TRIM(stax.tax-group)) - 10) EQ ar-inv.tax-code
        NO-ERROR.
    IF NOT AVAIL stax THEN
        FIND FIRST stax NO-LOCK WHERE 
            stax.company = ar-inv.company AND 
            stax.tax-group EQ ar-inv.tax-code
            NO-ERROR.
    IF NOT AVAIL stax THEN
        FIND FIRST stax NO-LOCK WHERE 
            stax.tax-group EQ ar-inv.tax-code 
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
            v-tot-qty  = v-tot-qty + ar-invl.ship-qty
            v-t-weight = v-t-weight + (ROUND(ar-invl.t-weight / ar-invl.qty, 2) * ar-invl.inv-qty).
        
        FOR EACH oe-bolh NO-LOCK WHERE 
            oe-bolh.b-no = ar-invl.b-no AND
            oe-bolh.ord-no = ar-invl.ord-no:
            FOR EACH oe-boll NO-LOCK WHERE 
                oe-boll.company = oe-bolh.company AND
                oe-boll.b-no = oe-bolh.b-no AND
                oe-boll.i-no = ar-invl.i-no:

                /** Bill Of Lading TOTAL CASES **/
                ASSIGN 
                    v-bol-cases = v-bol-cases + oe-boll.cases.
                RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).
                ASSIGN 
                    v-tot-pallets = v-tot-pallets + v-int.
            END. /* each oe-boll */
            ASSIGN 
                v-date-ship = oe-bolh.bol-date.
        END. /* each oe-bolh */
         
        FIND FIRST oe-bolh NO-LOCK WHERE 
            oe-bolh.b-no = ar-invl.b-no 
            NO-ERROR.
        IF AVAIL oe-bolh THEN v-date-ship = oe-bolh.bol-date.

        IF LAST-OF(ar-invl.i-no) THEN DO:
            IF ar-invl.est-no NE "" THEN DO:
                FIND FIRST eb NO-LOCK WHERE 
                    eb.company = ar-invl.company AND
                    eb.est-no = ar-invl.est-no AND
                    eb.form-no = ar-invl.form-no AND
                    eb.blank-no = ar-invl.blank-no 
                    NO-ERROR.

                IF ar-invl.form-no = 0 
                AND ar-invl.est-type = 2 THEN DO:
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
                        FIND fg-set NO-LOCK WHERE 
                            fg-set.company = ar-invl.company AND
                            fg-set.set-no = ar-invl.i-no  AND
                            fg-set.part-no = eb.stock-no 
                            NO-ERROR.

                        IF AVAIL fg-set 
                        AND fg-set.qtyPerSet NE 0 THEN ASSIGN 
                            v-part-qty = fg-set.qtyPerSet / v-set-qty.
                        ELSE ASSIGN 
                            v-part-qty = 1 / v-set-qty.

                        IF eb.cas-cnt = 0 THEN ASSIGN 
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
    END. /* each ar-invl */
    
    /** Build Salesman Id String **/
    v-salesman = "".
    FOR EACH w-sman BREAK BY w-sman.sman:
        IF FIRST-OF(w-sman.sman) THEN
            ASSIGN v-salesman = v-salesman + w-sman.sman.
        DELETE w-sman.
    END.
    IF v-salesman = "" THEN ASSIGN 
        v-salesman = cust.sman.
    IF v-salesman NE "" THEN DO:
        FIND FIRST sman NO-LOCK WHERE
            sman.company EQ ar-invl.company AND 
            sman.sman EQ v-salesman
            NO-ERROR.
        IF AVAIL sman THEN ASSIGN 
                v-salesman = sman.sname.
    END.  

    ASSIGN 
        v-po-no    = ar-inv.po-no
        v-bill-i   = ar-inv.bill-i[1]
        v-ord-no   = ar-inv.ord-no
        v-ord-date = ar-inv.ord-date.

    FIND FIRST ar-invl NO-LOCK WHERE 
        ar-invl.x-no  EQ ar-inv.x-no AND 
        (ar-invl.misc EQ NO OR ar-invl.billable)
        NO-ERROR.
    IF AVAIL ar-invl THEN ASSIGN 
        v-price-head = ar-invl.pr-uom
        v-po-no      = IF ar-invl.po-no <> "" THEN ar-invl.po-no ELSE ar-inv.po-no
        v-ord-no     = ar-invl.ord-no
        lv-bol-no    = ar-invl.bol-no.
        
    v-inv-date = ar-inv.inv-date.
        
    {ar/rep/invHarwell.i}

    ASSIGN
        v-subtot-lines = 0
        v-t-tax        = 0.

    FOR EACH ar-invl NO-LOCK WHERE 
        ar-invl.x-no  EQ ar-inv.x-no AND 
        (ar-invl.misc EQ NO OR ar-invl.billable) 
        BY ar-invl.misc  BY ar-invl.i-no:
        ASSIGN 
            v-case-line    = ""
            v-part-line    = ""
            v-case-cnt     = ""
            v-line         = v-line + 1
            v-beeler-lines = 0
            lv-inv-list    = ""
            v-ship-qty     = IF ar-invl.ord-no EQ 0 THEN ar-invl.qty ELSE ar-invl.ship-qty.

        FIND FIRST oe-ordl NO-LOCK WHERE 
            oe-ordl.company = cocode AND
            oe-ordl.ord-no = ar-invl.ord-no AND
            oe-ordl.i-no = ar-invl.i-no
            NO-ERROR.
        IF AVAIL oe-ordl THEN DO:
            ASSIGN 
                v-bo-qty = IF (ar-invl.qty - v-ship-qty - oe-ordl.t-ship-qty) < 0 THEN 0 ELSE 
                              (ar-invl.qty - v-ship-qty - oe-ordl.t-ship-qty).

            IF NOT CAN-FIND(FIRST oe-boll WHERE 
                oe-boll.company EQ ar-invl.company AND 
                oe-boll.b-no    EQ ar-invl.b-no AND 
                oe-boll.po-no   EQ ar-invl.po-no AND 
                oe-boll.ord-no  EQ oe-ordl.ord-no AND 
                oe-boll.i-no    EQ oe-ordl.i-no AND 
                oe-boll.line    EQ oe-ordl.line AND 
                oe-boll.s-code  EQ "I"
                USE-INDEX b-no) THEN FOR EACH xar-invl NO-LOCK WHERE 
                    xar-invl.company EQ oe-ordl.company AND 
                    xar-invl.ord-no  EQ oe-ordl.ord-no AND 
                    xar-invl.i-no    EQ oe-ordl.i-no AND 
                    ROWID(xar-invl)  NE ROWID(ar-invl) AND 
                    CAN-FIND(FIRST oe-boll WHERE 
                        oe-boll.company EQ xar-invl.company AND 
                        oe-boll.b-no    EQ xar-invl.b-no AND 
                        oe-boll.po-no   EQ xar-invl.po-no AND 
                        oe-boll.ord-no  EQ oe-ordl.ord-no AND 
                        oe-boll.i-no    EQ oe-ordl.i-no AND 
                        oe-boll.line    EQ oe-ordl.line AND 
                        oe-boll.s-code  EQ "I"
                        USE-INDEX b-no):
                    ASSIGN 
                        lv-inv-list = lv-inv-list + TRIM(STRING(xar-invl.inv-no,">>>>>>>>>>")) + " ".
            END.
        END.
        ELSE ASSIGN 
            v-bo-qty = IF (ar-invl.qty - v-ship-qty) LT 0 THEN 0 ELSE ar-invl.qty - v-ship-qty.

        ASSIGN 
            v-inv-qty      = ar-invl.qty
            v-i-no         = ar-invl.i-no
            v-i-dscr       = ar-invl.i-name
            v-price        = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
            v-t-price      = ar-invl.amt
            v-subtot-lines = v-subtot-lines + ar-invl.amt.
            
        /* Display first line of line block */
        PUT UNFORMATTED SKIP 
            v-inv-qty FORMAT "->>>>>9"          TO 9
            TRIM(STRING(ar-invl.i-no))          AT 12
            ar-invl.po-no                       AT 46
            v-ship-qty  FORMAT "->>>>>9"        TO 75
            IF v-ship-qty LT v-inv-qty THEN "P" ELSE "C" FORMAT "x" AT 78
            v-price     FORMAT "->,>>9.99<<"    TO 91
            ar-invl.amt FORMAT "->>>,>>9.99"    TO 105
            SKIP.
        ASSIGN 
            v-printline = v-printline + 1.
            
        /* Additional descriptive lines of line block */
        IF ar-invl.i-name NE "" THEN DO:
            PUT UNFORMATTED 
                ar-invl.i-name FORMAT "x(22)"       AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.                
        IF ar-invl.i-dscr NE "" THEN DO:
            PUT UNFORMATTED 
                ar-invl.i-dscr FORMAT "x(22)"       AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.
        IF ar-invl.part-dscr1 NE "" THEN DO:
            PUT UNFORMATTED 
                ar-invl.part-dscr1 FORMAT "x(22)"   AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.
        IF ar-invl.part-dscr2 NE "" THEN DO:
            PUT UNFORMATTED 
                ar-invl.part-dscr2 FORMAT "x(22)"   AT 12
                SKIP.
            ASSIGN 
                v-printline = v-printline + 1.
        END.
      
        PUT SKIP.
        ASSIGN 
            v-printline = v-printline + 1.

        IF v-print-dept AND AVAIL oe-ordl THEN DO:
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
                        notes.rec_key EQ job.rec_key AND /*for capitol task 06180806*/
                        CAN-DO(v-depts,notes.note_code)
                        BY notes.note_code:
                        
                        v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                    
                        IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                            IF v-printline > 62 THEN DO:
                                PAGE.
                                ASSIGN 
                                    v-printline = 0.
                                {ar/rep/invHarwell.i}
                            END.
                            PUT SKIP SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORMAT "x(80)".              
                            ASSIGN 
                                v-printline = v-printline + 1.
                        END.
                    END.
                    RELEASE job.
                END.
                RELEASE job-hdr.
            END.
        END.
            
        IF ar-invl.tax AND AVAIL stax THEN DO i = 1 TO 5:
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
                    w-tax FORMAT "->>>,>>9.99"   TO 105
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
                v-t-price FORMAT "->>>,>>9.99"  TO 105 
                SKIP(1).
            ASSIGN 
                v-printline = v-printline + 2.
        END.

        IF v-printline > 62 THEN DO:
            PAGE.
            ASSIGN 
                v-printline = 0.
            {ar/rep/invHarwell.i}
        END.

        FIND FIRST itemfg NO-LOCK WHERE 
            itemfg.company EQ cocode AND 
            itemfg.i-no    EQ ar-invl.i-no
            NO-ERROR.
        
        ASSIGN 
            v-tot-sqft = v-tot-sqft + ((IF AVAIL itemfg THEN itemfg.t-sqft ELSE ar-invl.sf-sht) * ar-invl.qty)
            v-tot-wght = v-tot-wght + ((IF AVAIL itemfg THEN itemfg.weight-100 * ar-invl.ship-qty / 100 ELSE 0)).

    END. /* each ar-invl */

    lv-line-chars = 80.

    IF v-prntinst THEN DO:
        {custom/notesprtA.i ar-inv v-inst 4}
        DO i = 1 TO 4:
            IF v-inst[i] <> "" THEN DO:                
                IF v-printline > 56 THEN DO:
                    PAGE.
                    ASSIGN 
                        v-printline = 0.
                    {ar/rep/invHarwell.i}
                END.
                PUT SKIP v-inst[i].
                ASSIGN 
                    v-printline = v-printline + 1.
            END.            
        END.
          
        DO i = 1 TO 4:
            IF ar-inv.bill-i[i] <> "" THEN DO:
                IF v-printline > 56 THEN DO:
                    PAGE.
                    ASSIGN 
                        v-printline = 0.
                    {ar/rep/invHarwell.i}
                END.
                PUT SKIP ar-inv.bill-i[i].
                ASSIGN 
                    v-printline = v-printline + 1.
            END.
        END.
    END.

    IF v-printline > 62 THEN DO:
        PAGE.
        ASSIGN 
            v-printline = 0.
        {ar/rep/invHarwell.i}
    END.

    ASSIGN 
        v-frt-tax = ar-inv.freight.        
    IF ar-inv.tax-code <> "" 
    AND (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0))
    AND ar-inv.freight <> 0
    AND AVAIL stax THEN DO i = 1 TO 5:
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
            v-tot-tax = v-tot-tax + v-t-tax[i].
        END.
        IF v-tot-tax EQ 0 THEN ASSIGN
            v-t-tax    = 0
            v-t-tax[1] = ar-inv.tax-amt.
        ELSE DO:
            IF v-tot-tax NE ar-inv.tax-amt THEN DO i = 1 TO 5:
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
                            ((IF AVAIL stax THEN STRING(CAPS(stax.tax-code1[i]),"x(5)") 
                            ELSE FILL(" ",5) ) + FILL(" ",7) + ":" + STRING(v-t-tax[i],"->>>>>9.99")) 
                            ELSE "".
    END.
    
    ASSIGN 
        v-inv-freight = IF (ar-inv.f-bill OR (cust.frt-pay = "B" AND ar-inv.ord-no = 0)) THEN ar-inv.freight ELSE 0.    

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
        "<R59.1><C71.2><B>" v-subtot-lines + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-inv-freight FORMAT "->>>,>>9.99"
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

    DO TRANSACTION:
        FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
        ASSIGN 
            xar-inv.printed = YES.
    END. /* DO TRANSACTION avail ar-inv */ 
END. /* each report, ar-inv */

