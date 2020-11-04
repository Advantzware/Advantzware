/* ------------------------------------------- cec/quote/quoMcElroy.p 10/02 YSK */
/* print quotes in Pacific - McElroy format                                               */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

DEFINE SHARED BUFFER xquo      FOR quotehd.
DEFINE        BUFFER xqitm     FOR quoteitm.
DEFINE        BUFFER xqqty     FOR quoteqty.
DEFINE        BUFFER xqchg     FOR quotechg.
DEFINE        BUFFER b-qi      FOR quoteitm.
DEFINE        BUFFER x-qi      FOR quoteitm.
DEFINE        BUFFER bf-report FOR report.
DEFINE        BUFFER bf-eb     FOR eb.

{est/printquo.i}

DEFINE NEW SHARED VARIABLE v-out1-id        AS RECID     NO-UNDO.  /* YSK 06/08/01  was~ local var */
DEFINE NEW SHARED VARIABLE v-out2-id        AS RECID     NO-UNDO.  /* YSK 06/08/01  was~ local var */

DEFINE            VARIABLE idx              AS INTEGER   NO-UNDO.
DEFINE            VARIABLE idummy           AS INTEGER   NO-UNDO.
DEFINE            VARIABLE k_frac           AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE            VARIABLE numfit           AS INTEGER   NO-UNDO.
DEFINE            VARIABLE sold             AS ch        EXTENT 5 FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE bill             AS ch        EXTENT 5 FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE ship             AS ch        EXTENT 5 FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE tot              AS de        NO-UNDO.
DEFINE            VARIABLE v-over-under     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-comp-name      LIKE company.name EXTENT 4.
DEFINE            VARIABLE trim-size        LIKE quoteitm.size NO-UNDO.
DEFINE            VARIABLE temp-trim-size   LIKE quoteitm.size NO-UNDO.
DEFINE            VARIABLE cc               AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-printline      AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE            VARIABLE v-first-q-no     LIKE quotehd.q-no NO-UNDO.
DEFINE            VARIABLE v-lines          AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-rels           AS INTEGER.
DEFINE            VARIABLE v-part           LIKE quoteitm.part-no NO-UNDO.
DEFINE            VARIABLE v-board          AS CHARACTER NO-UNDO.
DEFINE            VARIABLE cAddrDesc        AS CHARACTER EXTENT 6 NO-UNDO.
DEFINE            VARIABLE v-last           AS LOG       INITIAL NO NO-UNDO.
DEFINE            VARIABLE v-quo-date       AS DATE      FORM "99/99/9999" NO-UNDO.
DEFINE            VARIABLE v-contact        LIKE quotehd.contact NO-UNDO.
DEFINE            VARIABLE cAdder           AS CHARACTER NO-UNDO.

DEFINE            VARIABLE v-tel            AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-fax            AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add1      AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add2      AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add3      AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add4      AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-comp-add5      AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE v-line-total     AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-quo-total      AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-t-tax          AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE            VARIABLE v-bot-lab        AS CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE            VARIABLE style-dscr       AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE lv-est-no        AS cha       NO-UNDO.
DEFINE            VARIABLE lv-chg-amt       LIKE quotechg.amt NO-UNDO.
DEFINE            VARIABLE lv-display-comp  AS LOG       NO-UNDO.  /* display company address */
DEFINE            VARIABLE lv-comp-name     AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE lv-email         AS cha       FORM "x(40)" NO-UNDO.
DEFINE            VARIABLE v-note-lines     AS INTEGER   NO-UNDO.

DEFINE            VARIABLE lv-first-qreckey LIKE quotehd.rec_key NO-UNDO.
DEFINE            VARIABLE lv-first-qrecid  AS RECID     NO-UNDO.
DEFINE BUFFER bf-quo FOR quotehd.
DEFINE VARIABLE lv-comp-color  AS cha NO-UNDO.
DEFINE VARIABLE lv-other-color AS cha INIT "BLACK" NO-UNDO.
{custom/notesdef.i}
DEFINE VARIABLE v-inst2         AS cha       EXTENT 20 NO-UNDO.    
DEFINE VARIABLE v-dept-inst     AS cha       FORM "x(80)" EXTENT 20 NO-UNDO.
DEFINE VARIABLE v-note-length   AS INTEGER   INIT 80 NO-UNDO.
DEFINE VARIABLE li-cline        AS INTEGER   NO-UNDO.

DEFINE VARIABLE lv-pg-num       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-tot-pg       AS INTEGER   INIT 1 NO-UNDO.
DEFINE VARIABLE ln-cnt          AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-itm-ln-cnt   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-part-dscr1   AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-fg#          AS cha       FORM "x(15)" NO-UNDO.
DEFINE VARIABLE v-prep-printed  AS LOG       NO-UNDO.
DEFINE VARIABLE v-prep-prt-list AS cha       NO-UNDO.
DEFINE VARIABLE ld-metric       AS DECIMAL   INIT 1 NO-UNDO.
DEFINE VARIABLE lv-format       AS CHARACTER INIT ">>>>>9.9<<<<" NO-UNDO.
DEFINE VARIABLE ld-wid          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ld-len          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ld-dep          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-part-dscr2   AS cha       NO-UNDO.
DEFINE VARIABLE lv-i-coldscr    AS cha       NO-UNDO.
DEFINE VARIABLE ll-prt-dscr2    AS LOG       NO-UNDO.
DEFINE VARIABLE adder-print     AS LOG       NO-UNDO.


/* rdb 01/31/07 12060608 */	
DEFINE VARIABLE logSetPrinting  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrX            AS CHARACTER NO-UNDO.
DEFINE VARIABLE logPrint        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE intPageNum      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1    AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE lPrintSecDscr   AS LOGICAL   NO-UNDO .
DEFINE VARIABLE lValid          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCheckPrice     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lCheckUom       AS LOGICAL   NO-UNDO.

{sys/inc/f16to32.i}
{cecrep/jobtick2.i "new shared"}

ASSIGN 
    tmpstore = FILL("-",130).

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
    AND sys-ctrl.name    EQ "QUOPRINT" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.

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
            lv-comp-name = cust.NAME.
END.

FIND FIRST report WHERE report.term-id EQ v-term-id NO-LOCK NO-ERROR.
FIND FIRST xquo  WHERE RECID(xquo) EQ report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAILABLE xquo THEN RETURN.
v-quo-date = xquo.quo-date.

FIND FIRST est WHERE est.company = xquo.company
    AND est.est-no EQ xquo.est-no NO-LOCK NO-ERROR.
FIND FIRST sman
    WHERE sman.company EQ cocode
    AND sman.sman    EQ xquo.sman
    NO-LOCK NO-ERROR.
FIND FIRST carrier
    WHERE carrier.company EQ cocode
    AND carrier.carrier EQ xquo.carrier
    NO-LOCK NO-ERROR.
FIND FIRST terms
    WHERE terms.company EQ cocode
    AND terms.t-code  EQ xquo.terms
    NO-LOCK NO-ERROR.
FIND FIRST cust
    WHERE cust.company EQ xquo.company
    AND cust.cust-no EQ xquo.cust-no
    NO-LOCK NO-ERROR.

IF AVAILABLE cust THEN
    v-over-under = TRIM(STRING(cust.over-pct,">>9%")) + "-" +
        trim(STRING(cust.under-pct,">>9%")).

ASSIGN
    sold[5] = TRIM(STRING(xquo.sold-no))
    ship[5] = TRIM(STRING(xquo.ship-id))
    bill[5] = TRIM(STRING(xquo.cust-no)).

DO i = 1 TO 4:
    ASSIGN 
        sold[i] = xquo.soldto[i]
        ship[i] = xquo.shipto[i]
        bill[i] = xquo.billto[i].
END.

IF (xquo.shipto[1] EQ xquo.soldto[1] AND
    xquo.shipto[2] EQ xquo.soldto[2] AND
    xquo.shipto[3] EQ xquo.soldto[3] AND
    xquo.shipto[4] EQ xquo.soldto[4]) THEN
    ASSIGN
        ship[1] = ""
        ship[2] = ""
        ship[3] = ""
        ship[4] = ""
        ship[5] = "SAME".

ASSIGN
    v-first-q-no = xquo.q-no
    v-line-total = 0
    v-printline  = 0.

PUT "<Farial>". 

ln-cnt = 0.

IF (NOT ch-multi) THEN 
DO:

    /* get total page number */

    FOR EACH xqitm OF xquo NO-LOCK /*,
      EACH xqqty OF xqitm NO-LOCK*/:
        ln-cnt = ln-cnt + 1.
    END.
    ln-cnt = ln-cnt * 5.

    FOR EACH xqitm OF xquo NO-LOCK,
        EACH xqqty OF xqitm NO-LOCK,
        EACH xqchg WHERE xqchg.company = xqqty.company
        AND xqchg.loc = xqqty.loc
        AND xqchg.q-no = xqqty.q-no
        AND xqchg.LINE = xqqty.LINE
        AND xqchg.qty = xqqty.qty NO-LOCK
        BREAK /*BY xqchg.qty*/
        BY xqchg.charge:
      
        IF FIRST-OF(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
    END.
 
    FOR EACH xqchg OF xquo
        WHERE xqchg.qty  EQ 0
        AND xqchg.line EQ 0
        NO-LOCK
        BREAK BY xqchg.charge:
        IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
    END.
    IF s-print-comp THEN 
    DO :      /* Print components of a set */

        FIND FIRST est WHERE est.company EQ xquo.company
            AND est.est-no  EQ xquo.est-no NO-LOCK NO-ERROR.
        IF AVAILABLE est AND est.est-type EQ 6 AND est.form-qty GT 1 THEN
            FOR EACH ef WHERE ef.company EQ est.company
                AND ef.est-no  EQ est.est-no NO-LOCK,      
                EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
                ln-cnt = ln-cnt + 4.
            END.
    END.

    lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .
    /* get total page number */

    {cec/quote/quoMcElroy2.i}

    {cec/quote/quoMcElroy.i 1}
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
        
    IF LINE-COUNTER > PAGE-SIZE - 1 THEN 
    DO:      
        v-printline = 0.
        PAGE.  
        {cec/quote/quoMcElroy2.i}
    END.

    PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " .
      
    ASSIGN
        v-tmp-lines = 0
        li-cline    = 1.
    DO i = 1 TO 5:
        IF xquo.comment[i] NE "" THEN 
        DO: 
            PUT "<C1><R" STRING(58 + li-cline,">9") + "><C6>" xquo.comment[i]. 
            li-cline = li-cline + 1.
        END.
    END.

    v-printline = v-printline + 6.
    IF v-printline < 50 THEN PAGE.

    RELEASE est.
    IF v-prt-box THEN 
    DO:
        FIND FIRST est
            WHERE est.company EQ xquo.company
            AND est.loc     EQ xquo.loc
            AND est.est-no  EQ xquo.est-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE est THEN 
        DO:
            PAGE.
            PUT "<FCouriar New>" SKIP.
            FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
            RUN cec/desprnt2.p (?,
                INPUT-OUTPUT v-lines,
                RECID(xest)).
        END.
    END.
END.

ELSE 
DO:

    FOR EACH report WHERE report.term-id EQ v-term-id,
        FIRST xquo  WHERE RECID(xquo) EQ report.rec-id
        NO-LOCK
        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03
        TRANSACTION:

        FIND FIRST est
            WHERE est.company EQ xquo.company
            AND est.est-no  EQ xquo.est-no
            NO-LOCK NO-ERROR.
        FIND FIRST sman
            WHERE sman.company EQ cocode
            AND sman.sman    EQ xquo.sman
            NO-LOCK NO-ERROR.
        FIND FIRST carrier
            WHERE carrier.company EQ cocode
            AND carrier.carrier EQ xquo.carrier
            NO-LOCK NO-ERROR.
        FIND FIRST terms
            WHERE terms.company EQ cocode
            AND terms.t-code  EQ xquo.terms
            NO-LOCK NO-ERROR.
        FIND FIRST cust
            WHERE cust.company EQ xquo.company
            AND cust.cust-no EQ xquo.cust-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE cust THEN
            v-over-under = TRIM(STRING(cust.over-pct,">>9%")) + "-" +
                trim(STRING(cust.under-pct,">>9%")).


        ASSIGN
            sold[5] = TRIM(STRING(xquo.sold-no))
            ship[5] = TRIM(STRING(xquo.ship-id))
            bill[5] = TRIM(STRING(xquo.cust-no)).

        DO i = 1 TO 4:
            ASSIGN
                sold[i] = xquo.soldto[i]
                ship[i] = xquo.shipto[i]
                bill[i] = xquo.billto[i].
        END.
    
        IF s-sep-page OR first(report.key-01) THEN
            ASSIGN lv-first-qreckey = xquo.rec_key
                lv-first-qrecid  = RECID(xquo).

        IF s-sep-page OR first-of(report.key-01) THEN 
        DO:      
            v-printline = 0.
            IF NOT FIRST(report.key-01) THEN PAGE.
            ASSIGN
                v-first-q-no = xquo.q-no
                lv-tot-pg    = 1
                ln-cnt       = 0.
            /* get total page number */
            FOR EACH bf-report WHERE bf-report.term-id EQ v-term-id
                AND bf-report.key-01 EQ report.key-01,
                FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id,
                EACH xqitm OF bf-quo NO-LOCK
                BREAK BY bf-quo.q-no BY xqitm.part-no: 
          
                lv-itm-ln-cnt = lv-itm-ln-cnt + 1.
                IF LAST-OF(bf-quo.q-no) THEN 
                DO:
                    ASSIGN 
                        ln-cnt = ln-cnt + lv-itm-ln-cnt * 5.
                    lv-itm-ln-cnt = 0.              
                END.
            END.     

            FOR EACH bf-report WHERE bf-report.term-id EQ v-term-id
                AND bf-report.key-01 = report.key-01,
                FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id,
                EACH xqitm OF bf-quo NO-LOCK,
                EACH xqqty OF xqitm NO-LOCK,
                EACH xqchg WHERE xqchg.company = xqqty.company
                AND xqchg.loc = xqqty.loc
                AND xqchg.q-no = xqqty.q-no
                AND xqchg.LINE = xqqty.LINE
                AND xqchg.qty = xqqty.qty NO-LOCK
          
                BREAK BY xqchg.qty
                BY xqchg.charge:

                IF FIRST(xqchg.charge) THEN 
                    ln-cnt = ln-cnt + 1.

                ln-cnt = ln-cnt + 1.

                IF LAST-OF(xqchg.qty) THEN
                    LEAVE.
            END.

            FOR EACH bf-report WHERE bf-report.term-id EQ v-term-id
                AND bf-report.key-01 = report.key-01,
                FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id,
                EACH xqchg OF bf-quo WHERE xqchg.qty  EQ 0 AND 
                xqchg.line EQ 0 NO-LOCK
                BREAK BY xqchg.charge:
                IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
            END.

            IF s-print-comp THEN 
            DO :      /* Print components of a set */
                FOR EACH bf-report WHERE bf-report.term-id EQ v-term-id
                    AND bf-report.key-01 = report.key-01 NO-LOCK,
                    FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id NO-LOCK,
                    FIRST est WHERE est.company EQ bf-quo.company
                    AND est.est-no  EQ bf-quo.est-no 
                    AND est.est-type EQ 6 AND est.form-qty GT 1 NO-LOCK,
                    EACH ef WHERE ef.company EQ est.company
                    AND ef.est-no  EQ est.est-no NO-LOCK,      
                    EACH eb OF ef NO-LOCK BREAK BY ef.form-no :
                    ln-cnt = ln-cnt + 4.
                END.
            END.

            IF ch-inst THEN 
            DO:
                FOR EACH bf-report WHERE bf-report.term-id EQ v-term-id
                    AND bf-report.key-01 = report.key-01,
                    FIRST bf-quo  WHERE RECID(bf-quo) EQ bf-report.rec-id,
                    FIRST est WHERE est.company = bf-quo.company
                    AND est.est-no = bf-quo.est-no NO-LOCK.
                    v-inst2 = "".
                    {custom/notespr2.i job v-inst2 EXTENT(v-inst2) "notes.rec_key = est.rec_key and notes.note_code >= fdept and notes.note_code <= tdept " }
                    ln-cnt = ln-cnt + 1.
                    DO idx = 1 TO EXTENT(v-inst2):
                        IF v-inst2[idx] NE '' THEN ln-cnt = ln-cnt + 1.
                    END.
                END.
            END.

            lv-tot-pg = lv-tot-pg + TRUNC( ln-cnt / 25,0) .

            /* get total page number */
            {cec/quote/quoMcElroy2.i}
        END.

        ASSIGN
            v-last       = LAST-OF(report.key-01)
            v-line-total = 0.

        {cec/quote/quoMcElroy.i 2}  
        v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

        IF LINE-COUNTER > PAGE-SIZE - 1 THEN 
        DO:      
            v-printline = 0.
            PAGE.  
            {cec/quote/quoMcElroy2.i}
        END.

        IF (ch-multi AND (v-last OR s-sep-page)) THEN 
        DO:
            PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " .

            FIND bf-quo WHERE RECID(bf-quo) = lv-first-qrecid NO-LOCK NO-ERROR. 
            li-cline = 0.
            DO i = 1 TO 5:      
                IF bf-quo.comment[i] NE "" THEN 
                DO:
                    li-cline = li-cline + 1.
                    PUT "<C1><R" STRING(58 + li-cline,">9") + "><C6>" bf-quo.comment[i]  .            
                END.
            
            END.
            v-printline = v-printline + 6.
            IF v-printline < 50 THEN 
            DO:
                PAGE.
            END.

            IF v-prt-box THEN 
            DO:
                FIND FIRST est WHERE est.company EQ xquo.company
                    AND est.loc     EQ xquo.loc
                    AND est.est-no  EQ xquo.est-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE est THEN 
                DO:
       
                    PAGE.
                    PUT "<FCouriar New>" SKIP.
                    FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
                    RUN cec/desprnt2.p (?,
                        INPUT-OUTPUT v-lines,
                        RECID(xest)).
                END.
            END.
        END.  /*ch-multi and v-last */
   
    END. /* for each report */
  
END.  /* multi */

PROCEDURE printHeader:
    DEFINE INPUT PARAMETER ipPageOffSet AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opInitVar AS INTEGER NO-UNDO.

    IF LINE-COUNTER > PAGE-SIZE - ipPageOffSet THEN 
    DO:
        PAGE.
        {cec/quote/quoMcElroy2.i}
        opInitVar = 0.
        PUT SKIP .
    END.
END PROCEDURE.
/* end ---------------------------------- copr. 2000  advanced software, inc. */
