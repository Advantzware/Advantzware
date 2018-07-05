/* ------------------------------------------- cec/quote/quoxprnt11.p 10/02 YSK */
/* print quotes in Pacific - Xprint format                                               */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

define shared buffer xquo      for quotehd.
define        buffer xqitm     for quoteitm.
define        buffer xqqty     for quoteqty.
define        buffer xqchg     for quotechg.
define        buffer b-qi      for quoteitm.
define        buffer x-qi      for quoteitm.
DEFINE        BUFFER bf-report FOR report.
DEFINE        BUFFER bf-eb     FOR eb.

{est/printquo.i}

define new shared variable v-out1-id        as recid     no-undo.  /* YSK 06/08/01  was~ local var */
define new shared variable v-out2-id        as recid     no-undo.  /* YSK 06/08/01  was~ local var */

DEFINE            VARIABLE idx              AS INTEGER   NO-UNDO.
DEFINE            VARIABLE idummy           AS INTEGER   NO-UNDO.
DEFINE            VARIABLE k_frac           AS DECIMAL   INIT 6.25 NO-UNDO.
define            variable numfit           as integer   no-undo.
define            variable sold             as ch        extent 5 FORM "x(30)" no-undo.
define            variable bill             as ch        extent 5 FORM "x(30)" no-undo.
define            variable ship             as ch        extent 5 FORM "x(30)" no-undo.
define            variable tot              as de        no-undo.
define            variable v-over-under     as character no-undo.
define            variable v-comp-name      like company.name extent 4.
define            variable trim-size        like quoteitm.size no-undo.
define            variable temp-trim-size   like quoteitm.size no-undo.
define            variable cc               as integer   no-undo.
define            variable v-printline      as integer   initial 0 no-undo.
define            variable v-first-q-no     like quotehd.q-no no-undo.
DEFINE            VARIABLE v-lines          AS INTEGER   NO-UNDO.
define            variable v-rels           as integer.
define            variable v-part           like quoteitm.part-no no-undo.
define            variable v-board          as character no-undo.
define            variable v-last           as log       initial no no-undo.
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
define            variable v-t-tax          as decimal   extent 3 NO-UNDO.
define            variable v-bot-lab        as character format "x(63)" extent 3 NO-UNDO.
DEFINE            VARIABLE style-dscr       AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE lv-est-no        AS cha       NO-UNDO.
DEFINE            VARIABLE lv-chg-amt       LIKE quotechg.amt NO-UNDO.
DEFINE            VARIABLE lv-display-comp  AS LOG       NO-UNDO.  /* display company address */
DEFINE            VARIABLE lv-comp-name     AS cha       FORM "x(30)" NO-UNDO.
DEFINE            VARIABLE lv-email         AS cha       FORM "x(40)" NO-UNDO.
DEFINE            VARIABLE v-note-lines     AS INTEGER   NO-UNDO.

DEFINE            VARIABLE lv-first-qreckey LIKE quotehd.rec_key NO-UNDO.
DEFINE            VARIABLE lv-first-qrecid  AS recid     NO-UNDO.
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
define variable v-cas-cnt       as DECIMAL   NO-UNDO.
DEFINE VARIABLE v-blank         AS CHARACTER NO-UNDO.


/* rdb 01/31/07 12060608 */	
DEFINE VARIABLE logSetPrinting  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE chrX            AS CHARACTER NO-UNDO.
DEFINE VARIABLE logPrint        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE intPageNum      AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1    AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE lPrintSecDscr   AS LOGICAL   NO-UNDO .
{sys/inc/f16to32.i}
{cecrep/jobtick2.i "new shared"}

ASSIGN 
    tmpstore = fill("-",130).

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .

find first sys-ctrl where sys-ctrl.company eq cocode
    and sys-ctrl.name    eq "QUOPRINT" no-lock no-error.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND first sys-ctrl where sys-ctrl.company eq cocode
    and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.

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

FIND first report where report.term-id eq v-term-id NO-LOCK NO-ERROR.
FIND first xquo  where recid(xquo) eq report.rec-id NO-LOCK NO-ERROR.
IF NOT AVAILABLE xquo THEN RETURN.
v-quo-date = xquo.quo-date.

find first est where est.company = xquo.company
    AND est.est-no eq xquo.est-no no-lock no-error.
find first sman
    where sman.company eq cocode
    and sman.sman    eq xquo.sman
    no-lock no-error.
find first carrier
    where carrier.company eq cocode
    and carrier.carrier eq xquo.carrier
    no-lock no-error.
find first terms
    where terms.company eq cocode
    and terms.t-code  eq xquo.terms
    no-lock no-error.
find first cust
    where cust.company eq xquo.company
    and cust.cust-no eq xquo.cust-no
    no-lock no-error.

if available cust then
    v-over-under = trim(string(cust.over-pct,">>9%")) + "-" +
        trim(string(cust.under-pct,">>9%")).

assign
    sold[5] = trim(string(xquo.sold-no))
    ship[5] = trim(string(xquo.ship-id))
    bill[5] = trim(string(xquo.cust-no)).

do i = 1 to 4:
    ASSIGN 
        sold[i] = xquo.soldto[i]
        ship[i] = xquo.shipto[i]
        bill[i] = xquo.billto[i].
end.

if (xquo.shipto[1] eq xquo.soldto[1] and
    xquo.shipto[2] eq xquo.soldto[2] and
    xquo.shipto[3] eq xquo.soldto[3] and
    xquo.shipto[4] eq xquo.soldto[4]) then
    assign
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

if (not ch-multi) then 
do:

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
      
        IF first-of(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
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

    {cec/quote/quoxpnt11.i}

    {cec/quote/quoxprnt11.i 1}
    v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].
        
    IF LINE-COUNTER > PAGE-SIZE - 1 THEN 
    DO:      
        v-printline = 0.
        page.  
        {cec/quote/quoxpnt11.i}
    END.

    PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " .
      
    ASSIGN
        v-tmp-lines = 0
        li-cline    = 1.
    do i = 1 to 5:
        if xquo.comment[i] ne "" THEN 
        DO: 
            put "<C1><R" string(58 + li-cline,">9") + "><C6>" xquo.comment[i]. 
            li-cline = li-cline + 1.
        END.
    end.

    v-printline = v-printline + 6.
    IF v-printline < 50 THEN PAGE.

    release est.
    if v-prt-box then 
    DO:
        find first est
            where est.company eq xquo.company
            and est.loc     eq xquo.loc
            and est.est-no  eq xquo.est-no
            no-lock no-error.
        if available est then 
        do:
            PAGE.
            PUT "<FCouriar New>" SKIP.
            FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
            run cec/desprnt2.p (?,
                input-output v-lines,
                recid(xest)).
        end.
    END.
end.

else 
do:

    for each report where report.term-id eq v-term-id,
        first xquo  where recid(xquo) eq report.rec-id
        no-lock
        break by report.key-01
        by report.key-02
        by report.key-03
        transaction:

        find first est
            where est.company eq xquo.company
            AND est.est-no  EQ xquo.est-no
            no-lock no-error.
        find first sman
            where sman.company eq cocode
            and sman.sman    eq xquo.sman
            no-lock no-error.
        find first carrier
            where carrier.company eq cocode
            and carrier.carrier eq xquo.carrier
            no-lock no-error.
        find first terms
            where terms.company eq cocode
            and terms.t-code  eq xquo.terms
            no-lock no-error.
        find first cust
            where cust.company eq xquo.company
            and cust.cust-no eq xquo.cust-no
            no-lock no-error.

        if available cust then
            v-over-under = trim(string(cust.over-pct,">>9%")) + "-" +
                trim(string(cust.under-pct,">>9%")).


        assign
            sold[5] = trim(string(xquo.sold-no))
            ship[5] = trim(string(xquo.ship-id))
            bill[5] = trim(string(xquo.cust-no)).

        do i = 1 to 4:
            assign
                sold[i] = xquo.soldto[i]
                ship[i] = xquo.shipto[i]
                bill[i] = xquo.billto[i].
        end.
    
        IF s-sep-page OR first(report.key-01) THEN
            ASSIGN lv-first-qreckey = xquo.rec_key
                lv-first-qrecid  = recid(xquo).

        if s-sep-page OR first-of(report.key-01) then 
        do:      
            v-printline = 0.
            IF NOT first(report.key-01) THEN page.
            ASSIGN
                v-first-q-no = xquo.q-no
                lv-tot-pg    = 1
                ln-cnt       = 0.
            /* get total page number */
            FOR EACH bf-report where bf-report.term-id eq v-term-id
                AND bf-report.key-01 eq report.key-01,
                first bf-quo  where recid(bf-quo) eq bf-report.rec-id,
                EACH xqitm OF bf-quo NO-LOCK
                BREAK BY bf-quo.q-no by xqitm.part-no: 
          
                lv-itm-ln-cnt = lv-itm-ln-cnt + 1.
                IF last-OF(bf-quo.q-no) THEN 
                do:
                    ASSIGN 
                        ln-cnt = ln-cnt + lv-itm-ln-cnt * 5.
                    lv-itm-ln-cnt = 0.              
                END.
            END.     

            FOR EACH bf-report where bf-report.term-id eq v-term-id
                AND bf-report.key-01 = report.key-01,
                first bf-quo  where recid(bf-quo) eq bf-report.rec-id,
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

            FOR EACH bf-report where bf-report.term-id eq v-term-id
                AND bf-report.key-01 = report.key-01,
                first bf-quo  where recid(bf-quo) eq bf-report.rec-id,
                EACH xqchg OF bf-quo WHERE xqchg.qty  EQ 0 AND 
                xqchg.line EQ 0 NO-LOCK
                BREAK BY xqchg.charge:
                IF FIRST(xqchg.charge) THEN ln-cnt = ln-cnt + 1.
            END.

            IF s-print-comp THEN 
            DO :      /* Print components of a set */
                FOR EACH bf-report where bf-report.term-id eq v-term-id
                    AND bf-report.key-01 = report.key-01 NO-LOCK,
                    first bf-quo  where recid(bf-quo) eq bf-report.rec-id NO-LOCK,
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
                FOR EACH bf-report where bf-report.term-id eq v-term-id
                    AND bf-report.key-01 = report.key-01,
                    first bf-quo  where recid(bf-quo) eq bf-report.rec-id,
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
            {cec/quote/quoxpnt11.i}
        end.

        ASSIGN
            v-last       = last-of(report.key-01)
            v-line-total = 0.

        {cec/quote/quoxprnt11.i 2}  
        v-quo-total = v-line-total + v-t-tax[1] + v-t-tax[2] + v-t-tax[3].

        IF LINE-COUNTER > PAGE-SIZE - 1 THEN 
        DO:      
            v-printline = 0.
            page.  
            {cec/quote/quoxpnt11.i}
        END.

        IF (ch-multi and (v-last OR s-sep-page)) then 
        do:
            PUT "<FArial><R58><C1><P12><B> Comments </B> <P9> " .

            FIND bf-quo WHERE RECID(bf-quo) = lv-first-qrecid NO-LOCK NO-ERROR. 
            li-cline = 0.
            do i = 1 to 5:      
                if bf-quo.comment[i] ne "" then 
                DO:
                    li-cline = li-cline + 1.
                    put "<C1><R" string(58 + li-cline,">9") + "><C6>" bf-quo.comment[i]  .            
                END.
            
            end.
            v-printline = v-printline + 6.
            IF v-printline < 50 THEN 
            DO:
                page.
            END.

            if v-prt-box then 
            DO:
                find first est where est.company eq xquo.company
                    and est.loc     eq xquo.loc
                    and est.est-no  eq xquo.est-no
                    no-lock no-error.
                if available est then 
                do:
       
                    PAGE.
                    PUT "<FCouriar New>" SKIP.
                    FIND FIRST xest WHERE RECID(xest) = RECID(est) NO-LOCK NO-ERROR.
                    run cec/desprnt2.p (?,
                        input-output v-lines,
                        recid(xest)).
                end.
            END.
        END.  /*ch-multi and v-last */
   
    end. /* for each report */
  
end.  /* multi */

PROCEDURE printHeader:
    DEFINE INPUT PARAMETER ipPageOffSet AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opInitVar AS INTEGER NO-UNDO.

    IF LINE-COUNTER > PAGE-SIZE - ipPageOffSet THEN 
    DO:
        PAGE.
        {cec/quote/quoxpnt11.i}
        opInitVar = 0.
        PUT SKIP .
    END.
END PROCEDURE.
/* end ---------------------------------- copr. 2000  advanced software, inc. */
