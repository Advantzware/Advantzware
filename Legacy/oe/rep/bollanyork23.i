/* oe/rep/bollanyork23.i*/
DEFINE VARIABLE cBolFormat AS CHARACTER NO-UNDO .

IF FIRST-OF(tt-boll.LINE) THEN 
DO:
    
    RUN sys/ref/nk1look.p (INPUT cocode, "BOLFMT", "C" /* Logical */, YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, oe-bolh.cust-no /* cust */, oe-bolh.ship-id /* ship-to*/,
        OUTPUT cRtnChar, OUTPUT lRecFound).
    ASSIGN 
        cBolFormat = cRtnChar .
         
    FOR EACH w2.
        DELETE w2.
    END.
    ASSIGN
        i              = 0
        v-tot-case-qty = 0.

    FOR EACH bf-ttboll WHERE bf-ttboll.i-no = tt-boll.i-no
        AND bf-ttboll.po-no = tt-boll.po-no
        AND bf-ttboll.ord-no = tt-boll.ord-no
        AND bf-ttboll.LINE = tt-boll.LINE
        BREAK BY bf-ttboll.cases DESCENDING.
        FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ tt-boll.ord-no
            AND oe-ordl.i-no    EQ tt-boll.i-no
            AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.

        FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ tt-boll.ord-no NO-LOCK NO-ERROR.
        ASSIGN
            v-tot-case-qty = v-tot-case-qty + bf-ttboll.qty
            i              = i + 1.
        FIND FIRST w2 WHERE w2.cas-cnt EQ bf-ttboll.qty-case NO-ERROR.
        IF NOT AVAILABLE w2 THEN CREATE w2.
        ASSIGN 
            w2.job-po  = ""
            w2.i-no    = ""
            w2.cas-cnt = bf-ttboll.qty-case
            w2.cases   = w2.cases + bf-ttboll.cases
            w2.rec-id  = RECID(bf-ttboll).

        IF i = 1 THEN ASSIGN w2.job-po = bf-ttboll.po-no
                w2.dscr   = oe-ordl.part-no
                w2.qty    = oe-ordl.qty.
        ELSE IF i = 2 THEN 
                ASSIGN w2.job-po = IF oe-ordl.job-no EQ "" THEN "" ELSE
                             (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))
                    w2.dscr   = oe-ordl.i-no
                    w2.i-no   = oe-ordl.i-no.
            ELSE IF i EQ 3 THEN ASSIGN w2.dscr = oe-ordl.part-dscr1.
                ELSE IF i EQ 4 THEN ASSIGN w2.dscr = oe-ordl.part-dscr2.

    END.
    IF i < 4 THEN 
    DO i = i TO 4:
        CREATE w2.
    END.
    i = 0.
    FOR EACH w2  BREAK BY w2.cases DESCENDING:
        FIND FIRST bf-ttboll WHERE RECID(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.    
        i = i + 1.
        IF w2.rec-id = ? THEN 
        DO:
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ tt-boll.ord-no
                AND oe-ordl.i-no    EQ tt-boll.i-no
                AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.
            w2.i-no = "".
            IF i = 2 THEN 
                ASSIGN w2.job-po = IF oe-ordl.job-no EQ "" THEN "" ELSE
                             (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))
                    w2.dscr   = oe-ordl.i-no
                    w2.i-no   = oe-ordl.i-no.
            ELSE IF i EQ 3 THEN ASSIGN w2.dscr = oe-ordl.part-dscr1.
                ELSE IF i EQ 4 THEN ASSIGN w2.dscr = oe-ordl.part-dscr2.
        END.
        IF w2.qty = 0 AND w2.i-no = "" AND w2.dscr = "" AND w2.cas-cnt = 0 THEN DELETE w2.
    END.
    i = 0.
    RUN get_lot_no. 
    FOR EACH w2  BREAK BY w2.cases DESCENDING:
        FIND FIRST bf-ttboll WHERE RECID(bf-ttboll) = w2.rec-id NO-LOCK NO-ERROR.    
        i = i + 1.

        IF cBolFormat = "bolfmt 20" THEN 
        DO:
            IF v-printline >= 48 THEN 
            DO:
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bollanyork2.i}
            END.
        END. /* bolfmt20*/
        ELSE 
        DO:
            IF v-printline >= 48 THEN 
            DO:
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bollanyork22.i}
            END.
        END.

        IF w2.rec-id = ? THEN 
        DO:
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ cocode
                AND oe-ordl.ord-no  EQ tt-boll.ord-no
                AND oe-ordl.i-no    EQ tt-boll.i-no
                AND oe-ordl.line    EQ tt-boll.LINE NO-LOCK NO-ERROR.
            w2.i-no = "".
            IF i = 2 THEN 
                ASSIGN w2.job-po = IF oe-ordl.job-no EQ "" THEN "" ELSE
                             (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99"))
                    w2.dscr   = oe-ordl.i-no
                    w2.i-no   = oe-ordl.i-no.
            ELSE IF i EQ 3 THEN ASSIGN w2.dscr = oe-ordl.part-dscr1.
                ELSE IF i EQ 4 THEN ASSIGN w2.dscr = oe-ordl.part-dscr2.
        END.
        IF AVAILABLE bf-ttboll THEN 
            v-relpc     = IF bf-ttboll.p-c THEN "C" ELSE "P".
        IF w2.qty = 0 AND w2.i-no = "" AND w2.dscr = "" AND NOT last(w2.cases) AND w2.cas-cnt EQ 0 THEN .
        ELSE 
        DO:  
            PUT
                "<C2>" TRIM(STRING(w2.qty,"->>,>>>,>>>")) .
            IF i EQ 1 THEN 
            DO:
                PUT "<C12>" STRING(w2.cases,"->>>9") + " @ "
                    "<C16>" STRING(w2.cas-cnt,">>>,>>>").
            END.
            PUT "<C35>" w2.job-po FORMAT "x(15)" .
            .
            IF i EQ 1 THEN
                PUT "<C48>"  oe-ordl.ord-no .
            PUT "<C61>" w2.dscr FORMAT "x(23)" SKIP.
            v-printline = v-printline + 1.
        END.
    
        v-tot-cases = v-tot-cases + w2.cases.

    /*delete w2. */
    END. /* each w2 */

    PUT {1} SKIP(1).
    v-printline = v-printline + 1. 
    tt-boll.printed = YES.
  
    IF v-print-components AND itemfg.alloc NE YES THEN
        FOR EACH fg-set
            WHERE fg-set.company EQ cocode
            AND fg-set.set-no  EQ tt-boll.i-no
            NO-LOCK,
      
            FIRST b-itemfg
            WHERE b-itemfg.company EQ cocode
            AND b-itemfg.i-no    EQ fg-set.part-no
            NO-LOCK
      
            BREAK BY fg-set.set-no:
      
            {sys/inc/part-qty.i v-part-qty fg-set}

            IF cBolFormat = "bolfmt 20" THEN 
            DO:
                IF v-printline >= 48 THEN 
                DO:
                    v-printline = 0.
                    PAGE {1}.
                    {oe/rep/bollanyork2.i}
                END.
            END. /* bolfmt20*/
            ELSE 
            DO:
                IF v-printline >= 48 THEN 
                DO:
                    v-printline = 0.
                    PAGE {1}.
                    {oe/rep/bollanyork22.i}
                END.
            END.

            PUT
                "<C2>" TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>"))
                "<C24>" tt-boll.qty * v-part-qty 
                "<C61>" b-itemfg.part-no   FORMAT "x(23)" SKIP.

            v-printline = v-printline + 1.
            PUT
                "<C2>" fg-set.part-no 
                "<C35>" v-job-po FORMAT "x(15)"
                "<C61>" b-itemfg.i-name FORMAT "x(23)" SKIP.
    
            PUT {1} SKIP(1).
            v-printline = v-printline + 2.
        END.
END. /* first-of(tt-boll.line) */

