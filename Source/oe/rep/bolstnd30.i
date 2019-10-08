/* ---------------------------------------------- oe/rep/bolxprt30.i           */
/* PRINT detail                                                               */
/* -------------------------------------------------------------------------- */

v-tot-cases = 0.

FOR EACH tt-boll,
      
    FIRST itemfg
    WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ tt-boll.i-no
    NO-LOCK

    BREAK BY ( IF v-sort THEN  tt-boll.i-no ELSE "")
    BY ( IF NOT v-sort THEN tt-boll.job-no + STRING(tt-boll.job-no2) ELSE "")
    BY tt-boll.po-no
    BY tt-boll.ord-no
    BY tt-boll.line
    BY tt-boll.cases DESCENDING:
    IF ll-consol-bolls THEN 
    DO:
        {oe/rep/bolstnd23.i}
    END.
    ELSE 
    DO:
        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ cocode
            AND oe-ordl.ord-no  EQ tt-boll.ord-no
            AND oe-ordl.i-no    EQ tt-boll.i-no
            AND oe-ordl.line    EQ tt-boll.line
            NO-LOCK NO-ERROR.

        FIND FIRST oe-ord
            WHERE oe-ord.company EQ cocode
            AND oe-ord.ord-no  EQ tt-boll.ord-no
            NO-LOCK NO-ERROR.

        IF v-printline >= 48 THEN 
        DO:
            v-printline = 0.
            PAGE {1}.
            {oe/rep/bolstnd220.i}
        END.

        IF tt-boll.qty-case NE 0 AND tt-boll.cases NE 0 THEN 
        DO:
            FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.qty-case NO-ERROR.
            IF NOT AVAILABLE w2 THEN CREATE w2.
            ASSIGN
                w2.cas-cnt = tt-boll.qty-case
                w2.cases   = w2.cases + tt-boll.cases.
        END.

        IF tt-boll.partial NE 0 THEN 
        DO:
            FIND FIRST w2 WHERE w2.cas-cnt EQ tt-boll.partial NO-ERROR.
            IF NOT AVAILABLE w2 THEN CREATE w2.
            ASSIGN
                w2.cas-cnt = tt-boll.partial
                w2.cases   = w2.cases + 1.
        END.

        v-lines = 0.
        FOR EACH w2 BREAK BY w2.cases:
            v-lines = v-lines + 1.
        END. 
  
        DO i = v-lines + 1 TO 4:
            ASSIGN
                v-part-dscr = ""
                v-job-po    = "".

            IF i EQ 1 THEN
                ASSIGN
                    v-part-dscr = oe-ordl.part-no
                    v-job-po    = tt-boll.po-no.

            ELSE
                IF i EQ 2 THEN
                    ASSIGN
                        v-part-dscr = oe-ordl.i-name
                        v-job-po    = IF oe-ordl.job-no EQ "" THEN "" ELSE
                    (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
    
                ELSE
                    IF i EQ 3 THEN v-part-dscr = oe-ordl.part-dscr1.

                    ELSE
                        IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr2.
    
            IF v-part-dscr NE "" OR v-job-po NE "" OR i LE 2 THEN v-lines = v-lines + 1.
        END.
  
        v-lines = v-lines + 1.
  
        i = 0.
        FOR EACH w2 BREAK BY w2.cases:
            i = i + 1.

            ASSIGN
                v-part-dscr = ""
                v-job-po    = "".

            IF i EQ 1 THEN
                ASSIGN
                    v-part-dscr = oe-ordl.part-no
                    v-job-po    = tt-boll.po-no.

            ELSE
                IF i EQ 2 THEN
                    ASSIGN
                        v-part-dscr = oe-ordl.i-name
                        v-job-po    = IF oe-ordl.job-no EQ "" THEN "" ELSE
                    (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

                ELSE IF i EQ 3 THEN v-part-dscr = oe-ordl.part-dscr1.

                    ELSE IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr2.

            DISPLAY TRIM(STRING(oe-ordl.qty,"->>,>>>,>>>")) 
                WHEN i EQ 1
                @ oe-ordl.i-no
                /*oe-ordl.i-no                            when i eq 2*/
                v-job-po
                v-part-dscr
                w2.cases
                w2.cas-cnt
                tt-boll.qty                             
                WHEN LAST(w2.cases)
                tt-boll.p-c                             
                WHEN LAST(w2.cases)         
                WITH FRAME bol-mid2.
            DOWN  WITH FRAME bol-mid2.    
            v-printline = v-printline + 1.
            IF v-printline >= 48 THEN 
            DO:
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bolstnd220.i}
            END.
            v-tot-cases = v-tot-cases + w2.cases.

            DELETE w2.    
        END. /* each w2 */

        IF i < 4 THEN
        DO i = i + 1 TO 4:
            CLEAR FRAME bol-mid2 NO-PAUSE.

            ASSIGN
                v-part-dscr = ""
                v-job-po    = "".

            IF i EQ 1 THEN
                ASSIGN
                    v-part-dscr = oe-ordl.part-no
                    v-job-po    = tt-boll.po-no.

            ELSE
                IF i EQ 2 THEN
                    ASSIGN
                        v-part-dscr = oe-ordl.i-name
                        v-job-po    = IF oe-ordl.job-no EQ "" THEN "" ELSE
                    (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

                ELSE
                    IF i EQ 3 THEN v-part-dscr = oe-ordl.part-dscr1.

                    ELSE
                        IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr2.
    
            IF i = 2 AND v-job-po = "" THEN
                v-job-po = IF tt-boll.job-no EQ "" THEN "" ELSE
                    (TRIM(tt-boll.job-no) + "-" + string(tt-boll.job-no2,"99"))                 .

            IF v-part-dscr NE "" OR v-job-po NE "" OR i LE 2 THEN 
            DO:
                IF v-printline >= 48 THEN 
                DO:
                    v-printline = 0.
                    PAGE {1}.
                    {oe/rep/bolstnd220.i}
                END.
                DISPLAY {1}
                    /*oe-ordl.i-no                            when i eq 2*/
                    v-job-po
                    v-part-dscr              
                    WITH FRAME bol-mid2.
                DOWN {1} WITH FRAME bol-mid2. 
                v-printline = v-printline + 1.
            END.
        END.
  
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

                IF v-printline >= 48 THEN 
                DO:
                    v-printline = 0.
                    PAGE {1}.
                    {oe/rep/bolstnd220.i}
                END.

                DISPLAY {1}
                    TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>")) 
                    @ oe-ordl.i-no
                    b-itemfg.part-no                        @ v-part-dscr
                    tt-boll.qty * v-part-qty                @ tt-boll.qty        
                    WITH FRAME bol-mid2.
                DOWN {1} WITH FRAME bol-mid2.
                v-printline = v-printline + 1.
                DISPLAY {1}
                    fg-set.part-no                          @ oe-ordl.i-no
                    v-job-po
                    b-itemfg.i-name                         @ v-part-dscr
                    WITH FRAME bol-mid2.
                DOWN {1} WITH FRAME bol-mid2.
    
                PUT {1} SKIP(1).
                v-printline = v-printline + 2.
            END.
    END. /* else */
END. /* for each tt-boll */

v-tot-wt = oe-bolh.tot-wt.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
