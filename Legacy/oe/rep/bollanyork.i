/* ---------------------------------------------- oe/rep/bollanyork.i           */
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
        {oe/rep/bollanyork23.i}
    END.
    ELSE 
    DO:   
        RUN get_lot_no.
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
            {oe/rep/bollanyork2.i}
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
                    v-part-dscr = oe-ordl.i-no
                    v-job-po    = tt-boll.po-no.

            ELSE
                IF i EQ 2 THEN
                    ASSIGN
                        v-part-dscr = oe-ordl.part-no
                        v-job-po    = IF oe-ordl.job-no EQ "" THEN "" ELSE
                    (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).
    
                ELSE
                    IF i EQ 3 THEN v-part-dscr = oe-ordl.i-name.

                    ELSE
                        IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr1.
    
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
                    v-part-dscr = oe-ordl.i-no
                    v-job-po    = tt-boll.po-no.

            ELSE
                IF i EQ 2 THEN
                    ASSIGN
                        v-part-dscr = oe-ordl.part-no
                        v-job-po    = IF oe-ordl.job-no EQ "" THEN "" ELSE
                    (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

                ELSE IF i EQ 3 THEN v-part-dscr = oe-ordl.i-name.

                    ELSE IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr1.
            v-relpc     = IF tt-boll.p-c THEN "C" ELSE "P".

            PUT
                "<C2>" TRIM(STRING(oe-ordl.qty,"->>,>>>,>>>")) 
                "<C11>" STRING(w2.cases,"->>>9") + " @ "
                "<C17>" STRING(w2.cas-cnt,">>>>>>")
                "<C24>" STRING(tt-boll.qty) + " [" + STRING(v-relpc) + "]"
                "<C35>" v-job-po FORMAT "x(15)".
            IF i EQ 1 THEN
                PUT "<C48>" oe-ordl.ord-no  .
            PUT "<C61>" v-part-dscr FORMAT "x(23)" SKIP.
          
         
            v-printline = v-printline + 1.
            IF v-printline >= 48 THEN 
            DO:
                v-printline = 0.
                PAGE {1}.
                {oe/rep/bollanyork2.i}
            END.
            v-tot-cases = v-tot-cases + w2.cases.

            DELETE w2.    
        END. /* each w2 */
        IF i < 4 THEN
        DO i = i + 1 TO 4:
            /*clear frame bol-mid2 no-pause.*/

            ASSIGN
                v-part-dscr = ""
                v-job-po    = "".

            IF i EQ 1 THEN
                ASSIGN
                    v-part-dscr = oe-ordl.i-no
                    v-job-po    = tt-boll.po-no.

            ELSE
                IF i EQ 2 THEN
                    ASSIGN
                        v-part-dscr = oe-ordl.part-no
                        v-job-po    = IF oe-ordl.job-no EQ "" THEN "" ELSE
                    (TRIM(oe-ordl.job-no) + "-" + string(oe-ordl.job-no2,"99")).

                ELSE
                    IF i EQ 3 THEN v-part-dscr = oe-ordl.i-name .

                    ELSE
                        IF i EQ 4 THEN v-part-dscr = oe-ordl.part-dscr1.
    
            IF i = 2 AND v-job-po = "" THEN
                v-job-po = IF tt-boll.job-no EQ "" THEN "" ELSE
                    (TRIM(tt-boll.job-no) + "-" + string(tt-boll.job-no2,"99"))                 .

            IF v-part-dscr NE "" OR v-job-po NE "" OR i LE 2 OR v-lot# NE "" THEN 
            DO:
                IF v-printline >= 48 THEN 
                DO:
                    v-printline = 0.
                    PAGE {1}.
                    {oe/rep/bollanyork2.i}
                END.
                    IF i = 2 THEN
                        PUT "<C48>" v-lot# FORMAT "x(20)".
                   PUT "<C61>" v-part-dscr FORMAT "x(23)" SKIP.
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
                    {oe/rep/bollanyork2.i}
                END.
   
                PUT
                    "<C2>" TRIM(STRING(oe-ordl.qty * v-part-qty,">>>,>>>,>>>"))  
                    "<C24>" tt-boll.qty * v-part-qty
                    "<C61>" b-itemfg.part-no FORMAT "x(23)"  SKIP.
                v-printline = v-printline + 1.

                PUT
                    "<C2>" fg-set.part-no 
                    "<C36>" v-job-po FORMAT "x(15)"
                    "<C61>" b-itemfg.i-name FORMAT "x(23)" SKIP.
                PUT {1} SKIP(1).
                v-printline = v-printline + 2.
            END.
    END. /* else */
END. /* for each tt-boll */

v-tot-wt = oe-bolh.tot-wt.

/* end ---------------------------------- copr. 1998  Advanced Software, Inc. */
