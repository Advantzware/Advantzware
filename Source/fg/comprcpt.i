PROCEDURE processComponent:    
    /* Using b-itemfg of the component, fg-rctd of the set */
    DEFINE INPUT  PARAMETER ipcRitaCode     AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER iplIsAdjustment AS LOGICAL   NO-UNDO.
    
    DEFINE VARIABLE v-set-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-cost         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ldQty          AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iQtyFactor     AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
    DEFINE BUFFER b-w-fg-rctd FOR fg-rctd.
                
    EMPTY TEMP-TABLE tt-bin.
    
    iQtyFactor = IF iplIsAdjustment THEN -1 ELSE 1.
            
    v-set-qty = fg-rctd.t-qty * tt-fg-set.part-qty-dec.

    /* Detect if this was a delete and pull in the bins related to the original receipt */
    /* Taking t-qty lt 0 to indicate that this is coming from delete receipt screen,    */
    /* but could be delected through an input parameter if that is preferred            */
    IF fg-rctd.t-qty LT 0 THEN 
    DO:
 
        FIND FIRST b-fg-rctd 
            WHERE b-fg-rctd.company EQ fg-rctd.company
            AND b-fg-rctd.tag     EQ fg-rctd.tag
            AND b-fg-rctd.t-qty   GT 0
            NO-LOCK NO-ERROR.

        IF AVAILABLE b-fg-rctd THEN 
        DO:

            FOR EACH fg-rcpts
                WHERE fg-rcpts.company   EQ b-fg-rctd.company
                AND fg-rcpts.linker    EQ "fg-rctd: " + STRING(b-fg-rctd.r-no,"9999999999")
                AND fg-rcpts.i-no      EQ b-itemfg.i-no
                AND fg-rcpts.rita-code NE "set"
                NO-LOCK,
                EACH bf-fg-rctd
                WHERE bf-fg-rctd.company EQ fg-rcpts.company
                AND bf-fg-rctd.i-no    EQ b-itemfg.i-no
                AND bf-fg-rctd.r-no    EQ fg-rcpts.r-no
                AND bf-fg-rctd.rita-code EQ "P"
                AND bf-fg-rctd.qty     LT 0
                NO-LOCK:
        
                FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no EQ b-itemfg.i-no
                    AND fg-bin.tag  EQ bf-fg-rctd.tag
                    NO-LOCK NO-ERROR.
                IF AVAILABLE fg-bin THEN 
                DO: 
                    CREATE tt-bin.
                    ASSIGN 
                        tt-bin.tt-bin-row = ROWID(fg-bin).
            
                END. /* avail bin */
                ELSE 
                DO:
                    CREATE tt-bin.
                    ASSIGN 
                        tt-bin.tt-bin-row = ROWID(bf-fg-rctd).
                END.

            END. /* each fg-rcpts */

        END. /* avail(b-fg-rctd) */

        /* Same code as below but using specific bins, so v-set-qty may be reduced */
        FOR EACH tt-bin,
            FIRST fg-bin WHERE ROWID(fg-bin) EQ tt-bin.tt-bin-row
            NO-LOCK
            BY fg-bin.qty:
    
            {fg/fg-post2.i b- fg-bin.qty ipcRitaCode iQtyFactor}
        END.

        IF v-set-qty GT 0 THEN 
        DO:
            FOR EACH tt-bin,
                FIRST bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ tt-bin.tt-bin-row
                NO-LOCK 
                BY bf-fg-rctd.qty:
        /* 12181308 - searching for original matching transactions, per Joe, a bin existing should */
        /*            not be a requirement, therefore, we may create the fg-post2.i records via    */
        /*            bf-fg-rctd instead of fg-bin.                                                */
       
                {fg/fg-post2a.i b- ipcRitaCode iQtyFactor}
            END.
        END.
    END. /* if t-qty lt 0 */        
    /* End code for delete of receipt */

    IF v-set-qty GT 0 OR (v-set-qty LT 0 AND fg-rctd.t-qty LT 0) THEN 
    DO:

       
         IF fg-rctd.use-job THEN 
        DO:      
            IF lFGSetAssembly THEN 
            DO:              
                /* Check for Assembly bin if required */

                FOR EACH fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ b-itemfg.i-no
                    AND fg-bin.job-no  EQ fg-rctd.job-no
                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                    AND fg-bin.loc     EQ fg-rctd.loc
                    AND fg-bin.loc-bin EQ cFGSetAssembly
                    AND fg-bin.qty     GT 0
                    NO-LOCK BY fg-bin.rec_key:     

                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                    IF NOT ldQty GT 0 THEN
                        NEXT.

                    {fg/fg-post2.i b- ldQty ipcRitaCode iQtyFactor}
                END. 
            END.
            IF v-set-qty GT 0 OR (v-set-qty LT 0 AND fg-rctd.t-qty LT 0) THEN 
            DO:              
                FOR EACH fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ b-itemfg.i-no 
                    AND fg-bin.job-no  EQ fg-rctd.job-no
                    AND fg-bin.job-no2 EQ fg-rctd.job-no2
                    AND fg-bin.qty     GT 0
                    NO-LOCK BY fg-bin.rec_key:           
                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                    
                    IF NOT ldQty GT 0 THEN
                        NEXT.
                        
                    {fg/fg-post2.i b- ldQty ipcRitaCode iQtyFactor}
                END. 
            END.


        END. /* Avail use-job */
        ELSE 
        DO:
            IF lFGSetAssembly THEN 
            DO:          

                /* Check for assembly bin if required */
                FOR EACH fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ b-itemfg.i-no
                    AND fg-bin.job-no  NE ""
                    AND fg-bin.qty     GT 0
                    AND fg-bin.loc     EQ fg-rctd.loc
                    AND fg-bin.loc-bin EQ cFGSetAssembly
                    NO-LOCK BY fg-bin.rec_key:     
                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                    IF NOT ldQty GT 0 THEN
                        NEXT.
                    {fg/fg-post2.i b- ldQty ipcRitaCode iQtyFactor}
                END.  
            END.
    
            IF v-set-qty GT 0 THEN
                FOR EACH fg-bin
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ b-itemfg.i-no
                    AND fg-bin.job-no  EQ ""
                    AND fg-bin.qty     GT 0
                    AND fg-bin.loc     EQ fg-rctd.loc
                    AND fg-bin.loc-bin EQ cFGSetAssembly
                    NO-LOCK BY fg-bin.rec_key:                  
                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).

                    IF NOT ldQty GT 0 THEN
                        NEXT.
                    {fg/fg-post2.i b- ldQty ipcRitaCode iQtyFactor}
                END.

        END. /* if fgsetassembly */

        /* Normal check for non-assembly bins */
        IF v-set-qty GT 0 THEN
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ b-itemfg.i-no
                AND fg-bin.job-no  NE ""
                AND fg-bin.qty     GT 0
                NO-LOCK BY fg-bin.rec_key:

                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).

                IF NOT ldQty GT 0 THEN
                    NEXT.
                {fg/fg-post2.i b- ldQty ipcRitaCode iQtyFactor}
            END.  

        IF v-set-qty GT 0 THEN
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ b-itemfg.i-no
                AND fg-bin.job-no  EQ ""
                AND fg-bin.qty     GT 0
                NO-LOCK BY fg-bin.rec_key:
                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).

                IF NOT ldQty GT 0 THEN
                    NEXT.
                {fg/fg-post2.i b- ldQty ipcRitaCode iQtyFactor}

            END.
            
        IF v-set-qty GT 0 THEN
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ b-itemfg.i-no
                NO-LOCK BY fg-bin.rec_key:     
                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                
                IF NOT ldQty GT 0 THEN
                    NEXT.
                {fg/fg-post2.i b- ldQty ipcRitaCode iQtyFactor}
    
            END.

    END.
    v-cost = fg-rctd.ext-cost / fg-rctd.t-qty.

    FIND CURRENT fg-rctd EXCLUSIVE-LOCK NO-ERROR.
    IF DYNAMIC-FUNCTION("Conv_IsEAUOM", fg-rctd.company, fg-rctd.i-no, fg-rctd.cost-uom) THEN 
        fg-rctd.std-cost = v-cost.
    ELSE
        RUN sys/ref/convcuom.p("EA", fg-rctd.cost-uom, 0, 0, 0, 0,
            v-cost, OUTPUT fg-rctd.std-cost).
        
END. 

PROCEDURE bin-qty-used:
    DEFINE INPUT PARAMETER iprBinRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opdQty AS DECIMAL NO-UNDO.
    DEFINE BUFFER bf-fg-bin  FOR fg-bin.
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.
    DEFINE VARIABLE ldQty AS DECIMAL NO-UNDO.
  
    FIND FIRST bf-fg-bin WHERE ROWID(bf-fg-bin) EQ iprBinRow
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-fg-bin THEN
        RETURN.

    /* Subtract from bin qty the quantity of previous tags used */
    ldQty = bf-fg-bin.qty.
    FOR EACH bf-fg-rctd
        WHERE bf-fg-rctd.company EQ bf-fg-bin.company
        AND bf-fg-rctd.i-no EQ bf-fg-bin.i-no
        AND bf-fg-rctd.job-no EQ bf-fg-bin.job-no
        AND bf-fg-rctd.job-no2 EQ bf-fg-bin.job-no2
        AND bf-fg-rctd.loc     EQ bf-fg-bin.loc
        AND bf-fg-rctd.loc-bin EQ bf-fg-bin.loc-bin
        AND bf-fg-rctd.tag     EQ bf-fg-bin.tag
        AND bf-fg-rctd.rita-code EQ fg-rctd.rita-code
        AND bf-fg-rctd.t-qty LT 0
        NO-LOCK:
    
        /* These component records will be negative so add them to reduce total */
        ldQty = ldQty + bf-fg-rctd.t-qty.
    END.
  
    opdQty = ldQty.

END.
