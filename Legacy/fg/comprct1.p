
DEFINE INPUT PARAMETER ip-rowid AS ROWID NO-UNDO.

{sys/inc/var.i SHARED}

{fg/fullset.i NEW}

DEFINE BUFFER b-fg-rctd   FOR fg-rctd.
DEFINE BUFFER bf-fg-rctd  FOR fg-rctd.

DEFINE BUFFER b-w-fg-rctd FOR fg-rctd.
DEFINE BUFFER b-itemfg    FOR itemfg.
DEFINE BUFFER use-job     FOR reftable.
DEFINE BUFFER b-fg-rcpts  FOR fg-rcpts.

DEFINE VARIABLE li-max-qty     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-part-qty     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-set-qty      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-cost         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldQty          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE li             AS INTEGER   NO-UNDO.
DEFINE VARIABLE fg-uom-list    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lFGSetAssembly AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cFGSetAssembly AS CHARACTER NO-UNDO.
DEFINE VARIABLE lGetBin        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llDelRecs      AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "FGSetAssembly",
    INPUT "L",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cFGSetAssembly,
    OUTPUT lFound).
IF lFound THEN
    lFGSetAssembly = cFGSetAssembly EQ "YES".
RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "FGSetAssembly",
    INPUT "C",
    INPUT NO,
    INPUT NO,
    INPUT "",
    INPUT "",
    OUTPUT cFGSetAssembly,
    OUTPUT lFound).

DEFINE TEMP-TABLE tt-bin 
    FIELD tt-bin-row AS ROWID .

DEFINE TEMP-TABLE tt-del-fg-rctd
    FIELD fg-rctd-row AS ROWID.

DEFINE TEMP-TABLE tt-del-fg-rcpts
    FIELD fg-rcpts-row AS ROWID.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
  
FIND fg-rctd WHERE ROWID(fg-rctd) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAILABLE fg-rctd THEN
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
        NO-LOCK NO-ERROR.

IF AVAILABLE itemfg                                    
    AND itemfg.isaset
    AND itemfg.alloc NE YES    /*any assembled set*/
    AND fg-rctd.rita-code EQ "R"                       
    AND NOT CAN-FIND(FIRST fg-rcpts
    WHERE fg-rcpts.company   EQ fg-rctd.company
    AND fg-rcpts.linker    EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
    AND fg-rcpts.rita-code EQ "set")
    THEN 
DO:
  
    li-max-qty = fg-rctd.t-qty.
            
    RUN fg/checkset.w (?, ROWID(fg-rctd), fg-rctd.job-no, fg-rctd.job-no2,
        fg-rctd.loc, INPUT-OUTPUT li-max-qty).
  
    IF li-max-qty GE fg-rctd.t-qty THEN 
    DO:
      
        IF itemfg.alloc EQ ?  /*assembled w/part receipts*/    AND
            CAN-FIND(FIRST fg-rcpts
            WHERE fg-rcpts.company EQ fg-rctd.company
            AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")) THEN 
        DO:
    
            RUN fg/fullset.p (ROWID(itemfg)). 
            
            EMPTY TEMP-TABLE tt-del-fg-rctd.
            EMPTY TEMP-TABLE tt-del-fg-rcpts.
            DEFINE BUFFER bf-fg-rcpts FOR fg-rcpts.
            DEFINE BUFFER bf2-fg-rctd FOR fg-rctd.
            
            FOR EACH bf-fg-rcpts
                WHERE bf-fg-rcpts.company   EQ fg-rctd.company
                AND bf-fg-rcpts.linker    EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
                AND bf-fg-rcpts.rita-code NE "set"
                NO-LOCK,
                EACH bf2-fg-rctd NO-LOCK WHERE bf2-fg-rctd.company EQ bf-fg-rcpts.company
                AND bf2-fg-rctd.r-no    EQ bf-fg-rcpts.r-no
                USE-INDEX fg-rctd:
                                       
                FIND FIRST b-itemfg WHERE b-itemfg.company EQ fg-rctd.company
                    AND b-itemfg.i-no EQ bf2-fg-rctd.i-no 
                    NO-LOCK NO-ERROR.
                IF b-itemfg.pur-man EQ NO THEN 
                DO:   
                    /* Manufactured components */
                    li = 0.
                    FIND LAST b-w-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
                    IF AVAILABLE b-w-fg-rctd AND b-w-fg-rctd.r-no GT li THEN li = b-w-fg-rctd.r-no.
        
                    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
                    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
        
                    CREATE b-fg-rcpts.
                    BUFFER-COPY bf-fg-rcpts EXCEPT rec_key TO b-fg-rcpts
                        ASSIGN
                        b-fg-rcpts.r-no      = li + 1
                        b-fg-rcpts.rita-code = "set".

                    RELEASE b-fg-rcpts.
             
                    CREATE b-w-fg-rctd.
                    BUFFER-COPY bf2-fg-rctd EXCEPT rec_key TO b-w-fg-rctd
                        ASSIGN
                        b-w-fg-rctd.r-no     = li + 1
                        b-w-fg-rctd.cases    = bf2-fg-rctd.cases * -1
                        b-w-fg-rctd.partial  = bf2-fg-rctd.partial * -1
                        b-w-fg-rctd.t-qty    = bf2-fg-rctd.t-qty * -1
                        b-w-fg-rctd.ext-cost = bf2-fg-rctd.ext-cost * -1
                        b-w-fg-rctd.SetHeaderRno = bf2-fg-rctd.r-no
                        .

                        
                END. /* If pur-man eq NO i.e manufactured */
                ELSE 
                DO:
                    /* Handle purchased componenents similar to normal assembled set componenents */
                    /* multiple fg-rctd records created from existing bins to fulfill quantity   */ 
                                               
                    FIND FIRST tt-fg-set NO-LOCK 
                        WHERE tt-fg-set.part-no EQ b-itemfg.i-no NO-ERROR.
                        
                    IF AVAILABLE tt-fg-set THEN 
                        RUN processComponent.
                    
                    /* Look for Purchased component recs created with positive qty */
                    /* These are not needed for purchased componenents, only need the - */
                    FOR EACH fg-rcpts
                        WHERE fg-rcpts.company   EQ fg-rctd.company
                        AND fg-rcpts.linker    EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
                        AND fg-rcpts.i-no      EQ b-itemfg.i-no
                   
                        NO-LOCK,
                        EACH bf-fg-rctd
                        WHERE bf-fg-rctd.company EQ fg-rcpts.company
                        AND bf-fg-rctd.rita-code NE "P"
                        AND bf-fg-rctd.i-no    EQ b-itemfg.i-no
                        AND bf-fg-rctd.r-no    EQ fg-rcpts.r-no                      
                        AND bf-fg-rctd.t-qty     GT 0
                        NO-LOCK:
                        /* Delete the negatives for a receipt of assembled set */
                        /* with parts since only the negative is needed        */
                        /*DEF VAR llDelRecs AS LOG NO-UNDO. */
                        llDelRecs = FALSE.
                        IF AVAILABLE b-itemfg THEN 
                        DO:
                            IF fg-rctd.job-no NE "" THEN
                            DO:
                                FIND FIRST job WHERE
                                    job.company EQ cocode AND
                                    job.job-no EQ fg-rctd.job-no AND
                                    job.job-no2 EQ fg-rctd.job-no2
                                    NO-LOCK NO-ERROR.
    
                                IF AVAILABLE job THEN
                                DO:
                                    FIND FIRST eb WHERE
                                        eb.company  EQ cocode AND
                                        eb.est-no   EQ job.est-no AND
                                        eb.stock-no EQ b-itemfg.i-no
                                        NO-LOCK NO-ERROR.
                                    IF AVAILABLE eb THEN
                                    DO:
                                        IF eb.pur-man EQ YES THEN llDelRecs = TRUE.
                                    END.
                                    ELSE
                                        IF b-itemfg.pur-man EQ YES THEN llDelRecs = TRUE.
                     
                                END.
                            END.
                            ELSE           
                                IF b-itemfg.pur-man EQ YES THEN llDelRecs = TRUE.
                        END.                            
                        IF llDelRecs THEN 
                        DO:

                            CREATE tt-del-fg-rctd.
                            ASSIGN 
                                tt-del-fg-rctd.fg-rctd-row = ROWID(bf-fg-rctd).

                            CREATE tt-del-fg-rcpts.
                            ASSIGN 
                                tt-del-fg-rcpts.fg-rcpts-row = ROWID(fg-rcpts).

                        END.    
                    END.                    
                    
                END.
                 

                RELEASE b-w-fg-rctd.
            END. /* for each bf-fg-rcpts */
            /* Task 11111303 - This positive receipt of components for an assembled set */
            /* with parts is not needed, so delete them after they are used */
            /* to create the negative part of the transaction               */
            FOR EACH tt-del-fg-rctd.
                FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ tt-del-fg-rctd.fg-rctd-row
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE b-fg-rctd THEN
                    DELETE b-fg-rctd. 
                FIND fg-rcpts WHERE ROWID(fg-rcpts) EQ tt-del-fg-rcpts.fg-rcpts-row
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE fg-rcpts THEN
                    DELETE fg-rcpts. 
            END.
              
        END. /* alloc = ? */
        ELSE 
        DO:           
             
            RUN fg/fullset.p (ROWID(itemfg)).        

            FOR EACH tt-fg-set,

                FIRST b-itemfg
                WHERE b-itemfg.company EQ cocode      
                AND b-itemfg.i-no    EQ tt-fg-set.part-no
                AND b-itemfg.i-no    NE itemfg.i-no
                NO-LOCK
        
                BREAK BY b-itemfg.company:
                    
                IF FIRST(b-itemfg.company) THEN fg-rctd.ext-cost = 0.
                
                RUN processComponent.
                
                DELETE tt-fg-set.
            END. /* Each tt-fg-set */
        END. /* Alloc ne ? */
    END. /* li-max ge ... */
END. /* If avail itemfg and alloc ne yes */



/* **********************  Internal Procedures  *********************** */


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
        WHERE bf-fg-rctd.i-no EQ bf-fg-bin.i-no
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

PROCEDURE processComponent:    
    /* Using b-itemfg of the component, fg-rctd of the set */
        
    EMPTY TEMP-TABLE tt-bin.
            
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
    
            {fg/fg-post2.i b- fg-bin.qty}
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
       
                {fg/fg-post2a.i b- }
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
                    NO-LOCK BY fg-bin.qty DESCENDING:     

                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                    IF NOT ldQty GT 0 THEN
                        NEXT.

                    {fg/fg-post2.i b- ldQty}
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
                    NO-LOCK BY fg-bin.qty DESCENDING:           
                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                    
                    IF NOT ldQty GT 0 THEN
                        NEXT.
                        
                    {fg/fg-post2.i b- ldQty}
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
                    NO-LOCK BY fg-bin.qty DESCENDING:     
                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                    IF NOT ldQty GT 0 THEN
                        NEXT.
                    {fg/fg-post2.i b- ldQty}
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
                    NO-LOCK BY fg-bin.qty DESCENDING:                  
                    RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).

                    IF NOT ldQty GT 0 THEN
                        NEXT.
                    {fg/fg-post2.i b- ldQty}
                END.

        END. /* if fgsetassembly */

        /* Normal check for non-assembly bins */
        IF v-set-qty GT 0 THEN
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ b-itemfg.i-no
                AND fg-bin.job-no  NE ""
                AND fg-bin.qty     GT 0
                NO-LOCK BY fg-bin.qty DESCENDING:

                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).

                IF NOT ldQty GT 0 THEN
                    NEXT.
                {fg/fg-post2.i b- ldQty}
            END.  

        IF v-set-qty GT 0 THEN
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ b-itemfg.i-no
                AND fg-bin.job-no  EQ ""
                AND fg-bin.qty     GT 0
                NO-LOCK BY fg-bin.qty DESCENDING:
                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).

                IF NOT ldQty GT 0 THEN
                    NEXT.
                {fg/fg-post2.i b- ldQty}

            END.
            
        IF v-set-qty GT 0 THEN
            FOR EACH fg-bin
                WHERE fg-bin.company EQ cocode
                AND fg-bin.i-no    EQ b-itemfg.i-no
                NO-LOCK BY fg-bin.qty DESCENDING:     
                RUN bin-qty-used (INPUT ROWID(fg-bin), OUTPUT ldQty).
                
                IF NOT ldQty GT 0 THEN
                    NEXT.
                {fg/fg-post2.i b- ldQty}
    
            END.

    END.
    v-cost = fg-rctd.ext-cost / fg-rctd.t-qty.

    IF LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 THEN 
        fg-rctd.std-cost = v-cost.
    ELSE
        RUN sys/ref/convcuom.p("EA", fg-rctd.cost-uom, 0, 0, 0, 0,
            v-cost, OUTPUT fg-rctd.std-cost).
        
END. 
