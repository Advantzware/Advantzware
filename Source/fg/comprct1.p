
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
DEFINE VARIABLE li             AS INTEGER   NO-UNDO.

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
                        b-w-fg-rctd.SetHeaderRno = fg-rctd.r-no
                        .
                    FIND FIRST tt-fg-set NO-LOCK 
                        WHERE tt-fg-set.part-no EQ b-itemfg.i-no 
                        AND tt-fg-set.noReceipt NO-ERROR.
                    IF AVAIL tt-fg-set THEN DO:
                        FIND CURRENT bf-fg-rcpts EXCLUSIVE-LOCK NO-ERROR .
                        FIND CURRENT bf2-fg-rctd EXCLUSIVE-LOCK NO-ERROR .
                        DELETE bf-fg-rcpts .
                        DELETE bf2-fg-rctd .
                    END.  /* if pur-man eq NO then not create positive components set parts*/
                END. /* If pur-man eq NO i.e manufactured */
                ELSE 
                DO:
                    /* Handle purchased componenents similar to normal assembled set componenents */
                    /* multiple fg-rctd records created from existing bins to fulfill quantity   */ 
                                               
                    FIND FIRST tt-fg-set NO-LOCK 
                        WHERE tt-fg-set.part-no EQ b-itemfg.i-no NO-ERROR.
                        
                    IF AVAILABLE tt-fg-set THEN 
                        RUN processComponent ("set", FALSE /* Is an adjustment */).
                    
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

            FOR EACH tt-fg-set ,

                FIRST b-itemfg
                WHERE b-itemfg.company EQ cocode      
                AND b-itemfg.i-no    EQ tt-fg-set.part-no
                AND b-itemfg.i-no    NE itemfg.i-no
                NO-LOCK
        
                BREAK BY b-itemfg.company:
                    
                IF FIRST(b-itemfg.company) THEN fg-rctd.ext-cost = 0.
                
                RUN processComponent ("set", FALSE /* Is an adjustment */).
                
                DELETE tt-fg-set.
            END. /* Each tt-fg-set */
        END. /* Alloc ne ? */
    END. /* li-max ge ... */
END. /* If avail itemfg and alloc ne yes */



/* **********************  Internal Procedures  *********************** */
{fg/comprcpt.i}