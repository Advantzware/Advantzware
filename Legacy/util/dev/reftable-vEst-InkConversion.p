/* This utility will properly reset ink sides and unit vals from unconverted reftables */

/* NOTE: there are ONE or TWO reftables involved in this conversion.  The first handles inks 1-12;
the second handles any inks #13 or greater.  See ce/v-est3.w for usage. */

DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE iProcessCount AS INT NO-UNDO.
DEFINE BUFFER reftable1 FOR reftable1.
DEFINE BUFFER Breftable FOR reftable. 
DEFINE BUFFER b-rt FOR reftable.
DEFINE BUFFER eb  FOR eb.

DISABLE TRIGGERS FOR LOAD OF reftable.        
DISABLE TRIGGERS FOR LOAD OF eb.
DISABLE TRIGGERS FOR LOAD OF b-rt.

ASSIGN iProcessCount = 0.
FOR EACH reftable NO-LOCK WHERE 
    reftable.reftable EQ "ce/v-est3.w Unit#": /* Unit#, NOT Unit#1 */
    DO TRANSACTION:  
        ASSIGN 
            icnt = icnt + 1.  
        FIND FIRST eb EXCLUSIVE WHERE 
            eb.company     EQ reftable.company AND 
            eb.est-no      EQ reftable.loc AND 
            eb.form-no     EQ int(reftable.code) AND 
            eb.blank-no    EQ int(reftable.code2)
            NO-ERROR.
        IF AVAILABLE eb THEN DO:   
            DO i = 1 to 12: 
                /* Load 'side' value */
                ASSIGN eb.side[i] = SUBSTRING(reftable.dscr,i,1).
                /* Load 'unit' value */
                ASSIGN eb.unitNo[i] = reftable.val[i].                                      
            END.

            /* Find the second reftable, if it exists */
            FIND FIRST b-rt EXCLUSIVE WHERE 
                b-rt.reftable EQ "ce/v-est3.w Unit#1" AND   /* Unit#1, NOT Unit# */ 
                b-rt.company  EQ eb.company AND 
                b-rt.loc      EQ eb.est-no  AND 
                b-rt.code     EQ STRING(eb.form-no,"9999999999") AND 
                b-rt.code2    EQ STRING(eb.blank-no,"9999999999")
                NO-ERROR.            
            IF AVAIL b-rt THEN DO i = 13 to 20: 
                /* Load 'side' value */
                eb.side[i] = SUBSTRING(b-rt.dscr,i,1).
                /* Load 'unit' value */
                ASSIGN eb.unitNo[i] = b-rt.val[i - 12].
                CREATE reftable1.
                BUFFER-COPY b-rt TO reftable1.
                RELEASE reftable1.
                DELETE b-rt.
            END.
            
            ASSIGN 
                iProcessCount = iProcessCount + 1.  
        END.
        FIND CURRENT eb NO-LOCK NO-ERROR.    
        RELEASE eb. 
        
        CREATE reftable1.
        BUFFER-COPY reftable TO reftable1.
        RELEASE reftable1.

        FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE Breftable THEN DELETE Breftable. 
        
    END. /* Transaction */
END.  /*FOR EACH reftable*/          
