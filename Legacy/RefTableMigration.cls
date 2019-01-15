/*------------------------------------------------------------------------
   File        : RefTableMigration
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : jitender.gill
   Created     : Tue Sep 19 14:27:31 EDT 2017
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS RefTableMigration:
    
    DEFINE PUBLIC PROPERTY iProcessCount AS INTEGER GET. SET.
    
    
    METHOD PUBLIC INTEGER STYFLU(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER styleFluteXref FOR styleFluteXref.
        DEFINE BUFFER reftable1      FOR reftable1.
        DEFINE BUFFER Breftable      FOR reftable. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.           
        FOR EACH reftable WHERE reftable.reftable = "STYFLU"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST styleFluteXref WHERE styleFluteXref.rec_key = reftable.rec_key
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE styleFluteXref THEN 
                DO:
                    CREATE styleFluteXref.
                    BUFFER-COPY reftable TO styleFluteXref.
                    ASSIGN 
                        styleFluteXref.styleCode   = reftable.company
                        styleFluteXref.fluteCode   = reftable.loc
                        styleFluteXref.description = reftable.dscr
                        .   
                    RELEASE styleFluteXref.
                    ASSIGN iProcessCount = iProcessCount + 1.
                END.                 
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.
            END.  
        END.  /*FOR EACH reftable*/       
        RETURN iCnt.
    END METHOD. /*STYFLU*/
    
    METHOD PUBLIC INTEGER STYSCORE(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER styleScoreXref FOR styleScoreXref.
        DEFINE BUFFER reftable1      FOR reftable1.
        DEFINE BUFFER Breftable      FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF styleScoreXref.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "STYSCORE"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST styleScoreXref WHERE styleScoreXref.rec_key = reftable.rec_key
                    NO-LOCK NO-ERROR.
                IF NOT AVAILABLE styleScoreXref THEN 
                DO:    
                    CREATE styleScoreXref.
                    BUFFER-COPY reftable TO styleScoreXref.
                    ASSIGN 
                        styleScoreXref.styleCode   = reftable.company
                        styleScoreXref.scoreType   = reftable.loc
                        styleScoreXref.description = reftable.dscr
                        .   
                    RELEASE styleScoreXref.
                    ASSIGN iProcessCount = iProcessCount + 1.
                END.                 
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.
            END.
        END.  /*FOR EACH reftable*/       
        RETURN iCnt.
    END METHOD. /*STYSCORE*/
    METHOD PUBLIC INTEGER OeBollLotNo(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-boll   FOR oe-boll.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-boll.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-boll.lot-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-boll WHERE oe-boll.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-boll THEN    
                DO: 
                    IF oe-boll.lot-no EQ "" THEN ASSIGN oe-boll.lot-no = reftable.code.
                    IF oe-boll.frt-pay EQ "" THEN ASSIGN oe-boll.frt-pay = reftable.code2.
                    IF oe-boll.fob-code EQ "" THEN ASSIGN oe-boll.fob-code = reftable.dscr.
                    ASSIGN iProcessCount = iProcessCount + 1.
                END. 
                FIND CURRENT oe-boll NO-LOCK NO-ERROR.    
                RELEASE oe-boll.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-boll*/
    METHOD PUBLIC INTEGER OeRellLotNo(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-rell   FOR oe-rell.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-rell.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-rell.lot-no"
            NO-LOCK:
            DO TRANSACTION:                    
                FIND FIRST oe-rell WHERE oe-rell.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rell THEN    
                DO: 
                    IF oe-rell.lot-no EQ "" THEN ASSIGN oe-rell.lot-no = reftable.code.
                    IF oe-rell.frt-pay EQ "" THEN ASSIGN oe-rell.frt-pay = reftable.code2.
                    IF oe-rell.fob-code EQ "" THEN ASSIGN oe-rell.fob-code = reftable.dscr.
                    ASSIGN iProcessCount = iProcessCount + 1.
                END. 
                FIND CURRENT oe-rell NO-LOCK NO-ERROR.    
                RELEASE oe-rell.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-rell*/
    METHOD PUBLIC INTEGER OeRelLotNo(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-rel    FOR oe-rel.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-rel.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.lot-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.r-no = int(reftable.company)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                DO: 
                    IF oe-rel.lot-no EQ "" THEN ASSIGN oe-rel.lot-no = reftable.code.
                    IF oe-rel.frt-pay EQ "" THEN ASSIGN oe-rel.frt-pay = reftable.code2.
                    IF oe-rel.fob-code EQ "" THEN ASSIGN oe-rel.fob-code = reftable.dscr.
                    ASSIGN iProcessCount = iProcessCount + 1.
                END.   
                FIND CURRENT oe-rel NO-LOCK NO-ERROR.    
                RELEASE oe-rel.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-rel*/
    METHOD PUBLIC INTEGER OeRellSellPrice(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-rell   FOR oe-rell.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-rell.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-rell.sell-price"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rell WHERE oe-rell.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rell THEN    
                DO: 
                    IF oe-rell.newSellPrice EQ 0 THEN ASSIGN oe-rell.newSellPrice = reftable.val[1].
                    IF oe-rell.newZeroPrice EQ 0 THEN ASSIGN oe-rell.newZeroPrice = reftable.val[2].
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT oe-rell NO-LOCK NO-ERROR.    
                RELEASE oe-rell.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-rell*/
    METHOD PUBLIC INTEGER FgRctdUseJob(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER fg-rctd   FOR fg-rctd.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF fg-rctd.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "fg-rctd.use-job"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST fg-rctd WHERE fg-rctd.r-no = int(reftable.company)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE fg-rctd THEN    
                DO: 
                    IF fg-rctd.use-job EQ NO THEN ASSIGN fg-rctd.use-job = (reftable.val[1] EQ 1).
                    ASSIGN iProcessCount = iProcessCount + 1.                
                END    . 
                FIND CURRENT fg-rctd NO-LOCK NO-ERROR.    
                RELEASE fg-rctd.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.        
    END METHOD. /*fg-rctd*/
    METHOD PUBLIC INTEGER OeOrdlWhsItem(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-ordl   FOR oe-ordl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-ordl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-ordl.whs-item"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ordl WHERE oe-ordl.company = reftable.company
                    AND oe-ordl.ord-no = INT(reftable.loc)
                    AND oe-ordl.i-no = reftable.code
                    AND oe-ordl.line = INT(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ordl THEN    
                DO: 
                    IF oe-ordl.managed EQ NO THEN ASSIGN oe-ordl.managed = (reftable.val[1] EQ 1).
                    ASSIGN iProcessCount = iProcessCount + 1.                
                END    . 
                FIND CURRENT oe-ordl NO-LOCK NO-ERROR.    
                RELEASE oe-ordl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-ordl*/
    METHOD PUBLIC INTEGER OeOrdWhsOrder(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-ord    FOR oe-ord.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-ord.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-ord.whs-order"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ord WHERE oe-ord.company = reftable.company
                    AND oe-ord.ord-no = INT(reftable.loc)                                
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN    
                DO: 
                    IF oe-ord.managed EQ NO THEN 
                    
                        ASSIGN 
                            oe-ord.managed = (reftable.val[1] EQ 1). 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END. 
                FIND CURRENT oe-ord NO-LOCK NO-ERROR.    
                RELEASE oe-ord.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-ord*/
    METHOD PUBLIC INTEGER OeOrdlQNo(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-ordl   FOR oe-ordl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-ordl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-ordl.q-no"                            
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ordl WHERE oe-ordl.company = (reftable.company)
                    AND oe-ordl.ord-no = INT(reftable.loc)
                    AND oe-ordl.i-no = (reftable.code)
                    AND oe-ordl.line = INT(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ordl THEN    
                DO: 
                    IF oe-ordl.q-no EQ 0 THEN ASSIGN oe-ordl.q-no = INT(reftable.val[1] EQ 1). 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END    . 
                FIND CURRENT oe-ordl NO-LOCK NO-ERROR.    
                RELEASE oe-ordl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-ordl*/
    METHOD PUBLIC INTEGER OeRelJob(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-rel    FOR oe-rel.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-rel.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.job"                            
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.company = (reftable.company)
                    AND oe-rel.r-no = INT(reftable.code)                                                               
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                DO: 
                    IF oe-rel.job EQ 0 THEN ASSIGN oe-rel.job = INT(reftable.code2). 
                    ASSIGN iProcessCount = iProcessCount + 1.              
                END    . 
                FIND CURRENT oe-rel NO-LOCK NO-ERROR.    
                RELEASE oe-rel.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-rel*/
        METHOD PUBLIC INTEGER FgRctdUserId(INPUT ipCount AS INTEGER):
            DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
            DEFINE VARIABLE iTmpRno AS INTEGER NO-UNDO.
            DEFINE BUFFER reftable1 FOR reftable1.
            DEFINE BUFFER Breftable FOR reftable.
            DEFINE BUFFER fg-rctd   FOR fg-rctd.

            DISABLE TRIGGERS FOR LOAD OF reftable.
            DISABLE TRIGGERS FOR LOAD OF fg-rctd.
            ASSIGN iProcessCount = 0.
            FOR EACH reftable NO-LOCK
                WHERE reftable.reftable EQ "fg-rctd.user-id" :
                DO TRANSACTION:
                    FIND FIRST fg-rctd WHERE fg-rctd.r-no EQ INT(reftable.loc) EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE fg-rctd THEN
                    DO:
                        IF reftable.dscr BEGINS "fg-rctd:" THEN 
                        DO:
                            iTmpRno = INTEGER(SUBSTRING(reftable.dscr, 10, 10)) NO-ERROR.
                            IF NOT ERROR-STATUS:ERROR THEN 
                                fg-rctd.SetHeaderRno = iTmpRno.
                        END.
                        ELSE
                            fg-rctd.SetHeaderRno = 0.
                        IF NOT reftable.dscr BEGINS "fg-rctd:" THEN 
                            ASSIGN
                                fg-rctd.CreateInvoice      = reftable.val[1] NE 0
                                fg-rctd.BillableFreightAmt = reftable.val[2]
                                fg-rctd.EmailBOL           = reftable.val[3] EQ 1
                                fg-rctd.InvoiceFreight     = reftable.val[2] NE 0
                                fg-rctd.spare-char-1       = reftable.dscr /* tag # */
                                .                        
                            IF fg-rctd.created-by EQ "" THEN ASSIGN fg-rctd.created-by = reftable.code.
                            IF fg-rctd.updated-by EQ "" THEN ASSIGN fg-rctd.updated-by = reftable.code2.
                        ASSIGN iProcessCount = iProcessCount + 1.    
                    END.
                    FIND CURRENT fg-rctd NO-LOCK NO-ERROR.
                    RELEASE fg-rctd.
                    CREATE reftable1.
                    BUFFER-COPY reftable TO reftable1.
                    RELEASE reftable1.
                    FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                        EXCLUSIVE-LOCK NO-ERROR.
                    IF AVAILABLE Breftable THEN DELETE Breftable.
                    RELEASE Breftable.
                    iCnt = iCnt + 1.
                    IF iCnt >= ipCount THEN LEAVE.
                END.
            END.  /*FOR EACH reftable*/
            RETURN iCnt.
        END METHOD. /*fg-rctd*/
    METHOD PUBLIC INTEGER FgBinCost(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER fg-rdtlh  FOR fg-rdtlh.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF fg-rdtlh.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable NO-LOCK
            WHERE reftable.reftable EQ "fg-bin.cost" USE-INDEX rec_key:
            DO TRANSACTION:    
                FIND FIRST fg-rdtlh WHERE fg-rdtlh.rec_key = reftable.rec_key EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE fg-rdtlh THEN 
                DO:                    
                    IF fg-rdtlh.avg-cost EQ 0 THEN ASSIGN fg-rdtlh.avg-cost = reftable.val[1].
                    IF fg-rdtlh.last-cost EQ 0 THEN ASSIGN   fg-rdtlh.last-cost = reftable.val[2].
                    IF fg-rdtlh.std-fix-cost EQ 0 THEN ASSIGN   fg-rdtlh.std-fix-cost = reftable.val[3].
                    IF fg-rdtlh.std-lab-cost EQ 0 THEN ASSIGN   fg-rdtlh.std-lab-cost = reftable.val[4].
                    IF fg-rdtlh.std-mat-cost EQ 0 THEN ASSIGN   fg-rdtlh.std-mat-cost = reftable.val[5].
                    IF fg-rdtlh.std-tot-cost EQ 0 THEN ASSIGN   fg-rdtlh.std-tot-cost = reftable.val[6].
                    IF fg-rdtlh.std-var-cost EQ 0 THEN ASSIGN   fg-rdtlh.std-var-cost = reftable.val[7].
                    ASSIGN iProcessCount = iProcessCount + 1.
                END.
                FIND CURRENT fg-rdtlh NO-LOCK NO-ERROR.    
                RELEASE fg-rdtlh.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*fg-bin*/
    METHOD PUBLIC INTEGER OeRelSellPrice(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-rel    FOR oe-rel.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-rel.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.sell-price"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.r-no = INT(reftable.company)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                DO: 
                    IF oe-rel.sell-price EQ 0 THEN ASSIGN oe-rel.sell-price = reftable.val[1].
                    IF oe-rel.zeroPrice  EQ 0 THEN ASSIGN oe-rel.zeroPrice = reftable.val[2].
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT oe-rel NO-LOCK NO-ERROR.    
                RELEASE oe-rel.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-rel*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM5(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iHmCount AS INTEGER NO-UNDO .
        DEFINE VARIABLE iCnt     AS INTEGER NO-UNDO.
        DEFINE BUFFER user-print FOR user-print.
        DEFINE BUFFER reftable1  FOR reftable1.
        DEFINE BUFFER Breftable  FOR reftable. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        DISABLE TRIGGERS FOR LOAD OF user-print.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE
            reftable.reftable EQ "HM5" NO-LOCK:
            DO TRANSACTION:
                FIND FIRST user-print EXCLUSIVE-LOCK
                    WHERE user-print.company EQ reftable.loc
                    AND user-print.program-id EQ "HM5" 
                    AND user-print.user-id EQ reftable.company NO-ERROR.
            
                IF NOT AVAIL user-print THEN 
                DO:
                    CREATE user-print .
                    ASSIGN
                        user-print.company        = reftable.loc 
                        user-print.program-id     = "HM5" 
                        user-print.user-id        = reftable.company 
                        user-print.field-value[1] = reftable.CODE
                        . 
                    iHmCount = 1 . 
                    ASSIGN iProcessCount = iProcessCount + 1.
                END. 
                ELSE 
                DO: 
                    iHmCount = iHmCount + 1 . 
                    user-print.field-value[iHmCount] = reftable.CODE .
                    ASSIGN iProcessCount = iProcessCount + 1.
                END.
                RELEASE user-print.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.
            END.
        END.  /*FOR EACH reftable*/       
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM1SF(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt        AS INTEGER NO-UNDO.
        DEFINE VARIABLE iHm1SFCount AS INTEGER NO-UNDO .
        DEFINE BUFFER user-print FOR user-print.
        DEFINE BUFFER reftable1  FOR reftable1.
        DEFINE BUFFER Breftable  FOR reftable. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        DISABLE TRIGGERS FOR LOAD OF user-print.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE
            reftable.reftable EQ "HM1SF" NO-LOCK:
            DO TRANSACTION:
                FIND FIRST user-print EXCLUSIVE-LOCK
                    WHERE user-print.program-id EQ "HM1SF" 
                    AND user-print.user-id EQ reftable.company NO-ERROR.

                IF NOT AVAIL user-print THEN 
                DO:
                    CREATE user-print .
                    ASSIGN
                        user-print.program-id     = "HM1SF" 
                        user-print.user-id        = reftable.company 
                        user-print.field-value[1] = reftable.loc
                        . 
                    iHm1SFCount = 1 .
                    ASSIGN iProcessCount = iProcessCount + 1. 
                END. 
                ELSE 
                DO: 
                    iHm1SFCount = iHm1SFCount + 1 .
                    IF iHm1SFCount LT 100 THEN
                        user-print.field-value[iHm1SFCount] = reftable.loc .
                        ASSIGN iProcessCount = iProcessCount + 1.
                END.
                RELEASE user-print.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.
            END.
        END.  /*FOR EACH reftable*/       
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM1(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt      AS INTEGER NO-UNDO.
        DEFINE VARIABLE iHm1Count AS INTEGER NO-UNDO .
        DEFINE BUFFER user-print FOR user-print.
        DEFINE BUFFER reftable1  FOR reftable1.
        DEFINE BUFFER Breftable  FOR reftable. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        DISABLE TRIGGERS FOR LOAD OF user-print.        
        ASSIGN iProcessCount = 0.

        FOR EACH reftable WHERE
            reftable.reftable EQ "HM1" NO-LOCK:
            DO TRANSACTION:
                FIND FIRST user-print EXCLUSIVE-LOCK
                    WHERE user-print.company EQ reftable.loc
                    AND user-print.program-id EQ "HM1" 
                    AND user-print.user-id EQ reftable.company NO-ERROR.

                IF NOT AVAIL user-print THEN 
                DO:
                    CREATE user-print .
                    ASSIGN
                        user-print.company        = reftable.loc 
                        user-print.program-id     = "HM1" 
                        user-print.user-id        = reftable.company 
                        user-print.field-value[1] = reftable.CODE
                        . 
                    iHm1Count = 1 . 
                    ASSIGN iProcessCount = iProcessCount + 1.
                END. 
                ELSE 
                DO: 
                    iHm1Count = iHm1Count + 1 . 
                    IF iHm1Count LT 100 THEN
                        user-print.field-value[iHm1Count] = reftable.CODE .
                    ASSIGN iProcessCount = iProcessCount + 1.    
                END.
                RELEASE user-print.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.
            END.
        END.  /*FOR EACH reftable*/       
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM1Acct(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt         AS INTEGER NO-UNDO.
        DEFINE VARIABLE iHmAcctCount AS INTEGER NO-UNDO .
        DEFINE BUFFER user-print FOR user-print.
        DEFINE BUFFER reftable1  FOR reftable1.
        DEFINE BUFFER Breftable  FOR reftable. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        DISABLE TRIGGERS FOR LOAD OF user-print. 
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE
            reftable.reftable EQ "HM1Acct" NO-LOCK:
            DO TRANSACTION:
                FIND FIRST user-print EXCLUSIVE-LOCK
                    WHERE user-print.company EQ reftable.loc
                    AND user-print.program-id EQ "HM1Acct" 
                    AND user-print.user-id EQ reftable.company NO-ERROR.

                IF NOT AVAIL user-print THEN 
                DO:
                    CREATE user-print .
                    ASSIGN
                        user-print.company        = reftable.loc 
                        user-print.program-id     = "HM1Acct" 
                        user-print.user-id        = reftable.company 
                        user-print.field-value[1] = reftable.CODE
                        . 
                    iHmAcctCount = 1 .
                    ASSIGN iProcessCount = iProcessCount + 1. 
                END. 
                ELSE 
                DO: 
                    iHmAcctCount = iHmAcctCount + 1 . 
                    IF iHmAcctCount LT 100 THEN
                        user-print.field-value[iHmAcctCount] = reftable.CODE .
                    ASSIGN iProcessCount = iProcessCount + 1.    
                END.
                RELEASE user-print.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.
            END.
        END.  /*FOR EACH reftable*/       
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER OeBollSellPrice(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-boll   FOR oe-boll.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-boll.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-boll.sell-price"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-boll WHERE oe-boll.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-boll THEN    
                DO: 
                    IF oe-boll.sell-price EQ 0 THEN ASSIGN oe-boll.sell-price = reftable.val[1].
                    IF oe-boll.zeroPrice EQ 0 THEN ASSIGN oe-boll.zeroPrice = reftable.val[2].
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT oe-boll NO-LOCK NO-ERROR.    
                RELEASE oe-boll.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-boll*/
    METHOD PUBLIC INTEGER JobCreateTime(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER job       FOR job.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF job.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "job.create-time"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST job WHERE job.company = reftable.company
                    AND job.loc     = ""
                    AND job.job     = INT(reftable.code)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE job THEN    
                DO: 
                    IF job.create-time EQ 0 THEN ASSIGN job.create-time = reftable.val[1]. 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END    . 
                FIND CURRENT job NO-LOCK NO-ERROR.    
                RELEASE job.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*job*/
    METHOD PUBLIC INTEGER OeOrdlMisc(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-ordm   FOR oe-ordm.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-ordm.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe/ordlmisc.p"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ordm WHERE oe-ordm.company = reftable.company
                    AND oe-ordm.ord-no     = INT(reftable.loc)
                    AND oe-ordm.line       = INT(reftable.code)
                    AND oe-ordm.charge     = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ordm THEN    
                DO: 
                    IF oe-ordm.miscType EQ 0 THEN ASSIGN oe-ordm.miscType = INT(reftable.val[1]).
                    IF oe-ordm.estPrepEqty EQ 0 THEN ASSIGN oe-ordm.estPrepEqty = reftable.val[2].
                    IF oe-ordm.estPrepLine EQ 0 THEN ASSIGN oe-ordm.estPrepLine = INT(reftable.val[3]).
                    IF oe-ordm.miscInd EQ "" THEN ASSIGN oe-ordm.miscInd = STRING(reftable.val[4]).
                    IF oe-ordm.est-no EQ "" THEN ASSIGN oe-ordm.est-no = reftable.dscr. 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END    . 
                FIND CURRENT oe-ordm NO-LOCK NO-ERROR.    
                RELEASE oe-ordm.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-ordm*/
    METHOD PUBLIC INTEGER Factored(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER itemfg    FOR itemfg.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF itemfg.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "FACTORED"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST itemfg WHERE itemfg.company = reftable.company
                    AND itemfg.i-no     = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN    
                DO: 
                    IF itemfg.factored EQ NO THEN ASSIGN itemfg.factored = IF reftable.code2 = "YES" THEN YES ELSE NO.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT itemfg NO-LOCK NO-ERROR.    
                RELEASE itemfg.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*itemfg*/
    METHOD PUBLIC INTEGER Termscod(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER terms     FOR terms.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF terms.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "terms.cod"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST terms WHERE terms.company = reftable.company
                    AND terms.t-code  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE terms THEN    
                DO: 
                    IF terms.cod EQ NO THEN ASSIGN terms.cod = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE. 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END    . 
                FIND CURRENT terms NO-LOCK NO-ERROR.    
                RELEASE terms.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*terms*/
    METHOD PUBLIC INTEGER Stack(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1    FOR reftable1.
        DEFINE BUFFER Breftable    FOR reftable. 
        DEFINE BUFFER stackPattern FOR stackPattern.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF stackPattern.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "STACK"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST stackPattern WHERE stackPattern.stackCode  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE stackPattern THEN CREATE stackPattern.   
                ASSIGN 
                    stackPattern.stackCode        = reftable.code
                    stackPattern.rec_key          = reftable.rec_key
                    stackPattern.stackDescription = reftable.dscr
                    stackPattern.stackCount       = reftable.val[1]                           
                    . 
                ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT stackPattern NO-LOCK NO-ERROR.    
                RELEASE stackPattern.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.                               
        END.  /*FOR EACH reftable*/
        FOR EACH reftable WHERE reftable.reftable = "STACKSTRAP"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST stackPattern WHERE stackPattern.stackCode  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE stackPattern THEN CREATE stackPattern.   
                ASSIGN                   
                    stackPattern.strapCode    = reftable.code2
                    stackPattern.strapCount   = reftable.val[1] 
                    stackPattern.strapFormula = reftable.dscr        
                    .
                ASSIGN iProcessCount = iProcessCount + 1.
                FIND CURRENT stackPattern NO-LOCK NO-ERROR.    
                RELEASE stackPattern.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.                    
        END.  /*FOR EACH reftable*/
        FOR EACH reftable WHERE reftable.reftable = "STACKPAT"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST stackPattern WHERE stackPattern.stackCode  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE stackPattern THEN CREATE stackPattern.   
                ASSIGN                 
                    stackPattern.stackImage = reftable.dscr            
                    .
                ASSIGN iProcessCount = iProcessCount + 1.         
                FIND CURRENT stackPattern NO-LOCK NO-ERROR.    
                RELEASE stackPattern.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.                    
        END.  /*FOR EACH reftable*/             
        RETURN iCnt.
    END METHOD. /*stackPattern*/
    METHOD PUBLIC INTEGER ItemfgInkOccurs(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1  FOR reftable1.
        DEFINE BUFFER Breftable  FOR reftable. 
        DEFINE BUFFER itemfg-ink FOR itemfg-ink.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF itemfg-ink.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "itemfg-ink.occurs"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST itemfg-ink WHERE itemfg-ink.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE itemfg-ink THEN    
                DO: 
                    IF itemfg-ink.occurs EQ 0 THEN ASSIGN itemfg-ink.occurs = reftable.val[1]. 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END    . 
                FIND CURRENT itemfg-ink NO-LOCK NO-ERROR.    
                RELEASE itemfg-ink.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*itemfg-ink*/
    METHOD PUBLIC INTEGER ShiptoMandatoryTax(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER shipto    FOR shipto.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF shipto.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "shipto.mandatory-tax"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST shipto WHERE shipto.company = reftable.company
                    AND shipto.cust-no  = reftable.code
                    AND shipto.ship-id  = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN    
                DO: 
                    IF shipto.tax-mandatory EQ NO THEN ASSIGN shipto.tax-mandatory = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT shipto NO-LOCK NO-ERROR.    
                RELEASE shipto.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*shipto*/
    METHOD PUBLIC INTEGER CustPoMand(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER cust      FOR cust.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF cust.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "cust.po-mand"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST cust WHERE cust.company = reftable.company
                    AND cust.cust-no  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE cust THEN    
                DO: 
                    IF cust.po-mandatory EQ NO THEN ASSIGN cust.po-mandatory = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT cust NO-LOCK NO-ERROR.    
                RELEASE cust.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*customer*/
    METHOD PUBLIC INTEGER CustShowSet(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER cust      FOR cust.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF cust.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "cust.show-set"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST cust WHERE cust.company = reftable.company
                    AND cust.cust-no  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE cust THEN    
                DO: 
                    IF cust.show-set EQ NO THEN ASSIGN cust.show-set = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT cust NO-LOCK NO-ERROR.    
                RELEASE cust.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*customer*/
    METHOD PUBLIC INTEGER MachinePosition(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER est-op    FOR est-op.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF est-op.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "MachinePosition"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST est-op WHERE est-op.company = substring(reftable.company,1,10)
                    AND est-op.est-no  = substring(reftable.company,11,10)  
                    AND est-op.LINE = integer(reftable.loc)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE est-op THEN    
                DO: 
                    IF est-op.len-pos EQ 0 THEN ASSIGN est-op.len-pos = int(reftable.val[1]).
                    IF est-op.len EQ 0 THEN ASSIGN est-op.len = DECIMAL(reftable.val[4]).
                    IF est-op.wid-pos EQ 0 THEN ASSIGN est-op.wid-pos = INTEGER(reftable.val[2]).
                    IF est-op.wid EQ 0 THEN ASSIGN est-op.wid = DECIMAL(reftable.val[5]).
                    IF est-op.dep-pos EQ 0 THEN ASSIGN est-op.dep-pos = INTEGER(reftable.val[3]).
                    IF est-op.dep EQ 0 THEN ASSIGN est-op.dep = DECIMAL(reftable.val[6]). 
                    ASSIGN iProcessCount = iProcessCount + 1.              
                END    . 
                FIND CURRENT est-op NO-LOCK NO-ERROR.    
                RELEASE est-op.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*est-op*/
    METHOD PUBLIC INTEGER GlAcctDisc(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER account   FOR account.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF account.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "GLACCTDISC"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST account WHERE account.company = reftable.company
                    AND account.actnum     = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE account THEN    
                DO: 
                    IF account.terms-discount EQ NO THEN ASSIGN account.terms-discount = IF reftable.val[1] = 1 THEN YES ELSE NO.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT account NO-LOCK NO-ERROR.    
                RELEASE account.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*account*/
    METHOD PUBLIC INTEGER CustFlatComm(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER cust      FOR cust.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF cust.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "cust.flat-comm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST cust WHERE cust.company = reftable.company
                    AND cust.cust-no = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE cust THEN    
                DO: 
                    IF cust.flatCommPct EQ 0 THEN ASSIGN cust.flatCommPct = DEC(reftable.val[1]).                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT cust NO-LOCK NO-ERROR.    
                RELEASE cust.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*cust*/
    METHOD PUBLIC INTEGER StylePerMsf(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER style     FOR style.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF style.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "style.per-msf"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST style WHERE style.company = reftable.company
                                   AND style.style   = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE style THEN 
                DO:
                    IF style.sqft-len-trim = 0 THEN   
                        ASSIGN style.sqft-len-trim = reftable.val[1].
                    IF style.sqft-wid-trim = 0 THEN    
                        ASSIGN style.sqft-wid-trim = reftable.val[2]. 
                    ASSIGN iProcessCount = iProcessCount + 1.    
                END.    
                FIND CURRENT style NO-LOCK NO-ERROR.    
                RELEASE style.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*style*/
    METHOD PUBLIC INTEGER FreezeNote(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER job-hdr   FOR job-hdr.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF job-hdr.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "FREEZENOTE"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST job-hdr WHERE job-hdr.company = reftable.company
                    AND job-hdr.job-no  = reftable.loc
                    AND job-hdr.job-no2 = int(reftable.code)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE job-hdr THEN    
                DO: 
                    job-hdr.freezeNote = YES.
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    .                 
                FIND CURRENT job-hdr NO-LOCK NO-ERROR.    
                RELEASE job-hdr.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*job-hdr*/                   
    METHOD PUBLIC INTEGER MachPlainJobs(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER mach      FOR mach.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF mach.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "mach.plain-jobs"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST mach WHERE mach.company = reftable.company
                    AND mach.loc     = reftable.loc
                    AND mach.m-code = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE mach THEN    
                DO: 
                    IF mach.plain-job EQ NO THEN ASSIGN mach.plain-job = reftable.val[1] EQ 1.
                    ASSIGN iProcessCount = iProcessCount + 1.                
                END    . 
                FIND CURRENT mach NO-LOCK NO-ERROR.    
                RELEASE mach.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*mach*/
    METHOD PUBLIC INTEGER PrePlastJob(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER prep      FOR prep.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF prep.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "PREPLASTJOB"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST prep WHERE prep.company = reftable.company
                    AND prep.loc     = reftable.loc
                    AND prep.Code = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE prep THEN    
                DO: 
                    IF prep.last-job-no EQ "" THEN ASSIGN prep.last-job-no = reftable.code2.
                    IF prep.last-job-no2 EQ 0 THEN ASSIGN prep.last-job-no2 = reftable.val[1].
                    ASSIGN iProcessCount = iProcessCount + 1.                 
                END    . 
                FIND CURRENT prep NO-LOCK NO-ERROR.    
                RELEASE prep.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*prep*/
    METHOD PUBLIC INTEGER JobQtyChanged(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER job       FOR job.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF job.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "job.qty-changed"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST job WHERE job.company = reftable.company
                    AND job.job     = int(reftable.code)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE job THEN    
                    ASSIGN 
                        job.qty-changed = YES               
                        .
                    ASSIGN iProcessCount = iProcessCount + 1.     
                FIND CURRENT job NO-LOCK NO-ERROR.    
                RELEASE job.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*job*/
    METHOD PUBLIC INTEGER GlRptPctSubtotal(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER gl-rpt    FOR gl-rpt.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF gl-rpt.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "gl-rpt.pct-subtotal"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST gl-rpt WHERE gl-rpt.company = reftable.company
                    AND gl-rpt.rp      = reftable.code
                    AND gl-rpt.line    = int(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE gl-rpt THEN    
                DO: 
                    IF gl-rpt.pct-subtotal EQ NO THEN ASSIGN gl-rpt.pct-subtotal = reftable.val[1] EQ 1.
                    ASSIGN iProcessCount = iProcessCount + 1.              
                END    . 
                FIND CURRENT gl-rpt NO-LOCK NO-ERROR.    
                RELEASE gl-rpt.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*gl-rpt*/
    METHOD PUBLIC INTEGER CeCtrlBrokerPct(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ce-ctrl   FOR ce-ctrl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ce-ctrl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.broker-pct"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc      = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                DO: 
                    IF ce-ctrl.broker-pct EQ 0 THEN ASSIGN ce-ctrl.broker-pct = reftable.val[1].
                    ASSIGN iProcessCount = iProcessCount + 1.             
                END    . 
                FIND CURRENT ce-ctrl NO-LOCK NO-ERROR.    
                RELEASE ce-ctrl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ce-ctrl*/
    METHOD PUBLIC INTEGER RmBinAgeDate(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER rm-bin    FOR rm-bin.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF rm-bin.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "rm-bin.age-date"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST rm-bin WHERE rm-bin.company = reftable.company
                    AND rm-bin.loc     = SUBSTRING(reftable.code, 1, 50)
                    AND rm-bin.i-no    = reftable.loc
                    AND rm-bin.loc-bin   = SUBSTRING(reftable.code, 51, 50)
                    AND rm-bin.tag      = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE rm-bin THEN    
                DO: 
                    IF rm-bin.aging-date EQ ? THEN ASSIGN rm-bin.aging-date = DATE(INT(reftable.val[1])).
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT rm-bin NO-LOCK NO-ERROR.    
                RELEASE rm-bin.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*rm-bin*/
    METHOD PUBLIC INTEGER CeCtrlFoldPct(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ce-ctrl   FOR ce-ctrl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ce-ctrl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.fold-pct"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                DO: 
                    IF ce-ctrl.fold-pct EQ 0 THEN ASSIGN ce-ctrl.fold-pct = reftable.val[1].
                    ASSIGN iProcessCount = iProcessCount + 1.             
                END    . 
                FIND CURRENT ce-ctrl NO-LOCK NO-ERROR.    
                RELEASE ce-ctrl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.

    END METHOD. /*ce-ctrl*/
    METHOD PUBLIC INTEGER CeCtrlFgRateFarm(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ce-ctrl   FOR ce-ctrl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ce-ctrl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.fg-rate-farm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                DO: 
                    IF ce-ctrl.fg-rate-farm EQ 0 THEN ASSIGN ce-ctrl.fg-rate-farm = reftable.val[1].
                    ASSIGN iProcessCount = iProcessCount + 1.             
                END    . 
                FIND CURRENT ce-ctrl NO-LOCK NO-ERROR.    
                RELEASE ce-ctrl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ce-ctrl*/  
    METHOD PUBLIC INTEGER CeCtrlRmRateFarm(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ce-ctrl   FOR ce-ctrl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ce-ctrl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.rm-rate-farm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                DO: 
                    IF ce-ctrl.rm-rate-farm EQ 0 THEN ASSIGN ce-ctrl.rm-rate-farm = reftable.val[1].
                    ASSIGN iProcessCount = iProcessCount + 1.             
                END    . 
                FIND CURRENT ce-ctrl NO-LOCK NO-ERROR.    
                RELEASE ce-ctrl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ce-ctrl*/
    METHOD PUBLIC INTEGER CeCtrlHandPctFarm(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ce-ctrl   FOR ce-ctrl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ce-ctrl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.hand-pct-farm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                DO: 
                    IF ce-ctrl.hand-pct-farm EQ 0 THEN ASSIGN ce-ctrl.hand-pct-farm = reftable.val[1]. 
                    ASSIGN iProcessCount = iProcessCount + 1.         
                END    . 
                FIND CURRENT ce-ctrl NO-LOCK NO-ERROR.    
                RELEASE ce-ctrl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ce-ctrl*/        
    /*    METHOD PUBLIC INTEGER EstOpLock(INPUT ipCount AS INTEGER):                                      */
    /*        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.                                                    */
    /*        DEFINE BUFFER reftable1 FOR reftable1.                                                      */
    /*        DEFINE BUFFER Breftable FOR reftable.                                                       */
    /*        DEFINE BUFFER est       FOR est.                                                            */
    /*                                                                                                    */
    /*        DISABLE TRIGGERS FOR LOAD OF reftable.                                                      */
    /*        DISABLE TRIGGERS FOR LOAD OF est.                                                           */
    /*                                                                                                    */
    /*        FOR EACH reftable WHERE reftable.reftable = "est.op-lock"                                   */
    /*            NO-LOCK:                                                                                */
    /*            DO TRANSACTION:                                                                         */
    /*                FIND FIRST est WHERE est.company = reftable.company                                 */
    /*                    AND est.loc     = reftable.loc                                                  */
    /*                    AND est.est-no  = reftable.code                                                 */
    /*                    EXCLUSIVE-LOCK NO-ERROR.                                                        */
    /*                IF AVAILABLE est THEN                                                               */
    /*                    DO:                                                                             */
    /*                        IF est.recalc EQ NO THEN ASSIGN est.recalc    = LOGICAL(reftable.val[1]).   */
    /*                        IF est.recalc-mr EQ NO THEN ASSIGN est.recalc-mr = LOGICAL(reftable.val[2]).*/
    /*                    END    .                                                                        */
    /*                FIND CURRENT est NO-LOCK NO-ERROR.                                                  */
    /*                RELEASE est.                                                                        */
    /*                CREATE reftable1.                                                                   */
    /*                BUFFER-COPY reftable TO reftable1.                                                  */
    /*                RELEASE reftable1.                                                                  */
    /*                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)                             */
    /*                    EXCLUSIVE-LOCK NO-ERROR.                                                        */
    /*                IF AVAILABLE Breftable THEN DELETE Breftable.                                       */
    /*                RELEASE Breftable.                                                                  */
    /*                iCnt = iCnt + 1.                                                                    */
    /*                IF iCnt >= ipCount THEN LEAVE.                                                      */
    /*            END.                                                                                    */
    /*        END.  /*FOR EACH reftable*/                                                                 */
    /*        RETURN iCnt.                                                                                */
    /*    END METHOD. /*est*/                                                                             */
    METHOD PUBLIC INTEGER OeRelScode(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-rel    FOR oe-rel.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-rel.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.s-code"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.r-no = int(reftable.company)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                DO: 
/*                    27898 - remove test for blank s-code                                          */
/*                    IF oe-rel.s-code EQ "" THEN ASSIGN oe-rel.s-code = reftable.code.             */
/*                                                                                                  */
                    ASSIGN oe-rel.s-code = reftable.code.             

                    ASSIGN iProcessCount = iProcessCount + 1.       
                END    . 
                FIND CURRENT oe-rel NO-LOCK NO-ERROR.    
                RELEASE oe-rel.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-rel*/   
    METHOD PUBLIC INTEGER Splitship(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER job-hdr   FOR job-hdr.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF job-hdr.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "SPLITSHIP"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST job-hdr WHERE job-hdr.company = reftable.company
                    AND job-hdr.job-no     = reftable.loc
                    AND job-hdr.job-no2    = int(reftable.code)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE job-hdr THEN    
                    ASSIGN 
                        job-hdr.splitShip = YES                
                        . 
                    ASSIGN iProcessCount = iProcessCount + 1.   
                FIND CURRENT job-hdr NO-LOCK NO-ERROR.    
                RELEASE job-hdr.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*job-hdr*/   
    METHOD PUBLIC INTEGER Splitshp(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER job-hdr   FOR job-hdr.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF job-hdr.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "splitshp"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST job-hdr WHERE job-hdr.company = reftable.company
                    AND job-hdr.job-no     = reftable.loc
                    AND job-hdr.job-no2    = int(reftable.code)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE job-hdr THEN    
                    ASSIGN 
                        job-hdr.splitShip = YES                
                        . 
                    ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT job-hdr NO-LOCK NO-ERROR.    
                RELEASE job-hdr.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*job-hdr*/     
    METHOD PUBLIC INTEGER MachObsolete(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER mach      FOR mach.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF mach.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "mach.obsolete"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST mach WHERE mach.company = reftable.company
                    AND mach.loc     = reftable.loc
                    AND mach.m-code = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE mach THEN    
                DO: 
                    IF mach.obsolete EQ NO THEN ASSIGN mach.obsolete = reftable.val[1] EQ 1. 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END    . 
                FIND CURRENT mach NO-LOCK NO-ERROR.    
                RELEASE mach.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*mach*/
    METHOD PUBLIC INTEGER ExportCustId(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER shipto    FOR shipto.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF shipto.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "JDEDWARDCUST#"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST shipto WHERE shipto.company = reftable.company
                    AND shipto.cust-no = reftable.code 
                    AND shipto.ship-id = reftable.code2                           
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN    
                DO: 
                    IF shipto.exportCustID EQ "" THEN ASSIGN shipto.exportCustID = reftable.dscr. 
                    ASSIGN iProcessCount = iProcessCount + 1.               
                END    . 
                FIND CURRENT shipto NO-LOCK NO-ERROR.    
                RELEASE shipto.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*shipto*/ 
    
    METHOD PUBLIC INTEGER UserDocs(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER users     FOR users.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF users.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "users.user-docs"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.showOnPO    = IF users.showOnPO = TRUE OR reftable.val[1] = 1 THEN TRUE ELSE FALSE
                        users.showOnBOL   = IF users.showOnBOL = TRUE OR reftable.val[2] = 1 THEN TRUE ELSE FALSE
                        users.showOnInv   = IF users.showOnInv = TRUE OR reftable.val[3] = 1 THEN TRUE ELSE FALSE
                        users.showOnAck   = IF users.showOnAck = TRUE OR reftable.val[4] = 1 THEN TRUE ELSE FALSE
                        users.showOnQuote = IF users.showOnQuote = TRUE OR reftable.val[5] = 1 THEN TRUE ELSE FALSE
                        .
                    ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT users NO-LOCK NO-ERROR.    
                RELEASE users.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD.
    METHOD PUBLIC INTEGER UserPhoneNo(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER users     FOR users.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF users.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "users.phone-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.phone = IF users.phone = "" AND reftable.CODE = "" THEN reftable.CODE ELSE users.phone
                        .
                    ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT users NO-LOCK NO-ERROR.    
                RELEASE users.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD.
    METHOD PUBLIC INTEGER UserFaxNo(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER users     FOR users.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF users.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "users.fax-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.fax = IF users.fax = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax
                        .
                    ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT users NO-LOCK NO-ERROR.    
                RELEASE users.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD.
    METHOD PUBLIC INTEGER UsersPhoneCnty(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER users     FOR users.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF users.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "users.phone-cnty"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.phone-cnty = IF users.phone-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.phone-cnty
                        .
                    ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT users NO-LOCK NO-ERROR.    
                RELEASE users.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD.
    METHOD PUBLIC INTEGER UserFaxCnty(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER users     FOR users.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF users.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "users.fax-cnty"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.fax-cnty = IF users.fax-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax-cnty
                        . 
                    ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT users NO-LOCK NO-ERROR.    
                RELEASE users.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD.
    METHOD PUBLIC INTEGER InvLineLotNo(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER inv-line  FOR inv-line.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "inv-line.lot-no"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*inv-line*/ 
    METHOD PUBLIC INTEGER OeRelhCanPrint(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-relh.can-print"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*inv-line*/ 
    METHOD PUBLIC INTEGER Dropslit(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER est       FOR est.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF est.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "dropslit"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST est WHERE est.company   = reftable.company
                                 AND est.est-no    = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE est THEN    
                DO: 
                    IF est.dropslit EQ NO THEN ASSIGN est.dropslit = reftable.loc eq "yes".                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT est NO-LOCK NO-ERROR.    
                RELEASE est.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*est*/
    METHOD PUBLIC INTEGER NextRelh(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "NextRelh"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*NextRelh*/ 
    METHOD PUBLIC INTEGER PoUserId(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER po-ord    FOR po-ord.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF po-ord.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "POUserid"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST po-ord WHERE po-ord.company = reftable.company
                                    AND po-ord.po-no   = int(reftable.loc)                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE po-ord THEN    
                DO: 
                    IF po-ord.user-id EQ "" THEN ASSIGN po-ord.user-id = reftable.code.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT po-ord NO-LOCK NO-ERROR.    
                RELEASE po-ord.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*po-ord*/
    METHOD PUBLIC INTEGER OeOrdCloseChecked(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-ord.close-checked"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-ord*/ 
    METHOD PUBLIC INTEGER RmRctdUserId(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "rm-rctd.user-id"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*rm-rctd*/
    METHOD PUBLIC INTEGER ItemfgExemptDisc(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "itemfg.exempt-disc"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*itemfg*/
    METHOD PUBLIC INTEGER FGStatus(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "FGSTATUS"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*FGStatus*/
    METHOD PUBLIC INTEGER JobCloseChecked(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "job.close-checked"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*job*/
    METHOD PUBLIC INTEGER NextRelease(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "NextRelease#"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*NextRelease*/ 
    METHOD PUBLIC INTEGER Rmglobpr(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "rm/rmglobpr.w"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*Rmglobpr*/ 
    METHOD PUBLIC INTEGER OeRellSelected(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-rell.selected"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-rell*/
    METHOD PUBLIC INTEGER Fgtaxable(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "FGTAXABLE"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*Fgtaxable*/ 
    METHOD PUBLIC INTEGER ScoreType(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1      FOR reftable1.
        DEFINE BUFFER Breftable      FOR reftable.
        DEFINE BUFFER scoreType      FOR scoreType. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.           
        FOR EACH reftable WHERE reftable.reftable = "score-type"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST scoreType WHERE scoreType.company   = reftable.company
                                       AND scoreType.scoreType = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF NOT AVAILABLE scoreType THEN 
                DO:
                    CREATE scoreType.
                    BUFFER-COPY reftable TO scoreType.
                    ASSIGN 
                        scoreType.company     = reftable.company
                        scoreType.scoreType   = reftable.code
                        scoreType.description = reftable.dscr
                        .   
                    RELEASE scoreType.
                    ASSIGN iProcessCount = iProcessCount + 1.
                END. 
                ELSE DO:
                        IF scoreType.description EQ "" THEN ASSIGN scoreType.description = reftable.dscr
                        . 
                    ASSIGN iProcessCount = iProcessCount + 1.
                END.        
                FIND CURRENT scoreType NO-LOCK NO-ERROR.    
                RELEASE scoreType.                      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.
            END.  
        END.  /*FOR EACH reftable*/       
        RETURN iCnt.
    END METHOD. /*scoreType*/
    METHOD PUBLIC INTEGER VendQty(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER e-item-vend  FOR e-item-vend.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF e-item-vend.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "vend-qty"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST e-item-vend WHERE e-item-vend.i-no     = reftable.code
                                         AND e-item-vend.company  = reftable.company
                                         AND e-item-vend.vend-no  = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE e-item-vend THEN DO:   
                    DO i = 1 to 10: 
                        IF e-item-vend.runQtyXtra[i] EQ 0 THEN ASSIGN e-item-vend.runQtyXtra[i] = reftable.val[i]. 
                                     
                    END.
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT e-item-vend NO-LOCK NO-ERROR.    
                RELEASE e-item-vend.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*e-item-vend*/
    METHOD PUBLIC INTEGER VendCost(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER e-item-vend  FOR e-item-vend.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF e-item-vend.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "vend-cost"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST e-item-vend WHERE e-item-vend.i-no     = reftable.code
                                         AND e-item-vend.company  = reftable.company
                                         AND e-item-vend.vend-no  = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE e-item-vend THEN DO:   
                    DO i = 1 to 10: 
                        IF e-item-vend.runCostXtra[i] EQ 0 THEN ASSIGN e-item-vend.runCostXtra[i] = reftable.val[i]. 
                                     
                    END.
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT e-item-vend NO-LOCK NO-ERROR.    
                RELEASE e-item-vend.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*e-item-vend*/
    METHOD PUBLIC INTEGER VendSetup(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER e-item-vend  FOR e-item-vend.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF e-item-vend.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "vend-setup"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST e-item-vend WHERE e-item-vend.i-no     = reftable.code
                                         AND e-item-vend.company  = reftable.company
                                         AND e-item-vend.vend-no  = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE e-item-vend THEN DO:   
                    DO i = 1 to 10: 
                        IF e-item-vend.setupsXtra[i] EQ 0 THEN ASSIGN e-item-vend.setupsXtra[i] = reftable.val[i]. 
                                     
                    END.
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT e-item-vend NO-LOCK NO-ERROR.    
                RELEASE e-item-vend.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*e-item-vend*/
    METHOD PUBLIC INTEGER DArtios(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "cecrep/d-artios.w"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*DArtios*/ 
    METHOD PUBLIC INTEGER Flute(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER flute     FOR flute.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF flute.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "Flute"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST flute WHERE flute.code = reftable.code         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE flute THEN    
                    ASSIGN
                        flute.thickness = IF flute.thickness = 0 AND reftable.val[1] NE 0 THEN reftable.val[1] ELSE flute.thickness
                        flute.class = IF flute.class = "" AND reftable.code2 NE "" THEN reftable.code2 ELSE flute.class
                        flute.dscr = IF flute.dscr = "" AND reftable.dscr NE "" THEN reftable.dscr ELSE flute.dscr
                        . 
                    ASSIGN iProcessCount = iProcessCount + 1.    
                FIND CURRENT flute NO-LOCK NO-ERROR.    
                RELEASE flute.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD.    
    METHOD PUBLIC INTEGER aoaReport(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "aoaReport"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*aoaReport*/ 
    
    METHOD PUBLIC INTEGER relcredconv(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "relcredconv"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*relcredconv*/  

    METHOD PUBLIC INTEGER StdUom(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER e-itemfg-vend  FOR e-itemfg-vend.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF e-itemfg-vend.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "e-itemfg-vend.std-uom"
            NO-LOCK:
            DO TRANSACTION:    
                FOR EACH e-itemfg-vend WHERE e-itemfg-vend.est-no   = reftable.code
                                           AND e-itemfg-vend.company  = reftable.company
                                           AND e-itemfg-vend.form-no  = reftable.val[1]
                                           AND e-itemfg-vend.blank-no = reftable.val[2]
                                           AND e-itemfg-vend.est-no NE ""
                    EXCLUSIVE-LOCK:
                    IF e-itemfg-vend.std-uom EQ "" THEN ASSIGN e-itemfg-vend.std-uom = reftable.code2. 
                                     
                END.
                ASSIGN iProcessCount = iProcessCount + 1.              
                FIND CURRENT e-itemfg-vend NO-LOCK NO-ERROR.    
                RELEASE e-itemfg-vend.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*e-itemfg-vend*/ 

    METHOD PUBLIC INTEGER Arcashhold(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 

        DEFINE BUFFER ar-cash  FOR ar-cash.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ar-cash.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "ARCASHHOLD"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ar-cash WHERE ar-cash.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ar-cash THEN DO:
                        IF ar-cash.stat EQ "" THEN ASSIGN ar-cash.stat = reftable.code. 
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT ar-cash NO-LOCK NO-ERROR.    
                RELEASE ar-cash.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ar-cash*/
    METHOD PUBLIC INTEGER Saletool(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "SALETOOL"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*Saletool*/ 
    
    
    
    METHOD PUBLIC INTEGER print42(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "print42"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*print42*/ 
    METHOD PUBLIC INTEGER ColorAudit(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "COLOR AUDIT"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ColorAudit*/
    METHOD PUBLIC INTEGER Machcrew(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER mach  FOR mach.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF mach.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "MACH-CREW"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST mach WHERE mach.company = reftable.company
                                  AND mach.loc     = reftable.loc
                                  AND mach.m-code  = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE mach THEN DO:
                    IF reftable.code2 = "M R-QTY" THEN DO:
                        DO i = 1 TO 20:
                            IF mach.mr-crusiz-qty[i] EQ 0 THEN ASSIGN mach.mr-crusiz-qty[i] = reftable.val[i].
                        END.
                    END. /*IF reftable.code2 = "M R-QTY" THEN DO:*/
                    
                    IF reftable.code2 = "M R-CST" THEN DO:
                        DO i = 1 TO 20:
                            IF mach.mr-crusiz-qty[i] EQ 0 THEN ASSIGN mach.mr-crusiz-cst[i] = reftable.val[i].
                        END.
                    END. /*IF reftable.code2 = "M R-CST" THEN DO:*/
                    
                    IF reftable.code2 = "RUN-QTY" THEN DO:
                        DO i = 1 TO 20:
                            IF mach.mr-crusiz-qty[i] EQ 0 THEN ASSIGN mach.run-crusiz-qty[i] = reftable.val[i].
                        END.
                    END. /*IF reftable.code2 = "RUN-QTY" THEN DO:*/
                    
                    IF reftable.code2 = "RUN-CST" THEN DO:
                        DO i = 1 TO 20:
                            IF mach.mr-crusiz-qty[i] EQ 0 THEN ASSIGN mach.run-crusiz-cst[i] = reftable.val[i].
                        END.
                    END. /*IF reftable.code2 = "RUN-CST" THEN DO:*/
                    
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT mach NO-LOCK NO-ERROR.    
                RELEASE mach.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*mach*/
    METHOD PUBLIC INTEGER ItemfgMarkup(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER e-itemfg-vend  FOR e-itemfg-vend.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF e-itemfg-vend.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "e-itemfg-vend.markup"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST e-itemfg-vend WHERE e-itemfg-vend.est-no  = ""
                                           AND e-itemfg-vend.company = reftable.company
                                           AND e-itemfg-vend.i-no    = reftable.loc
                                           AND e-itemfg-vend.vend-no = reftable.code
                EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE e-itemfg-vend THEN DO:
                    IF e-itemfg-vend.markup EQ 0 THEN ASSIGN e-itemfg-vend.markup = reftable.val[1].                    
                  ASSIGN iProcessCount = iProcessCount + 1. 
                END.             
                FIND CURRENT e-itemfg-vend NO-LOCK NO-ERROR.    
                RELEASE e-itemfg-vend.      
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*e-itemfg-vend*/
    METHOD PUBLIC INTEGER TruckRunPrint(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER truck-run-print    FOR truck-run-print.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF truck-run-print.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "trp-car"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST truck-run-print WHERE truck-run-print.rec_key = reftable.rec_key            
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE truck-run-print THEN    
                DO: 
                    IF truck-run-print.carrier EQ "" THEN ASSIGN truck-run-print.carrier = reftable.code.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT truck-run-print NO-LOCK NO-ERROR.    
                RELEASE truck-run-print.      
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
    END METHOD. /*e-itemfg-vend*/ 
    METHOD PUBLIC INTEGER ArCashlReturn(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ar-cashl     FOR ar-cashl.
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ar-cashl.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ar-cashl.return"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ar-cashl WHERE ar-cashl.c-no    = int(reftable.code)
                                      AND ar-cashl.company = reftable.company
                                      AND ar-cashl.line    = int(reftable.code2)         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ar-cashl THEN    
                    DO:
                        IF ar-cashl.r-no EQ 0 THEN ASSIGN reftable.val[1] = ar-cashl.r-no.
                        IF ar-cashl.dscr EQ "" THEN ASSIGN reftable.dscr = ar-cashl.dscr 
                        . 
                    ASSIGN iProcessCount = iProcessCount + 1.
                    END.    
                FIND CURRENT ar-cashl NO-LOCK NO-ERROR.    
                RELEASE ar-cashl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ArCashlReturn*/   
    METHOD PUBLIC INTEGER Proboard(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER probe     FOR probe.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF probe.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "probe.board"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST probe WHERE probe.company = reftable.company
                                   AND probe.est-no  = reftable.code
                                   AND probe.line    = int(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE probe THEN    
                DO: 
                    IF probe.boardCostTotal EQ 0 THEN ASSIGN probe.boardCostTotal = reftable.val[1].
                    IF probe.boardCostPerM EQ 0 THEN ASSIGN probe.boardCostPerM = reftable.val[2].  
                    IF probe.boardCostPct EQ 0 THEN ASSIGN probe.boardCostPct = reftable.val[3].
                    IF probe.boardContributionPerM EQ 0 THEN ASSIGN probe.boardContributionPerM = reftable.val[4].
                    IF probe.boardContributionTotal EQ 0 THEN ASSIGN probe.boardContributionTotal = reftable.val[5].
                    IF probe.manHoursTotal EQ 0 THEN ASSIGN probe.manHoursTotal = reftable.val[6].
                    IF probe.directMaterialCost EQ 0 THEN ASSIGN probe.directMaterialCost = reftable.val[7].
                    IF probe.grossProfitPerM EQ 0 THEN ASSIGN probe.grossProfitPerM = reftable.val[8].
                    IF probe.grossProfitPerManhourAssemb EQ 0 THEN ASSIGN probe.grossProfitPerManhourAssemb = reftable.val[9].
                    IF probe.grossProfitPerManHourOther EQ 0 THEN ASSIGN probe.grossProfitPerManHourOther = reftable.val[10].
                    IF probe.grossProfitPctTemp EQ 0 THEN ASSIGN probe.grossProfitPctTemp = reftable.val[11].
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT probe NO-LOCK NO-ERROR.    
                RELEASE probe.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ProBoard*/
    METHOD PUBLIC INTEGER CpLabP(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER cust-part FOR cust-part.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF cust-part.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "cp-lab-p"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST cust-part WHERE cust-part.company = reftable.company
                                       AND cust-part.i-no    = reftable.loc
                                       AND cust-part.cust-no = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE cust-part THEN    
                DO: 
                    IF cust-part.labelCase EQ "" THEN ASSIGN cust-part.labelCase = reftable.code2.
                    IF cust-part.labelPallet EQ "" THEN ASSIGN cust-part.labelPallet = reftable.dscr.
                    ASSIGN iProcessCount = iProcessCount + 1.                 
                END    . 
                FIND CURRENT cust-part NO-LOCK NO-ERROR.    
                RELEASE cust-part.      
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*cp-lab-p*/ 
    METHOD PUBLIC INTEGER PoordlDepth(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER po-ordl   FOR po-ordl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF po-ordl.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "POORDLDEPTH"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST po-ordl WHERE po-ordl.company = reftable.company
                                     AND po-ordl.i-no    = reftable.loc
                                     AND po-ordl.vend-no = reftable.code
                EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE po-ordl THEN DO:
                    IF po-ordl.s-dep EQ 0 THEN ASSIGN po-ordl.s-dep = int(reftable.code2).                    
                  ASSIGN iProcessCount = iProcessCount + 1. 
                END.             
                FIND CURRENT po-ordl NO-LOCK NO-ERROR.    
                RELEASE po-ordl.      
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*PoordlDepth*/ 
    METHOD PUBLIC INTEGER Ar-cash(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ar-cash    FOR ar-cash.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ar-cash.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "AR-CASH"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ar-cash WHERE ar-cash.c-no = int(reftable.code)         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ar-cash THEN    
                DO: 
                    ASSIGN ar-cash.printed = YES .                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT ar-cash NO-LOCK NO-ERROR.    
                RELEASE ar-cash.      
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
    END METHOD. /*Itemfgmarkup*/ 
    METHOD PUBLIC INTEGER ShiftDays(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER shifts    FOR shifts.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF shifts.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ShiftDays"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST shifts WHERE shifts.rec_key = reftable.code              
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE shifts THEN    
                DO: 
                    IF shifts.useSpecificDays EQ NO THEN ASSIGN shifts.useSpecificDays = IF reftable.loc = "1" THEN YES ELSE NO.
                    IF shifts.dayList EQ "" THEN ASSIGN shifts.dayList = reftable.code2.                
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    . 
                FIND CURRENT shifts NO-LOCK NO-ERROR.    
                RELEASE shifts.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*shiftDays*/
    METHOD PUBLIC INTEGER EitemvendAdders (INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER e-item-vend  FOR e-item-vend.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF e-item-vend.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "e-item-vend.adders"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST e-item-vend WHERE e-item-vend.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE e-item-vend THEN DO:
                        IF e-item-vend.underWidth EQ 0 THEN ASSIGN e-item-vend.underWidth = reftable.val[1].
                        IF e-item-vend.underLength EQ 0 THEN ASSIGN e-item-vend.underWidth = reftable.val[2].
                        IF e-item-vend.underWidthCost EQ 0 THEN ASSIGN e-item-vend.underWidth = reftable.val[3].
                        IF e-item-vend.underLengthCost EQ 0 THEN ASSIGN e-item-vend.underWidth = reftable.val[4]. 
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT e-item-vend NO-LOCK NO-ERROR.    
                RELEASE e-item-vend.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*EitemvendAdders*/
    METHOD PUBLIC INTEGER Batchrpt (INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER user-print  FOR user-print.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF user-print.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "Batchrpt"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST user-print WHERE user-print.company = reftable.company
                                        AND user-print.program-id = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE user-print THEN DO:
                        IF user-print.prgmName EQ "" THEN ASSIGN user-print.prgmName = reftable.code2. 
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT user-print NO-LOCK NO-ERROR.    
                RELEASE user-print.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*Batchrpt*/
    METHOD PUBLIC INTEGER CeComProbemk (INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER probeit   FOR probeit.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF probeit.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "ce/com/probemk.p"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST probeit WHERE probeit.company = reftable.company
                                     AND probeit.est-no = reftable.loc
                                     AND probeit.line = int(reftable.code)
                                     AND probeit.part-no = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE probeit THEN DO:
                        IF probeit.releaseCount EQ 0 THEN ASSIGN probeit.releaseCount = reftable.val[1].
                        IF probeit.pctCommission EQ 0 THEN ASSIGN probeit.pctCommission = reftable.val[2].
                        IF probeit.pctRoyalty EQ 0 THEN ASSIGN probeit.pctRoyalty = reftable.val[3].
                        IF probeit.pctWarehouse EQ 0 THEN ASSIGN probeit.pctWarehouse = reftable.val[4].
                        IF probeit.pctCustMargin EQ 0 THEN ASSIGN probeit.pctCustMargin = reftable.val[5].
                        IF probeit.totCostCommission EQ 0 THEN ASSIGN probeit.totCostCommission = reftable.val[6].
                        IF probeit.totCostRoyalty EQ 0 THEN ASSIGN probeit.totCostRoyalty = reftable.val[7].
                        IF probeit.totCostWarehousr EQ 0 THEN ASSIGN probeit.totCostWarehousr = reftable.val[8]. 
                        IF probeit.totCostCustMargin EQ 0 THEN ASSIGN probeit.totCostCustMargin = reftable.val[9].
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT probeit NO-LOCK NO-ERROR.    
                RELEASE probeit.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*CeComProbemk*/
    METHOD PUBLIC INTEGER CeComSelwhif1 (INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER eb   FOR eb.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF eb.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "ce/com/selwhif1.w"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST eb WHERE eb.company = reftable.company
                                     AND eb.est-no = reftable.loc
                                     AND eb.form-no = int(reftable.code)
                                     AND eb.blank-no = int(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE eb THEN DO:
                        IF eb.releaseCount EQ 0 THEN ASSIGN eb.releaseCount = reftable.val[1].
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT eb NO-LOCK NO-ERROR.    
                RELEASE eb.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*CeComSelwhif1*/
    METHOD PUBLIC INTEGER Arcashlvdate(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ar-cashl   FOR ar-cashl.
        FOR EACH reftable WHERE reftable.reftable = "ARCASHLVDDATE"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ar-cashl WHERE ar-cashl.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ar-cashl THEN    
                DO: 
                   ASSIGN ar-cashl.voided = YES.
                    IF ar-cashl.voidDate EQ ? THEN ASSIGN ar-cashl.voidDate = DATE(reftable.code).
                    ASSIGN iProcessCount = iProcessCount + 1.
                END    .                 
                FIND CURRENT ar-cashl NO-LOCK NO-ERROR.    
                RELEASE ar-cashl.      
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*Ar-cash*/
    METHOD PUBLIC INTEGER EstGetqtyw2(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER est  FOR est.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF est.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "est/getqty.w2"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST est WHERE est.est-no  = reftable.code
                                 AND est.company = reftable.company
                EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE est THEN DO:
                    IF est.markupPct EQ 0 THEN ASSIGN est.markupPct = reftable.val[1].                    
                  ASSIGN iProcessCount = iProcessCount + 1. 
                END.             
                FIND CURRENT est NO-LOCK NO-ERROR.    
                RELEASE est.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*EstGetQty*/
  METHOD PUBLIC INTEGER EstGlobquot(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "est/globquot.w"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*EstGlobquot*/
    METHOD PUBLIC INTEGER ArcashlInvline(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER ar-cashl  FOR ar-cashl.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF ar-cashl.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "ar-cashl.inv-line"
            NO-LOCK:
            DO TRANSACTION:    
                 FIND FIRST ar-invl WHERE ar-invl.company = reftable.company
                                AND ar-invl.x-no = int(SUBSTRING(reftable.code2,1,10)) 
                                AND ar-invl.line = int(SUBSTRING(reftable.code2,11,20)) 
                                EXCLUSIVE-LOCK NO-ERROR.
 
                            FIND FIRST ar-cashl WHERE ar-cashl.company = reftable.company
                                AND ar-cashl.c-no = int(SUBSTRING(reftable.code,1,10)) 
                                AND ar-cashl.LINE = int(SUBSTRING(reftable.code,11,20)) 
                                EXCLUSIVE-LOCK NO-ERROR.
                            IF AVAILABLE  ar-cashl AND AVAIL ar-invl THEN  
                                DO: 
                                    ASSIGN ar-cashl.invoiceXNo = ar-invl.x-no
                                           ar-cashl.invoiceLine = ar-invl.LINE.  
                                                          
                  ASSIGN iProcessCount = iProcessCount + 1. 
                END.             
                FIND CURRENT ar-cashl NO-LOCK NO-ERROR.    
                RELEASE ar-cashl.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*ArcashlInvline*/ 
    METHOD PUBLIC INTEGER OeBollSelected(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "oe-boll.selected"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*oe-boll*/
    METHOD PUBLIC INTEGER GsaFm(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 

        DEFINE BUFFER probe  FOR probe.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF probe.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "gsa-fm"
            NO-LOCK:
            DO TRANSACTION:    
                FOR EACH probe WHERE probe.company = reftable.company
                                 AND probe.est-no  = reftable.code
                    EXCLUSIVE-LOCK:
                        IF probe.gsa-fm EQ "" THEN ASSIGN probe.gsa-fm = string(reftable.val[1]). 
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT probe NO-LOCK NO-ERROR.    
                RELEASE probe.      
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*GsaFm*/
    METHOD PUBLIC INTEGER ProbePerMsf(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER probe     FOR probe.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF probe.
        ASSIGN iProcessCount = 0.        
        FOR EACH reftable WHERE reftable.reftable = "probe.per-msf"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST probe WHERE probe.company = reftable.company
                                   AND probe.est-no  = reftable.code
                                   AND probe.line    = int(reftable.code2)
                EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE probe THEN DO:
                    IF  probe.per-msf EQ 0 THEN ASSIGN  probe.per-msf = reftable.val[1].
                    IF  probe.setup EQ 0 THEN ASSIGN   probe.setup = reftable.val[2].                    
                  ASSIGN iProcessCount = iProcessCount + 1. 
                END.             
                FIND CURRENT probe NO-LOCK NO-ERROR.    
                RELEASE probe.      
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*ProbePerMsf*/
    METHOD PUBLIC INTEGER BlankRefTable(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = ""
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*Blank*/
    METHOD PUBLIC INTEGER CevEst3(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE i AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER eb  FOR eb.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF eb.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "ce/v-est3.w Unit#"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST eb WHERE eb.company     = reftable.company
                                AND eb.est-no      = reftable.loc
                                AND eb.form-no     = int(reftable.code2)
                                AND eb.blank-no    = int(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE eb THEN DO:   
                    DO i = 1 to 20: 
                        IF eb.side[i] EQ "" THEN ASSIGN eb.side[i] = reftable.dscr.
                        IF eb.unitNo[i] EQ 0 THEN ASSIGN eb.unitNo[i] = reftable.val[i].                                      
                    END.
                    ASSIGN iProcessCount = iProcessCount + 1.  
                END.
                FIND CURRENT eb NO-LOCK NO-ERROR.    
                RELEASE eb.      
        END.  /*FOR EACH reftable*/          
        RETURN iCnt.
    END METHOD. /*CeVest*/
    METHOD PUBLIC INTEGER v10TaxCode(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
        ASSIGN iProcessCount = 0.
        FOR EACH reftable WHERE reftable.reftable = "v10-TaxCode-Upgrade"
            NO-LOCK:
            DO TRANSACTION:
                CREATE reftable1.
                BUFFER-COPY reftable TO reftable1.
                RELEASE reftable1.
                FIND Breftable WHERE ROWID(Breftable) = ROWID(reftable)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE Breftable THEN DELETE Breftable. 
                RELEASE Breftable.   
                iCnt = iCnt + 1.
                IF iCnt >= ipCount THEN LEAVE.   
            END.              
        END.  /*FOR EACH reftable*/  
        RETURN iCnt.
    END METHOD. /*v10TaxCode*/
END CLASS.


