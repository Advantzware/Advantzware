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
    
        
    METHOD PUBLIC INTEGER STYFLU(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER styleFluteXref FOR styleFluteXref.
        DEFINE BUFFER reftable1      FOR reftable1.
        DEFINE BUFFER Breftable      FOR reftable. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
                   
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-boll.lot-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-boll WHERE oe-boll.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-boll THEN    
                    ASSIGN 
                        oe-boll.lot-no   = reftable.code
                        oe-boll.frt-pay  = reftable.code2
                        oe-boll.fob-code = reftable.dscr
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-rell.lot-no"
            NO-LOCK:
            DO TRANSACTION:                    
                FIND FIRST oe-rell WHERE oe-rell.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rell THEN    
                    ASSIGN 
                        oe-rell.lot-no   = reftable.code
                        oe-rell.frt-pay  = reftable.code2
                        oe-rell.fob-code = reftable.dscr
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.lot-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                    ASSIGN 
                        oe-rel.lot-no   = reftable.code
                        oe-rel.frt-pay  = reftable.code2
                        oe-rel.fob-code = reftable.dscr
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-rell.sell-price"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rell WHERE oe-rell.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rell THEN    
                    ASSIGN 
                        oe-rell.newSellPrice = reftable.val[1]
                        oe-rell.newZeroPrice = reftable.val[2]
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "fg-rctd.use-job"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST fg-rctd WHERE fg-rctd.r-no = int(reftable.company)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE fg-rctd THEN    
                    ASSIGN 
                        fg-rctd.use-job = (reftable.val[1] EQ 1)                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-ordl.whs-item"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ordl WHERE oe-ordl.company = (reftable.company)
                    AND oe-ordl.ord-no = INT(reftable.loc)
                    AND oe-ordl.i-no = (reftable.code)
                    AND oe-ordl.line = INT(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ordl THEN    
                    ASSIGN 
                        oe-ordl.managed = (reftable.val[1] EQ 1)                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-ord.whs-order"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ord WHERE oe-ord.company = (reftable.company)
                    AND oe-ord.ord-no = INT(reftable.loc)                                
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ord THEN    
                    ASSIGN 
                        oe-ord.managed = (reftable.val[1] EQ 1)                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-ordl.q-no"                            
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ordl WHERE oe-ordl.company = (reftable.company)
                    AND oe-ordl.ord-no = INT(reftable.loc)
                    AND oe-ordl.i-no = (reftable.code)
                    AND oe-ordl.line = INT(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ordl THEN    
                    ASSIGN 
                        oe-ordl.q-no = INT(reftable.val[1] EQ 1)                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.job"                            
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.company = (reftable.company)
                    AND oe-rel.r-no = INT(reftable.code)                                                               
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                    ASSIGN 
                        oe-rel.job = INT(reftable.code2)               
                        . 
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
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER fg-rctd   FOR fg-rctd.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF fg-rctd.
        FOR EACH reftable NO-LOCK
            WHERE reftable.reftable EQ "fg-rctd.user-id" :
            DO TRANSACTION:    
                FIND FIRST fg-rctd WHERE fg-rctd.company = reftable.company
                    AND fg-rctd.r-no = INT(reftable.loc) EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE fg-rctd THEN 
                DO:
                    ASSIGN
                        fg-rctd.created-by = reftable.code
                        fg-rctd.updated-by = reftable.code2.
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
        
        FOR EACH reftable NO-LOCK
            WHERE reftable.reftable EQ "fg-bin.cost" USE-INDEX rec_key:
            DO TRANSACTION:    
                FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = reftable.company
                    AND fg-rdtlh.rec_key = reftable.rec_key EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE fg-rdtlh THEN 
                DO:
                    ASSIGN
                        fg-rdtlh.avg-cost     = reftable.val[1]
                        fg-rdtlh.last-cost    = reftable.val[2]
                        fg-rdtlh.std-fix-cost = reftable.val[3]
                        fg-rdtlh.std-lab-cost = reftable.val[4]
                        fg-rdtlh.std-mat-cost = reftable.val[5]
                        fg-rdtlh.std-tot-cost = reftable.val[6]
                        fg-rdtlh.std-var-cost = reftable.val[7].
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.sell-price"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.r-no = INT(reftable.company)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                    ASSIGN 
                        oe-rel.sell-price = reftable.val[1]
                        oe-rel.zeroPrice  = reftable.val[2]
                        . 
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
                END. 
                ELSE 
                DO: 
                    iHmCount = iHmCount + 1 . 
                    user-print.field-value[iHmCount] = reftable.CODE .
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
                END. 
                ELSE 
                DO: 
                    iHm1SFCount = iHm1SFCount + 1 .
                    IF iHm1SFCount LT 100 THEN
                        user-print.field-value[iHm1SFCount] = reftable.loc .
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
                END. 
                ELSE 
                DO: 
                    iHm1Count = iHm1Count + 1 . 
                    IF iHm1Count LT 100 THEN
                        user-print.field-value[iHm1Count] = reftable.CODE .
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
                END. 
                ELSE 
                DO: 
                    iHmAcctCount = iHmAcctCount + 1 . 
                    IF iHmAcctCount LT 100 THEN
                        user-print.field-value[iHmAcctCount] = reftable.CODE .
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe-boll.sell-price"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-boll WHERE oe-boll.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-boll THEN    
                    ASSIGN 
                        oe-boll.sell-price = reftable.val[1]
                        oe-boll.zeroPrice  = reftable.val[2]
                        . 
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
        FOR EACH reftable WHERE reftable.reftable = "job.create-time"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST job WHERE job.company = reftable.company
                    AND job.loc     = ""
                    AND job.job     = INT(reftable.code)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE job THEN    
                    ASSIGN 
                        job.create-time = reftable.val[1]                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "oe/ordlmisc.p"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-ordm WHERE oe-ordm.company = reftable.company
                    AND oe-ordm.ord-no     = INT(reftable.loc)
                    AND oe-ordm.line       = INT(reftable.code)
                    AND oe-ordm.charge     = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-ordm THEN    
                    ASSIGN 
                        oe-ordm.miscType    = INT(reftable.val[1])
                        oe-ordm.estPrepEqty = reftable.val[2]
                        oe-ordm.estPrepLine = INT(reftable.val[3])
                        oe-ordm.miscInd     = STRING(reftable.val[4])
                        oe-ordm.est-no      = reftable.dscr                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "FACTORED"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST itemfg WHERE itemfg.company = reftable.company
                    AND itemfg.i-no     = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN    
                    ASSIGN 
                        itemfg.factored = IF reftable.code2 = "YES" THEN YES ELSE NO                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "terms.cod"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST terms WHERE terms.company = reftable.company
                    AND terms.t-code  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE terms THEN    
                    ASSIGN 
                        terms.cod = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "itemfg-ink.occurs"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST itemfg-ink WHERE itemfg-ink.rec_key = reftable.rec_key
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE itemfg-ink THEN    
                    ASSIGN 
                        itemfg-ink.occurs = reftable.val[1]                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "shipto.mandatory-tax"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST shipto WHERE shipto.company = reftable.company
                    AND shipto.cust-no  = reftable.code
                    AND shipto.ship-id  = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN    
                    ASSIGN 
                        shipto.tax-mandatory = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "cust.po-mand"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST cust WHERE cust.company = reftable.company
                    AND cust.cust-no  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE cust THEN    
                    ASSIGN 
                        cust.po-mandatory = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "cust.show-set"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST cust WHERE cust.company = reftable.company
                    AND cust.cust-no  = reftable.code                             
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE cust THEN    
                    ASSIGN 
                        cust.show-set = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE                
                        . 
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

        FOR EACH reftable WHERE reftable.reftable = "MachinePosition"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST est-op WHERE est-op.company = substring(reftable.company,1,10)
                    AND est-op.est-no  = substring(reftable.company,11,10)  
                    AND est-op.LINE = integer(reftable.loc)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE est-op THEN    
                    ASSIGN 
                        est-op.len-pos = int(reftable.val[1])
                        est-op.len     = DECIMAL(reftable.val[4])
                        est-op.wid-pos = INTEGER(reftable.val[2])
                        est-op.wid     = DECIMAL(reftable.val[5])
                        est-op.dep-pos = INTEGER(reftable.val[3])
                        est-op.dep     = DECIMAL(reftable.val[6])               
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "GLACCTDISC"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST account WHERE account.company = reftable.company
                    AND account.actnum     = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE account THEN    
                    ASSIGN 
                        account.terms-discount = IF reftable.val[1] = 1 THEN YES ELSE NO                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "cust.flat-comm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST cust WHERE cust.company = reftable.company
                    AND cust.cust-no = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE cust THEN    
                    ASSIGN 
                        cust.flatCommPct = DEC(reftable.val[1])                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "FREEZENOTE"
            NO-LOCK:
            FIND FIRST job-hdr WHERE job-hdr.company = reftable.company
                                 AND job-hdr.job-no  = reftable.loc
                                 AND job-hdr.job-no2 = int(reftable.code)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE job-hdr THEN    
            ASSIGN 
                job-hdr.freezeNote = YES   
                . 
            RELEASE job-hdr.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*job-hdr*/
    METHOD PUBLIC INTEGER DropslitToHighlight():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH est EXCLUSIVE-LOCK:
            ASSIGN 
                est.highlight = est.dropslit                
                est.dropslit = no
                .                 
            RELEASE est.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*est*/
    METHOD PUBLIC INTEGER Dropslit():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "dropslit"
            NO-LOCK:
            FIND FIRST est WHERE est.company = reftable.company AND
                est.dropslit = reftable.loc eq "yes"                
                             AND est.est-no  = reftable.code
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE est THEN    
            ASSIGN 
                est.recalc    = logical(reftable.val[1]) 
                est.recalc-mr = logical(reftable.val[2])         
    END METHOD. /*est*/
    METHOD PUBLIC INTEGER MachPlainJob():
            DO TRANSACTION:    
                FIND FIRST job-hdr WHERE job-hdr.company = reftable.company
                    AND job-hdr.job-no  = reftable.loc
                    AND job-hdr.job-no2 = int(reftable.code)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE job-hdr THEN    
                    ASSIGN 
                        job-hdr.freezeNote = YES
                        .                 
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
    METHOD PUBLIC INTEGER MachPlainJob(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER mach      FOR mach.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF mach.
        
        FOR EACH reftable WHERE reftable.reftable = "mach.plain-job"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST mach WHERE mach.company = reftable.company
                    AND mach.loc     = reftable.loc
                    AND mach.m-code = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE mach THEN    
                    ASSIGN 
                        mach.plain-job = reftable.val[1] EQ 1                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "PREPLASTJOB"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST prep WHERE prep.company = reftable.company
                    AND prep.loc     = reftable.loc
                    AND prep.Code = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE prep THEN    
                    ASSIGN 
                        prep.last-job-no  = reftable.code2
                        prep.last-job-no2 = reftable.val[1]                 
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "gl-rpt.pct-subtotal"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST gl-rpt WHERE gl-rpt.company = reftable.company
                    AND gl-rpt.rp      = reftable.code
                    AND gl-rpt.line    = int(reftable.code2)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE gl-rpt THEN    
                    ASSIGN 
                        gl-rpt.pct-subtotal = reftable.val[1] EQ 1              
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.broker-pct"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc      = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                    ASSIGN 
                        ce-ctrl.broker-pct = reftable.val[1]             
                        . 
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
    METHOD PUBLIC INTEGER RmBinAgingDate(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER rm-bin    FOR rm-bin.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF rm-bin.
        
        FOR EACH reftable WHERE reftable.reftable = "rm-bin.aging-date"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST rm-bin WHERE rm-bin.company = reftable.company
                    AND rm-bin.loc     = SUBSTRING(reftable.code, 1, 50)
                    AND rm-bin.i-no    = reftable.loc
                    AND rm-bin.loc-bin   = SUBSTRING(reftable.code, 51, 50)
                    AND rm-bin.tag      = reftable.code2
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE rm-bin THEN    
                    ASSIGN 
                        rm-bin.aging-date = DATE(INT(reftable.val[1]))             
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.fold-pct"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                    ASSIGN 
                        ce-ctrl.fold-pct = reftable.val[1]             
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.fg-rate-farm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                    ASSIGN 
                        ce-ctrl.fg-rate-farm = reftable.val[1]             
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.rm-rate-farm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                    ASSIGN 
                        ce-ctrl.fg-rate-farm = reftable.val[1]             
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "ce-ctrl.hand-pct-farm"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST ce-ctrl WHERE ce-ctrl.company = reftable.company
                    AND ce-ctrl.loc     = reftable.loc
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE ce-ctrl THEN    
                    ASSIGN 
                        ce-ctrl.hand-pct-farm = reftable.val[1]          
                        . 
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
    METHOD PUBLIC INTEGER EstOpLock(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER est       FOR est.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF est.
        
        FOR EACH reftable WHERE reftable.reftable = "est.op-lock"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST est WHERE est.company = reftable.company
                    AND est.loc     = reftable.loc
                    AND est.est-no  = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE est THEN    
                    ASSIGN 
                        est.recalc    = LOGICAL(reftable.val[1]) 
                        est.recalc-mr = LOGICAL(reftable.val[2])         
                        . 
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
    METHOD PUBLIC INTEGER OeRelScode(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER reftable1 FOR reftable1.
        DEFINE BUFFER Breftable FOR reftable. 
        DEFINE BUFFER oe-rel    FOR oe-rel.
        
        DISABLE TRIGGERS FOR LOAD OF reftable.        
        DISABLE TRIGGERS FOR LOAD OF oe-rel.
        
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.s-code"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST oe-rel WHERE oe-rel.r-no = int(reftable.company)
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN    
                    ASSIGN 
                        oe-rel.s-code = reftable.code        
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "mach.obsolete"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST mach WHERE mach.company = reftable.company
                    AND mach.loc     = reftable.loc
                    AND mach.m-code = reftable.code
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE mach THEN    
                    ASSIGN 
                        mach.obsolete = reftable.val[1] EQ 1                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "JDEDWARDCUST#"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST shipto WHERE shipto.company = reftable.company
                    AND shipto.cust-no = reftable.code 
                    AND shipto.ship-id = reftable.code2                           
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN    
                    ASSIGN 
                        shipto.exportCustID = reftable.dscr                
                        . 
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
        
        FOR EACH reftable WHERE reftable.reftable = "users.phone-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.phone = IF users.phone = "" AND reftable.CODE = "" THEN reftable.CODE ELSE users.phone
                        .
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
        
        FOR EACH reftable WHERE reftable.reftable = "users.fax-no"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.fax = IF users.fax = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax
                        .
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
        
        FOR EACH reftable WHERE reftable.reftable = "users.phone-cnty"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.phone-cnty = IF users.phone-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.phone-cnty
                        .
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
        
        FOR EACH reftable WHERE reftable.reftable = "users.fax-cnty"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST users WHERE users.user_id = reftable.company         
                    EXCLUSIVE-LOCK NO-ERROR.
                IF AVAILABLE users THEN    
                    ASSIGN
                        users.fax-cnty = IF users.fax-cnty = "" AND reftable.CODE NE "" THEN reftable.CODE ELSE users.fax-cnty
                        . 
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
    METHOD PUBLIC INTEGER scoreType(INPUT ipCount AS INTEGER):
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE BUFFER scoreType      FOR scoreType.
        DEFINE BUFFER reftable1      FOR reftable1.
        DEFINE BUFFER Breftable      FOR reftable. 
        
        DISABLE TRIGGERS FOR LOAD OF reftable.
                   
        FOR EACH reftable WHERE reftable.reftable = "scoreType"
            NO-LOCK:
            DO TRANSACTION:    
                FIND FIRST scoreType WHERE scoreType.company   = reftable.company
                                       AND scoreType.scoreType = reftable.code
                    NO-LOCK NO-ERROR.
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
    END METHOD. /*scoreType*/
END CLASS.


