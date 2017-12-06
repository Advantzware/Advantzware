 
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
    METHOD PUBLIC INTEGER STYFLU():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH styleFluteXref:
            DELETE styleFluteXref.
        END.    
        FOR EACH reftable WHERE reftable.reftable = "STYFLU"
            NO-LOCK:
            CREATE styleFluteXref.
            BUFFER-COPY reftable TO styleFluteXref.
            ASSIGN 
                styleFluteXref.styleCode = reftable.company
                styleFluteXref.fluteCode = reftable.loc
                styleFluteXref.description = reftable.dscr
                .   
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*STYFLU*/
    
    METHOD PUBLIC INTEGER STYSCORE():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH styleScoreXref:
            DELETE styleScoreXref.
        END.    
        FOR EACH reftable WHERE reftable.reftable = "STYSCORE"
            NO-LOCK:
            CREATE styleScoreXref.
            BUFFER-COPY reftable TO styleScoreXref.
            ASSIGN 
                styleScoreXref.styleCode   = reftable.company
                styleScoreXref.scoreType   = reftable.loc
                styleScoreXref.description = reftable.dscr
                .   
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*STYSCORE*/
    METHOD PUBLIC INTEGER OeBollLotNo():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-boll.lot-no"
            NO-LOCK:
            FIND FIRST oe-boll WHERE oe-boll.rec_key = reftable.rec_key
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-boll THEN    
            ASSIGN 
                oe-boll.lot-no   = reftable.code
                oe-boll.frt-pay  = reftable.code2
                oe-boll.fob-code = reftable.dscr
                . 
            RELEASE oe-boll.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-boll*/
    METHOD PUBLIC INTEGER OeRellLotNo():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-rell.lot-no"
            NO-LOCK:
            FIND FIRST oe-rell WHERE oe-rell.rec_key = reftable.rec_key
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rell THEN    
            ASSIGN 
                oe-rell.lot-no   = reftable.code
                oe-rell.frt-pay  = reftable.code2
                oe-rell.fob-code = reftable.dscr
                . 
            RELEASE oe-rell.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-rell*/
    METHOD PUBLIC INTEGER OeRelLotNo():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.lot-no"
            NO-LOCK:
            FIND FIRST oe-rel WHERE oe-rel.rec_key = reftable.rec_key
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN    
            ASSIGN 
                oe-rel.lot-no   = reftable.code
                oe-rel.frt-pay  = reftable.code2
                oe-rel.fob-code = reftable.dscr
                . 
            RELEASE oe-rel.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-rel*/
    METHOD PUBLIC INTEGER OeRellSellPrice():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-rell.sell-price"
            NO-LOCK:
            FIND FIRST oe-rell WHERE oe-rell.rec_key = reftable.rec_key
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rell THEN    
            ASSIGN 
                oe-rell.newSellPrice  = reftable.val[1]
                oe-rell.newZeroPrice  = reftable.val[2]
                . 
            RELEASE oe-rell.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-rell*/
    METHOD PUBLIC INTEGER FgRctdUseJob():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "fg-rctd.use-job"
            NO-LOCK:
            FIND FIRST fg-rctd WHERE fg-rctd.r-no = int(reftable.company)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE fg-rctd THEN    
            ASSIGN 
                fg-rctd.use-job  = (reftable.val[1] EQ 1)                
                . 
            RELEASE fg-rctd.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*fg-rctd*/
    METHOD PUBLIC INTEGER OeOrdlWhsItem():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-ordl.whs-item"
            NO-LOCK:
            FIND FIRST oe-ordl WHERE oe-ordl.company = (reftable.company)
                                AND oe-ordl.ord-no = INT(reftable.loc)
                                AND oe-ordl.i-no = (reftable.code)
                                AND oe-ordl.line = INT(reftable.code2)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-ordl THEN    
            ASSIGN 
                oe-ordl.managed  = (reftable.val[1] EQ 1)                
                . 
            RELEASE oe-ordl.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-ordl*/
    METHOD PUBLIC INTEGER OeOrdWhsOrder():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-ord.whs-order"
            NO-LOCK:
            FIND FIRST oe-ord WHERE oe-ord.company = (reftable.company)
                                AND oe-ord.ord-no = INT(reftable.loc)                                
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-ord THEN    
            ASSIGN 
                oe-ord.managed  = (reftable.val[1] EQ 1)                
                . 
            RELEASE oe-ord.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-ord*/
    METHOD PUBLIC INTEGER OeOrdlQNo():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-ordl.q-no"                            
            NO-LOCK:
            FIND FIRST oe-ordl WHERE oe-ordl.company = (reftable.company)
                                AND oe-ordl.ord-no = INT(reftable.loc)
                                AND oe-ordl.i-no = (reftable.code)
                                AND oe-ordl.line = INT(reftable.code2)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-ordl THEN    
            ASSIGN 
                oe-ordl.q-no  = INT(reftable.val[1] EQ 1)                
                . 
            RELEASE oe-ordl.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-ordl*/
    METHOD PUBLIC INTEGER OeRelJob():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.job"                            
            NO-LOCK:
            FIND FIRST oe-rel WHERE oe-rel.company = (reftable.company)
                                AND oe-rel.r-no = INT(reftable.code)                                                               
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN    
            ASSIGN 
                oe-rel.job  = INT(reftable.code2)               
                . 
            RELEASE oe-rel.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-rel*/
    METHOD PUBLIC INTEGER FgRctdUserId():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable NO-LOCK
         WHERE reftable.reftable EQ "fg-rctd.user-id" :
         FIND FIRST fg-rctd WHERE fg-rctd.company = reftable.company
          AND fg-rctd.r-no = INT(reftable.loc) EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE fg-rctd THEN DO:
          ASSIGN
              fg-rctd.created-by = reftable.code
              fg-rctd.updated-by = reftable.code2.
          END.
         END.       
        RETURN iCnt.
    END METHOD. /*fg-bin*/
    METHOD PUBLIC INTEGER FgBinCost():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable NO-LOCK
          WHERE reftable.reftable EQ "fg-bin.cost" USE-INDEX rec_key:
          FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = reftable.company
              AND fg-rdtlh.rec_key = reftable.rec_key EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE fg-rdtlh THEN DO:
              ASSIGN
                  fg-rdtlh.avg-cost     = reftable.val[1]
                  fg-rdtlh.last-cost    = reftable.val[2]
                  fg-rdtlh.std-fix-cost = reftable.val[3]
                  fg-rdtlh.std-lab-cost = reftable.val[4]
                  fg-rdtlh.std-mat-cost = reftable.val[5]
                  fg-rdtlh.std-tot-cost = reftable.val[6]
                  fg-rdtlh.std-var-cost = reftable.val[7].
          END.
        END.  
        RETURN iCnt.
    END METHOD. /*fg-bin*/
END CLASS.