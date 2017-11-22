 
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
END CLASS.