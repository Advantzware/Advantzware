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
    END METHOD. /*fg-rctd*/
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
    METHOD PUBLIC INTEGER OeRelSellPrice():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-rel.sell-price"
            NO-LOCK:
            FIND FIRST oe-rel WHERE oe-rel.r-no = INT(reftable.company)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN    
            ASSIGN 
                oe-rel.sell-price = reftable.val[1]
                oe-rel.zeroPrice  = reftable.val[2]
                . 
            RELEASE oe-rel.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-rel*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM5():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        DEFINE VARIABLE iHmCount as integer NO-UNDO .
        FOR EACH reftable WHERE
            reftable.reftable EQ "HM5" NO-LOCK:
            
            FIND FIRST user-print exclusive-LOCK
                WHERE user-print.company EQ reftable.loc
                AND user-print.program-id EQ "HM5" 
                AND user-print.user-id EQ reftable.company NO-ERROR.
            
            if not avail user-print then do:
                CREATE user-print .
                ASSIGN
                    user-print.company = reftable.loc 
                    user-print.program-id = "HM5" 
                    user-print.user-id = reftable.company 
                    user-print.field-value[1] = reftable.CODE
                    . 
                iHmCount = 1 . 
            end. 
            else do: 
                iHmCount = iHmCount + 1 . 
                user-print.field-value[iHmCount] = reftable.CODE .
            end.
            iCnt = iCnt + 1. 
        end.
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM1SF():
      DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
      DEFINE VARIABLE iHm1SFCount as integer NO-UNDO .

      FOR EACH reftable WHERE
          reftable.reftable EQ "HM1SF" NO-LOCK:

          FIND FIRST user-print exclusive-LOCK
              WHERE user-print.program-id EQ "HM1SF" 
              AND user-print.user-id EQ reftable.company NO-ERROR.

          if not avail user-print then do:
              CREATE user-print .
              ASSIGN
                  user-print.program-id = "HM1SF" 
                  user-print.user-id = reftable.company 
                  user-print.field-value[1] = reftable.loc
                  . 
              iHm1SFCount = 1 . 
         end. 
         else do: 
             iHm1SFCount = iHm1SFCount + 1 .
             IF iHm1SFCount LT 100 THEN
                 user-print.field-value[iHm1SFCount] = reftable.loc .
         end.
         iCnt = iCnt + 1. 
       end.
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM1():
      DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
      DEFINE VARIABLE iHm1Count as integer NO-UNDO .

      FOR EACH reftable WHERE
          reftable.reftable EQ "HM1" NO-LOCK:
          
          FIND FIRST user-print exclusive-LOCK
              WHERE user-print.company EQ reftable.loc
              AND user-print.program-id EQ "HM1" 
              AND user-print.user-id EQ reftable.company NO-ERROR.

          if not avail user-print then do:
              CREATE user-print .
              ASSIGN
                  user-print.company = reftable.loc 
                  user-print.program-id = "HM1" 
                  user-print.user-id = reftable.company 
                  user-print.field-value[1] = reftable.CODE
                  . 
              iHm1Count = 1 . 
          end. 
          else do: 
              iHm1Count = iHm1Count + 1 . 
              IF iHm1Count LT 100 THEN
                  user-print.field-value[iHm1Count] = reftable.CODE .
          end.
         iCnt = iCnt + 1. 
       end.
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER ReftoUserPrintHM1Acct():
      DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
      DEFINE VARIABLE iHmAcctCount as integer NO-UNDO .

      FOR EACH reftable WHERE
          reftable.reftable EQ "HM1Acct" NO-LOCK:

          FIND FIRST user-print exclusive-LOCK
              WHERE user-print.company EQ reftable.loc
              AND user-print.program-id EQ "HM1Acct" 
              AND user-print.user-id EQ reftable.company NO-ERROR.

          if not avail user-print then do:
              CREATE user-print .
              ASSIGN
                  user-print.company = reftable.loc 
                  user-print.program-id = "HM1Acct" 
                  user-print.user-id = reftable.company 
                  user-print.field-value[1] = reftable.CODE
                  . 
              iHmAcctCount = 1 . 
          end. 
          else do: 
              iHmAcctCount = iHmAcctCount + 1 . 
              IF iHmAcctCount LT 100 THEN
                  user-print.field-value[iHmAcctCount] = reftable.CODE .
          end.
          iCnt = iCnt + 1. 
       end.
        RETURN iCnt.
    END METHOD. /*User-print*/
    METHOD PUBLIC INTEGER OeBollSellPrice():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe-boll.sell-price"
            NO-LOCK:
            FIND FIRST oe-boll WHERE oe-boll.rec_key = reftable.rec_key
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-boll THEN    
            ASSIGN 
                oe-boll.sell-price = reftable.val[1]
                oe-boll.zeroPrice  = reftable.val[2]
                . 
            RELEASE oe-boll.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-boll*/
    METHOD PUBLIC INTEGER JobCreateTime():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "job.create-time"
            NO-LOCK:
            FIND FIRST job WHERE job.company = reftable.company
                             AND job.loc     = ""
                             AND job.job     = INT(reftable.code)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE job THEN    
            ASSIGN 
                job.create-time = reftable.val[1]                
                . 
            RELEASE job.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*job*/
    METHOD PUBLIC INTEGER OeOrdlMisc():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "oe/ordlmisc.p"
            NO-LOCK:
            FIND FIRST oe-ordm WHERE oe-ordm.company = reftable.company
                              AND oe-ordm.ord-no     = INT(reftable.loc)
                              AND oe-ordm.line       = INT(reftable.code)
                              AND oe-ordm.charge     = reftable.code2
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-ordm THEN    
            ASSIGN 
                oe-ordm.miscType = INT(reftable.val[1])
                oe-ordm.estPrepEqty = reftable.val[2]
                oe-ordm.estPrepLine = INT(reftable.val[3])
                oe-ordm.miscInd = STRING(reftable.val[4])
                oe-ordm.est-no = reftable.dscr                
                . 
            RELEASE oe-ordm.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*oe-ordm*/
    METHOD PUBLIC INTEGER Factored():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "FACTORED"
            NO-LOCK:
            FIND FIRST itemfg WHERE itemfg.company = reftable.company
                             AND itemfg.i-no     = reftable.code                             
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN    
            ASSIGN 
                itemfg.factored = IF reftable.code2 = "YES" THEN YES ELSE NO                
                . 
            RELEASE itemfg.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*itemfg*/
    METHOD PUBLIC INTEGER Termscod():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "terms.cod"
            NO-LOCK:
            FIND FIRST terms WHERE terms.company = reftable.company
                               AND terms.t-code  = reftable.code                             
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE terms THEN    
            ASSIGN 
                terms.cod = IF reftable.val[1] = 1 THEN TRUE ELSE FALSE                
                . 
            RELEASE terms.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*terms*/
    METHOD PUBLIC INTEGER Stack():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "STACK"
            NO-LOCK:
            FIND FIRST stackPattern WHERE stackPattern.stackCode  = reftable.code                             
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE stackPattern THEN CREATE stackPattern.   
            ASSIGN 
                stackPattern.stackCode        = reftable.code
                stackPattern.rec_key          = reftable.rec_key
                stackPattern.stackDescription = reftable.dscr
                stackPattern.stackCount       = reftable.val[1]                           
                . 
            RELEASE stackPattern.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/
        FOR EACH reftable WHERE reftable.reftable = "STACKSTRAP"
            NO-LOCK:
            FIND FIRST stackPattern WHERE stackPattern.stackCode  = reftable.code                             
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE stackPattern THEN CREATE stackPattern.   
            ASSIGN                   
                stackPattern.strapCode    = reftable.code2
                stackPattern.strapCount   = reftable.val[1] 
                stackPattern.strapFormula = reftable.dscr        
                . 
            RELEASE stackPattern.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/
        FOR EACH reftable WHERE reftable.reftable = "STACKPAT"
            NO-LOCK:
            FIND FIRST stackPattern WHERE stackPattern.stackCode  = reftable.code                             
                EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE stackPattern THEN CREATE stackPattern.   
            ASSIGN                 
                stackPattern.stackImage = reftable.dscr            
                . 
            RELEASE stackPattern.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*stackPattern*/
    METHOD PUBLIC INTEGER ItemfgInkOccurs():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "itemfg-ink.occurs"
            NO-LOCK:
            FIND FIRST itemfg-ink WHERE itemfg-ink.company = reftable.company
                                    AND itemfg-ink.rec_key = reftable.rec_key
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE itemfg-ink THEN    
            ASSIGN 
                itemfg-ink.occurs = reftable.val[1]                
                . 
            RELEASE itemfg-ink.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*itemfg-ink*/
    METHOD PUBLIC INTEGER MachinePosition():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "MachinePosition"
            NO-LOCK:
            FIND FIRST est-op WHERE est-op.company = substring(reftable.company,1,10)
                               AND est-op.est-no  = substring(reftable.company,11,10)  
                               AND est-op.LINE = integer(reftable.loc)
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE est-op THEN    
            ASSIGN 
                est-op.len-pos = int(reftable.val[1])
                est-op.len = DECIMAL(reftable.val[4])
                est-op.wid-pos = INTEGER(reftable.val[2])
                est-op.wid = DECIMAL(reftable.val[5])
                est-op.dep-pos = integer(reftable.val[3])
                est-op.dep = DECIMAL(reftable.val[6])               
                . 
            RELEASE est-op.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*est-op*/
    METHOD PUBLIC INTEGER GlAcctDisc():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "GLACCTDISC"
            NO-LOCK:
            FIND FIRST account WHERE account.company = reftable.company
                             AND account.actnum     = reftable.code
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE account THEN    
            ASSIGN 
                account.terms-discount = IF reftable.val[1] = 1 THEN YES ELSE NO                
                . 
            RELEASE account.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*account*/
    METHOD PUBLIC INTEGER CustFlatComm():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "cust.flat-comm"
            NO-LOCK:
            FIND FIRST cust WHERE cust.company = reftable.company
                              AND cust.cust-no = reftable.code
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE cust THEN    
            ASSIGN 
                cust.flatCommPct = DEC(reftable.val[1])                
                . 
            RELEASE cust.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*cust*/
    METHOD PUBLIC INTEGER StylePerMsf():
        DEFINE VARIABLE iCnt AS INTEGER NO-UNDO.
        FOR EACH reftable WHERE reftable.reftable = "style.per-msf"
            NO-LOCK:
            FIND FIRST style WHERE style.company = reftable.company
                               AND style.style   = reftable.code
                EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE style THEN DO:
                IF style.sqft-len-trim = 0 THEN   
                    ASSIGN style.sqft-len-trim = reftable.val[1].
                IF style.sqft-wid-trim = 0 THEN    
                    ASSIGN style.sqft-wid-trim = reftable.val[2]. 
            END.    
            RELEASE style.      
            iCnt = iCnt + 1.                 
        END.  /*FOR EACH reftable*/         
        RETURN iCnt.
    END METHOD. /*style*/
END CLASS.
