/* cec/slotwidth.i   Slot Width
*/
DEF VAR gotAttAdder AS LOG NO-UNDO.
DEF VAR iDieQty AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.

IF eb.gluelap = 0 THEN DO:
    FIND FIRST style WHERE style.company EQ eb.company
                     AND style.style EQ eb.style NO-LOCK NO-ERROR.
    IF AVAIL style AND lookup(style.TYPE,'P,R') > 0 THEN DO:
       FOR EACH est-op NO-LOCK WHERE est-op.company = eb.company
                                 AND est-op.est-no = eb.est-no
                                 AND est-op.s-num = eb.form-no
                                 AND est-op.LINE < 500:
           IF can-do(cepdies-cha,est-op.m-code) THEN DO:
              gotAttAdder = NO.
              

              mach-attach-style:
              FOR EACH mach-attach-pat NO-LOCK
                  WHERE mach-attach-pat.company  EQ est-op.company
                    AND mach-attach-pat.m-code   EQ est-op.m-code
                    AND mach-attach-pat.style EQ eb.style
                    AND mach-attach-pat.priority > 0
                    BY mach-attach-pat.priority:
                FIND mach-attach OF mach-attach-pat NO-LOCK NO-ERROR.
                iDieQty = IF mach-attach.qty > 0 THEN mach-attach.qty ELSE style.dim-df.
           
                IF ef.cal >= mach-attach-pat.caliperMin AND
                   ef.cal <= mach-attach-pat.caliperMax AND
                   eb.t-wid >= mach-attach-pat.blankwidthmin AND
                   eb.t-wid <= mach-attach-pat.blankwidthmax AND
                   iDieQty <= mach-attach-pat.onHandDieQty 
                THEN DO:
                  DO i = 1 TO 30:
                    IF eb.k-len-array2[i] <> 0  AND
                      (eb.k-len-array2[i] < mach-attach-pat.internalCellMin OR
                       eb.k-len-array2[i] > mach-attach-pat.internalCellMax)
                     THEN NEXT mach-attach-style.
                  END.
                  ASSIGN gotAttAdder = YES
                         eb.gluelap = mach-attach-pat.slotwidth.    
                  LEAVE mach-attach-style.
                END.            
              END.
              IF NOT gotAttAdder THEN
              mach-attach-nostyle:
              FOR EACH mach-attach-pat NO-LOCK WHERE mach-attach-pat.company  EQ est-op.company
                      AND mach-attach-pat.m-code   EQ est-op.m-code
                      AND mach-attach-pat.style EQ ""
                      AND mach-attach-pat.priority > 0
                      BY mach-attach-pat.priority  :
              
                   FIND mach-attach OF mach-attach-pat NO-LOCK NO-ERROR.
                   iDieQty = IF mach-attach.qty > 0 THEN mach-attach.qty ELSE style.dim-df.
        
                   IF ef.cal >= mach-attach-pat.caliperMin AND
                      ef.cal <= mach-attach-pat.caliperMax AND
                      eb.t-wid >= mach-attach-pat.blankwidthmin AND
                      eb.t-wid <= mach-attach-pat.blankwidthmax AND
                      iDieQty <= mach-attach-pat.onHandDieQty 
                   THEN DO: 
                       DO i = 1 TO 30:
                          IF eb.k-len-array2[i] <> 0  AND
                             (eb.k-len-array2[i] < mach-attach-pat.internalCellMin OR
                             eb.k-len-array2[i] > mach-attach-pat.internalCellMax)
                             THEN NEXT mach-attach-nostyle.
                       END.
                       ASSIGN gotAttAdder = YES
                              eb.gluelap = mach-attach-pat.slotwidth.
                       LEAVE mach-attach-nostyle.
                   END.            
              END.  /* for each mach-attach-pat  noStyle */

           END. /* if */
       END.  /* each est-op*/
    END.  /* style P, R */       
END.
