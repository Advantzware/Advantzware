
  
  /* Obtain next available r-no for release creation 
  */
  
  
  DEF INPUT PARAMETER ip-mode AS CHAR NO-UNDO. /* oe-rel or oe-relh */
  DEF OUTPUT PARAMETER op-r-no AS INT NO-UNDO.

  {custom/globdefs.i}
  {custom/gcompany.i}
  {sys/inc/var.i NEW SHARED}
  {sys/inc/varasgn.i}


  DEF VAR iCnt AS INT NO-UNDO.
  DEF VAR v-nxt-r-no AS INT NO-UNDO.
  DEF VAR v-last-r-no AS INT NO-UNDO.
  DEF VAR v-error AS LOG NO-UNDO.
  DEF VAR v-retries AS INT NO-UNDO.
  DEF BUFFER bf-reftable FOR reftable.
  DEF VAR v-all-locked AS LOG NO-UNDO.
  DEF VAR v-ref-value AS CHAR NO-UNDO.

  /* Original Logic, use as a starting point  */
  IF ip-mode = "oe-rel" THEN DO:
      v-Ref-Value = "NextRelease".
      FIND FIRST oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.
      v-last-r-no = IF AVAIL oe-rel THEN oe-rel.r-no + 1 ELSE 1.
  END.
  ELSE IF ip-mode = "release#" THEN DO:
    v-ref-value = "NextRelease#".
    FIND LAST oe-relh
        WHERE oe-relh.company EQ cocode
        USE-INDEX release# NO-LOCK NO-ERROR.
    v-last-r-no = IF AVAIL oe-relh THEN oe-relh.release# + 1 ELSE 1.
   
  END.
  ELSE DO:
      v-ref-value = "NextRelH".
      FIND LAST oe-relh USE-INDEX r-no NO-LOCK NO-ERROR.
      v-last-r-no = IF AVAIL oe-relh THEN oe-relh.r-no + 1 ELSE 1.
  END.
  
  v-error = NO. v-retries = 0. v-nxt-r-no = 0.
  test-r-no:
  REPEAT:
    IF v-nxt-r-no EQ 0 THEN
        v-nxt-r-no = v-last-r-no.
    ELSE
       ASSIGN v-nxt-r-no = v-nxt-r-no + 1 v-retries = v-retries + 1.
    
    IF v-retries GT 2000 THEN DO:
        v-error = YES.
        LEAVE test-r-no.
    END.

    /* Try to find a reftable with this r-no */
    FIND FIRST reftable  WHERE reftable.reftable EQ v-ref-value
                          AND reftable.company EQ cocode
                          AND reftable.loc     EQ STRING(v-nxt-r-no)
                        NO-LOCK NO-ERROR.

    /* If the reftable record exists, try with the next number up */
    IF AVAIL reftable THEN DO:       
        NEXT test-r-no.
    END.

    /* If it doesn't exist, create it */
    IF NOT AVAIL reftable THEN DO:
        CREATE reftable.
        ASSIGN reftable.reftable = v-Ref-value
               reftable.company = (IF v-ref-value EQ "NextRelease#" THEN cocode ELSE "")
               reftable.loc     = STRING(v-nxt-r-no).
    END.
    
    /* Make sure someone isn't creating at the same time */
    iCnt = 0. v-all-locked = TRUE.
    FOR EACH reftable FIELDS(company reftable loc) WHERE reftable.reftable EQ v-ref-value
                        AND reftable.company EQ (IF v-ref-value EQ "NextRelease#" THEN cocode ELSE "")
                        AND reftable.loc     EQ STRING(v-nxt-r-no)
                     NO-LOCK.
        iCnt = iCnt + 1.
        /* Try to get an exclusive-lock on any created reftable records */
        FIND bf-reftable WHERE rowid(bf-reftable) = ROWID(reftable)
                         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
        IF AVAIL bf-reftable AND bf-reftable.val[1] EQ 0 THEN DO:
            bf-reftable.val[1] = 1.
            RELEASE bf-reftable.
        END.
        ELSE
            v-all-locked = FALSE.
    END.


    IF v-all-locked THEN
        LEAVE test-r-no.
    ELSE DO:
        /* Could not lock each records, so try next number */    
        NEXT test-r-no.
    END.


  END.

  IF v-error THEN
     op-r-no = 0.
  ELSE
     op-r-no = v-nxt-r-no.


