FOR EACH xest
    WHERE xest.company EQ cocode
      AND xest.est-no GE FILL(" ",8 - LENGTH(fest)) + TRIM(fest)
      AND xest.est-no LE FILL(" ",8 - LENGTH(test)) + TRIM(test) 
    NO-LOCK
    ,
    EACH est-op
    WHERE est-op.company EQ xest.company
      AND est-op.est-no EQ xest.est-no
      AND est-op.m-code GE fmch
      AND est-op.m-code LE tmch
      ,
    FIRST mach
    WHERE mach.company EQ est-op.company
      AND mach.m-code EQ est-op.m-code
    NO-LOCK
   
            :
    
    STATUS DEFAULT " Processing...    Est: " + TRIM(STRING(xest.est-no)) +
			     "  Machine: " + est-op.m-code.
    
        
    IF lImportStandards THEN DO:
        FIND FIRST ef EXCLUSIVE-LOCK
            WHERE ef.company EQ xest.company
            AND ef.est-no  EQ xest.est-no
            AND ef.form-no EQ est-op.s-num
            NO-ERROR.
        IF AVAILABLE ef THEN 
        DO:
            ef.op-lock = NO.
            RELEASE ef.
        END.
        fil_id  = RECID(est-op).
        RUN cec/mach-rek.p (ROWID(est-op)).
        FIND FIRST ef EXCLUSIVE-LOCK
            WHERE ef.company EQ xest.company
            AND ef.est-no  EQ xest.est-no
            AND ef.form-no EQ est-op.s-num
            NO-ERROR.
        IF AVAILABLE ef THEN 
        DO:
            ef.op-lock = YES.
            RELEASE ef.
        END.
    END.
    ELSE
        ASSIGN
            est-op.op-rate[1] = mach.mr-trate
            est-op.op-rate[2] = mach.run-trate
            .
END.

STATUS DEFAULT "".

