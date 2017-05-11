/* ------------------------------------------------- cec/mach-seq.i 08/96 JLF */
/* create machine routing sequence - mach parameters                          */
/* -------------------------------------------------------------------------- */

IF AVAILABLE mach THEN 
DO:
    CREATE tt-mach-exc.
    ASSIGN
        tt-mach-exc.form-no  = xef.form-no
        tt-mach-exc.blank-no = IF xest.est-type EQ 5 THEN 1 ELSE
                          IF AVAILABLE xeb AND mach.p-type EQ "B" THEN xeb.blank-no ELSE 0
        tt-mach-exc.m-code   = mach.m-code
        tt-mach-exc.dept     = mach.dept[1]
        tt-mach-exc.defr     = "{&defr}" NE "".

        
    RUN cec/mach-qty.p (ROWID(mach), ROWID(xeb), v-on-f, {1}, qty, OUTPUT v-run).
    
    /* RUN cec/mach-dim.p (ROWID(tt-mach-exc), ROWID(xeb), {1}, {2}, {3}, v-run,OUTPUT tt-mach-exc.reason). - Replaced with est/machLimitsDims.p */
    /*{1}, {2}, {3} are L, W, D respectively*/
    RUN est/MachLimitsDims.p (ROWID(style), ROWID(mach), ROWID(xeb), {1}, {2}, {3}, v-run, OUTPUT tt-mach-exc.reason).
    IF tt-mach-exc.reason NE '' THEN RELEASE mach.       
        
    /* {cec/machslot.i} - Replaced with est\MachLimitsSlot.p*/
    IF AVAILABLE mach THEN 
        RUN est/MachLimitsSlot.p (ROWID(style), ROWID(mach), ROWID(xeb), OUTPUT tt-mach-exc.reason).
    IF tt-mach-exc.reason NE '' THEN RELEASE mach.
         
    /* {cec/mach-pan.i} - Replaced with new est\MachLimitsPanel.p*/
    IF AVAILABLE mach THEN 
        RUN est/MachLimitsPanel.p (ROWID(style), ROWID(mach), ROWID(xeb), OUTPUT tt-mach-exc.reason).
    IF tt-mach-exc.reason NE '' THEN RELEASE mach.
           
    IF AVAILABLE mach THEN DO:
        IF AVAILABLE tt-mach-exc THEN DELETE tt-mach-exc.
        LEAVE.
    END.
END.

/* end ---------------------------------- copr. 1996  advanced software, inc. */
