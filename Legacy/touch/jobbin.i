  
  FOR EACH job-hdr
      WHERE job-hdr.company   EQ company_code
        AND job-hdr.job-no    EQ job-mch.job-no
        AND job-hdr.job-no2   EQ job-mch.job-no2
        AND (job-hdr.frm      EQ job-mch.frm OR v-est-type       EQ 2 )
        AND (job-hdr.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0 ),
      FIRST itemfg
      WHERE itemfg.company EQ job-hdr.company
        AND itemfg.i-no    EQ job-hdr.i-no
      NO-LOCK:

    /*IF itemfg.isaset AND itemfg.alloc NE YES THEN DO:
      ASSIGN
       v-set  = itemfg.i-no
       v-qty  = pc-prdd.qty.
            
      RUN fg/checkset.p (RECID(itemfg), ?, INPUT-OUTPUT v-qty).
          
      IF v-qty LT pc-prdd.qty THEN DO:
        choice = NO.
        MESSAGE "Insufficient components for AUTOPOST, process anyway?"
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE choice.
        IF NOT choice THEN RETURN ERROR.
      END.
    END.*/
        
    RUN fg/autopost.p (ROWID(itemfg), job-hdr.job-no, job-hdr.job-no2,
                       OUTPUT v-loc, OUTPUT v-loc-bin).
        
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ itemfg.company
          AND fg-bin.i-no    EQ itemfg.i-no
          AND fg-bin.loc     EQ v-loc
          AND fg-bin.loc-bin EQ v-loc-bin  
          AND fg-bin.tag     EQ ""
          AND fg-bin.job-no  EQ job-hdr.job-no
          AND fg-bin.job-no2 EQ job-hdr.job-no2
        NO-ERROR.
    IF NOT AVAIL fg-bin THEN DO:
      CREATE fg-bin.
      ASSIGN
       fg-bin.company      = itemfg.company
       fg-bin.loc          = v-loc
       fg-bin.loc-bin      = v-loc-bin
       fg-bin.i-no         = itemfg.i-no
       fg-bin.tag          = ""
       fg-bin.job-no       = job-hdr.job-no
       fg-bin.job-no2      = job-hdr.job-no2
       fg-bin.std-mat-cost = job-hdr.std-mat-cost
       fg-bin.std-lab-cost = job-hdr.std-lab-cost
       fg-bin.std-fix-cost = job-hdr.std-fix-cost
       fg-bin.std-var-cost = job-hdr.std-var-cost
       fg-bin.std-tot-cost = job-hdr.std-tot-cost
       fg-bin.last-cost    = job-hdr.std-tot-cost
       fg-bin.unit-count   = itemfg.case-count.
    END.
      
    IF fg-bin.cases-unit   LE 0 THEN fg-bin.cases-unit   = 1.
    IF fg-bin.units-pallet LE 0 THEN fg-bin.units-pallet = 1.
    
    FIND FIRST reftable
        WHERE reftable.reftable EQ "ts/jobdata.p"
          AND reftable.company  EQ company_code
          AND reftable.code     EQ job-hdr.rec_key
        EXCLUSIVE NO-ERROR.
    IF AVAIL reftable THEN DELETE reftable.
    CREATE reftable.
    ASSIGN
     reftable.reftable = "ts/jobdata.p"
     reftable.company  = company_code
     reftable.code     = job-hdr.rec_key
     reftable.code2    = fg-bin.rec_key.
    
    v-runqty = 0. 
    FOR EACH bf-machtran WHERE bf-machtran.company = company_code AND
                               bf-machtran.machine = machine_code AND
                               bf-machtran.job_number = job_number AND
                               bf-machtran.job_sub = INTEGER(job_sub) AND
                               bf-machtran.form_number = INTEGER(form_number) AND
                               bf-machtran.blank_number = INTEGER(blank_number) AND
                               bf-machtran.pass_sequence = INTEGER(pass_sequence) NO-LOCK:
                v-runqty = v-runqty + bf-machtran.run_qty.
    END.
    RUN touch/d-updbin.w  (ROWID(fg-bin), v-runqty,employee_code,company_code). /* pc-prdd.qty*/
  END.  /* job-hdr */
