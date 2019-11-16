
DISABLE TRIGGERS FOR LOAD OF job-mch.
DISABLE TRIGGERS FOR LOAD OF mch-act.
                    
PAUSE 0 BEFORE-HIDE.

FOR EACH company NO-LOCK,
    EACH mach WHERE mach.company EQ company.company NO-LOCK:

  FOR EACH job-mch
      WHERE job-mch.company EQ mach.company
        AND job-mch.m-code  EQ mach.m-code
      USE-INDEX mach
      TRANSACTION:

    DISPLAY "Processing Company/Machine/Job#: " +
            TRIM(job-mch.company) + "/"         +
            TRIM(job-mch.m-code)  + "/"         +
            TRIM(job-mch.job-no)  + "-"         +
            TRIM(STRING(job-mch.job-no2,"99")) FORMAT "x(70)"
        WITH FRAME f1 1 DOWN.

    IF job-mch.run-rate  EQ 0 AND
       job-mch.run-fixoh EQ 0 AND
       job-mch.run-varoh EQ 0 THEN
      ASSIGN
       job-mch.run-rate  = mach.run-rate
       job-mch.run-fixoh = mach.run-fixoh
       job-mch.run-varoh = mach.run-varoh.
  END.

  HIDE FRAME f1 NO-PAUSE.

  FOR EACH mch-act
      WHERE mch-act.company EQ mach.company
        AND mch-act.m-code  EQ mach.m-code
      USE-INDEX operation,
      FIRST job-code WHERE job-code.code EQ mch-act.code NO-LOCK
      TRANSACTION:

    DISPLAY "Processing Company/Machine/Job#: " +
            TRIM(mch-act.company) + "/"         +
            TRIM(mch-act.m-code)  + "/"         +
            TRIM(mch-act.job-no)  + "-"         +
            TRIM(STRING(mch-act.job-no2,"99")) FORMAT "x(70)"
        WITH FRAME f2 1 DOWN.

    IF mch-act.crew EQ 0 OR mch-act.crew GT EXTENT(mch-act.crew) THEN
      mch-act.crew = 1.

    IF mch-act.rate[INTEGER(mch-act.crew)] EQ 0 THEN
      IF job-code.cat EQ "MR" THEN
        mch-act.rate[INTEGER(mch-act.crew)] = mach.mr-rate.
      ELSE
      IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
        mch-act.rate[INTEGER(mch-act.crew)] = mach.run-rate.

    IF mch-act.fixoh EQ 0 AND
       mch-act.varoh EQ 0 THEN
      IF job-code.cat EQ "MR" THEN
        ASSIGN
         mch-act.fixoh = mach.mr-fixoh
         mch-act.varoh = mach.mr-varoh. 
      ELSE
      IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
        ASSIGN
         mch-act.fixoh = mach.run-fixoh
         mch-act.varoh = mach.run-varoh.
  END.

  HIDE FRAME f2 NO-PAUSE.
END.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
