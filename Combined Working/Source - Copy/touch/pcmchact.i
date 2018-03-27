
DEF BUFFER b-job-mch FOR job-mch.
                               
IF AVAIL job THEN DO:
  IF job.opened THEN job.stat = "W".

  CREATE mch-act.
  ASSIGN
   mch-act.company    = company_code
   mch-act.op-date    = machtran.end_date /*pc-prdd.op-date, machtran.end_date*/
   mch-act.op-time    = machtran.End_time /*TIME*/
   mch-act.job        = job.job
   mch-act.job-no     = job-mch.job-no
   mch-act.job-no2    = job-mch.job-no2
   mch-act.frm        = job-mch.frm
   mch-act.blank-no   = job-mch.blank-no
   mch-act.m-code     = job-mch.m-code
   mch-act.i-name     = job-mch.i-name
   mch-act.i-no       = job-mch.i-no
   mch-act.pass       = job-mch.pass
   mch-act.opn        = YES
   mch-act.complete   = machtran.complete
   mch-act.code       = machtran.charge_code
   mch-act.crew       = mch-act.crew + 1
   mch-act.dept       = job-mch.dept
   mch-act.hours      = machtran.total_time / 3600
   mch-act.j-no       = job-mch.j-no
 /*  mch-act.notes[1]   = job-mch.notes[1]
   mch-act.notes[2]   = job-mch.notes[2]
   mch-act.notes[3]   = job-mch.notes[3] */
     
   mch-act.qty        = machtran.run_qty / v-up-hs
   mch-act.shift      = int(machtran.shift)
   mch-act.speed      = job-mch.speed
   mch-act.waste      = machtran.waste_qty / v-up-hs
   mch-act.start      = machtran.start_time
   mch-act.stopp      = machtran.end_time

  /* mch-act.bc-job     = job-mch.bc-job
   mch-act.bc-i-no    = job-mch.bc-i-no
   mch-act.bc-code    = job-mch.bc-code
   mch-act.bc-crew-id = job-mch.bc-crew-id
   mch-act.bc-emp-id  = job-mch.bc-emp-id*/.


 /* ??? 
  FIND FIRST b-job-mch
      WHERE b-job-mch.company   EQ job-mch.company
        AND b-job-mch.job       EQ job-mch.job
        AND b-job-mch.job-no    EQ job-mch.job-no
        AND b-job-mch.job-no2   EQ job-mch.job-no2
        AND b-job-mch.m-code    EQ job-mch.m-code
        AND b-job-mch.frm       EQ job-mch.frm
        AND (b-job-mch.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
        AND b-job-mch.pass      EQ job-mch.pass
      USE-INDEX seq-idx NO-LOCK NO-ERROR.

  IF NOT AVAIL b-job-mch THEN DO:
    CREATE job-mch.

    FIND FIRST b-job-mch
        WHERE b-job-mch.company   EQ job-mch.company
          AND b-job-mch.job       EQ job-mch.job
          AND b-job-mch.job-no    EQ job-mch.job-no
          AND b-job-mch.job-no2   EQ job-mch.job-no2
          AND b-job-mch.frm       EQ job-mch.frm
          AND (b-job-mch.blank-no EQ job-mch.blank-no OR job-mch.blank-no EQ 0)
          AND b-job-mch.dept      EQ job-mch.dept
          AND b-job-mch.pass      EQ job-mch.pass
        USE-INDEX seq-idx NO-LOCK NO-ERROR.

    IF AVAIL b-job-mch THEN
      BUFFER-COPY b-job-mch EXCEPT rec_key TO job-mch
      ASSIGN
       job-mch.m-code = job-mch.m-code.

    ELSE
      ASSIGN
       job-mch.company  = job-mch.company
       job-mch.job      = job-mch.job
       job-mch.job-no   = job-mch.job-no
       job-mch.job-no2  = job-mch.job-no2
       job-mch.frm      = job-mch.frm
       job-mch.blank-no = job-mch.blank-no
       job-mch.pass     = job-mch.pass
       job-mch.m-code   = job-mch.m-code
       job-mch.dept     = job-mch.dept
       job-mch.i-name   = job-mch.i-name
       job-mch.dept     = job-mch.dept
       job-mch.speed    = job-mch.speed
       job-mch.run-hr   = job-mch.run-hr
       job-mch.mr-rate  = mach.mr-rate
       job-mch.mr-varoh = mach.mr-varoh
       job-mch.mr-fixoh = mach.mr-fixoh
       job-mch.wst-prct = mach.run-spoil.
  END.
  */

  IF mch-act.crew EQ 0 THEN mch-act.crew = 1.
  
  FIND job-code NO-LOCK WHERE job-code.code EQ mch-act.code NO-LOCK NO-ERROR.
                       
  IF AVAIL job-code THEN
    IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
      ASSIGN
       mch-act.rate[INTEGER(mch-act.crew)] = mach.run-rate.
       mch-act.fixoh = job-mch.run-fixoh
       mch-act.varoh = job-mch.run-varoh.
    ELSE
    IF job-code.cat EQ "MR" THEN
      ASSIGN
       mch-act.rate[INTEGER(mch-act.crew)] = mach.mr-rate
       mch-act.fixoh = job-mch.mr-fixoh
       mch-act.varoh = job-mch.mr-varoh.

  RELEASE mch-act.
END.
