
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR v-parts AS DEC NO-UNDO.
DEF VAR v-dec AS DEC NO-UNDO.
DEF VAR lv-cmp-job-found AS LOG NO-UNDO.

DEF BUFFER b-job-mch FOR job-mch.
                              
IF AVAIL job THEN DO:
  IF job.opened THEN job.stat = "W".

  CREATE mch-act.
  ASSIGN
   mch-act.company    = cocode
   mch-act.op-date    = pc-prdd.op-date
   mch-act.op-time    = TIME
   mch-act.job        = job.job
   mch-act.job-no     = pc-prdd.job-no
   mch-act.job-no2    = pc-prdd.job-no2
   mch-act.frm        = pc-prdd.frm
   mch-act.blank-no   = pc-prdd.blank-no
   mch-act.m-code     = pc-prdd.m-code
   mch-act.i-name     = pc-prdd.i-name
   mch-act.i-no       = pc-prdd.i-no
   mch-act.pass       = pc-prdd.pass
   mch-act.opn        = YES
   mch-act.complete   = pc-prdd.complete
   mch-act.code       = pc-prdd.code
   mch-act.crew       = pc-prdd.crew
   mch-act.dept       = pc-prdd.dept
   mch-act.hours      = pc-prdd.hours
   mch-act.j-no       = pc-prdd.j-no
   mch-act.notes[1]   = pc-prdd.notes[1]
   mch-act.notes[2]   = pc-prdd.notes[2]
   mch-act.notes[3]   = pc-prdd.notes[3]
   mch-act.qty        = pc-prdd.qty / v-up-hs
   mch-act.shift      = pc-prdd.shift
   mch-act.speed      = pc-prdd.speed
   mch-act.waste      = pc-prdd.waste / v-up-hs
   mch-act.start      = pc-prdd.start
   mch-act.stopp      = pc-prdd.stopp
   mch-act.bc-job     = pc-prdd.bc-job
   mch-act.bc-i-no    = pc-prdd.bc-i-no
   mch-act.bc-code    = pc-prdd.bc-code
   mch-act.bc-crew-id = pc-prdd.bc-crew-id
   mch-act.bc-emp-id  = pc-prdd.bc-emp-id.

  IF mch-act.crew EQ 0 THEN mch-act.crew = 1.

  find job-code where job-code.code = mch-act.CODE no-lock no-error.

  /* changes for TSPOST task# 03080410  */
  DO i = 1 TO pc-prdd.crew:
      IF i <= 50 THEN
         ASSIGN mch-act.emp-id[i] =  pc-prdd.emp-id[i]
                mch-act.rate[i] = pc-prdd.rate[i].
  END.

  IF v-tspost-val NE "Actual" THEN
  DO:
     IF AVAIL job-code THEN
       IF job-code.cat EQ "MR" THEN
          mch-act.rate[INTEGER(mch-act.crew)] = (mach.mr-rate / 
                                 (IF mach.run-crusiz EQ 0 THEN 1 ELSE mach.run-crusiz) *
                                 mch-act.crew). 
       ELSE
       IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
          mch-act.rate[INTEGER(mch-act.crew)] = (mach.run-rate / 
                                 (IF mach.run-crusiz EQ 0 THEN 1 ELSE mach.run-crusiz) *
                                 mch-act.crew).
  END.
  ELSE IF INDEX(PROGRAM-NAME(1),"touch") EQ 0 AND /*not called from addon post*/
     CONNECTED("emptrack") THEN
     DO:
        /*if touchscreen data collection was not used, used mach rate*/
        RUN pc\cmpltjob.p(INPUT cocode,
                          INPUT mach.m-code,
                          INPUT pc-prdd.job-no,
                          INPUT pc-prdd.job-no2,
                          INPUT pc-prdd.blank-no,
                          INPUT pc-prdd.frm,
                          INPUT pc-prdd.pass,
                          OUTPUT lv-cmp-job-found).

        IF NOT lv-cmp-job-found THEN
        DO:
           IF AVAIL job-code THEN
              IF job-code.cat EQ "MR" THEN
                 mch-act.rate[INTEGER(mch-act.crew)] = mach.mr-rate. 
              ELSE
                 IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
                    mch-act.rate[INTEGER(mch-act.crew)] = mach.run-rate.
        END.
     END.

  IF AVAIL job-code THEN
    IF job-code.cat EQ "MR" THEN
      ASSIGN
       mch-act.fixoh = mach.mr-fixoh
       mch-act.varoh = mach.mr-varoh. 
    ELSE
    IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
      ASSIGN
       mch-act.fixoh = mach.run-fixoh
       mch-act.varoh = mach.run-varoh.

  FIND FIRST b-job-mch
      WHERE b-job-mch.company   EQ pc-prdd.company
        AND b-job-mch.job       EQ pc-prdd.job
        AND b-job-mch.job-no    EQ pc-prdd.job-no
        AND b-job-mch.job-no2   EQ pc-prdd.job-no2
        AND b-job-mch.m-code    EQ pc-prdd.m-code
        AND b-job-mch.frm       EQ pc-prdd.frm
        AND (b-job-mch.blank-no EQ pc-prdd.blank-no OR pc-prdd.blank-no EQ 0)
        AND b-job-mch.pass      EQ pc-prdd.pass
      USE-INDEX seq-idx NO-LOCK NO-ERROR.

  IF AVAIL b-job-mch THEN lv-rowid = ROWID(b-job-mch).

  ELSE DO:
    CREATE job-mch.
    lv-rowid = ROWID(job-mch).

    FIND FIRST b-job-mch
        WHERE b-job-mch.company   EQ pc-prdd.company
          AND b-job-mch.job       EQ pc-prdd.job
          AND b-job-mch.job-no    EQ pc-prdd.job-no
          AND b-job-mch.job-no2   EQ pc-prdd.job-no2
          AND b-job-mch.frm       EQ pc-prdd.frm
          AND (b-job-mch.blank-no EQ pc-prdd.blank-no OR pc-prdd.blank-no EQ 0)
          AND b-job-mch.dept      EQ pc-prdd.dept
          AND b-job-mch.pass      EQ pc-prdd.pass
        USE-INDEX seq-idx NO-LOCK NO-ERROR.

    IF AVAIL b-job-mch THEN
      BUFFER-COPY b-job-mch EXCEPT rec_key TO job-mch
      ASSIGN
       job-mch.m-code = pc-prdd.m-code.

    ELSE  DO:
      ASSIGN job-mch.company  = pc-prdd.company
             job-mch.job      = pc-prdd.job
             job-mch.job-no   = pc-prdd.job-no
             job-mch.job-no2  = pc-prdd.job-no2
             job-mch.frm      = pc-prdd.frm
             job-mch.blank-no = pc-prdd.blank-no
             job-mch.pass     = pc-prdd.pass
             job-mch.m-code   = pc-prdd.m-code
             job-mch.dept     = pc-prdd.dept
             job-mch.i-name   = pc-prdd.i-name
             job-mch.dept     = pc-prdd.dept
             job-mch.speed    = pc-prdd.speed
             job-mch.run-hr   = pc-prdd.hours.

      IF AVAIL job-code THEN
        IF job-code.cat EQ "MR" THEN
          ASSIGN
           job-mch.mr-rate  = mch-act.rate[INTEGER(mch-act.crew)]
           job-mch.mr-varoh = mch-act.varoh
           job-mch.mr-fixoh = mch-act.fixoh.
        ELSE
        IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
          ASSIGN
           job-mch.run-rate  = mch-act.rate[INTEGER(mch-act.crew)]
           job-mch.run-varoh = mch-act.varoh
           job-mch.run-fixoh = mch-act.fixoh.
    END.

    RELEASE job-mch.
  END.

  IF pc-prdd.complete THEN DO:
    FIND b-job-mch WHERE ROWID(b-job-mch) EQ lv-rowid NO-ERROR.
    IF AVAIL b-job-mch THEN DO:
      IF pc-prdd.code EQ 'MR' THEN
      b-job-mch.mr-complete  = YES.
      ELSE
      IF pc-prdd.code EQ 'RUN' THEN
      b-job-mch.run-complete = YES.

      FIND CURRENT b-job-mch NO-LOCK.
    END.
  END.

  IF v-est-type EQ 2 AND mach.p-type EQ "P" AND TRIM(job.est-no) NE "" THEN DO:
    v-parts = 0.
    FOR EACH eb NO-LOCK
        WHERE eb.company EQ job.company
          AND eb.est-no  EQ job.est-no
          AND eb.form-no NE 0:
      v-parts = v-parts +
                (IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty).
    END.

    IF v-parts GT 0 THEN DO:
      v-dec = mch-act.qty * v-parts.
      {sys/inc/roundup.i v-dec}
      mch-act.qty = v-dec.

      v-dec = mch-act.waste * v-parts.
      {sys/inc/roundup.i v-dec}
      mch-act.waste = v-dec.
    END.
  END.

  RELEASE mch-act.
END.
