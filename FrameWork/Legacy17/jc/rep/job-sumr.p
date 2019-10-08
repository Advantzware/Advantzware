/* #PN# Creates a work-mch record for each job-mch, then populates        */
/* #PN# with info from the mach table, mch-act table, mch-act table       */
/* #PN# It then merges all these records into one per machine, into form  */
/* #PN# number 0                                                          */

/* Turning this include into a proceudre ... */
DEF INPUT PARAMETER iprJobRow AS ROWID NO-UNDO.
DEF INPUT PARAMETER cocode AS CHAR NO-UNDO.
DEF INPUT PARAMETER locode AS CHAR NO-UNDO.
DEF INPUT PARAMETER tb_exclude_run_if_no_prod AS LOG  NO-UNDO.
DEF INPUT PARAMETER v-tspost-val AS CHAR NO-UNDO.
DEF INPUT PARAMETER tb_curr AS LOG NO-UNDO.
DEF INPUT PARAMETER tb_curr-crew AS LOG NO-UNDO.

{jc/rep/job-sum.i } /* with share vars */

/* end new parameters */
DEF VAR ll-act-rate AS LOG NO-UNDO.
DEF VAR ld-tot-rate AS DEC NO-UNDO.

DEF BUFFER b-work-mch FOR work-mch.
DEFINE VARIABLE v-est-run-rate  AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-hours-entered AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-num-speeds    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-run-hr        AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-act-run       AS DECIMAL NO-UNDO. /* Overrides est-run-hr */
DEFINE VARIABLE v-rate-found    AS LOGICAL NO-UNDO.
DEFINE VARIABLE lDebug          AS LOGICAL NO-UNDO INIT NO.
DEFINE VARIABLE ldBoardLen      AS DECIMAL NO-UNDO.
DEFINE VARIABLE liNumForms     AS INTEGER     NO-UNDO.
DEF VAR i AS INT.
v-hours-entered = NO.
DEF STREAM sTest.

FIND job WHERE ROWID(job) EQ iprJobRow NO-LOCK.

/* #BL# For a particular job, examine each job machine record.  */
for each job-mch where
    job-mch.company = cocode and
    job-mch.job = job.job
    AND job-mch.job-no EQ job.job-no
    AND job-mch.job-no2 EQ job.job-no2
    use-index seq-idx
    NO-LOCK,
    FIRST mach where
          mach.company = cocode and
          mach.loc     = locode and
          mach.m-code  = job-mch.m-code
          NO-LOCK:
   
    
   /* #BL# Create a temp record called work-mch for each machine, form, blank, pass, est-speed, run-qty */
   FIND FIRST work-mch
       WHERE work-mch.m-code    EQ job-mch.m-code
         AND work-mch.frm       EQ job-mch.frm
         AND (work-mch.blk      EQ job-mch.blank-no OR
              (work-mch.blk     EQ 1 AND
               mach.p-type      EQ "B" AND
               job-mch.blank-no EQ 0))
          AND work-mch.pass     EQ job-mch.pass
        NO-ERROR.
    IF NOT AVAIL work-mch THEN DO:
      CREATE work-mch.
      ASSIGN work-mch.m-code    = job-mch.m-code
             work-mch.frm       = job-mch.frm
             work-mch.blk       = IF mach.p-type EQ "B"    AND
                                     job-mch.blank-no EQ 0 THEN 1
                                                           ELSE job-mch.blank-no
             work-mch.pass      = job-mch.pass
             work-mch.d-seq     = mach.d-seq
             work-mch.est-speed = job-mch.speed
             work-mch.est-qty   = job-mch.run-qty.

   END. /* not avail work-mch */


   /* #BL# Add in to the temp record: */
   /* #BL#  the job machine mr-hr into the est-mr-hr  */
   /* #BL#  the job machine run-hr into the est-run-hr  */
   /* #BL#  the job machine mr-waste into the mr-waste  */
   ASSIGN work-mch.est-mr-hr  = work-mch.est-mr-hr + job-mch.mr-hr
          work-mch.est-run-hr = work-mch.est-run-hr + job-mch.run-hr
          work-mch.mr-waste   = work-mch.mr-waste + job-mch.mr-waste.

   
   v-run-hr = job-mch.run-hr.
   /* #BL# if the run hours are zero in job machine, take them from */
   /* #BL# the machine actual table                                 */
   IF job-mch.run-hr EQ 0 THEN DO:
       FOR EACH mch-act 
         WHERE mch-act.company = cocode 
           AND mch-act.job = job.job 
           AND mch-act.m-code EQ job-mch.m-code
           AND mch-act.frm    EQ job-mch.frm
           AND mch-act.pass   EQ job-mch.pass
         NO-LOCK,

         FIRST job-code where
                   job-code.code = mch-act.CODE AND
                   LOOKUP(job-code.cat,"RUN,DT") GT 0
                   NO-LOCK.
         IF AVAIL mch-act AND AVAIL(job-code) AND mch-act.hours GT 0 THEN
           LEAVE.
       END.
       IF NOT AVAIL mch-act THEN DO:       
           FOR EACH mch-act 
             WHERE mch-act.company = cocode 
               AND mch-act.job = job.job 
               AND mch-act.m-code EQ job-mch.m-code
             NO-LOCK,
    
             FIRST job-code where
                       job-code.code = mch-act.CODE AND
                       LOOKUP(job-code.cat,"RUN,DT") GT 0
                       NO-LOCK.
             IF AVAIL mch-act AND AVAIL(job-code) AND mch-act.hours GT 0 THEN
               LEAVE.
           END.
       END.
       IF AVAIL mch-act THEN
           ASSIGN v-run-hr = mch-act.hours.
   END.

   IF tb_curr THEN DO:
     /* #BL# If current checkbox then calculate the job mch temp table */
     /* #BL# using the machine table */
       IF work-mch.m-code = "BMA" AND lDebug THEN
           MESSAGE "mch" work-mch.m-code SKIP
           "work-mch.est-run-cost tb_curr" work-mch.est-run-cost SKIP
           "ading to cost"  ((mach.run-rate + mach.run-varoh + mach.run-fixoh) * job-mch.run-hr ) SKIP
             "job-mch.run-hr" job-mch.run-hr
             "override est-run-hr with v-run-hr" v-run-hr
             "std mach rate" mach.run-rate + mach.run-varoh + mach.run-fixoh
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN
      work-mch.est-mr-cost   = work-mch.est-mr-cost +
                               ((mach.mr-rate + mach.mr-varoh + mach.mr-fixoh) * job-mch.mr-hr)
      work-mch.est-mr-cost1  = work-mch.est-mr-cost1 + 
                               (mach.mr-rate * job-mch.mr-hr)
      work-mch.est-mr-cost2  = work-mch.est-mr-cost2 +
                               (mach.mr-fixoh * job-mch.mr-hr)
      work-mch.est-mr-cost3  = work-mch.est-mr-cost3 +
                               (mach.mr-varoh * job-mch.mr-hr)

      work-mch.est-run-cost  = work-mch.est-run-cost +
                               ((mach.run-rate + mach.run-varoh + mach.run-fixoh) * job-mch.run-hr )                         
      work-mch.est-run-cost1 = work-mch.est-run-cost1 + 
                               (mach.run-rate * v-run-hr)
      work-mch.est-run-cost2 = work-mch.est-run-cost2 +
                               (mach.run-fixoh * v-run-hr)
      work-mch.est-run-cost3 = work-mch.est-run-cost3 +
                               (mach.run-varoh * v-run-hr)
      work-mch.est-run-hr = v-run-hr.

   END. /* if tb_curr */
   ELSE
   DO:

     /* #BL# Otherwise, calculate the job mch temp table */
     /* #BL# using the job machine table */
          
      ASSIGN
       work-mch.est-mr-cost   = work-mch.est-mr-cost +
                                ((job-mch.mr-rate + job-mch.mr-varoh + job-mch.mr-fixoh) * job-mch.mr-hr)
       work-mch.est-mr-cost1  = work-mch.est-mr-cost1 + 
                                (job-mch.mr-rate * job-mch.mr-hr)
       work-mch.est-mr-cost2  = work-mch.est-mr-cost2 +
                                (job-mch.mr-fixoh * job-mch.mr-hr)
       work-mch.est-mr-cost3  = work-mch.est-mr-cost3 +
                                (job-mch.mr-varoh * job-mch.mr-hr).
      
      IF job-mch.run-rate NE 0 THEN DO:
         /* #BL# If the job machine run rate is not zero, calculate the  */
         /* #BL# job mach temp-table est-run-costs using it              */
      IF work-mch.m-code = "BMA" AND lDebug THEN
        MESSAGE   "mch" work-mch.m-code SKIP
                  "work-mch.est-run-cost" work-mch.est-run-cost SKIP
        "adding to cost2" ((job-mch.run-rate + job-mch.run-varoh + job-mch.run-fixoh) * v-run-hr /* job-mch.run-hr */)
        "if est-run-hr = 0 then override" work-mch.est-run-hr "v-run-hr" v-run-hr
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

         ASSIGN
         work-mch.est-run-cost  = work-mch.est-run-cost +
                                  ((job-mch.run-rate + job-mch.run-varoh + job-mch.run-fixoh) * v-run-hr /* job-mch.run-hr */)
         work-mch.est-run-cost1 = work-mch.est-run-cost1 + 
                                  (job-mch.run-rate * v-run-hr /* job-mch.run-hr*/)
         work-mch.est-run-cost2 = work-mch.est-run-cost2 +
                                  (job-mch.run-fixoh * v-run-hr /* job-mch.run-hr */)
         work-mch.est-run-cost3 = work-mch.est-run-cost3 +                          
                                  (job-mch.run-varoh * v-run-hr /* job-mch.run-hr */).
         IF work-mch.est-run-hr EQ 0 THEN
             work-mch.est-run-hr = v-run-hr.
      END.
      ELSE
      DO:
         /* #BL# Otherwise, use the machine actual run rate */
         v-est-run-rate = 0.
         v-rate-found = NO.
         /* #PN# get cost from first run transaction*/
         for EACH mch-act where
             mch-act.company = cocode and
             mch-act.job = job.job AND
             mch-act.m-code EQ job-mch.m-code AND
             mch-act.frm    EQ job-mch.frm    AND
             mch-act.pass   EQ job-mch.pass 
             NO-LOCK,
             FIRST job-code where
                   job-code.code = mch-act.CODE AND
                   LOOKUP(job-code.cat,"RUN,DT") GT 0
                   NO-LOCK:
        
            IF v-tspost-val EQ "Actual" THEN
             DO i = 1 TO mch-act.crew:
                v-est-run-rate = v-est-run-rate +  mch-act.rate[i].
             END.
             ELSE
                v-est-run-rate = mch-act.rate[INT(mch-act.crew)].

             IF v-est-run-rate EQ 0 THEN
                 v-est-run-rate = mach.run-rate.
             v-rate-found  = YES.
             IF work-mch.m-code = "BMA" AND lDebug THEN
             MESSAGE "mch" work-mch.m-code SKIP
                 "work-mch.est-run-cost add to cost actuals " work-mch.est-run-cost SKIP
  "add cost3" ((v-est-run-rate + mch-act.varoh + mch-act.fixoh)
                                          * v-run-hr /* job-mch.run-hr */) SKIP
              "if est-run-hr = 0 then override" est-run-hr "v-run-hr" v-run-hr
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

             ASSIGN
                work-mch.est-run-cost  = work-mch.est-run-cost +
                                        ((v-est-run-rate + mch-act.varoh + mch-act.fixoh)
                                          * v-run-hr /* job-mch.run-hr */)
                work-mch.est-run-cost1 = work-mch.est-run-cost1 + 
                                        (v-est-run-rate * v-run-hr /* job-mch.run-hr */)
                work-mch.est-run-cost2 = work-mch.est-run-cost2 +
                                        (mch-act.fixoh * v-run-hr /* job-mch.run-hr*/)
                work-mch.est-run-cost3 = work-mch.est-run-cost3 +
                                        (mch-act.varoh * v-run-hr /* job-mch.run-hr */).
             IF work-mch.est-run-hr = 0 THEN
                 work-mch.est-run-hr = v-run-hr.
              
             LEAVE. 
         END. /* each mch-act */
         IF v-rate-found = NO THEN DO:
             /* #PN# Use first one found - Task 02191301 */
             /* #BL# if there is no actual to get run rate, take it from the */
             /* #BL# first machine actual that has one ignoring form/pass    */
             for EACH mch-act where
                 mch-act.company = cocode and
                 mch-act.job = job.job AND
                 mch-act.m-code EQ job-mch.m-code
                 NO-LOCK,
                 FIRST job-code where
                       job-code.code = mch-act.CODE AND
                       LOOKUP(job-code.cat,"RUN,DT") GT 0
                       NO-LOCK:

                IF v-tspost-val EQ "Actual" THEN
                 DO i = 1 TO mch-act.crew:
                    v-est-run-rate = v-est-run-rate + mch-act.rate[i].
                 END.
                 ELSE
                    v-est-run-rate = mch-act.rate[INT(mch-act.crew)].

                 IF v-est-run-rate EQ 0 THEN
                     v-est-run-rate = mach.run-rate.
                 v-rate-found  = YES.
                 IF work-mch.m-code = "BMA" AND lDebug THEN
             MESSAGE "mch" work-mch.m-code SKIP
                     "work-mch.est-run-cost" work-mch.est-run-cost SKIP
  "ading hours4" ((v-est-run-rate + mch-act.varoh + mch-act.fixoh)
                                              * v-run-hr )
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 ASSIGN
                    work-mch.est-run-cost  = work-mch.est-run-cost +
                                            ((v-est-run-rate + mch-act.varoh + mch-act.fixoh)
                                              * v-run-hr )
                    work-mch.est-run-cost1 = work-mch.est-run-cost1 + 
                                            (v-est-run-rate * v-run-hr)
                    work-mch.est-run-cost2 = work-mch.est-run-cost2 +
                                            (mch-act.fixoh * v-run-hr)
                    work-mch.est-run-cost3 = work-mch.est-run-cost3 +
                                            (mch-act.varoh * v-run-hr).
                 LEAVE.
             END.
         END.
      END. /* run-rate eq 0 */
   END. /* not tb_curr */
END. /* each job-mch */

/* #PN# wfk - new section added to clear up issue of missing costs */
/* #BL# Examine each job machine to check for a zero est-run-cost2 */
/* #BL# in the temp table work-mch. Fill in actual run hours to include  */
/* #BL# Actuals */
RUN fix-missing-costs (INPUT job.job).

RUN add-in-actuals (INPUT job.job).

RELEASE work-mch.

/* #PN# Section appears to Clear Out Unknown Values - wfk */
RUN CLEAR-unknown-values.


/* #PN# Section Accumulates Values Into the Form 0 Record             */
/* #BL# Add up all the information from all forms to get the total    */
/* #BL# for each machine                                              */

RUN merge-into-mch-rec.




PROCEDURE merge-into-mch-rec:
DEF VAR v-hours-entered AS LOG.
DEF BUFFER bf-work-mch FOR work-mch.
    IF lDebug THEN DO:
        OUTPUT STREAM sTest TO c:\temp\stest.txt.
        FOR EACH work-mch:
            EXPORT STREAM sTest work-mch.
        END.
        OUTPUT STREAM sTest CLOSE.
    END.
    FOR EACH b-work-mch WHERE b-work-mch.frm GT 0
        BREAK BY b-work-mch.m-code
              BY b-work-mch.frm
              BY b-work-mch.blk
              BY b-work-mch.pass:
    
      v-hours-entered = NO.
      for EACH mch-act where
         mch-act.company = cocode and
         mch-act.job = job.job AND
         mch-act.m-code EQ b-work-mch.m-code AND
         mch-act.pass   EQ b-work-mch.pass
         NO-LOCK,
         FIRST job-code where
               job-code.code = mch-act.CODE AND
               LOOKUP(job-code.cat,"RUN,DT") GT 0
               NO-LOCK:
                 IF b-work-mch.m-code = "BMA" AND lDebug THEN
                     MESSAGE "mch" work-mch.m-code SKIP
                     "in mch act" mch-act.frm b-work-mch.frm mch-act.hours
                         VIEW-AS ALERT-BOX INFO BUTTONS OK.
        IF mch-act.hours GT 0 
            /* AND mch-act.frm EQ b-work-mch.frm  */ /* This condition caused only first hours to be entered */
             THEN
          v-hours-entered = YES.
          
      END. /* each mch-act */
    
      IF FIRST-OF(b-work-mch.m-code) THEN DO:
        ASSIGN v-num-speeds = 0.
      END. /* first-of b-work-mch.m-code */
    
      v-num-speeds = v-num-speeds + 1.
      FIND FIRST work-mch
          WHERE work-mch.m-code EQ b-work-mch.m-code
            AND work-mch.frm    EQ 0
            AND work-mch.blk    EQ 0
            AND work-mch.pass   EQ 0
          NO-ERROR.
    
      IF NOT AVAIL work-mch THEN DO:
        CREATE work-mch.
        ASSIGN
         work-mch.m-code = b-work-mch.m-code
         work-mch.frm    = 0
         work-mch.blk    = 0
         work-mch.pass   = 0
         work-mch.d-seq  = b-work-mch.d-seq.
    
      END.
      DEF VAR llDuplicate AS LOG.
       DEF BUFFER bf-dept FOR dept.

      /* Look for a machine with the same department and actual run hours */
      llDuplicate = NO.
      FOR EACH bf-work-mch WHERE bf-work-mch.frm EQ b-work-mch.frm
          AND ROWID(bf-work-mch) NE ROWID(b-work-mch)
          AND bf-work-mch.m-code NE b-work-mch.m-code
          AND bf-work-mch.mr-hr GT 0 
          NO-LOCK:

          FIND FIRST mach WHERE mach.company EQ cocode
              AND mach.m-code = b-work-mch.m-code NO-LOCK NO-ERROR.

          FIND FIRST dept WHERE  dept.code = mach.dept[1] NO-LOCK NO-ERROR.
          FIND FIRST mach WHERE mach.company EQ cocode
              AND mach.m-code = bf-work-mch.m-code NO-LOCK NO-ERROR.
         
          
          FIND FIRST bf-dept WHERE  bf-dept.code = mach.dept[1] NO-LOCK NO-ERROR.

          /* If 2 from same dept and the other has actual,  */
          /* set llDuplicate, don't add in est values       */
          IF AVAIL(bf-dept) 
              AND AVAIL(dept) 
              AND bf-dept.CODE EQ dept.CODE 
              /* Following line is so that if both machines have actual, 
                 don't consider that to be a duplicate */
              AND b-work-mch.mr-hr EQ 0 THEN DO:
            llDuplicate = YES.
          END.
            
      END.

      ASSIGN
       work-mch.run-qty       = work-mch.run-qty       + b-work-mch.run-qty
       work-mch.run-feet      = work-mch.run-feet      + b-work-mch.run-feet
       work-mch.est-qty       = work-mch.est-qty       + b-work-mch.est-qty
       work-mch.est-feet      = work-mch.est-feet      + b-work-mch.est-feet
       work-mch.wst-qty       = work-mch.wst-qty       + b-work-mch.wst-qty
       work-mch.mr-qty        = work-mch.mr-qty        + b-work-mch.mr-qty
       work-mch.mr-waste      = work-mch.mr-waste      + b-work-mch.mr-waste
       /* Task 07031302 moves this to next assign */

       work-mch.est-mr-hr     = work-mch.est-mr-hr     + (IF llDuplicate THEN 0 ELSE b-work-mch.est-mr-hr)
    
       work-mch.est-mr-cost   = work-mch.est-mr-cost   + (IF llDuplicate THEN 0 ELSE b-work-mch.est-mr-cost)
    
       work-mch.est-run-cost1 = work-mch.est-run-cost1 + b-work-mch.est-run-cost1
       work-mch.est-run-cost2 = work-mch.est-run-cost2 + b-work-mch.est-run-cost2
       work-mch.est-run-cost3 = work-mch.est-run-cost3 + b-work-mch.est-run-cost3
       work-mch.mr-hr         = work-mch.mr-hr         + b-work-mch.mr-hr
       work-mch.run-hr        = work-mch.run-hr        + b-work-mch.run-hr
       work-mch.mr-cost1      = work-mch.mr-cost1      + b-work-mch.mr-cost1
       work-mch.mr-cost2      = work-mch.mr-cost2      + b-work-mch.mr-cost2
       work-mch.mr-cost3      = work-mch.mr-cost3      + b-work-mch.mr-cost3
       work-mch.run-cost1     = work-mch.run-cost1     + b-work-mch.run-cost1
       work-mch.run-cost2     = work-mch.run-cost2     + b-work-mch.run-cost2
       work-mch.run-cost3     = work-mch.run-cost3     + b-work-mch.run-cost3
       work-mch.run-waste     = work-mch.run-waste     + b-work-mch.run-waste
       work-mch.est-speed     = work-mch.est-speed     + b-work-mch.est-speed.

      /* split up statement for clarity */
       IF NOT tb_exclude_run_if_no_prod  THEN DO:
           IF b-work-mch.m-code = "BMA" AND lDebug THEN
       MESSAGE "mch" work-mch.m-code
               "v-hours-entered" v-hours-entered  SKIP
           "b-work-mch.frm" b-work-mch.frm  SKIP
           "b-work-mch.pass" b-work-mch.pass SKIP
            "b-work-mch.run-hr" b-work-mch.mr-hr            SKIP
           "b-work-mch.est-run-cost" b-work-mch.est-run-cost    SKIP
           "est-run-hr" b-work-mch.est-run-hr SKIP
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
           ASSIGN work-mch.est-run-hr    = work-mch.est-run-hr    + b-work-mch.est-run-hr
                  work-mch.est-run-cost  = work-mch.est-run-cost  + b-work-mch.est-run-cost  
                  work-mch.est-mr-cost1  = work-mch.est-mr-cost1  + b-work-mch.est-mr-cost1
                  work-mch.est-mr-cost2  = work-mch.est-mr-cost2  + b-work-mch.est-mr-cost2
                  work-mch.est-mr-cost3  = work-mch.est-mr-cost3  + b-work-mch.est-mr-cost3.           .
       END.
       ELSE  DO:
    
         /* tb_exclude_run_if_no_prod meaning they only enter on first one, so 
            exclude 2nd pass */
           IF b-work-mch.m-code = "BMA" AND lDebug THEN
       MESSAGE "mch" work-mch.m-code SKIP
               "run-qty" b-work-mch.run-qty SKIP
           "v-hours-entered" v-hours-entered  SKIP
           "b-work-mch.frm" b-work-mch.frm        SKIP
           "b-work-mch.pass" b-work-mch.pass      SKIP
            "b-work-mch.run-hr" b-work-mch.mr-hr  SKIP
           "b-work-mch.est-run-cost if run-hr gt 0 " b-work-mch.est-run-cost "run-hr" b-work-mch.est-run-hr 
               "frm" b-work-mch.frm SKIP
           "est-run-hr" b-work-mch.est-run-hr SKIP
           VIEW-AS ALERT-BOX INFO BUTTONS OK.

        /* new test code */

        IF b-work-mch.run-qty GT 0 THEN DO:
        
     /* IF (v-hours-entered OR (b-work-mch.frm LE 1 AND b-work-mch.pass EQ 1)) THEN DO: */
     /*  task 07031302 has this instead  */
         IF (v-hours-entered OR  (b-work-mch.frm LE 1 AND b-work-mch.pass EQ 1
                               /* 7/30 test   AND b-work-mch.run-hr + b-work-mch.mr-hr GT 0) */ )) THEN DO:
    
           IF (b-work-mch.est-run-cost1 GT 0 OR b-work-mch.est-run-cost2 GT 0
               OR b-work-mch.est-run-cost3 GT 0) AND b-work-mch.est-run-cost EQ 0 THEN
               b-work-mch.est-run-cost = b-work-mch.est-run-cost1 +
               b-work-mch.est-run-cost2 +
               b-work-mch.est-run-cost3.
    /*  task 07031302 inserts this condition */
    IF (b-work-mch.run-hr GT 0 OR b-work-mch.mr-hr GT 0
               OR (b-work-mch.frm NE 1 AND work-mch.est-mr-hr GT 0)) THEN
    
    /* task 07031302 puts adding in of est-mr-hr in this assign, */
    /* but that is incorrect since mr should not be affected     */
           ASSIGN work-mch.est-run-hr    = work-mch.est-run-hr    + b-work-mch.est-run-hr
                  work-mch.est-run-cost  = work-mch.est-run-cost  + b-work-mch.est-run-cost
                  work-mch.est-mr-cost1  = work-mch.est-mr-cost1  + b-work-mch.est-mr-cost1
                  work-mch.est-mr-cost2  = work-mch.est-mr-cost2  + b-work-mch.est-mr-cost2
                  work-mch.est-mr-cost3  = work-mch.est-mr-cost3  + b-work-mch.est-mr-cost3           .
         END. /* If v-hours-entered */

         END.

       END. /* if user choose exclude_run_if_no_prod */
       
       IF LAST-OF(b-work-mch.m-code) THEN DO:
          /* work-mch.est-speed = work-mch.est-speed / v-num-speeds. */
    /* Change to calc in task 03041306 */
          liNumForms = 0.
          FOR EACH job-mat WHERE job-mat.company = cocode            
                AND job-mat.job = job.job 
              NO-LOCK BREAK BY job-mat.frm:
              IF FIRST-OF(job-mat.frm) THEN
                  liNumForms = liNumForms + 1.
          END.
          FIND mach WHERE mach.company = cocode
               AND mach.m-code = b-work-mch.m-code
            NO-LOCK NO-ERROR.
          /* If using lineal feet and not a combo or tandem */
          IF AVAIL mach AND mach.therm AND liNumForms EQ 1 THEN DO:


              ldBoardLen = 0.
              FOR EACH job-mat WHERE job-mat.company = cocode            
                    AND job-mat.job = job.job 
                    AND job-mat.frm EQ b-work-mch.frm
                  NO-LOCK,
                  FIRST ITEM WHERE ITEM.company = job-mat.company
                      AND ITEM.i-no = job-mat.rm-i-no
                      AND ITEM.mat-type = "B"
                  NO-LOCK.
                  ldBoardLen = job-mat.len.
                  /*DISP job-mat.rm-i-no job-mat.len ITEM.mat-type. */
                  LEAVE.
              END.
              IF ldBoardLen GT 0 THEN 
                  work-mch.est-speed = work-mch.run-qty * ldBoardLen / work-mch.est-run-hr / 12.
          END.
          ELSE              
            work-mch.est-speed = work-mch.run-qty / work-mch.est-run-hr.
       END.
    
      ASSIGN 
    /*    wfk - get speed from job-mch per Joe                                   */       
    /*    work-mch.est-speed     = (IF work-mch.est-feet GT 0 THEN               */
    /*                                work-mch.est-feet ELSE work-mch.est-qty) / */
    /*                             work-mch.est-run-hr                           */
       work-mch.run-speed     = (IF work-mch.run-feet GT 0 THEN
                                   work-mch.run-feet ELSE work-mch.run-qty) /
                                work-mch.run-hr.
       
      /* DELETE b-work-mch. */
    END.
    FOR EACH b-work-mch WHERE b-work-mch.frm GT 0:
        DELETE b-work-mch.
    END.
END PROCEDURE. /* Merge-into-mch-rec */


PROCEDURE clear-unknown-values:
    FOR EACH work-mch
        WHERE work-mch.frm GT 0
          AND CAN-FIND(FIRST mach
                       WHERE mach.company EQ cocode
                         AND mach.loc     EQ locode
                         AND mach.m-code  EQ work-mch.m-code
                         AND mach.therm   EQ YES
                         AND (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")),
        FIRST work-mat
        WHERE work-mat.form-no EQ work-mch.frm
          AND work-mat.len     GT 0:
       IF work-mch.m-code = "BMA" AND lDebug THEN
           MESSAGE "recalc est-run-hr" work-mch.est-feet / work-mch.est-speed
               VIEW-AS ALERT-BOX INFO BUTTONS OK.
       /* Prevents costs from being zeroed out incorrectly */
       IF work-mch.est-run-hr EQ 0 THEN
          ASSIGN work-mch.run-feet      = work-mch.run-qty * work-mat.len / 12
                 work-mch.est-feet      = work-mch.est-qty * work-mat.len / 12
                 work-mch.est-run-hr    = work-mch.est-feet / work-mch.est-speed.
    
      ASSIGN
       work-mch.est-run-cost1 = work-mch.est-run-cost1 / work-mch.est-run-hr
       work-mch.est-run-cost2 = work-mch.est-run-cost2 / work-mch.est-run-hr
       work-mch.est-run-cost3 = work-mch.est-run-cost3 / work-mch.est-run-hr
       work-mch.run-feet      = work-mch.run-qty * work-mat.len / 12
       work-mch.est-feet      = work-mch.est-qty * work-mat.len / 12
       work-mch.est-run-hr    = work-mch.est-feet / work-mch.est-speed.
      
      IF work-mch.est-run-cost1 EQ ? THEN work-mch.est-run-cost1 = 0.
      IF work-mch.est-run-cost2 EQ ? THEN work-mch.est-run-cost2 = 0.
      IF work-mch.est-run-cost3 EQ ? THEN work-mch.est-run-cost2 = 0.
      IF work-mch.est-run-hr    EQ ? THEN work-mch.est-run-hr    = 0.
      IF work-mch.m-code = "BMA" AND lDebug THEN
      MESSAGE "Reassign " SKIP
          work-mch.est-run-cost1  "hr" work-mch.est-run-hr SKIP
          work-mch.est-run-cost2  "hr" work-mch.est-run-hr SKIP
          work-mch.est-run-cost3  "hr" work-mch.est-run-hr SKIP

          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      ASSIGN
       work-mch.est-run-cost1 = work-mch.est-run-cost1 * work-mch.est-run-hr
       work-mch.est-run-cost2 = work-mch.est-run-cost2 * work-mch.est-run-hr
       work-mch.est-run-cost3 = work-mch.est-run-cost3 * work-mch.est-run-hr
       work-mch.est-run-cost  = work-mch.est-run-cost1 +
                                work-mch.est-run-cost2 +
                                work-mch.est-run-cost3.
    END. /* Each work-mch */
END PROCEDURE. /* clear-unknow-values */


PROCEDURE fix-missing-costs:

    DEF INPUT PARAMETER ipcJob LIKE job.job.
    
    for each job-mch 
      WHERE job-mch.company = cocode 
        and job-mch.job = ipcJob
      use-index seq-idx NO-LOCK,
      FIRST mach 
        WHERE mach.company = cocode 
          AND mach.loc     = locode 
          AND mach.m-code  = job-mch.m-code
        NO-LOCK:
      
      FIND FIRST work-mch
        WHERE work-mch.m-code    EQ job-mch.m-code
          AND work-mch.frm       EQ job-mch.frm
          AND (work-mch.blk      EQ job-mch.blank-no OR
              (work-mch.blk     EQ 1 AND
                mach.p-type      EQ "B" AND
                job-mch.blank-no EQ 0))
          AND work-mch.pass     EQ job-mch.pass
        NO-ERROR.
        
/*       IF AVAIL(work-mch) AND work-mch.est-run-hr EQ 0 AND work-mch.est-run-cost1 EQ 0 */
/*          AND work-mch.est-run-cost2 EQ 0 THEN DO:                                     */
        
        /*get cost from first run transaction*/
        for EACH mch-act 
          WHERE mch-act.company = cocode 
            AND mch-act.job = job.job 
            AND mch-act.m-code EQ job-mch.m-code 
            AND mch-act.frm    EQ job-mch.frm 
            AND mch-act.pass   EQ work-mch.pass
          NO-LOCK,
          FIRST job-code 
            WHERE job-code.code = mch-act.CODE 
              AND LOOKUP(job-code.cat,"RUN,DT") GT 0
            NO-LOCK:

            /* 03041306 v-act-run overrides est-run-hr */
            IF  work-mch.est-speed GT 0 THEN
                v-act-run = v-act-run + (mch-act.qty / work-mch.est-speed).
            
             IF AVAIL(work-mch) AND work-mch.est-run-hr EQ 0 AND work-mch.est-run-cost1 EQ 0
                   AND work-mch.est-run-cost2 EQ 0 THEN DO:
        
                  v-run-hr = job-mch.run-hr.
                  
                  IF job-mch.run-hr EQ 0 OR work-mch.est-run-hr = 0 THEN DO:
                    IF AVAIL mch-act AND mch-act.hours GT 0 THEN
                    v-run-hr = mch-act.hours.
                    IF work-mch.est-qty EQ 0 THEN
                    work-mch.est-qty   = mch-act.qty.
                  IF work-mch.m-code = "BMA" AND lDebug THEN
                   MESSAGE "recalc est run hr2 " work-mch.est-qty / work-mch.est-speed
                       VIEW-AS ALERT-BOX INFO BUTTONS OK.
                    IF work-mch.est-run-hr EQ 0 THEN
                    work-mch.est-run-hr    = work-mch.est-qty / work-mch.est-speed.
                    IF work-mch.est-run-hr NE 0 THEN
                    v-run-hr = work-mch.est-run-hr.
                  END.
                  
                  /* wfk - 7/31/13 - testing */
                  IF v-act-run GT 0 THEN
                      v-run-hr = v-act-run.

                  IF v-tspost-val EQ "Actual" THEN
                  DO i = 1 TO mch-act.crew:
                    v-est-run-rate = v-est-run-rate + mch-act.rate[i].
                  END. /* postval eq actual */
                  ELSE
                  v-est-run-rate = mch-act.rate[INT(mch-act.crew)].
                  IF work-mch.m-code = "BMA" AND lDebug THEN
                      MESSAGE "add in 6"  ((v-est-run-rate + mch-act.varoh + mch-act.fixoh)
                       * v-run-hr)
                          VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  ASSIGN
                    work-mch.est-run-cost  = work-mch.est-run-cost +
                       ((v-est-run-rate + mch-act.varoh + mch-act.fixoh)
                       * v-run-hr)
                    work-mch.est-run-cost1 = work-mch.est-run-cost1 +
                      (v-est-run-rate * v-run-hr)
                    work-mch.est-run-cost2 = work-mch.est-run-cost2 +
                      (mch-act.fixoh * v-run-hr)
                    work-mch.est-run-cost3 = work-mch.est-run-cost3 +
                      (mch-act.varoh * v-run-hr).
                 
                END. /* then do...  (each mch-act) */
                /* recalc of est-run-hrs to include actuals */
          LEAVE.
      END. /* each mch-act (avail work-mch and costs are zero) */
      /* 03041306 */
      IF work-mch.m-code = "BMA" AND lDEbug THEN
      MESSAGE "recalc of est-run-hr3" v-act-run "Original was" work-mch.est-run-hr
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      /* wfk - 7/31/13 - This causes a mismatch between cost and hours */
      /* if est-run-hr is overriden, costs should change */
     /* wfk - 7/31/13 - taking out until figure out whats right: work-mch.est-run-hr = v-act-run. */
      v-act-run = 0.
      
    END. /* each job-mch */
END PROCEDURE. /* fix-missing-costs */


PROCEDURE add-in-actuals:
DEF INPUT PARAMETER ipcJob LIKE job.job NO-UNDO.
/* #BL# Examine each machine actual and find the matching temp record work-mch*/
for each mch-act where
    mch-act.company = cocode and
    mch-act.job = ipcJob
    NO-LOCK,
    FIRST mach where
          mach.company = cocode and
          mach.loc     = locode and
          mach.m-code  = mch-act.m-code
          NO-LOCK:
   
   FIND FIRST work-mch
       WHERE work-mch.m-code    EQ mch-act.m-code
         AND work-mch.frm       EQ mch-act.frm
         AND (work-mch.blk      EQ mch-act.blank-no OR
              (work-mch.blk     EQ 1 AND
               mach.p-type      EQ "B" AND
               mch-act.blank-no EQ 0))
         AND work-mch.pass      EQ mch-act.pass
       NO-ERROR.


   IF NOT AVAIL work-mch THEN DO:
      CREATE work-mch.
      ASSIGN work-mch.m-code    = mch-act.m-code
             work-mch.frm       = mch-act.frm
             work-mch.blk       = IF mach.p-type EQ "B"    AND
                                     mch-act.blank-no EQ 0 THEN 1
                                                           ELSE mch-act.blank-no
             work-mch.pass      = mch-act.pass
             work-mch.d-seq     = mach.d-seq
             work-mch.est-speed = mch-act.qty / mch-act.hours
             work-mch.run-speed = mch-act.qty / mch-act.hours.

   END. /* not avail work-mch */


   find job-code where job-code.code = mch-act.code
                       no-lock.
   /* #BL# If the 'current' toggle-box is not checked, run the get actual rate program */
   ll-act-rate = NO.
   IF NOT tb_curr THEN DO:
     /* #PN# Get actual rate */
     RUN jc/getactrt.p (ROWID(mch-act), OUTPUT ll-act-rate, OUTPUT ld-tot-rate).
     IF NOT ll-act-rate THEN
       ASSIGN
        ll-act-rate = YES
        ld-tot-rate = mch-act.rate[INT(mch-act.crew)].
   END. /* not tb_curr */
   /* #BL# If the job code category is 'run' or 'DT' then add in */
   /* #BL# run-qty, wst-qty, run-hr, run-costs from the machine actual */
   IF work-mch.m-code = "BMA" AND lDebug AND AVAIL job-code THEN
   MESSAGE "job-code" job-code.cat "if run, add in hours " mch-act.hours " times rate = " mach.run-rate ld-tot-rate
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
   IF AVAIL job-code AND job-code.cat eq "RUN" or job-code.cat eq "DT" then do:
      assign
       work-mch.run-qty   = work-mch.run-qty   + mch-act.qty
       work-mch.wst-qty   = work-mch.wst-qty   + mch-act.waste
       work-mch.run-hr    = work-mch.run-hr    + mch-act.hours
       work-mch.run-cost1 = work-mch.run-cost1 +
                            (mch-act.hours *
                             IF ll-act-rate THEN ld-tot-rate
                             ELSE IF tb_curr-crew THEN mach.run-rate
                             ELSE (mach.run-rate / 
                                 (IF mach.run-crusiz EQ 0 THEN 1 ELSE mach.run-crusiz) *
                                 mch-act.crew)) /*edited to share logic with JR7*/
       work-mch.run-cost2 = work-mch.run-cost2 +
                            ((IF tb_curr THEN mach.run-fixoh ELSE mch-act.fixoh) * mch-act.hours)
       work-mch.run-cost3 = work-mch.run-cost3 +
                            ((IF tb_curr THEN mach.run-varoh ELSE mch-act.varoh) * mch-act.hours).
   end. /* cat = run or dt */
   
   else
   IF AVAIL job-code AND job-code.cat eq "MR" THEN DO:
       /* #BL# If the job code category is 'MR' then add in the */
       /* #BL# mr-qty, mr-hr, mr-costs from the machine actual */
       assign
       work-mch.mr-qty   = work-mch.mr-qty   + mch-act.qty + mch-act.waste
       work-mch.mr-hr    = work-mch.mr-hr    + mch-act.hours
       work-mch.mr-cost1 = work-mch.mr-cost1 +
                           (mch-act.hours *
                            IF ll-act-rate THEN ld-tot-rate
                            ELSE IF tb_curr-crew THEN mach.mr-rate
                            ELSE (mach.mr-rate / 
                                 (IF mach.mr-crusiz EQ 0 THEN 1 ELSE mach.mr-crusiz) *
                                 mch-act.crew)) /*edited to share logic with JR7*/
       work-mch.mr-cost2 = work-mch.mr-cost2 +
                           ((IF tb_curr THEN mach.mr-fixoh ELSE mch-act.fixoh) * mch-act.hours)
       work-mch.mr-cost3 = work-mch.mr-cost3 +
                           ((IF tb_curr THEN mach.mr-varoh ELSE mch-act.varoh) * mch-act.hours).
  END. /* else if job-code... */
  
  IF work-mch.m-code = "BMA" AND work-mch.run-qty GT 0 AND lDebug THEN
      MESSAGE "override of est run-qty with actual: " work-mch.run-qty
          VIEW-AS ALERT-BOX INFO BUTTONS OK.

  /* #PN# Estimated run-qty overridden by actual */
  IF work-mch.run-qty GT 0 THEN work-mch.est-qty = work-mch.run-qty.

  IF work-mch.est-qty   GT 0 AND
     work-mch.est-speed GT 0 THEN DO:
      IF work-mch.m-code = "BMA" AND lDebug THEN
    MESSAGE "recalc costs from qty, speed" work-mch.est-qty work-mch.est-speed SKIP
          "cost1" work-mch.est-run-cost1 SKIP
          "cost2" work-mch.est-run-cost2 SKIP
          "cost3" work-mch.est-run-cost3 SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    /* #PN# Recalc est-run-costs */
    ASSIGN
     /* work-mch.est-run-hr    = work-mch.est-qty / work-mch.est-speed */
     work-mch.est-run-cost1 = work-mch.est-run-cost1 / work-mch.est-run-hr
     work-mch.est-run-cost2 = work-mch.est-run-cost2 / work-mch.est-run-hr
     work-mch.est-run-cost3 = work-mch.est-run-cost3 / work-mch.est-run-hr
     work-mch.est-run-hr    = work-mch.est-qty / work-mch.est-speed.

    /* #PN# Recalc costs if error */
    IF work-mch.est-run-cost1 EQ ?
        OR work-mch.est-run-cost2 EQ ?
        OR work-mch.est-run-cost3 EQ ? THEN DO:
           IF work-mch.m-code = "BMA" AND lDebug THEN
            MESSAGE "error in calculation for mach" work-mch.m-code
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            ASSIGN  
             work-mch.est-run-cost1 = work-mch.est-run-cost1 / work-mch.est-run-hr
             work-mch.est-run-cost2 = work-mch.est-run-cost2 / work-mch.est-run-hr
             work-mch.est-run-cost3 = work-mch.est-run-cost3 / work-mch.est-run-hr.        
    END.

    IF work-mch.est-run-cost1 EQ ? THEN work-mch.est-run-cost1 = 0.
    IF work-mch.est-run-cost2 EQ ? THEN work-mch.est-run-cost2 = 0.
    IF work-mch.est-run-cost3 EQ ? THEN work-mch.est-run-cost3 = 0.
    IF work-mch.est-run-hr    EQ ? THEN work-mch.est-run-hr    = 0.
IF work-mch.m-code = "BMA" AND lDebug THEN
    MESSAGE "Add in 7" work-mch.est-run-cost1  work-mch.est-run-hr SKIP
    "tot cost2" work-mch.est-run-cost2 * work-mch.est-run-hr SKIP
    "ext cost3" work-mch.est-run-cost3 * work-mch.est-run-hr SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN
       work-mch.est-run-cost1 = work-mch.est-run-cost1 * work-mch.est-run-hr
       work-mch.est-run-cost2 = work-mch.est-run-cost2 * work-mch.est-run-hr
       work-mch.est-run-cost3 = work-mch.est-run-cost3 * work-mch.est-run-hr
       work-mch.est-run-cost  = work-mch.est-run-cost1 +
                                work-mch.est-run-cost2 +
                                work-mch.est-run-cost3.

  END. /* if qty or speed > 0 */

END. /* Each mach-act */
END PROCEDURE. /* add-in-actuals */


