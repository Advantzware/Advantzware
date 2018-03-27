
DEF BUFFER b-work-mch FOR work-mch.
DEFINE VARIABLE v-est-run-rate  AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-hours-entered AS LOGICAL NO-UNDO.
DEFINE VARIABLE v-num-speeds    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-run-hr        AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-rate-found    AS LOGICAL NO-UNDO.

v-hours-entered = NO.

for each job-mch where
    job-mch.company = cocode and
    job-mch.job = job.job
    use-index seq-idx
    NO-LOCK,
    FIRST mach where
          mach.company = cocode and
          mach.loc     = locode and
          mach.m-code  = job-mch.m-code
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

   ASSIGN
      work-mch.est-mr-hr  = work-mch.est-mr-hr + job-mch.mr-hr
      work-mch.est-run-hr = work-mch.est-run-hr + job-mch.run-hr
      work-mch.mr-waste   = work-mch.mr-waste + job-mch.mr-waste.

   
   v-run-hr = job-mch.run-hr.
   /* If hours are zero, try to get actual - wfk */
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

         v-est-run-rate = 0.
         v-rate-found = NO.
         /*get cost from first run transaction*/
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
             /* Use first one found - Task 02191301 */
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
end. /* each job-mch */

/* wfk - new section added to clear up issue of missing costs */
for each job-mch 
  WHERE job-mch.company = cocode 
    and job-mch.job = job.job
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
    
  IF AVAIL(work-mch) AND work-mch.est-run-hr EQ 0 AND work-mch.est-run-cost1 EQ 0
     AND work-mch.est-run-cost2 EQ 0 THEN DO:
    
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
      
      v-run-hr = job-mch.run-hr.
      
      IF job-mch.run-hr EQ 0 OR work-mch.est-run-hr = 0 THEN DO:
        IF AVAIL mch-act AND mch-act.hours GT 0 THEN
        v-run-hr = mch-act.hours.
        IF work-mch.est-qty EQ 0 THEN
        work-mch.est-qty   = mch-act.qty.
        
        IF work-mch.est-run-hr EQ 0 THEN
        work-mch.est-run-hr    = work-mch.est-qty / work-mch.est-speed.
        IF work-mch.est-run-hr NE 0 THEN
        v-run-hr = work-mch.est-run-hr.
      END.
      
      IF v-tspost-val EQ "Actual" THEN
      DO i = 1 TO mch-act.crew:
        v-est-run-rate = v-est-run-rate + mch-act.rate[i].
      END. /* postval eq actual */
      ELSE
      v-est-run-rate = mch-act.rate[INT(mch-act.crew)].

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
      LEAVE.
    END. /* each mch-act */
    
  END. /* avail work-mch and costs are zero */
  
END. /* each job-mch */

for each mch-act where
    mch-act.company = cocode and
    mch-act.job = job.job
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

   ll-act-rate = NO.
   IF NOT tb_curr THEN DO:
     RUN jc/getactrt.p (ROWID(mch-act), OUTPUT ll-act-rate, OUTPUT ld-tot-rate).
     IF NOT ll-act-rate THEN
       ASSIGN
        ll-act-rate = YES
        ld-tot-rate = mch-act.rate[INT(mch-act.crew)].
   END. /* not tb_curr */

   if job-code.cat eq "RUN" or job-code.cat eq "DT" then do:
      
       assign
       work-mch.run-qty   = work-mch.run-qty   + mch-act.qty
       work-mch.wst-qty   = work-mch.wst-qty   + mch-act.waste
       work-mch.run-hr    = work-mch.run-hr    + mch-act.hours
       work-mch.run-cost1 = work-mch.run-cost1 +
                            (mch-act.hours *
                             IF ll-act-rate THEN ld-tot-rate
                             ELSE mach.run-rate)
       work-mch.run-cost2 = work-mch.run-cost2 +
                            ((IF tb_curr THEN mach.run-fixoh ELSE mch-act.fixoh) * mch-act.hours)
       work-mch.run-cost3 = work-mch.run-cost3 +
                            ((IF tb_curr THEN mach.run-varoh ELSE mch-act.varoh) * mch-act.hours).
   end. /* cat = run or dt */
   
   else
   if job-code.cat eq "MR" THEN DO:
      
       assign
       work-mch.mr-qty   = work-mch.mr-qty   + mch-act.qty + mch-act.waste
       work-mch.mr-hr    = work-mch.mr-hr    + mch-act.hours
       work-mch.mr-cost1 = work-mch.mr-cost1 +
                           (mch-act.hours *
                            IF ll-act-rate THEN ld-tot-rate
                            ELSE mach.mr-rate)
       work-mch.mr-cost2 = work-mch.mr-cost2 +
                           ((IF tb_curr THEN mach.mr-fixoh ELSE mch-act.fixoh) * mch-act.hours)
       work-mch.mr-cost3 = work-mch.mr-cost3 +
                           ((IF tb_curr THEN mach.mr-varoh ELSE mch-act.varoh) * mch-act.hours).
  END. /* else if job-code... */

  IF work-mch.run-qty GT 0 THEN work-mch.est-qty = work-mch.run-qty.

  IF work-mch.est-qty   GT 0 AND
     work-mch.est-speed GT 0 THEN DO:
    
    ASSIGN
     /* work-mch.est-run-hr    = work-mch.est-qty / work-mch.est-speed */
     work-mch.est-run-cost1 = work-mch.est-run-cost1 / work-mch.est-run-hr
     work-mch.est-run-cost2 = work-mch.est-run-cost2 / work-mch.est-run-hr
     work-mch.est-run-cost3 = work-mch.est-run-cost3 / work-mch.est-run-hr
     work-mch.est-run-hr    = work-mch.est-qty / work-mch.est-speed.
    IF work-mch.est-run-cost1 EQ ?
        OR work-mch.est-run-cost2 EQ ?
        OR work-mch.est-run-cost3 EQ ? THEN DO:

            ASSIGN  
             work-mch.est-run-cost1 = work-mch.est-run-cost1 / work-mch.est-run-hr
             work-mch.est-run-cost2 = work-mch.est-run-cost2 / work-mch.est-run-hr
             work-mch.est-run-cost3 = work-mch.est-run-cost3 / work-mch.est-run-hr.
        

    END.

        IF work-mch.est-run-cost1 EQ ? THEN work-mch.est-run-cost1 = 0.
        IF work-mch.est-run-cost2 EQ ? THEN work-mch.est-run-cost2 = 0.
        IF work-mch.est-run-cost3 EQ ? THEN work-mch.est-run-cost3 = 0.
        IF work-mch.est-run-hr    EQ ? THEN work-mch.est-run-hr    = 0.

        ASSIGN
         work-mch.est-run-cost1 = work-mch.est-run-cost1 * work-mch.est-run-hr
         work-mch.est-run-cost2 = work-mch.est-run-cost2 * work-mch.est-run-hr
         work-mch.est-run-cost3 = work-mch.est-run-cost3 * work-mch.est-run-hr
         work-mch.est-run-cost  = work-mch.est-run-cost1 +
                                  work-mch.est-run-cost2 +
                                  work-mch.est-run-cost3.

  END. /* speed > 0 */

END. /* Each mach-act */

RELEASE work-mch.

/* Section appears to Clear Out Unknown Values - wfk */
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

  ASSIGN
   work-mch.est-run-cost1 = work-mch.est-run-cost1 * work-mch.est-run-hr
   work-mch.est-run-cost2 = work-mch.est-run-cost2 * work-mch.est-run-hr
   work-mch.est-run-cost3 = work-mch.est-run-cost3 * work-mch.est-run-hr
   work-mch.est-run-cost  = work-mch.est-run-cost1 +
                            work-mch.est-run-cost2 +
                            work-mch.est-run-cost3.
END. /* Each work-mch */

/* Section Accumulates Values Into the Form 0 Record */
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
      
      IF mch-act.hours GT 0 AND mch-act.frm EQ b-work-mch.frm /*AND mch-act.m-code BEGINS "SHT" */ THEN
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

  ASSIGN
   work-mch.run-qty       = work-mch.run-qty       + b-work-mch.run-qty
   work-mch.run-feet      = work-mch.run-feet      + b-work-mch.run-feet
   work-mch.est-qty       = work-mch.est-qty       + b-work-mch.est-qty
   work-mch.est-feet      = work-mch.est-feet      + b-work-mch.est-feet
   work-mch.wst-qty       = work-mch.wst-qty       + b-work-mch.wst-qty
   work-mch.mr-qty        = work-mch.mr-qty        + b-work-mch.mr-qty
   work-mch.mr-waste      = work-mch.mr-waste      + b-work-mch.mr-waste
   work-mch.est-mr-hr     = work-mch.est-mr-hr     + b-work-mch.est-mr-hr

   work-mch.est-mr-cost   = work-mch.est-mr-cost   + b-work-mch.est-mr-cost

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
       ASSIGN work-mch.est-run-hr    = work-mch.est-run-hr    + b-work-mch.est-run-hr
              work-mch.est-run-cost  = work-mch.est-run-cost  + b-work-mch.est-run-cost  
              work-mch.est-mr-cost1  = work-mch.est-mr-cost1  + b-work-mch.est-mr-cost1
              work-mch.est-mr-cost2  = work-mch.est-mr-cost2  + b-work-mch.est-mr-cost2
              work-mch.est-mr-cost3  = work-mch.est-mr-cost3  + b-work-mch.est-mr-cost3.           .
   END.
   ELSE DO:

     /* tb_exclude_run_if_no_prod meaning they only enter on first one, so 
        exclude 2nd pass */
     IF (v-hours-entered OR (b-work-mch.frm LE 1 AND b-work-mch.pass EQ 1)) THEN DO:

       IF (b-work-mch.est-run-cost1 GT 0 OR b-work-mch.est-run-cost2 GT 0
           OR b-work-mch.est-run-cost3 GT 0) AND b-work-mch.est-run-cost EQ 0 THEN
           b-work-mch.est-run-cost = b-work-mch.est-run-cost1 +
           b-work-mch.est-run-cost2 +
           b-work-mch.est-run-cost3.

       ASSIGN work-mch.est-run-hr    = work-mch.est-run-hr    + b-work-mch.est-run-hr
              work-mch.est-run-cost  = work-mch.est-run-cost  + b-work-mch.est-run-cost
              work-mch.est-mr-cost1  = work-mch.est-mr-cost1  + b-work-mch.est-mr-cost1
              work-mch.est-mr-cost2  = work-mch.est-mr-cost2  + b-work-mch.est-mr-cost2
              work-mch.est-mr-cost3  = work-mch.est-mr-cost3  + b-work-mch.est-mr-cost3           .
     END.
   END.
   
   IF LAST-OF(b-work-mch.m-code) THEN DO:
      work-mch.est-speed = work-mch.est-speed / v-num-speeds.
   END.

  ASSIGN 
/*    wfk - get speed from job-mch per Joe                                   */       
/*    work-mch.est-speed     = (IF work-mch.est-feet GT 0 THEN               */
/*                                work-mch.est-feet ELSE work-mch.est-qty) / */
/*                             work-mch.est-run-hr                           */
   work-mch.run-speed     = (IF work-mch.run-feet GT 0 THEN
                               work-mch.run-feet ELSE work-mch.run-qty) /
                            work-mch.run-hr.
   
  DELETE b-work-mch.
END.

