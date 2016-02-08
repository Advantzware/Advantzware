/* -------------------------------------------------- jc/chkrebld.p 06/00 JLF */
/* Job Costing - Check for GL entries to determine if stds may be rebuilt     */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM  v-recid        AS   RECID.
DEF OUTPUT PARAM v-locked       AS   LOG.

{sys/inc/var.i SHARED}


FIND job WHERE RECID(job) EQ v-recid NO-LOCK.

v-locked = NO.

FOR EACH est-op NO-LOCK
    WHERE est-op.company EQ job.company
      AND est-op.est-no  EQ job.est-no
      AND est-op.line    LT 500,
    FIRST mach NO-LOCK
    {sys/look/machW.i}
      AND mach.m-code EQ est-op.m-code,
    FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "mach.obsolete"
      AND reftable.company  EQ mach.company
      AND reftable.loc      EQ mach.loc
      AND reftable.code     EQ mach.m-code
      AND reftable.val[1]   EQ 1:
  MESSAGE "Machine: " + TRIM(mach.m-code) +
          " is obsolete, please replace to create/update job standards..."
      VIEW-AS ALERT-BOX ERROR.
  v-locked = YES.
  RETURN.
END.

FOR EACH fg-act NO-LOCK
    WHERE fg-act.company EQ cocode
      AND fg-act.job     EQ job.job
      AND fg-act.job-no  EQ job.job-no
      AND fg-act.job-no2 EQ job.job-no2
      AND fg-act.opn     EQ NO
    USE-INDEX job-idx
    BREAK BY fg-act.i-no:
    
  ACCUM fg-act.qty (TOTAL). 

  IF LAST-OF(fg-act.i-no) AND (ACCUM TOTAL fg-act.qty) NE 0 THEN DO:
    v-locked = YES.
    RETURN.
  END.
END.
        
FOR EACH mat-act NO-LOCK
    WHERE mat-act.company EQ cocode
      AND mat-act.job     EQ job.job
      AND mat-act.job-no  EQ job.job-no
      AND mat-act.job-no2 EQ job.job-no2
      AND mat-act.opn     EQ NO:
    
  v-locked = YES.
  RETURN.
END.
    
FOR EACH mch-act NO-LOCK
    WHERE mch-act.company EQ cocode
      AND mch-act.job     EQ job.job
      AND mch-act.job-no  EQ job.job-no
      AND mch-act.job-no2 EQ job.job-no2
      AND mch-act.opn     EQ NO:  
    
  v-locked = YES.
  RETURN.
END.
    
FOR EACH misc-act NO-LOCK
    WHERE misc-act.company EQ cocode
      AND misc-act.job     EQ job.job
      AND misc-act.job-no  EQ job.job-no
      AND misc-act.job-no2 EQ job.job-no2
      AND misc-act.opn     EQ NO:
    
  v-locked = YES.
  RETURN.
END.
