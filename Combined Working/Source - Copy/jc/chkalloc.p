
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.


{sys/inc/var.i SHARED}

DEF VAR li AS INT NO-UNDO.


FIND job WHERE ROWID(job) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job THEN DO:
  FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ job.company NO-LOCK NO-ERROR.

  FOR EACH job-mat
      WHERE job-mat.company EQ job.company
        AND job-mat.job     EQ job.job
        AND job-mat.job-no  EQ job.job-no
        AND job-mat.job-no2 EQ job.job-no2
        /*AND job-mat.post    EQ YES*/
      EXCLUSIVE,
    
      FIRST item
      WHERE item.company EQ job-mat.company
        AND item.i-no    EQ job-mat.i-no
        AND item.i-code  EQ "R"
        AND item.alloc   EQ YES
      NO-LOCK:

    IF job-mat.all-flg THEN RUN jc/jc-all2.p (ROWID(job-mat), -1).

    job-mat.all-flg = YES.

    RUN jc/jc-all2.p (ROWID(job-mat), 1).
  END.
END.
