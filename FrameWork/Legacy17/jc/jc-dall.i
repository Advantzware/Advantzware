
SESSION:SET-WAIT-STATE ("general").

DEF VAR lv-stat LIKE job.stat NO-UNDO.


FOR EACH job-mat
    WHERE job-mat.company EQ job.company
      AND job-mat.job     EQ job.job
      AND job-mat.job-no  EQ job.job-no
      AND job-mat.job-no2 EQ job.job-no2
    NO-LOCK
    BREAK BY job-mat.job:

  RUN jc/jc-all.p (ROWID(job-mat), -1, INPUT-OUTPUT lv-stat).

  IF LAST(job-mat.job) AND job.stat EQ "A" THEN job.stat = "L".
END.

SESSION:SET-WAIT-STATE ("").
