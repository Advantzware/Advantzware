
{sys/inc/var.i NEW SHARED}


FOR EACH mch-act BREAK BY company BY job BY job-no BY job-no2:
  IF FIRST-OF(job-no2) THEN DO:
    FIND FIRST job NO-LOCK
        WHERE job.company EQ mch-act.company
          AND job.job     EQ mch-act.job
          AND job.job-no  EQ mch-act.job-no
          AND job.job-no  EQ mch-act.job-no
        NO-ERROR.

    IF AVAIL job THEN DO:
      DISPLAY "Processing Job#: " +
             TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")
             FORMAT "x(50)" WITH 1 DOWN.
      cocode = job.company.
      RUN jc/job-cls2.p (RECID(job)).
    END.
  END.
END.

HIDE ALL NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.


