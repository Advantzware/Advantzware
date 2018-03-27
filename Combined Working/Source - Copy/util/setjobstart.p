
DEF VAR li AS INT NO-UNDO.


PAUSE 0 BEFORE-HIDE.

FOR EACH company NO-LOCK,
    EACH job
    WHERE job.company EQ company.company
      AND job.opened  EQ NO
    USE-INDEX opened:

  DISPLAY job.company   LABEL "Company"
          TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")
                        LABEL "Job#"        FORMAT "x(12)"
      WITH FRAME f1 TITLE "Checking".

  li = 0.

  DO WHILE li LT 2                          AND
           (job.start-date LT 01/01/0001 OR
            job.start-date GT 12/31/9999 OR
            job.start-date EQ ?):

    li = li + 1.

    IF li EQ 1 THEN DO:
      DISPLAY job.company   LABEL "Company"
              TRIM(job.job-no) + "-" + STRING(job.job-no2,"99")
                            LABEL "Job#"        FORMAT "x(12)"
          WITH FRAME f2 TITLE "Fixed" 20 DOWN COLUMN 40.

      job.start-date = job.close-date.
    END.

    ELSE
    IF li EQ 2 THEN
    FOR EACH fg-rcpth
        WHERE fg-rcpth.company   EQ job.company
          AND fg-rcpth.job-no    EQ job.job-no
          AND fg-rcpth.job-no2   EQ job.job-no2
          AND fg-rcpth.rita-code EQ "R"
        USE-INDEX job
        BY fg-rcpth.trans-date DESC:
      job.start-date = fg-rcpth.trans-date.
      LEAVE.
    END.
  END.
END.

MESSAGE "Procedure complete..." VIEW-AS ALERT-BOX.

HIDE FRAME f1 NO-PAUSE.
HIDE FRAME f2 NO-PAUSE.
