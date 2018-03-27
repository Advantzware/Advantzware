DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR li-unit AS INT NO-UNDO.


FOR EACH company NO-LOCK,
    EACH job-mat WHERE job-mat.company EQ company.company,
    FIRST item
    WHERE item.company EQ job-mat.company
      AND item.i-no    EQ job-mat.rm-i-no
      AND INDEX("BPR1234",item.mat-type) GT 0
    NO-LOCK,
    FIRST job
    WHERE job.company EQ job-mat.company
      AND job.job     EQ job-mat.job
      AND job.job-no  EQ job-mat.job-no
      AND job.job-no2 EQ job-mat.job-no2
      AND TRIM(job.est-no) NE ""
    NO-LOCK,
    EACH ef
    WHERE ef.company EQ job.company
      AND ef.est-no  EQ job.est-no
      AND ef.form-no EQ job-mat.frm
    NO-LOCK:

  DISPLAY job-mat.company LABEL "Company"
          TRIM(job-mat.job-no) + STRING(job-mat.job-no2,"99")
                          LABEL "Job#"
                          FORMAT "x(9)"
          job-mat.rm-i-no LABEL "RM Item#"
                          FORMAT "X(20)".

  IF ef.board NE job-mat.rm-i-no THEN job-mat.j-no = 1.
END.

HIDE ALL NO-PAUSE.

