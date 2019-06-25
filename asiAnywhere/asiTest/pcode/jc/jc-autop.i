/* -------------------------------------------------- jc/jc-autop.i 12/96 JLF */
/* Job Costing - Create autopost rm issues                                    */
/* -------------------------------------------------------------------------- */

FOR EACH job-mat NO-LOCK
    WHERE job-mat.company EQ job.company
      AND job-mat.job     EQ job.job
      AND job-mat.job-no  EQ job.job-no
      AND job-mat.job-no2 EQ job.job-no2
    USE-INDEX seq-idx,

    FIRST item NO-LOCK
    WHERE item.company EQ job-mat.company
      AND item.i-no    EQ job-mat.rm-i-no
      AND (item.industry NE "1" OR
           NOT CAN-DO("C,D",item.mat-type))
    USE-INDEX i-no
    
    BREAK BY job-mat.frm
          BY job-mat.blank-no
          BY job-mat.i-no
          BY ROWID(job-mat):

  IF FIRST-OF(job-mat.i-no) THEN RUN jc/jc-autop.p (ROWID(job-mat), {1}, 0).
END.
