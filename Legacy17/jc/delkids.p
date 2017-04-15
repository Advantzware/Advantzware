
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-manual-too AS LOG NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.


FIND job WHERE ROWID(job) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job THEN DO:
  FOR EACH job-mat
      WHERE job-mat.company EQ job.company
        AND job-mat.job     EQ job.job
        AND job-mat.job-no  EQ job.job-no
        AND job-mat.job-no2 EQ job.job-no2
      EXCLUSIVE:

    RUN jc/maydeletejob-mat.p (BUFFER job-mat, OUTPUT ll).

    IF ll THEN DO:
      FIND FIRST item
          WHERE item.company EQ job-mat.company
            AND item.i-no    EQ job-mat.i-no
          NO-LOCK NO-ERROR.

      IF NOT AVAIL item    OR
         job-mat.j-no EQ 0 OR
         ip-manual-too     THEN DO:

        /*IF job-mat.all-flg THEN RUN jc/jc-all2.p (ROWID(job-mat), -1).*/

        DELETE job-mat.
      END.
    END.
  END.

  li = 0.
  FOR EACH job-mat
      WHERE job-mat.company EQ job.company
        AND job-mat.job     EQ job.job
        AND job-mat.job-no  EQ job.job-no
        AND job-mat.job-no2 EQ job.job-no2
      EXCLUSIVE
      BY job-mat.line:

    ASSIGN
     li           = li + 1
     job-mat.line = li.
  END.

  FOR EACH job-mch
      WHERE job-mch.company EQ job.company
        AND job-mch.job     EQ job.job
        AND job-mch.job-no  EQ job.job-no
        AND job-mch.job-no2 EQ job.job-no2
        AND (job-mch.j-no   EQ 0 OR ip-manual-too)
        AND NOT CAN-FIND(FIRST mch-act
                         WHERE mch-act.company  EQ job-mch.company
                           AND mch-act.job      EQ job-mch.job
                           AND mch-act.job-no   EQ job-mch.job-no
                           AND mch-act.job-no2  EQ job-mch.job-no2
                           AND mch-act.frm      EQ job-mch.frm
                           AND mch-act.blank-no EQ job-mch.blank-no
                           AND mch-act.m-code   EQ job-mch.m-code
                           AND mch-act.pass     EQ job-mch.pass)
      EXCLUSIVE:
    DELETE job-mch.
  END.

  FOR EACH job-farm
      WHERE job-farm.company EQ job.company
        AND job-farm.job-no  EQ job.job-no
        AND job-farm.job-no2 EQ job.job-no2
      EXCLUSIVE:
    DELETE job-farm.
  END.

  FOR EACH job-farm-rctd
      WHERE job-farm-rctd.company EQ job.company
        AND job-farm-rctd.job-no  EQ job.job-no
        AND job-farm-rctd.job-no2 EQ job.job-no2
      EXCLUSIVE:
    DELETE job-farm-rctd.
  END.

  FOR EACH job-prep
      WHERE job-prep.company EQ job.company
        AND job-prep.job     EQ job.job
        AND job-prep.job-no  EQ job.job-no
        AND job-prep.job-no2 EQ job.job-no2
        AND NOT CAN-FIND(FIRST misc-act
                         WHERE misc-act.company  EQ job-prep.company
                           AND misc-act.job      EQ job-prep.job
                           AND misc-act.job-no   EQ job-prep.job-no
                           AND misc-act.job-no2  EQ job-prep.job-no2
                           AND misc-act.frm      EQ job-prep.frm
                           AND misc-act.blank-no EQ job-prep.blank-no
                           AND misc-act.m-code   EQ job-prep.code
                           AND misc-act.ml       EQ job-prep.ml)
      EXCLUSIVE:
    DELETE job-prep.
  END.
END.
