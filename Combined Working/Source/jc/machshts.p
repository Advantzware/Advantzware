
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-qty AS DEC NO-UNDO.
DEF INPUT PARAM ip-line LIKE job-mch.line NO-UNDO.

DEF VAR ld AS DEC NO-UNDO.
DEF VAR ld1 AS DEC NO-UNDO.
DEF VAR lv-n-on LIKE job-mch.n-on NO-UNDO.


FIND job-mat WHERE ROWID(job-mat) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL job-mat THEN
FOR EACH job-hdr
    WHERE job-hdr.company EQ job-mat.company
      AND job-hdr.job     EQ job-mat.job
      AND job-hdr.job-no  EQ job-mat.job-no
      AND job-hdr.job-no2 EQ job-mat.job-no2
    NO-LOCK
    BREAK BY job-hdr.frm DESC:

 IF job-hdr.frm EQ job-mat.frm OR
    LAST(job-hdr.frm)          THEN DO:

  ld = ip-qty.

  FOR EACH job-mch
      WHERE job-mch.company  EQ job-mat.company
        AND job-mch.job      EQ job-mat.job
        AND job-mch.job-no   EQ job-mat.job-no
        AND job-mch.job-no2  EQ job-mat.job-no2
        AND job-mch.frm      EQ job-mat.frm
        AND job-mch.blank-no EQ 0
        AND job-mch.line     GE ip-line
      USE-INDEX line-idx:

    ld1 = TRUNC(ld / job-mch.n-on,0).

    ASSIGN
     job-mch.run-qty = ld1
     ld1             = job-mch.mr-waste * job-mch.n-on
     ld              = ld - ld1
     ld              = ld1 + (ld * (job-mch.wst-prct / 100)).

    {sys/inc/roundup.i ld}

    ASSIGN
     ld      = (job-mch.run-qty * job-mch.n-on) - ld.
     lv-n-on = job-mch.n-on.
  END.

  ld = TRUNC(ld / lv-n-on,0).

  FOR EACH job-mch
      WHERE job-mch.company  EQ job-mat.company
        AND job-mch.job      EQ job-mat.job
        AND job-mch.job-no   EQ job-mat.job-no
        AND job-mch.job-no2  EQ job-mat.job-no2
        AND job-mch.frm      EQ job-mat.frm
        AND job-mch.blank-no NE 0
        AND job-mch.line     GE ip-line
      USE-INDEX line-idx
      BREAK BY job-mch.blank-no
            BY job-mch.line:

    IF FIRST-OF(job-mch.blank-no) THEN DO:
      RELEASE eb.
      IF TRIM(job-hdr.est-no) NE "" THEN
      FIND FIRST eb
          WHERE eb.company  EQ job-hdr.company
            AND eb.est-no   EQ job-hdr.est-no
            AND eb.form-no  EQ job-mch.frm
            AND eb.blank-no EQ job-mch.blank-no
          NO-LOCK NO-ERROR.
      ld1 = ld * IF AVAIL eb THEN eb.num-up ELSE 1.
    END.

    ASSIGN
     job-mch.run-qty = ld1
     ld1               = job-mch.run-qty - job-mch.mr-waste
     ld1               = job-mch.mr-waste + (ld1 * (job-mch.wst-prct / 100)).
    {sys/inc/roundup.i ld1}
    ld1 = job-mch.run-qty - ld1.
  END.
 END.
END.
