
DEF INPUT        PARAM ip-job-no LIKE job-mat.job-no NO-UNDO.
DEF INPUT        PARAM ip-job-no2 LIKE job-mat.job-no2 NO-UNDO.
DEF INPUT        PARAM ip-i-no LIKE job-mat.rm-i-no NO-UNDO.
DEF INPUT        PARAM ip-frm LIKE job-mat.frm NO-UNDO.
DEF INPUT        PARAM ip-blk LIKE job-mat.blank-no NO-UNDO.
DEF INPUT-OUTPUT PARAM io-b-qty AS DEC NO-UNDO.

{sys/inc/var.i SHARED}

DEF VAR ld-save-qty AS DEC NO-UNDO.


FIND FIRST job
    WHERE job.company EQ cocode
      AND job.job-no  EQ ip-job-no
      AND job.job-no2 EQ ip-job-no2
    NO-LOCK NO-ERROR.

IF AVAIL job THEN
FIND FIRST job-mat
    WHERE job-mat.company  EQ job.company
      AND job-mat.job      EQ job.job
      AND job-mat.job-no   EQ job.job-no
      AND job-mat.job-no2  EQ job.job-no2
      AND job-mat.rm-i-no  EQ ip-i-no
      AND job-mat.frm      EQ ip-frm
      AND job-mat.blank-no EQ ip-blk
    NO-LOCK NO-ERROR.

IF AVAIL job-mat THEN
FOR EACH eb
    WHERE eb.company EQ job.company
      AND eb.est-no  EQ job.est-no
      AND eb.form-no EQ job-mat.frm
    NO-LOCK
    BREAK BY eb.blank-no DESC:

  IF job-mat.blank-no EQ eb.blank-no OR LAST(eb.blank-no) THEN DO:
    RUN cec/groupcst.p (ROWID(eb), INPUT-OUTPUT ld-save-qty,
                                   INPUT-OUTPUT io-b-qty).
    LEAVE.
  END.
END.

