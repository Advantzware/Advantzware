
DEF PARAM BUFFER io-job FOR job.

DEF INPUT PARAM ip-i-no LIKE job-hdr.i-no NO-UNDO.
DEF INPUT PARAM ip-frm LIKE job-hdr.frm NO-UNDO.
DEF INPUT PARAM ip-blk LIKE job-hdr.blank-no NO-UNDO.
DEF INPUT PARAM ip-fac AS INT NO-UNDO.

DEF VAR li-cases AS DEC NO-UNDO.
DEF VAR li-palls AS DEC NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.


IF AVAIL io-job THEN DO:
  FOR EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company   EQ io-job.company
        AND fg-rcpth.i-no      EQ ip-i-no
        AND fg-rcpth.job-no    EQ io-job.job-no
        AND fg-rcpth.job-no2   EQ io-job.job-no2
        AND fg-rcpth.rita-code EQ "R"
      USE-INDEX job,

      EACH fg-rdtlh NO-LOCK
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:

    ASSIGN
     li-cases = li-cases + fg-rdtlh.cases
     ld       = fg-rdtlh.cases / (IF fg-rdtlh.stacks-unit EQ 0 THEN 1
                                  ELSE fg-rdtlh.stacks-unit)
     ld       = TRUNC(ld,0)
     ld       = ld + INT(fg-rdtlh.partial GT 0)
     ld       = ld - INT(fg-rdtlh.partial LT 0)
     li-palls = li-palls + ld.
  END.

  ASSIGN
   li-cases = li-cases * ip-fac
   li-palls = li-palls * ip-fac.

  FOR EACH job-mat NO-LOCK
      WHERE job-mat.company  EQ io-job.company
        AND job-mat.job      EQ io-job.job
        AND job-mat.job-no   EQ io-job.job-no
        AND job-mat.job-no2  EQ io-job.job-no2
        AND job-mat.frm      EQ ip-frm
        AND job-mat.blank-no EQ ip-blk
      USE-INDEX seq-idx,

      FIRST item NO-LOCK
      WHERE item.company  EQ job-mat.company
        AND item.i-no     EQ job-mat.rm-i-no
        AND item.industry EQ "1"
        AND CAN-DO("C,D",item.mat-type)
      USE-INDEX i-no
    
      BREAK BY job-mat.frm
            BY job-mat.blank-no
            BY job-mat.i-no
            BY ROWID(job-mat):

    IF FIRST-OF(job-mat.i-no) THEN
      RUN jc/jc-autop.p (ROWID(job-mat), 0,
                         IF item.mat-type EQ "C" THEN li-cases ELSE li-palls).
  END.
END.
