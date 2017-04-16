
{sys/inc/var.i NEW SHARED}

DEF VAR ll AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.


PAUSE 0 BEFORE-HIDE.

FOR EACH company,
    FIRST sys-ctrl
    WHERE sys-ctrl.company  EQ company.company
      AND sys-ctrl.name     EQ "AUTOISSU"
      AND sys-ctrl.char-fld EQ "JobClose":

  cocode = company.company.

  FOR EACH job NO-LOCK
      WHERE job.company EQ company.company
        AND job.opened  EQ YES
        AND CAN-FIND(FIRST job-hdr
                     WHERE job-hdr.company EQ job.company
                       AND job-hdr.job     EQ job.job
                       AND job-hdr.job-no  EQ job.job-no
                       AND job-hdr.job-no2 EQ job.job-no2)
      USE-INDEX opened,

      EACH fg-rcpth NO-LOCK
      WHERE fg-rcpth.company   EQ job.company
        AND fg-rcpth.job-no    EQ job.job-no
        AND fg-rcpth.job-no2   EQ job.job-no2
        AND fg-rcpth.rita-code EQ "R"
      USE-INDEX job,

      EACH fg-rdtlh NO-LOCK
      WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
      USE-INDEX rm-rdtl

      BY job.company BY job.job-no BY job.job-no2 BY job.job:

    DISPLAY "Processing Company/Job#: " +
            TRIM(job.company) + "/"     +
            TRIM(job.job-no) + "-"      +
            STRING(job.job-no2,"99") FORMAT "x(50)"
        WITH FRAME f1 1 DOWN.
                
    ll = TRIM(job.est-no) NE "" AND
         CAN-FIND(FIRST est
                  WHERE est.company   EQ job.company
                    AND est.est-no    EQ job.est-no
                    AND (est.est-type EQ 2 OR
                         est.est-type EQ 6)).

    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job     EQ job.job
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
          AND job-hdr.i-no    EQ fg-rcpth.i-no:
      ACCUMULATE job-hdr.qty (TOTAL).
    END.

    IF (ACCUM TOTAL job-hdr.qty) NE 0 THEN
    FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company EQ job.company
          AND job-hdr.job-no  EQ job.job-no
          AND job-hdr.job-no2 EQ job.job-no2
          AND job-hdr.i-no    EQ fg-rcpth.i-no,
                
        EACH job-mat NO-LOCK
        WHERE job-mat.company     EQ job-hdr.company
          AND job-mat.job         EQ job-hdr.job
          AND job-mat.job-no      EQ job-hdr.job-no
          AND job-mat.job-no2     EQ job-hdr.job-no2
          AND ((job-mat.frm       EQ job-hdr.frm AND
                (job-mat.blank-no EQ job-hdr.blank-no OR
                 job-mat.blank-no eq 0)) OR
               ll)
        USE-INDEX seq-idx,

        FIRST item NO-LOCK
        WHERE item.company EQ job-mat.company
          AND item.i-no    EQ job-mat.rm-i-no
          AND (NOT CAN-DO("C,D",item.mat-type) OR
               NOT ll)
    
        BREAK BY job-mat.frm
              BY job-mat.blank-no
              BY job-mat.i-no
              BY ROWID(job-mat):

      IF FIRST-OF(job-mat.i-no) THEN
        IF item.industry EQ "1"        AND
           CAN-DO("C,D",item.mat-type) THEN DO:

          ld = fg-rdtlh.cases.

          IF item.mat-type EQ "D" THEN
            ASSIGN
             ld = ld / (IF fg-rdtlh.stacks-unit EQ 0 THEN 1
                        ELSE fg-rdtlh.stacks-unit)
             ld = TRUNC(ld,0)
             ld = ld + INT(fg-rdtlh.partial GT 0)
             ld = ld - INT(fg-rdtlh.partial LT 0).
                                                  
          RUN jc/jc-autop.p (ROWID(job-mat), 0, ld).
        END.

        ELSE DO:
          ld = fg-rdtlh.qty / (ACCUM TOTAL job-hdr.qty) *
               (IF job-mat.blank-no EQ 0 THEN (job-hdr.sq-in / 100)
                                         ELSE 1). 
                                                  
          RUN jc/jc-autop.p (ROWID(job-mat), ld, 0).
        END.
    END.

    /* Case & Pallet Auto Issue for sets */
    IF ll THEN
    FOR EACH reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job.job,"999999999")
          AND reftable.code2    EQ fg-rcpth.i-no,

        EACH job-mat NO-LOCK
        WHERE job-mat.company  EQ job.company
          AND job-mat.job      EQ job.job
          AND job-mat.job-no   EQ job.job-no
          AND job-mat.job-no2  EQ job.job-no2
          AND job-mat.frm      EQ INT(reftable.val[12])
          AND job-mat.blank-no EQ INT(reftable.val[13])
        USE-INDEX seq-idx,

        FIRST item NO-LOCK
        WHERE item.company  EQ job.company
          AND item.i-no     EQ job-mat.rm-i-no
          AND item.industry EQ "1"
          AND CAN-DO("C,D",item.mat-type)
    
        BREAK BY job-mat.frm
              BY job-mat.blank-no
              BY job-mat.i-no
              BY ROWID(job-mat):

      IF FIRST-OF(job-mat.i-no) THEN DO:
        ld = fg-rdtlh.cases.

        IF item.mat-type EQ "D" THEN
          ASSIGN
           ld = ld / (IF fg-rdtlh.stacks-unit EQ 0 THEN 1
                      ELSE fg-rdtlh.stacks-unit)
           ld = TRUNC(ld,0)
           ld = ld + INT(fg-rdtlh.partial GT 0)
           ld = ld - INT(fg-rdtlh.partial LT 0).
                                                  
        RUN jc/jc-autop.p (ROWID(job-mat), 0, ld).
      END.
    END.
  END.

  sys-ctrl.char-fld = "FGPost".
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
