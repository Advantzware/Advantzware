
FOR EACH est
    WHERE NOT CAN-FIND(FIRST est-op
                       WHERE est-op.company EQ est.company
                         AND est-op.est-no  EQ est.est-no
                         AND est-op.LINE    LT 500)
    TRANSACTION:

  IF (est.est-type EQ 1 OR
      est.est-type EQ 5 OR
      est.est-type EQ 6)                           AND
     CAN-FIND(FIRST est-qty
              WHERE est-qty.company EQ est.company
                AND est-qty.est-no  EQ est.est-no) THEN
  FOR EACH est-qty
      WHERE est-qty.company EQ est.company
        AND est-qty.est-no  EQ est.est-no
      NO-LOCK:
    RUN build-route (est-qty.eqty).
  END.

  ELSE RUN build-route (0). 
END.

RETURN.

PROCEDURE build-route:
  DEF INPUT PARAM ip-qty LIKE est-qty.eqty NO-UNDO.


  FOR EACH job
      WHERE job.company EQ est.company
        AND job.est-no  EQ est.est-no
        AND CAN-FIND(FIRST job-mch
                     WHERE job-mch.company EQ job.company
                       AND job-mch.job     EQ job.job
                       AND job-mch.job-no  EQ job.job-no
                       AND job-mch.job-no2 EQ job.job-no2)
      BY job.job:

    FOR EACH job-mch
        WHERE job-mch.company EQ job.company
          AND job-mch.job     EQ job.job
          AND job-mch.job-no  EQ job.job-no
          AND job-mch.job-no2 EQ job.job-no2
        NO-LOCK,

        FIRST mach 
        WHERE mach.company EQ job-mch.company
          AND mach.m-code  EQ job-mch.m-code
        NO-LOCK:

      CREATE est-op.
      BUFFER-COPY job-mch EXCEPT rec_key TO est-op
      ASSIGN
       est-op.est-no     = est.est-no
       est-op.qty        = ip-qty
       est-op.s-num      = job-mch.frm
       est-op.b-num      = IF est.est-type EQ 5 THEN 1 ELSE job-mch.blank-no
       est-op.m-dscr     = mach.m-dscr
       est-op.op-mr      = job-mch.mr-hr
       est-op.op-waste   = job-mch.mr-waste
       est-op.op-speed   = job-mch.speed
       est-op.op-spoil   = job-mch.wst-prct
       est-op.num-sh     = job-mch.run-qty
       est-op.op-sb      = mach.p-type ne "B"
       est-op.d-seq      = mach.d-seq
       est-op.op-pass    = job-mch.pass
       est-op.op-crew[1] = mach.mr-crusiz
       est-op.op-crew[2] = mach.run-crusiz.

      FIND FIRST eb
          WHERE eb.company   EQ est-op.company
            AND eb.est-no    EQ est-op.est-no
            AND eb.form-no   EQ est-op.s-num
            AND (eb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
          NO-LOCK NO-ERROR.

      IF AVAIL eb THEN DO:
        RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "M R",
                            INPUT-OUTPUT est-op.op-crew[1]).
        RUN est/getcrusz.p (ROWID(mach), ROWID(eb), est-op.dept, "RUN",
                            INPUT-OUTPUT est-op.op-crew[2]).
      END.

      ASSIGN
       est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                           mach.mr-varoh  + mach.mr-fixoh
       est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                           mach.run-varoh + mach.run-fixoh.
    END.

    LEAVE.
  END.
END.
