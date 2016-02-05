
DEF PARAM BUFFER io-job FOR job.

{sys/inc/var.i SHARED}

DEF BUFFER b-jc-calc FOR reftable.

DEF VAR v-up        AS   INT                                            NO-UNDO.
DEF VAR v-on        AS   INT                                            NO-UNDO.
DEF VAR v-out       AS   INT                                            NO-UNDO.
DEF VAR v-qty       AS   DEC                                            NO-UNDO.
DEF VAR v-fin       AS   DEC                                            NO-UNDO.
DEF VAR v-wst       AS   DEC                                            NO-UNDO.
DEF VAR v-rs-seq    LIKE dept.fc                                        NO-UNDO.
DEF VAR v-rc-seq    LIKE dept.fc                                        NO-UNDO.

DEF TEMP-TABLE tt-jh LIKE job-hdr.

{jc/job-cls3.i}


IF NOT AVAIL io-job THEN LEAVE.

FIND FIRST dept WHERE dept.code EQ "RS" NO-LOCK NO-ERROR.
v-rs-seq = IF AVAIL dept THEN dept.fc ELSE 1.

FIND FIRST dept WHERE dept.code EQ "RC" NO-LOCK NO-ERROR.
v-rc-seq = IF AVAIL dept THEN dept.fc ELSE 1.

FIND FIRST est NO-LOCK
    WHERE est.company EQ io-job.company
      AND est.est-no  EQ io-job.est-no
    NO-ERROR.

IF AVAIL est AND (est.est-type EQ 2 OR est.est-type EQ 6) THEN
FOR EACH b-jc-calc NO-LOCK
    WHERE b-jc-calc.reftable EQ "jc/jc-calc.p"
      AND b-jc-calc.company  EQ io-job.company
      AND b-jc-calc.loc      EQ ""
      AND b-jc-calc.code     EQ STRING(io-job.job,"999999999"),
    FIRST job-hdr NO-LOCK
    WHERE job-hdr.company  EQ io-job.company
      AND job-hdr.job      EQ io-job.job
      AND job-hdr.job-no   EQ io-job.job-no
      AND job-hdr.job-no2  EQ io-job.job-no2:

  ACCUM 1 (TOTAL).

  CREATE tt-jh.
  BUFFER-COPY job-hdr TO tt-jh
  ASSIGN
   tt-jh.j-no     = (ACCUM TOTAL 1)
   tt-jh.i-no     = b-jc-calc.code2
   tt-jh.n-on     = b-jc-calc.val[11]
   tt-jh.frm      = b-jc-calc.val[12]
   tt-jh.blank-no = b-jc-calc.val[13].
END.

ELSE
FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company  EQ io-job.company
      AND job-hdr.job      EQ io-job.job
      AND job-hdr.job-no   EQ io-job.job-no
      AND job-hdr.job-no2  EQ io-job.job-no2:

  CREATE tt-jh.
  BUFFER-COPY job-hdr TO tt-jh.
END.

FOR EACH tt-jh:
  ASSIGN
   v-up  = 1
   v-out = 1.
   
  IF AVAIL est THEN DO:
    RUN sys/inc/numup.p (est.company, est.est-no, tt-jh.frm, OUTPUT v-up).

    FIND FIRST ef NO-LOCK
        WHERE ef.company EQ est.company
          AND ef.est-no  EQ est.est-no
          AND ef.form-no EQ tt-jh.frm
        NO-ERROR.
    IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).
  END.
  
  IF tt-jh.n-on NE 0 THEN v-on = tt-jh.n-on.
  
  FOR EACH mch-act
      WHERE mch-act.company   EQ tt-jh.company
        AND mch-act.job       EQ tt-jh.job
        AND mch-act.job-no    EQ tt-jh.job-no
        AND mch-act.job-no2   EQ tt-jh.job-no2
        AND mch-act.frm       EQ tt-jh.frm
        AND (mch-act.blank-no EQ tt-jh.blank-no OR mch-act.blank-no EQ 0),

      FIRST dept NO-LOCK
      WHERE dept.code EQ mch-act.dept
        AND dept.fc   GE v-rs-seq,

      FIRST mach NO-LOCK
      WHERE mach.company EQ mch-act.company
        AND mach.m-code  EQ mch-act.m-code,

      FIRST job-code NO-LOCK
      WHERE job-code.code EQ mch-act.code:

    FIND FIRST w-qty
        WHERE w-qty.s-num  EQ tt-jh.frm
          AND (w-qty.b-num EQ tt-jh.blank-no OR tt-jh.blank-no EQ 0)
          AND w-qty.m-code EQ mch-act.m-code
          AND w-qty.dept   EQ mch-act.dept
          AND w-qty.pass   EQ mch-act.pass
        NO-ERROR.

    IF NOT AVAIL w-qty THEN DO:
      v-on = 1.

      IF INDEX("APB",mach.p-type) LE 0            AND
         (mch-act.dept NE "HS" OR NOT mach.therm) THEN DO:
        FIND FIRST job-mch NO-LOCK
            WHERE job-mch.company  EQ mch-act.company
              AND job-mch.job      EQ mch-act.job
              AND job-mch.job-no   EQ mch-act.job-no
              AND job-mch.job-no2  EQ mch-act.job-no2
              AND job-mch.frm      EQ mch-act.frm
              AND job-mch.blank-no EQ mch-act.blank-no
              AND job-mch.m-code   EQ mch-act.m-code
              AND job-mch.pass     EQ mch-act.pass
            NO-ERROR.

        IF NOT AVAIL job-mch OR job-mch.n-on EQ 0 THEN
        FIND FIRST job-mch NO-LOCK
            WHERE job-mch.company  EQ mch-act.company
              AND job-mch.job      EQ mch-act.job
              AND job-mch.job-no   EQ mch-act.job-no
              AND job-mch.job-no2  EQ mch-act.job-no2
              AND job-mch.frm      EQ mch-act.frm
              AND job-mch.blank-no EQ mch-act.blank-no
              AND job-mch.dept     EQ mch-act.dept
              AND job-mch.pass     EQ mch-act.pass
            NO-ERROR.

        IF NOT AVAIL job-mch OR job-mch.n-on EQ 0 THEN
        FIND FIRST job-mch NO-LOCK
            WHERE job-mch.company  EQ mch-act.company
              AND job-mch.job      EQ mch-act.job
              AND job-mch.job-no   EQ mch-act.job-no
              AND job-mch.job-no2  EQ mch-act.job-no2
              AND job-mch.frm      EQ mch-act.frm
              AND job-mch.dept     EQ mch-act.dept
            NO-ERROR.

        IF AVAIL job-mch AND job-mch.n-out NE 0 THEN
          v-on = tt-jh.n-on * job-mch.n-out.

        ELSE
        IF AVAIL est THEN DO:
          FIND FIRST est-op NO-LOCK
              WHERE est-op.company EQ est.company
                AND est-op.est-no  EQ est.est-no
                AND est-op.s-num   EQ mch-act.frm
                AND (est-op.b-num  EQ mch-act.blank-no OR
                     mch-act.blank-no EQ 0)
                AND est-op.m-code  EQ mch-act.m-code
                AND est-op.op-pass EQ mch-act.pass
                AND est-op.dept    EQ mch-act.dept
                AND est-op.line    LT 500
              NO-ERROR.

          IF AVAIL est-op THEN
            RUN sys/inc/numout.p (RECID(est-op), OUTPUT v-on).

          ELSE v-on = 1.
 
          v-on = (v-up * v-out) / v-on.
        END.
      END.

      CREATE w-qty.
      ASSIGN
       w-qty.s-num   = tt-jh.frm
       w-qty.b-num   = tt-jh.blank-no
       w-qty.p-type  = IF mach.p-type EQ "B" THEN 1 ELSE 0
       w-qty.seq     = dept.fc
       w-qty.pass    = mch-act.pass
       w-qty.m-code  = mch-act.m-code
       w-qty.dept    = mch-act.dept
       w-qty.n-on    = v-on
       w-qty.out     = v-up * v-out.
      
      IF w-qty.b-num EQ 0 THEN w-qty.b-num = 1.
    END.

    IF mch-act.waste LT 0   OR
       mch-act.waste GT 500 THEN mch-act.waste = 0.

    IF job-code.cat EQ "RUN" OR job-code.cat EQ "DT" THEN
      w-qty.fin = w-qty.fin + (mch-act.qty * w-qty.n-on).
    ELSE
    IF job-code.cat EQ "MR" THEN
      w-qty.wst = w-qty.wst + (mch-act.qty * w-qty.n-on).

    /* w-qty.wst = w-qty.wst + (mch-act.waste * w-qty.n-on). */
  END.
END.

ASSIGN
 v-fin = 0
 v-wst = 0.

FOR EACH w-qty
    BREAK BY w-qty.s-num
          BY w-qty.b-num
          BY w-qty.seq
          BY w-qty.dept
          BY w-qty.pass:

  ASSIGN
   v-fin = v-fin + w-qty.fin
   v-wst = v-wst + w-qty.wst.

  IF LAST-OF(w-qty.pass) THEN
    ASSIGN
     w-qty.fin  = v-fin
     w-qty.wst  = v-wst
     v-fin      = 0
     v-wst      = 0.

  ELSE DELETE w-qty.
END.

