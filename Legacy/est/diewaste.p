
DEF PARAM BUFFER io-est-op FOR est-op.

{sys/inc/var.i SHARED}

DEF BUFFER b-est-op FOR est-op.
DEF BUFFER b-mach FOR mach.
DEF BUFFER mach-plain-jobs FOR reftable.


IF AVAIL io-est-op THEN
FIND FIRST mach NO-LOCK
    {sys/look/machW.i}
      AND mach.m-code EQ io-est-op.m-code
    NO-ERROR.

IF AVAIL mach THEN DO:
  FIND FIRST ef NO-LOCK
      WHERE ef.company EQ io-est-op.company
        AND ef.est-no  EQ io-est-op.est-no
        AND ef.form-no EQ io-est-op.s-num
      NO-ERROR.

  IF AVAIL ef                                                                           AND
     ef.f-col + ef.f-coat GT 0                                                          AND
     mach.plain-job = TRUE AND
     
     CAN-FIND(FIRST b-est-op
              WHERE b-est-op.company EQ io-est-op.company 
                AND b-est-op.est-no  EQ io-est-op.est-no
                AND b-est-op.s-num   EQ io-est-op.s-num
                AND (CAN-DO("PR,CT",b-est-op.dept) OR
                     CAN-FIND(FIRST b-mach
                              WHERE b-mach.company EQ b-est-op.company
                                AND b-mach.m-code  EQ b-est-op.m-code
                                AND (CAN-DO("PR,CT",b-mach.dept[1]) OR
                                     CAN-DO("PR,CT",b-mach.dept[2]) OR
                                     CAN-DO("PR,CT",b-mach.dept[3]) OR
                                     CAN-DO("PR,CT",b-mach.dept[4]))))
                AND (b-est-op.qty    EQ io-est-op.qty       OR
                                        ef.est-type EQ 2 OR
                                        ef.est-type EQ 3 OR
                                        ef.est-type EQ 4 OR
                                        ef.est-type GE 7)
                AND b-est-op.line    GE (IF io-est-op.line GE 500 THEN 500  ELSE 0)
                AND b-est-op.line    LT (IF io-est-op.line GE 500 THEN 1000 ELSE 500)) THEN
    io-est-op.op-waste = 0.
END.

/* end ---------------------------------- copr. 2003  advanced software, inc. */
