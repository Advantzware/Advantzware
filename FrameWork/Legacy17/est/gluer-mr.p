/* ------------------------------------------------- est/gluer-mr.p 08/03 JLF */
/* Set Gluer MR Hours for the non-first occurences of this machine in routing */
/* -------------------------------------------------------------------------- */

DEF PARAM BUFFER io-est-op FOR est-op.

{sys/inc/var.i SHARED}

DEF BUFFER b-est-op FOR est-op.
DEF BUFFER b-eb FOR eb.
DEF BUFFER b-mach FOR mach.

DEF VAR ll-error AS LOG INIT YES NO-UNDO.


IF AVAIL io-est-op THEN
FIND FIRST mach NO-LOCK
    {sys/look/machW.i}
      AND mach.m-code EQ io-est-op.m-code
    NO-ERROR.

IF AVAIL mach THEN DO:
  IF mach.tan-mrp NE 0 THEN DO:
    FIND FIRST eb NO-LOCK
        WHERE eb.company   EQ io-est-op.company
          AND eb.est-no    EQ io-est-op.est-no
          AND eb.form-no   EQ io-est-op.s-num
          AND (eb.blank-no EQ io-est-op.b-num OR io-est-op.b-num EQ 0)
        NO-ERROR.

    IF AVAIL eb THEN
    FOR EACH b-est-op NO-LOCK
        WHERE b-est-op.company EQ io-est-op.company 
          AND b-est-op.est-no  EQ io-est-op.est-no
          AND b-est-op.m-code  EQ io-est-op.m-code
          AND (b-est-op.qty    EQ io-est-op.qty       OR
                                  eb.est-type EQ 2 OR
                                  eb.est-type EQ 3 OR
                                  eb.est-type EQ 4 OR
                                  eb.est-type GE 7)
          AND b-est-op.line    GE (IF io-est-op.line GE 500 THEN 500  ELSE 0)
          AND b-est-op.line    LT (IF io-est-op.line GE 500 THEN 1000 ELSE 500),

        FIRST b-eb NO-LOCK
        WHERE b-eb.company   EQ b-est-op.company
          AND b-eb.est-no    EQ b-est-op.est-no
          AND b-eb.form-no   EQ b-est-op.s-num
          AND (b-eb.blank-no EQ b-est-op.b-num OR b-est-op.b-num EQ 0)
          AND b-eb.style     EQ eb.style
          AND b-eb.len       EQ eb.len
          AND b-eb.wid       EQ eb.wid
          AND b-eb.dep       EQ eb.dep

        BY b-est-op.line:

      IF ROWID(io-est-op) NE ROWID(b-est-op) THEN
        ASSIGN
         io-est-op.op-mr = mach.tan-mrp
         ll-error        = NO.
      LEAVE.
    END.
  END.
END.

IF ll-error THEN RETURN ERROR.

/* end ---------------------------------- copr. 2003  advanced software, inc. */
