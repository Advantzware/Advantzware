
DEF INPUT PARAM ip-frm LIKE job-mch.frm NO-UNDO.
DEF INPUT PARAM ip-blk LIKE job-mch.blank-no NO-UNDO.

DEF PARAM BUFFER io-job FOR job.
DEF PARAM BUFFER io-mach FOR mach.
DEF PARAM BUFFER io-job-mch FOR job-mch.

{sys/inc/var.i SHARED}

DEF BUFFER b-mach FOR mach.

DEF VAR v-est-type LIKE est.est-type NO-UNDO.
DEF VAR v-assembled AS LOG NO-UNDO.
DEF VAR ll-set-mach AS LOG NO-UNDO.


RELEASE io-job-mch.

IF AVAIL io-job AND AVAIL io-mach THEN DO:
  cocode = io-job.company.

  DO TRANSACTION:
    {sys/inc/autopdc.i}
  END.

  FOR EACH io-job-mch NO-LOCK
      WHERE io-job-mch.company   EQ io-job.company
        AND io-job-mch.job       EQ io-job.job
        AND io-job-mch.job-no    EQ io-job.job-no
        AND io-job-mch.job-no2   EQ io-job.job-no2
        AND (io-job-mch.frm      EQ ip-frm OR v-est-type EQ 2)
        AND CAN-DO(autopdc,io-job-mch.m-code)
        AND autopdc              NE "*"
      USE-INDEX line-idx,

      FIRST b-mach NO-LOCK
      WHERE b-mach.company EQ io-job-mch.company
        AND b-mach.m-code  EQ io-job-mch.m-code
        AND ((NOT CAN-DO("A,P",b-mach.p-type) AND NOT ll-set-mach) OR
             (CAN-DO("A,P",b-mach.p-type)     AND ll-set-mach))

      BY io-job-mch.line DESC:
    LEAVE.
  END.

  IF AVAIL io-job-mch AND io-job-mch.blank-no NE 0 THEN
  FOR EACH io-job-mch NO-LOCK
      WHERE io-job-mch.company   EQ io-job.company
        AND io-job-mch.job       EQ io-job.job
        AND io-job-mch.job-no    EQ io-job.job-no
        AND io-job-mch.job-no2   EQ io-job.job-no2
        AND (io-job-mch.frm      EQ ip-frm OR v-est-type EQ 2)
        AND CAN-DO(autopdc,io-job-mch.m-code)
        AND autopdc              NE "*"
        AND (io-job-mch.blank-no EQ ip-blk OR
             io-mach.p-type      NE "B"    OR
             v-est-type          EQ 1)
      USE-INDEX line-idx,

      FIRST b-mach NO-LOCK
      WHERE b-mach.company EQ io-job-mch.company
        AND b-mach.m-code  EQ io-job-mch.m-code
        AND ((NOT CAN-DO("A,P",b-mach.p-type) AND NOT ll-set-mach) OR
             (CAN-DO("A,P",b-mach.p-type)     AND ll-set-mach))

      BY io-job-mch.line DESC:
    LEAVE.
  END.
END.
