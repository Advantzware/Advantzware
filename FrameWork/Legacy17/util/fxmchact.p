
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR ld-qty AS DEC NO-UNDO.
DEF VAR v-parts AS DEC NO-UNDO.
DEF VAR v-dec AS DEC NO-UNDO.


ASSIGN
 cocode = g_company
 locode = g_loc.

PAUSE 0 BEFORE-HIDE.

FOR EACH mch-act WHERE mch-act.company EQ cocode,
    FIRST mach NO-LOCK
    WHERE mach.company EQ mch-act.company
      AND mach.m-code  EQ mch-act.m-code
      AND mach.p-type  EQ "P",
    FIRST job NO-LOCK
    WHERE job.company EQ mch-act.company
      AND job.job     EQ mch-act.job
      AND job.job-no  EQ mch-act.job-no
      AND job.job-no2 EQ mch-act.job-no2
      AND TRIM(job.est-no) NE "",
    FIRST est NO-LOCK
    WHERE est.company EQ job.company
      AND est.est-no  EQ job.est-no
      AND (est.est-type EQ 2 OR est.est-type EQ 6)

    BREAK BY mch-act.job-no
          BY mch-act.job-no2
          BY mch-act.job
          BY mch-act.m-code:

  DISPLAY "Processing Job#/Machine: " +
          TRIM(mch-act.job-no) + "-" +
          STRING(mch-act.job-no2,"99") + "/" +
          TRIM(mch-act.m-code)  FORMAT "x(50)" WITH 1 DOWN.

  v-parts = 0.
  FOR EACH eb NO-LOCK
      WHERE eb.company EQ job.company
        AND eb.est-no  EQ job.est-no
        AND eb.form-no NE 0:
    v-parts = v-parts +
              (IF eb.yld-qty LT 0 THEN (-1 / eb.yld-qty) ELSE eb.yld-qty).
  END.

  IF v-parts GT 0 THEN DO:
    v-dec = mch-act.qty * v-parts.
    {sys/inc/roundup.i v-dec}
    mch-act.qty = v-dec.

    v-dec = mch-act.waste * v-parts.
    {sys/inc/roundup.i v-dec}
    mch-act.waste = v-dec.
  END.
END.

HIDE ALL NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

