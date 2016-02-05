
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR lv-stat LIKE job.stat NO-UNDO.


ASSIGN
 cocode = g_company
 locode = g_loc.

PAUSE 0 BEFORE-HIDE.

FOR EACH job WHERE company EQ cocode:

  DISPLAY "Processing Job#: " +
          TRIM(job.job-no) + "-" +
          STRING(job.job-no2,"99") FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  IF opened NE (INDEX("CZ",stat) LE 0) THEN DO:
    lv-stat = job.stat.

    DO TRANSACTION:
      job.stat = "".
    END.

    DO TRANSACTION:
      job.stat = lv-stat.
    END.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

IF NOT PROGRAM-NAME(2) BEGINS "util/fxopened." THEN
  MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
