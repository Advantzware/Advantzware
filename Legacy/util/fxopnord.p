
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR lv-stat LIKE oe-ord.stat NO-UNDO.


ASSIGN
 cocode = g_company
 locode = g_loc.

PAUSE 0 BEFORE-HIDE.

FOR EACH oe-ord WHERE company EQ cocode:

  DISPLAY "Processing Order#: " +
          TRIM(STRING(oe-ord.ord-no,">>>>>>>>")) FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  IF opened NE (INDEX("CDZ",stat) LE 0) THEN DO:
    lv-stat = oe-ord.stat.

    DO TRANSACTION:
      oe-ord.stat = "".
    END.

    DO TRANSACTION:
      oe-ord.stat = lv-stat.
    END.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

IF NOT PROGRAM-NAME(2) BEGINS "util/fxopened." THEN
  MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

