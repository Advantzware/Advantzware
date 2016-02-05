
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR lv-stat LIKE po-ord.stat NO-UNDO.


ASSIGN
 cocode = g_company
 locode = g_loc.

PAUSE 0 BEFORE-HIDE.

FOR EACH po-ord WHERE company EQ cocode:

  DISPLAY "Processing PO#: " +
          TRIM(STRING(po-ord.po-no,">>>>>>>>")) FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  IF opened NE (stat NE "C") THEN DO:
    lv-stat = po-ord.stat.

    DO TRANSACTION:
      po-ord.stat = "".
    END.

    DO TRANSACTION:
      po-ord.stat = lv-stat.
    END.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

IF NOT PROGRAM-NAME(2) BEGINS "util/fxopened." THEN
  MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
