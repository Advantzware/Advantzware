
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR lv-stat LIKE po-ord.stat NO-UNDO.


ASSIGN
 cocode = g_company
 locode = g_loc.

PAUSE 0 BEFORE-HIDE.

FOR EACH oe-ordl WHERE company EQ cocode:

  DISPLAY "Processing Order#: " +
          TRIM(STRING(oe-ordl.ord-no,">>>>>>>>")) FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  IF NOT CAN-FIND(FIRST oe-ord OF oe-ordl) THEN DELETE oe-ordl.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.
