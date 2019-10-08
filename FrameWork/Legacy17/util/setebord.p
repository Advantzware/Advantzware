
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}


ASSIGN
 cocode = g_company
 locode = g_loc.

PAUSE 0 BEFORE-HIDE.

FOR EACH eb
    WHERE eb.company EQ cocode
      AND eb.ord-no  EQ 0,
    FIRST est NO-LOCK
    WHERE est.company EQ eb.company
      AND est.est-no  EQ eb.est-no
      AND est.ord-no  NE 0
    BREAK BY eb.est-no:

  DISPLAY "Processing Est#: " +
          TRIM(eb.est-no) FORMAT "x(50)" WITH 1 DOWN.

  eb.ord-no = est.ord-no.
END.

HIDE ALL NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

