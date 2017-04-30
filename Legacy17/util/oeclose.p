
{custom/globdefs.i}

{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

{oe/closchk.i NEW}


SESSION:SET-WAIT-STATE("general").

FOR EACH oe-ord
    WHERE oe-ord.company EQ cocode
      AND oe-ord.opened  EQ YES
    NO-LOCK:

  STATUS DEFAULT "Processing Order#: " +
                 TRIM(STRING(oe-ord.ord-no,">>>>>>>>>>")).

  FOR EACH w-ord:
    DELETE w-ord.
  END.

  RUN oe/closchk.p (oe-ord.ord-no).

  FIND FIRST w-ord WHERE w-ord.ord-no EQ oe-ord.ord-no NO-ERROR.
  IF AVAIL w-ord THEN RUN oe/close.p (RECID(oe-ord), YES).

  STATUS DEFAULT "".
END.

SESSION:SET-WAIT-STATE("").
