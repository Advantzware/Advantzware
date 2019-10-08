
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}


SESSION:SET-WAIT-STATE("general").

assign
 cocode = g_company
 locode = g_loc.

FOR EACH ar-invl
    WHERE ar-invl.company EQ cocode
      AND ar-invl.b-no    NE 0,

    FIRST oe-bolh WHERE oe-bolh.b-no EQ ar-invl.b-no NO-LOCK:

  ar-invl.bol-no = oe-bolh.bol-no.
END.

SESSION:SET-WAIT-STATE("").
