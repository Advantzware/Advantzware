/* fg/chkfgloc.p - Create itemfg-loc if missing */
DEF INPUT PARAMETER ip-i-no AS CHAR NO-UNDO.
DEF INPUT PARAMETER ip-loc AS CHAR NO-UNDO.

/* Local Variable Definitions ---                                       */
{custom/globdefs.i}

{sys/inc/VAR.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.
IF ip-loc EQ "" THEN
    ip-loc = locode.
FIND FIRST itemfg-loc WHERE itemfg-loc.company EQ cocode
                        AND itemfg-loc.i-no EQ ip-i-no
                        AND itemfg-loc.loc  EQ ip-loc
                      NO-LOCK NO-ERROR.
IF NOT AVAIL itemfg-loc AND ip-loc GT "" THEN DO:

    CREATE itemfg-loc.
    ASSIGN itemfg-loc.company = cocode
           itemfg-loc.i-no    = ip-i-no
           itemfg-loc.loc     = ip-loc.

END.
