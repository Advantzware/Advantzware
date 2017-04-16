/* ------------------------------------------------- cec/desprncap3.p  */
/* Box Design Print    for bigger box image landscape                                                       */
/* -------------------------------------------------------------------------- */

def input parameter v-ef-recid   as   recid.
DEF INPUT PARAMETER v-xeb-rowid  AS ROWID.
DEF INPUT PARAMETER v-coldscr AS CHAR NO-UNDO.
DEF INPUT PARAMETER d2-text AS CHAR FORMAT "X(45)" NO-UNDO.
DEF INPUT PARAMETER d3-text AS CHAR FORMAT "X(45)" NO-UNDO.
DEF INPUT PARAMETER d4-text AS CHAR FORMAT "X(45)" NO-UNDO.
DEF INPUT PARAMETER d5-text AS CHAR FORMAT "X(45)" NO-UNDO.
DEF INPUT PARAMETER pr-text AS CHAR FORMAT "X(45)" NO-UNDO.
def input-output parameter v-lines as int.
.
{sys/inc/VAR.i SHARED}

PUT UNFORMATTED "<P12>" SKIP.

{cec/desprncap3.i}

/* end ---------------------------------- copr. 1997  advanced software, inc. */
