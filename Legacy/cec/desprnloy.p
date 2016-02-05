/* ------------------------------------------------- cec/desprncap.p  */
/* Box Design Print    for bigger box image landscape                                                       */
/* -------------------------------------------------------------------------- */

def input parameter v-ef-recid          as   recid.

def input-output parameter v-lines      as   int.
{sys/inc/VAR.i SHARED}

PUT UNFORMATTED "<P12>" SKIP.

{cec/desprnloy.i}

/* end ---------------------------------- copr. 1997  advanced software, inc. */
