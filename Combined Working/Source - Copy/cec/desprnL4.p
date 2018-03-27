/* ------------------------------------------------- cec/desprnL4.p 10/09 GDM */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */

def input parameter v-ef-recid          as   recid.

def input-output parameter v-lines      as   int.
{sys/inc/VAR.i SHARED}

PUT UNFORMATTED "<P12>" SKIP.
{cec/desprnL4.i}   

/* end ---------------------------------- copr. 1997  advanced software, inc. */
