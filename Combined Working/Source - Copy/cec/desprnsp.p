/* -------------------------------------------- */
/* cec/despntpa.p  from cec/desprntL.p  Box Design Print for Pacific Landscape    */
/* -------------------------------------------------------------------------- */

def input parameter v-ef-recid          as   recid.

def input-output parameter v-lines      as   int.
{sys/inc/VAR.i SHARED}

PUT UNFORMATTED "<P12>" SKIP.
{cec/desprnsp.i}

/* end ---------------------------------- copr. 1997  advanced software, inc. */
