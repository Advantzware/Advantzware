/* ------------------------------------------------- cec/desprntPrem.p 04/97 JLF */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */

DEFINE input parameter v-ef-recid          as   recid.

DEFINE input-output parameter v-lines      as   int.

{sys/inc/VAR.i SHARED}

PUT UNFORMATTED "<P12>" SKIP.
{cec/desprntPrem.i}   /* YSK Print box design on next page if box-line ha~s more than 15 lines */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
