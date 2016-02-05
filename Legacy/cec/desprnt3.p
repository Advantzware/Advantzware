/* ------------------------------------------------- cec/desprnt1.p 04/97 JLF */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */

def input parameter v-ef-recid          as   recid.

def input-output parameter v-lines      as   int.
{sys/inc/VAR.i SHARED}

PUT UNFORMATTED "<P12>" SKIP.
{cec/desprnt3.i}   /* 6/22/01  YSK Print box design on next page if box-line ha~s more than 15 lines */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
