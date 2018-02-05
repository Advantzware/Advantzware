/* ------------------------------------------------- cec/desprnL2.p  */
/* Box Design Print    for bigger box image landscape                                                       */
/* -------------------------------------------------------------------------- */

def input parameter v-ef-recid          as   recid.

def input-output parameter v-lines      as   int.
{sys/inc/VAR.i SHARED}

PUT UNFORMATTED "<P12>" SKIP.
{cec/desprnL2.i}   /* 6/22/01  YSK Print box design on next page if box-line ha~s more than 15 lines */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
