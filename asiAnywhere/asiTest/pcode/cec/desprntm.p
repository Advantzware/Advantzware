/* ------------------------------------------------- cec/desprnt1.p 04/97 JLF */
/* Box Design Print  for MidWest Fibre with 32th                                                         */
/* -------------------------------------------------------------------------- */

def input parameter v-ef-recid          as   recid.

def input-output parameter v-lines      as   int.
{sys/inc/VAR.i SHARED}
DEF VAR k_frac AS DEC INIT 6.25 NO-UNDO.
{sys/inc/f16to32.i}

PUT UNFORMATTED "<P12>" SKIP.
{cec/desprntm.i}   /* 6/22/01  YSK Print box design on next page if box-line ha~s more than 15 lines */

/* end ---------------------------------- copr. 1997  advanced software, inc. */
