/* -------------------------------------------------- jc/kiwiexp1.p 07/01 JLF */
/*                                                                            */
/* Export 1 Job to Kiwi Scheduling Package                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}


find job where recid(job) eq v-recid.

{jc/kiwiexp4.i}


