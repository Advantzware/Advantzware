/* ---------------------------------------------- jc/rep/job-sumi.p 05/99 JLF */
/* Job Summary Report                                                         */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}

{jc/rep/job-sum.i}

DEF BUFFER b-wi FOR work-item.

def var v-qty as int.
def var v-uom like oe-ordl.pr-uom.

DEF TEMP-TABLE tt-ordm-amt NO-UNDO
    FIELD company LIKE oe-ordm.company
    FIELD ord-no  LIKE oe-ordm.ord-no
    FIELD LINE    LIKE oe-ordm.LINE
    FIELD charge  LIKE oe-ordm.charge
    FIELD bill    LIKE oe-ordm.bill
    FIELD amt     LIKE oe-ordm.amt.

find job where recid(job) eq v-recid no-lock.

find first est
    where est.company eq job.company
      AND est.est-no  EQ job.est-no
    no-lock.
 
{jc/rep/job-sumi.i}

/* end ---------------------------------- copr. 1999  advanced software, inc. */
