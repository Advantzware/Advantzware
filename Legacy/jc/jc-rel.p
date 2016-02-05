/* ----------------------------------------------------- jc/jc-rel.p 7/94 gb */
/* Job Costing - Pending Job Release                                          */
/* -------------------------------------------------------------------------- */

def input parameter job_id as recid                                     no-undo.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var upd         as   logical init yes                               no-undo.
def var v-comm      as   dec                                            no-undo.
def var v-bwt       like item.basis-w                                   no-undo.
def var v-len       like item.s-len                                     no-undo.
def var v-wid       like item.s-wid                                     no-undo.

find job where recid(job) eq job_id.

if not upd then leave.

for each job-mat
    where job-mat.company eq cocode
      and job-mat.job     eq job.job
      and job-mat.all-flg eq no
      and job-mat.qty-all gt 0,

  {jc/jc-rel.i}

  assign
   job-mat.all-flg = yes
   item.q-comm     = item.q-comm + v-comm
   item.q-avail    = item.q-onh + item.q-ono - item.q-comm.

  if last(job-mat.job) and job.stat eq "W" then job.stat = "A".
end.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
