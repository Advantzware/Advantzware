/* ----------------------------------------------------- jc/jc-dall.p 7/94 gb */
/* Job Costing - Released Job Deallocation Process                            */
/* -------------------------------------------------------------------------- */

def input parameter job_id as recid                                     no-undo.

{sys/inc/var.i shared}

find job where recid(job) eq job_id.

{jc/jc-dall.i}

/* end ---------------------------------- copr. 1993  advanced software, inc. */
