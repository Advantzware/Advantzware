/* ------------------------------------------------------ jc/jc-mch.w 1/94 rd */
/* Job Costing - lines 'where' statement                                      */
/* -------------------------------------------------------------------------- */

use-index line-idx
where (job-mch.company eq cocode
  and  job-mch.job     eq job.job
  and  job-mch.job-no  eq job.job-no
  and  job-mch.job-no2 eq job.job-no2)


/* end ---------------------------------- corp. 1994  advanced software, inc. */
