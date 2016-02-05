/* -------------------------------------------- sys/look/item-job.w 02/01 JLF */
/* -------------------------------------------------------------------------- */

where (job-mat.company eq cocode
  and  job-mat.rm-i-no eq item.i-no
  and  job-mat.all-flg eq yes
  and  can-find(first job where job.company          eq cocode
                            and job.job              eq job-mat.job
                            and job.job-no           eq job-mat.job-no
                            and job.job-no2          eq job-mat.job-no2
                            and job.opened)
                               
/* end ---------------------------------- copr. 2001  advanced software, inc. */
