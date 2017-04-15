/* -------------------------------------------------- jc/jc-close.i 10/97 JLF */
/* Job Costing - Close Each Job specified by jc-close.p                       */
/* -------------------------------------------------------------------------- */

        each job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
          and job-hdr.ord-no  ge begin_ord
          and job-hdr.ord-no  le end_ord
        use-index job 
        TRANSACTION:

      {jc/job-clos.i}

      FIND CURRENT reftable NO-LOCK NO-ERROR.

      job-hdr.opened = job.opened.
