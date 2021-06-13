/* -------------------------------------------------- jc/jc-close.i 10/97 JLF */
/* Job Costing - Close Each Job specified by jc-close.p                       */
/* -------------------------------------------------------------------------- */

/* find original buffer for comparison of change later */
BUFFER-COPY job TO old-job.
FOR EACH job-hdr EXCLUSIVE-LOCK
    WHERE job-hdr.company EQ cocode
      AND job-hdr.job     EQ job.job
      AND job-hdr.job-no  EQ job.job-no
      AND job-hdr.job-no2 EQ job.job-no2
      AND job-hdr.ord-no  GE begin_ord
      AND job-hdr.ord-no  LE end_ord
    USE-INDEX job 
    TRANSACTION:
    /* find original buffer for comparison of change later */
    BUFFER-COPY job-hdr TO old-job-hdr.
    {jc/job-clos.i}
    FIND CURRENT reftable NO-LOCK NO-ERROR.
    job-hdr.opened = job.opened.
    /* run procedure to compare original buffer to current changes */
    RUN pAuditjob-hdr.
END.
/* run procedure to compare original buffer to current changes */
RUN pAuditjob.
