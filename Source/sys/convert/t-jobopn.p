  /* t-jobopn.p  update job.opened and job-hdr.opend - new fields*/
 DISABLE TRIGGERS FOR LOAD OF job-hdr.

 FOR EACH job:
 
   job.opened = LOOKUP(job.stat,"C,Z") EQ 0.

  
  
  FOR EACH job-hdr
      WHERE job-hdr.company EQ job.company
        AND job-hdr.job     EQ job.job
        AND job-hdr.job-no  EQ job.job-no
        AND job-hdr.job-no2 EQ job.job-no2:

    job-hdr.opened = job.opened.
  END.
END.
