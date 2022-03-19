/* ********************************************************* cecrep/jobpqp.i  */
/*  N-K = JOBCARC - FACTORY TICKET FOR PQP                                    */
/* ************************************************************************** */
/* Mod: Ticket - 103137 (Format Change for Order No. and Job No). */

    for each job-hdr
        where job-hdr.company               eq cocode
          and job-hdr.ftick-prnt            eq reprint

          AND FILL(" ",9 - LENGTH(TRIM(job-hdr.job-no))) +
	      TRIM(job-hdr.job-no) +
	      STRING(job-hdr.job-no2,"999")  GE fjob-no
	  AND FILL(" ",9 - LENGTH(TRIM(job-hdr.job-no))) +
	      TRIM(job-hdr.job-no) +
	      STRING(job-hdr.job-no2,"999")  LE tjob-no
	  AND job-hdr.job-no2 GE fjob-no2
          AND job-hdr.job-no2 LE tjob-no2,

        first job
        where job.company                   eq cocode
          and job.job                       eq job-hdr.job
          and job.job-no                    eq job-hdr.job-no
          and job.job-no2                   eq job-hdr.job-no2
          and job.stat                      ne "H"
        no-lock,
        
        first est
        where est.company = job.company
          AND est.est-no                    eq job.est-no
          and est.est-type                  gt 4
        no-lock,

        first cust
        where cust.company                  eq cocode
          and cust.cust-no                  eq job-hdr.cust-no
        no-lock,

        first itemfg
        where itemfg.company                eq cocode
          and itemfg.i-no                   eq job-hdr.i-no
        no-lock

        
