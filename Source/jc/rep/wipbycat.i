/* ---------------------------------------------- jc/rep/wipbycat.i 11/00 JLF */
/* WIP by Product Category                                                    */
/*  Mod: Ticket - 103137 Format Change for Order No. and Job No.       */
/* -------------------------------------------------------------------------- */

      and job.company    eq cocode      
      and FILL(" ", iJobLen - length(trim(job.job-no))) +
          trim(job.job-no) + string(job.job-no2,"999")
                         ge v-fjob
      and FILL(" ", iJobLen - length(trim(job.job-no))) +
          trim(job.job-no) + string(job.job-no2,"999")
                         le v-tjob
     AND job.job-no2 GE int(begin_job-no2)
     AND job.job-no2 LE int(end_job-no2)
    {1} no-lock,
    
    each job-hdr
    where job-hdr.company eq job.company
      and job-hdr.job     eq job.job
      and job-hdr.job-no  eq job.job-no
      and job-hdr.job-no2 eq job.job-no2
    no-lock,

    first itemfg
    where itemfg.company eq job-hdr.company
      and itemfg.i-no    eq job-hdr.i-no
      and itemfg.procat  ge v-fcat
      and itemfg.procat  le v-tcat
    no-lock

    transaction:

  find first est where est.company EQ job.company
                   AND est.est-no  EQ job.est-no
                  no-lock no-error.

  if v-indus ne "B" then
    if avail est then
      if (v-indus eq "F" and est.est-type ge 5) or
         (v-indus eq "C" and est.est-type le 4) then next.

  create tt-report.
  assign
   tt-report.key-01  = itemfg.procat
   tt-report.key-02  = job-hdr.cust-no
   tt-report.key-03  = job-hdr.i-no
   tt-report.key-04  = itemfg.part-no
   tt-report.key-05  = FILL(" ", iJobLen - length(trim(job.job-no))) +
                       trim(job.job-no) + "-" + string(job.job-no2,"99")
   tt-report.key-06  = if avail est                                and
                          (est.est-type eq 2 or est.est-type eq 6) then
                         "SET" else ""
   tt-report.rec-id  = recid(job-hdr).
end.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
