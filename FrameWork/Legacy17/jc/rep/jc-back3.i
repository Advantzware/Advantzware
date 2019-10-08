/* ----------------------------------------------- jc/rep/jc-back.i 05/99 JLF */
/* Job Backlog by Machine                                                     */
/* -------------------------------------------------------------------------- */

      and job.job-no     ge substr(v-fjob,1,6)
      and job.job-no     le substr(v-tjob,1,6)
      and fill(" ",6 - length(trim(job.job-no))) +
          trim(job.job-no) + string(job.job-no2,"99")
                         ge v-fjob
      and fill(" ",6 - length(trim(job.job-no))) +
          trim(job.job-no) + string(job.job-no2,"99")
                         le v-tjob
    use-index stat-idx no-lock,

    each job-mch
    where job-mch.company eq cocode
      and job-mch.job     eq job.job
      and job-mch.job-no  eq job.job-no
      and job-mch.job-no2 eq job.job-no2
      and can-find(first mach
                   where mach.company eq cocode
                     and mach.loc     eq locode
                     and mach.m-code  eq job-mch.m-code
                     and ((mach.industry eq "1" and v-indus EQ "F")  or
                          (mach.industry eq "2" and v-indus EQ "C")  or
                          (mach.industry eq "") or
                          v-indus eq "B")
                   use-index m-code)
    use-index job no-lock,

    each job-hdr no-lock
    where job-hdr.company   eq cocode
      and job-hdr.job       eq job-mch.job
      and job-hdr.job-no    eq job-mch.job-no
      and job-hdr.job-no2   eq job-mch.job-no2
      and job-hdr.frm       eq job-mch.frm
      and (job-hdr.blank-no eq job-mch.blank-no or
           job-mch.blank-no eq 0)
      AND job-hdr.opened EQ YES 
      AND job-hdr.cust-no GE v-fcust
      AND job-hdr.cust-no LE v-tcust
    use-index job
    
    break by job-mch.job
          by job-mch.job-no
          by job-mch.job-no2
          by job-mch.frm
          by job-mch.blank-no:
  
  ASSIGN v-date = job.start-date.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq job-hdr.ord-no
        and oe-ordl.i-no    eq job-hdr.i-no
        and oe-ordl.job-no  eq job-hdr.job-no
        and oe-ordl.job-no2 eq job-hdr.job-no2
      no-lock no-error.
  if avail oe-ordl  THEN /* only due date */
    v-date = if oe-ordl.req-date  ne ? then oe-ordl.req-date
             ELSE if oe-ordl.prom-date ne ? then oe-ordl.prom-date
             else v-date.

  if v-date ne ? and v-date le v-fdate then do:
    find first est
        where est.company eq job.company
          and est.est-no  eq job.est-no
        no-lock no-error.
    
    FIND FIRST tt-report NO-LOCK WHERE RECID(job-mch) = tt-report.rec-id NO-ERROR.
    IF NOT AVAIL tt-report THEN DO:
        create tt-report.
        assign
           tt-report.term-id = ""
           tt-report.key-01  = string(year(v-date),"9999") +
                               string(month(v-date),"99")  +
                               string(day(v-date),"99")
           tt-report.key-02  = job-hdr.cust-no
           tt-report.key-03  = if avail est                                and 
                               (est.est-type eq 2 or est.est-type eq 6) then
                               "SET" else ""
           tt-report.rec-id  = recid(job-mch).
    END.
  end.
 /*END. first-of job-mch */
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
