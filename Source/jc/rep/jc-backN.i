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
      and job-mch.m-code  ge v-fmach
      and job-mch.m-code  le v-tmach
      and can-find(first mach
                   where mach.company eq cocode
                     and mach.loc     eq locode
                     and mach.m-code  eq job-mch.m-code
                     and ((mach.industry eq "1" and index("BF",v-indus) gt 0 /*v-indus eq "F"*/)  or
                          (mach.industry eq "2" and index("BC",v-indus) gt 0 /*v-indus eq "C"*/)  or
                          (mach.industry eq ""  and v-indus eq "B" /*index("BFC",v-indus) gt 0*/) or
                          v-indus eq "A")
                   use-index m-code)
      and not can-find(first mch-act
                       where mch-act.company  eq cocode
                         and mch-act.job      eq job-mch.job
                         and mch-act.job-no   eq job-mch.job-no
                         and mch-act.job-no2  eq job-mch.job-no2
                         and mch-act.frm      eq job-mch.frm
                         and mch-act.blank-no eq job-mch.blank-no
                         and mch-act.m-code   eq job-mch.m-code
                         and mch-act.pass     eq job-mch.pass
                         and mch-act.complete eq yes
                       use-index job)
    use-index job no-lock,

    each job-hdr no-lock
    where job-hdr.company   eq cocode
      and job-hdr.job       eq job-mch.job
      and job-hdr.job-no    eq job-mch.job-no
      and job-hdr.job-no2   eq job-mch.job-no2
      and job-hdr.frm       eq job-mch.frm
      and (job-hdr.blank-no eq job-mch.blank-no or
           job-mch.blank-no eq 0)
    use-index job
    
    break by job-mch.job
          by job-mch.job-no
          by job-mch.job-no2
          by job-mch.frm
          by job-mch.blank-no
          by job-mch.m-code:

 IF FIRST-OF(job-mch.m-code) THEN DO:
  v-date = job.due-date.

  find first oe-ordl
      where oe-ordl.company eq cocode
        and oe-ordl.ord-no  eq job-hdr.ord-no
        and oe-ordl.i-no    eq job-hdr.i-no
        and oe-ordl.job-no  eq job-hdr.job-no
        and oe-ordl.job-no2 eq job-hdr.job-no2
      no-lock no-error.
  if avail oe-ordl  then
    v-due-date = if oe-ordl.req-date  ne ? then oe-ordl.req-date else v-date.

  if (v-date ne ? and v-date le v-fdate) OR (v-due-date NE ? AND v-due-date LE v-fdate) OR (v-date eq ?) then do:
    find first est
        where est.company eq job.company
          and est.est-no  eq job.est-no
        no-lock no-error.

    create tt-report.
    assign
     tt-report.term-id = ""
     tt-report.key-01  = job-mch.m-code
     tt-report.key-02  = string(year(v-date),"9999") +
                         string(month(v-date),"99")  +
                         string(day(v-date),"99")
     tt-report.key-03  = job-hdr.cust-no
     tt-report.key-04  = if avail est                                and 
                            (est.est-type eq 2 or est.est-type eq 6) then
                           "SET" else ""
     tt-report.key-05  = string(year(v-due-date),"9999") +
                         string(month(v-due-date),"99")  +
                         string(day(v-due-date),"99")
     tt-report.rec-id  = recid(job-hdr)
                               .
  end.
 END.
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
