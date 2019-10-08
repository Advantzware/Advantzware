/* ---------------------------------------------- jc/rep/jc-back2.i           */
/*                                                                            */
/* -------------------------------------------------------------------------- */

      and job.job-no     ge substr(v-fjob,1,6)
      and job.job-no     le substr(v-tjob,1,6)
      and fill(" ",6 - length(trim(job.job-no))) +
          trim(job.job-no) + string(job.job-no2,"99")
                         ge v-fjob
      and fill(" ",6 - length(trim(job.job-no))) +
          trim(job.job-no) + string(job.job-no2,"99")
                         le v-tjob
    use-index stat-idx NO-LOCK,

    each job-mch
    where job-mch.company eq cocode
      and job-mch.job     eq job.job
      and job-mch.job-no  eq job.job-no
      and job-mch.job-no2 eq job.job-no2
      and job-mch.m-code  ge v-fmach
      and job-mch.m-code  le v-tmach
      AND job-mch.run-complete EQ NO
    use-index job no-lock

    transaction:

  ASSIGN
     v-date = job.start-date
     v-fg-qty = 0.

  for each mch-act FIELDS(CODE qty)
      where mch-act.company  eq cocode
        and mch-act.job      eq job-mch.job
        and mch-act.job-no   eq job-mch.job-no
        and mch-act.job-no2  eq job-mch.job-no2
        and mch-act.frm      eq job-mch.frm
        and mch-act.blank-no eq job-mch.blank-no
        and mch-act.m-code   eq job-mch.m-code
        and mch-act.pass     eq job-mch.pass
      use-index job no-lock,
      
      first job-code
      where job-code.code eq mch-act.code
        and (job-code.cat eq "RUN" or
             job-code.cat eq "DT")
      no-lock:
      
    v-fg-qty = v-fg-qty + mch-act.qty.
  end.

  for each job-hdr
      where job-hdr.company   eq cocode
        and job-hdr.job       eq job-mch.job
        and job-hdr.job-no    eq job-mch.job-no
        and job-hdr.job-no2   eq job-mch.job-no2
        and job-hdr.frm       eq job-mch.frm
        and (job-hdr.blank-no eq job-mch.blank-no or
             job-mch.blank-no eq 0)
      use-index job no-lock:

    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq job-hdr.ord-no
          and oe-ordl.i-no    eq job-hdr.i-no
          and oe-ordl.job-no  eq job-hdr.job-no
          and oe-ordl.job-no2 eq job-hdr.job-no2
        no-lock no-error.
    if avail oe-ordl then
      v-date = if oe-ordl.prom-date ne ? then oe-ordl.prom-date
               else
               if oe-ordl.req-date  ne ? then oe-ordl.req-date else v-date.

    if v-date ne ? and v-date le v-fdate then do:
      find first itemfg
          where itemfg.company eq cocode
            and itemfg.i-no    eq job-hdr.i-no
          no-lock no-error.  

      v-on = 1.
      
      find est where est.company EQ job-hdr.company
                 AND est.est-no  EQ job-hdr.est-no
               no-lock no-error.
      release eb.
      if avail est then
      find first eb
          where eb.company    EQ est.company
            AND eb.est-no     EQ est.est-no
            and eb.form-no    EQ job-hdr.frm
            and (est.est-type EQ 1                                       or
                 est.est-type EQ 5                                       or
                 (eb.blank-no EQ job-hdr.blank-no and
                  (est.est-type EQ 3 OR
                   est.est-type EQ 4 OR
                   est.est-type EQ 8))                                   or
                 (eb.blank-no eq 0                and
                  (est.est-type eq 2 or est.est-type eq 6)))
          no-lock no-error.

      find first mach
          where mach.company eq cocode
            and mach.m-code  eq job-mch.m-code
          no-lock no-error.
            
      if avail eb and avail mach and INDEX("AP",mach.p-type) LE 0 then do:
        find first est-op
            where est-op.company EQ est.company
              AND est-op.est-no  EQ est.est-no
              and est-op.s-num   EQ job-mch.frm
              and est-op.b-num   EQ job-mch.blank-no
              and est-op.m-code  EQ job-mch.m-code
              and est-op.op-pass EQ job-mch.pass
              and est-op.dept    EQ job-mch.dept
              and est-op.line    lt 500
            no-lock no-error.

        if ((avail est-op) and est-op.op-sb)           or
           ((not avail est-op) and mach.p-type ne "B") then do:
        
          if avail est-op then
            run sys/inc/numout.p (recid(est-op), output v-out).

          else v-out = 1.

          v-on = eb.num-up * v-out.
        end.
      end.
      
      create tt-report.
      assign
       tt-report.key-01  = job-mch.m-code
       tt-report.key-02  = string(year(v-date),"9999") +
                           string(month(v-date),"99")  +
                           string(day(v-date),"99")
       tt-report.key-03  = job-hdr.cust-no
       tt-report.key-04  = if avail eb then eb.part-no else
                           if avail itemfg then itemfg.part-no else ""
       tt-report.key-05  = fill(" ",6 - length(trim(job.job-no))) +
                           trim(job.job-no) + string(job.job-no2,"99")
       tt-report.key-06  = if avail est and
                              (est.est-type eq 2 or est.est-type eq 6) then
                             "SET" else ""
       tt-report.key-07  = string(v-on,"9999")
       tt-report.key-08  = string(v-fg-qty * v-on,"->>>>>>>>>9")
       tt-report.rec-id  = recid(job-hdr).
    end.
  end.
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */
