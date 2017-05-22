/* ---------------------------------------------- rm/rep/inkbymchN.i 12/00 JLF */
/* Ink by Machine                                                             */
/* -------------------------------------------------------------------------- */

      and job-hdr.company eq cocode
      and job-hdr.job-no  ge substr(v-fjob,1,6)
      and job-hdr.job-no  le substr(v-tjob,1,6)
      and fill(" ",6 - length(trim(job-hdr.job-no))) +
          trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99")
                          ge v-fjob
      and fill(" ",6 - length(trim(job-hdr.job-no))) +
          trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99")
                          le v-tjob
    use-index opened no-lock,
    
    first job
    where job.company eq cocode
      and job.job     eq job-hdr.job
      and job.job-no  eq job-hdr.job-no
      and job.job-no2 eq job-hdr.job-no2
    use-index job no-lock:

  find first est
      where est.company eq job.company
        and est.est-no  eq job.est-no
      no-lock no-error.  

  if v-indus ne "B" then
    if avail est then
      if (v-indus eq "F" and est.est-type ge 5) or
         (v-indus eq "C" and est.est-type le 4) then next.
  
  v-pct = 1.
  
  if avail est and est.est-type eq 3 then do:
    v-qty = 0.
    
    for each b-jh
        where b-jh.company eq job-hdr.company
          and b-jh.job     eq job-hdr.job
          and b-jh.job-no  eq job-hdr.job-no
          and b-jh.job-no2 eq job-hdr.job-no2
        no-lock:
        
      v-qty = v-qty + b-jh.qty.
    end.
    
    v-pct = job-hdr.qty / v-qty.
  end.
  
  if not avail est or est.est-type eq 4 or est.est-type eq 8 then
    v-pct = job-hdr.sq-in / 100.
  
  assign
   v-set  = if avail est and (est.est-type eq 2 or est.est-type eq 6) then
              "SET" else ""
   v-date = job.start-date.
  
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
  
  if v-date ne ? and v-date ge v-fdat and v-date le v-tdat then
  for each job-mch
      where job-mch.company   eq cocode
        and job-mch.job       eq job-hdr.job
        and job-mch.job-no    eq job-hdr.job-no
        and job-mch.job-no2   eq job-hdr.job-no2
        and (job-mch.frm      eq job-hdr.frm or v-set eq "SET")
        and (job-mch.blank-no eq job-hdr.blank-no or job-mch.blank-no eq 0)
        and job-mch.m-code    ge v-fmch
        and job-mch.m-code    le v-tmch
        and job-mch.dept      eq "PR"
      no-lock,
      
      first mach
      where mach.company eq cocode
        and mach.loc     eq locode
        and mach.m-code  eq job-mch.m-code
      no-lock,
     
      each job-mat
      where job-mat.company eq cocode
        and job-mat.job     eq job-mch.job
        and job-mat.job-no  eq job-mch.job-no
        and job-mat.job-no2 eq job-mch.job-no2
        and job-mat.frm     eq job-mch.frm
        and can-find(first item where item.company   eq cocode
                                  and item.i-no      eq job-mat.i-no
                                  and (item.mat-type eq "I" or
                                       item.mat-type eq "V"))
      use-index seq-idx no-lock,
      first item no-lock
        where item.company   eq cocode
        and item.i-no      eq job-mat.rm-i-no
          
      
      transaction:
      
    assign
     v-cov  = 0
     v-skip = avail est.
    
    if v-skip then
    for each eb of est
        where (eb.form-no   eq job-mch.frm or v-set eq "SET")
          and (eb.blank-no eq job-hdr.blank-no or v-set eq "SET")
        no-lock:
        
      IF eb.est-type LE 4 THEN
      do i = 1 to 12:
        if eb.i-code2[i] eq job-mat.i-no and eb.i-ps2[i] eq job-mat.pass then do:
          assign
           v-skip = no
           v-cov  = eb.i-%2[i] * v-pct.
           
          leave.
        end.  
      end.

      ELSE
      do i = 1 to 10:
        if eb.i-code[i] eq job-mat.i-no and eb.i-ps[i] eq job-mat.pass then do:
          assign
           v-skip = no
           v-cov  = eb.i-%[i] * v-pct.
           
          leave.
        end.  
      end.
    end.
    
    if v-skip then next.

    FIND FIRST tt-report WHERE tt-report.key-03 EQ string(job-mch.frm,"9999999999")
        AND tt-report.key-02 EQ (string(year(v-date),"9999") +
                                 string(month(v-date),"99") + 
                                 string(day(v-date),"99") +
                                 fill(" ",6 - length(trim(job.job-no))) +
                                 trim(job.job-no) + "-" + string(job.job-no2,"99")) 
         NO-LOCK NO-ERROR.

    IF NOT AVAIL tt-report THEN do:
        create tt-report.
            assign
             tt-report.term-id = ""
             tt-report.key-01  = job-mch.m-code                  
             tt-report.key-02  = string(year(v-date),"9999") +
                                 string(month(v-date),"99") + 
                                 string(day(v-date),"99") +
                                 fill(" ",6 - length(trim(job.job-no))) +
                                 trim(job.job-no) + "-" + string(job.job-no2,"99")
             tt-report.key-03  = string(job-mch.frm,"9999999999")
             tt-report.key-04  = string(9999999999 - job-mch.pass,"9999999999")
             tt-report.key-05  = job-mat.i-no
             tt-report.key-06  = string(v-cov,"999.999999")
             tt-report.key-07  = string(job-mch.run-qty,"9999999999")
             tt-report.key-08  = v-set
             tt-report.rec-id  = recid(job-mat)
             tt-report.ink-code[1] = job-mat.i-no
             tt-report.rm-name[1] = item.i-name
             tt-report.rm-dscr[1] = item.i-dscr
             tt-report.qty[1] = string(job-mat.qty,">>,>>9")
             k = 2 .
    END.
    ELSE DO:
        IF k GT 15 THEN NEXT .
        tt-report.ink-code[k] = job-mat.i-no .
        tt-report.rm-name[k] = item.i-name.
        tt-report.rm-dscr[k] = item.i-dscr .
        tt-report.qty[k] = string(job-mat.qty,">>,>>9").
        k = k + 1 . 
    END.

  end.
end.

/* end ---------------------------------- copr. 2000  advanced software, inc. */
