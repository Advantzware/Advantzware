for each pc-prdd
    where pc-prdd.company eq "001"
      and pc-prdd.m-code  ge ""
      and pc-prdd.m-code  le "ZZZ"
      and pc-prdd.op-date ge 01/01/04
      and pc-prdd.op-date le 12/31/04
      and pc-prdd.shift   ge 1
      and pc-prdd.shift   le 99
      and pc-prdd.job-no  EQ "  5937"
    /*
      and fill(" ",6 - length(trim(pc-prdd.job-no))) +
          trim(pc-prdd.job-no) + string(int(pc-prdd.job-no2),"99")
                          ge v-job-no[1]
      and fill(" ",6 - length(trim(pc-prdd.job-no))) +
          trim(pc-prdd.job-no) + string(int(pc-prdd.job-no2),"99")
                          le v-job-no[2]
      and ((pc-prdd.stopp - pc-prdd.start
                          ne 0) or
           (pc-prdd.qty   ne 0) or
           (pc-prdd.waste ne 0)) */
    no-lock,
          
    first mach
    where (mach.company = pc-prdd.company and
       mach.loc     = "main")
      and mach.m-code eq pc-prdd.m-code
    no-lock,

    first job
    where job.company eq pc-prdd.company
      and job.job     eq pc-prdd.job
      and job.job-no  eq pc-prdd.job-no
      and job.job-no2 eq pc-prdd.job-no2
    NO-LOCK:

    DISP pc-prdd.m-code pc-prdd.CODE pc-prdd.dept pc-prdd.START.
