FOR EACH mch-act .
    FIND FIRST mach WHERE mach.m-code = mch-act.m-code NO-LOCK NO-ERROR.
    IF AVAIL mach THEN DO:
       ASSIGN mch-act.crew = 1.
       find job-code where job-code.code = mch-act.CODE no-lock no-error.
       if AVAIL job-code AND job-code.cat = "MR" then
             mch-act.rate[1] = mach.mr-rate.
       ELSE if AVAIL job-code AND job-code.cat = "RUN" then
             mch-act.rate[1] = mach.run-rate.
       
    END.

END.
