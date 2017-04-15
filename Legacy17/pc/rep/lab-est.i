find first work-rep where work-rep.job-no   = mch-act.job-no and
                          work-rep.job-no2  = mch-act.job-no2 and
                          work-rep.form-no  = mch-act.frm and
                          work-rep.blank-no = mch-act.blank-no and
                          work-rep.{1}      = {2} and
                          work-rep.m-code   = mch-act.m-code and
                          work-rep.pass     = mch-act.pass
                          no-error.
if not available work-rep then
do:
   create work-rep.
   assign work-rep.{1}      = {2} 
          work-rep.m-code   = mch-act.m-code
          work-rep.job      = mch-act.job
          work-rep.job-no   = mch-act.job-no
          work-rep.job-no2  = mch-act.job-no2
          work-rep.form-no  = mch-act.frm
          work-rep.blank-no = mch-act.blank-no
          work-rep.pass     = mch-act.pass.
end.
find job-code where job-code.code    = mch-act.code
                    no-lock no-error.
if not available job-code then next.
if job-code.cat = "RUN" then
   assign
    work-rep.r-act-hrs    = work-rep.r-act-hrs    + mch-act.hours
    work-rep.qty-prod     = work-rep.qty-prod +
                            IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.
else if job-code.cat = "MR" then
   work-rep.m-act-hrs    = work-rep.m-act-hrs    + mch-act.hours.
else if job-code.cat = "DT" then
   work-rep.dt-chg-hrs   = work-rep.dt-chg-hrs   + mch-act.hours.
else
   work-rep.dt-nochg-hrs = work-rep.dt-nochg-hrs + mch-act.hours.
