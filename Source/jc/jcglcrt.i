if {2} ne 0 or {3} ne 0 then do:
  find first work-gl
      where work-gl.job     eq job-hdr.job
        and work-gl.job-no  eq job-hdr.job-no
        and work-gl.job-no2 eq job-hdr.job-no2
        and work-gl.actnum  eq {1}
      no-lock no-error.
      
  if not avail work-gl then do:
    create work-gl.
    assign
     work-gl.job      = job-hdr.job
     work-gl.job-no   = job-hdr.job-no
     work-gl.job-no2  = job-hdr.job-no2
     work-gl.actnum   = {1}      
     work-gl.cDesc    = "Job: " + TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job-hdr.job-no, job-hdr.job-no2))).
     IF {4} NE 0 THEN
     ASSIGN
     work-gl.cDesc = work-gl.cDesc + " Cost $" + string({4}) + " / " + {5} .
  end.

  if ({2} eq ? or {3} eq ? or work-gl.actnum eq "") and
     work-gl.err-desc eq ""                         then
    work-gl.err-desc = if work-gl.actnum eq "" then
                            "No GL Acct Number" else v-err-desc.

  assign
   work-gl.credits = work-gl.credits + {2}
   work-gl.debits  = work-gl.debits  + {3}.
end.
