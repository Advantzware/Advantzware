if {2} ne 0 or {3} ne 0 then do:
  find first work-gl{4}
      where work-gl{4}.job     eq job-hdr.job
        and work-gl{4}.job-no  eq job-hdr.job-no
        and work-gl{4}.job-no2 eq job-hdr.job-no2
        and work-gl{4}.actnum  eq {1}
      no-lock no-error.
      
  if not avail work-gl{4} then do:
    create work-gl{4}.
    assign
     work-gl{4}.job      = job-hdr.job
     work-gl{4}.job-no   = job-hdr.job-no
     work-gl{4}.job-no2  = job-hdr.job-no2
     work-gl{4}.actnum   = {1}.
  end.

  if ({2} eq ? or {3} eq ? or work-gl{4}.actnum eq "") and
     work-gl{4}.err-desc eq ""                         then
    work-gl{4}.err-desc = if work-gl{4}.actnum eq "" then
                            "No GL Acct Number" else v-err-desc.

  assign
   work-gl{4}.credits = work-gl{4}.credits + {2}
   work-gl{4}.debits  = work-gl{4}.debits  + {3}.
end.
