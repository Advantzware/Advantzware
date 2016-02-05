
find first w-job-mch no-error.

find first job-mch
    where job-mch.company eq cocode
      and job-mch.job     eq job.job
    no-lock no-error.
choice = not avail w-job-mch and not avail job-mch.

if not choice then
for each job-mch
    where job-mch.company eq cocode
      and job-mch.job     eq job.job
    no-lock:
    
  find first w-job-mch
      where w-job-mch.company eq cocode
        and w-job-mch.job     eq job-mch.job
        and w-job-mch.line    eq job-mch.line
      no-lock no-error.
  
  choice = avail w-job-mch.
  
  if choice then buffer-compare job-mch to w-job-mch save result in choice.

  if not choice then leave.
end.

if not choice then run jc/kiwiexp2.p (recid(job)).
