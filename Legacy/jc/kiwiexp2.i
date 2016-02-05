
def TEMP-TABLE w-job-mch NO-UNDO like job-mch.

for each job-mch
    where job-mch.company eq cocode
      and job-mch.job     eq job.job
    no-lock:
  
  create w-job-mch.
  buffer-copy job-mch to w-job-mch.
end.
