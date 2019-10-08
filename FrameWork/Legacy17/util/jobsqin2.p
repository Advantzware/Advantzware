
def input parameter v-recid as recid no-undo.

def buffer xjob-hdr for job-hdr.

def var v-tot-sqin as dec no-undo.


find job where recid(job) eq v-recid no-lock no-error.

if avail job then
for each job-hdr
    where job-hdr.company eq job.company
      and job-hdr.job     eq job.job
      and job-hdr.job-no  eq job.job-no
      and job-hdr.job-no2 eq job.job-no2
    break by job-hdr.frm
    
    transaction:
      
  if first-of(job-hdr.frm) then v-tot-sqin = 0.
    
  if job-hdr.n-on eq 0 then job-hdr.n-on = 1.

  find first itemfg
      where itemfg.company eq job.company
        and itemfg.i-no    eq job-hdr.i-no
      no-lock no-error.
        
  if avail itemfg then
    v-tot-sqin = v-tot-sqin + (itemfg.t-sqin * job-hdr.n-on).
      
  if last-of(job-hdr.frm) then
  for each xjob-hdr
      where xjob-hdr.company eq job-hdr.company
        and xjob-hdr.job     eq job-hdr.job
        and xjob-hdr.job-no  eq job-hdr.job-no
        and xjob-hdr.job-no2 eq job-hdr.job-no2
        and xjob-hdr.frm     eq job-hdr.frm:
            
     find first itemfg
         where itemfg.company eq xjob-hdr.company
           and itemfg.i-no    eq xjob-hdr.i-no
         no-lock no-error.

     xjob-hdr.sq-in = (if avail itemfg then
                        (itemfg.t-sqin * xjob-hdr.n-on) / v-tot-sqin
                       else 0) * 100.
  end.
end.
