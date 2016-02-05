
     if (v-len eq 0 or v-wid eq 0 or v-bwt eq 0) then do:
       find first job
           where job.company eq {1}po-ordl.company
             and job.job-no  eq {1}po-ordl.job-no
             and job.job-no2 eq {1}po-ordl.job-no2
           no-lock no-error.

       if avail job then
       for each job-mat
           where job-mat.company eq job.company
             and job-mat.job     eq job.job
             and job-mat.job-no  eq job.job-no
             and job-mat.job-no2 eq job.job-no2 
             and job-mat.i-no    eq {1}po-ordl.i-no
           use-index job no-lock
           break by job-mat.frm desc:
                  
         if last(job-mat.frm) or job-mat.frm eq {1}po-ordl.s-num then do:
           assign
            v-len = if v-len eq 0 then job-mat.len     else v-len
            v-wid = if v-wid eq 0 then job-mat.wid     else v-wid
            v-bwt = if v-bwt eq 0 then job-mat.basis-w else v-bwt.
            
           leave.
         end.
       end.

       if v-len eq 0 then v-len = item.s-len.

       if v-wid eq 0 then
         v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

       if v-bwt eq 0 then v-bwt = item.basis-w.
     end.
     
