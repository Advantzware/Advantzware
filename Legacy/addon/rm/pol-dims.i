
     if (v-len eq 0 or v-wid eq 0 or v-bwt eq 0) then do:
       find first job
           where job.company eq cocode
             and job.job-no  eq po-ordl.job-no
             and job.job-no2 eq po-ordl.job-no2
           no-lock no-error.

       if avail job then do :
         for each job-mat
             where job-mat.company eq cocode
               and job-mat.job     eq job.job
               and job-mat.job-no  eq job.job-no
               and job-mat.job-no2 eq job.job-no2
               and job-mat.i-no    eq po-ordl.i-no
             no-lock
             by job-mat.frm desc:
                  
           if job-mat.frm eq po-ordl.s-num then leave.
         end.
              
         if avail job-mat then
           assign
            v-len = if v-len eq 0 then job-mat.len     else v-len
            v-wid = if v-wid eq 0 then job-mat.wid     else v-wid
            v-bwt = if v-bwt eq 0 then job-mat.basis-w else v-bwt.
       end.

       if v-len eq 0 then v-len = item.s-len.

       if v-wid eq 0 then
         v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

       if v-bwt eq 0 then v-bwt = item.basis-w.
     end.
     
