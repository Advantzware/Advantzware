/* -------------------------------------------------- rm/rm-addcr.i 02/97 JLF */
/*                                                                            */
/* raw materials posting - create adder for posted board                      */
/*                                                                            */
/* -------------------------------------------------------------------------- */

      for each job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq job.job
            and job-mat.frm     eq rm-rctd.s-num
          use-index seq-idx no-lock,

          first {4}item
          where {4}item.company  eq cocode
            and {4}item.i-no     eq job-mat.rm-i-no
            and {4}item.mat-type eq "A"
            and {4}item.i-code   eq "{1}"
          no-lock:

        /*{rm/rm-autoi.i {3} {3} {4}item}

        {3}.cost = 0.
        
        if rm-rctd.pur-uom eq  job-mat.qty-uom then
          {3}.qty = rm-rctd.qty.

        else
          if {4}item.r-wid eq 0 then
            run rm/convquom.p(rm-rctd.pur-uom,  job-mat.qty-uom,
                             (if job-mat.basis-w  ne 0 then job-mat.basis-w
                              else {4}item.basis-w),
                             (if job-mat.len      ne 0 then job-mat.len
                              else {4}item.s-len),
                             (if job-mat.wid      ne 0 then job-mat.wid
                              else {4}item.s-wid),
                             {4}item.s-dep,
                             rm-rctd.qty,
                             output {3}.qty).
          else
            run rm/convquom.p(rm-rctd.pur-uom,  job-mat.qty-uom,
                             (if job-mat.basis-w  ne 0 then job-mat.basis-w
                              else {4}item.basis-w),
                             job-mat.len,
                             (if job-mat.wid      ne 0 then job-mat.wid
                              else {4}item.r-wid),
                             {4}item.s-dep,
                             rm-rctd.qty,
                             output {3}.qty).*/

/* end ---------------------------------- copr. 1997  advanced software, inc. */
