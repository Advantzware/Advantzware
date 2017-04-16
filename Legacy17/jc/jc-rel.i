
    first item
    where item.company eq cocode
      and item.i-no    eq job-mat.i-no
      and item.i-code  eq "R"

    break by job-mat.job:

  if job-mat.qty-uom eq item.cons-uom then
    v-comm = job-mat.qty-all.

  else do:
    assign
     v-bwt = job-mat.basis-w
     v-len = job-mat.len
     v-wid = job-mat.wid.

    if v-len eq 0 then v-len = item.s-len.

    if v-wid eq 0 then
      v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.

    if v-bwt eq 0 then v-bwt = item.basis-w.

    run sys/ref/convquom.p(job-mat.qty-uom, item.cons-uom,
                           v-bwt, v-len, v-wid, item.s-dep,
                           job-mat.qty-all, output v-comm).
  end.
  
