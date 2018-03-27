
/* no standard cost*/
FOR EACH job-mat NO-LOCK
      WHERE job-mat.company EQ job.company
        AND job-mat.job     EQ job.job
        AND job-mat.job-no  EQ job.job-no
        AND job-mat.job-no2 EQ job.job-no2,
    first item
        where item.company eq job-mat.company
          and item.i-no    eq job-mat.rm-i-no
          AND ITEM.mat-type = "B"
        NO-LOCK:
      
    v-pct = 1.
    FIND FIRST tt-mat WHERE tt-mat.job-no = job-mat.job-no
        AND tt-mat.job-no2 = job-mat.job-no2
        AND tt-mat.form-no = job-mat.frm
        AND tt-mat.blank-no = job-mat.blank-no
        AND tt-mat.rm-i-no = job-mat.rm-i-no
        NO-LOCK NO-ERROR.
    IF NOT AVAIL tt-mat THEN create tt-mat.
    assign
     tt-mat.job-no = job-mat.job-no
     tt-mat.job-no2 = job-mat.job-no2
     tt-mat.form-no  = job-mat.frm
     tt-mat.blank-no = job-mat.blank-no
     tt-mat.rm-i-no  = job-mat.rm-i-no
     tt-mat.basis-w  = job-mat.basis-w
     tt-mat.len      = job-mat.len
     tt-mat.wid      = job-mat.wid
     tt-mat.cst-uom  = job-mat.sc-uom.

    IF TRUE /* job-mat.j-no EQ 0  */ THEN DO:
      assign
       tt-mat.qty-std = job-mat.qty * v-pct
       v-cost      = job-mat.std-cost.

      IF job-mat.sc-uom NE job-mat.qty-uom THEN
      if item.r-wid eq 0 then do:
        run sys/ref/convcuom.p(job-mat.sc-uom,
                               job-mat.qty-uom,
                               (if job-mat.basis-w  ne 0 then job-mat.basis-w
                                else item.basis-w),
                               (if job-mat.len      ne 0 then job-mat.len
                                else item.s-len),
                               (if job-mat.wid      ne 0 then job-mat.wid
                                else item.s-wid),
                               item.s-dep, 
                               job-mat.std-cost,
                               output v-cost).
      end.

      else do:
        run sys/ref/convcuom.p(job-mat.sc-uom,
                               job-mat.qty-uom,
                               (if job-mat.basis-w  ne 0 then job-mat.basis-w
                                else item.basis-w),
                               job-mat.len,
                               (if job-mat.wid      ne 0 then job-mat.wid
                                else item.r-wid),
                               item.s-dep, 
                               job-mat.std-cost,
                               output v-cost).
      end.
      tt-mat.cst-std = job-mat.qty * v-cost * v-pct.
    END.
  end.

  FOR EACH mat-act NO-LOCK
      WHERE mat-act.company EQ job.company
        AND mat-act.job     EQ job.job
        AND mat-act.job-no  EQ job.job-no
        AND mat-act.job-no2 EQ job.job-no2,
     first item
        where item.company eq mat-act.company
          and item.i-no    eq mat-act.rm-i-no
          AND ITEM.mat-type = "B"
        use-index i-no no-lock.
      
    v-pct = 1.
    
    find first tt-mat
        where tt-mat.job-no = job.job-no
          AND tt-mat.job-no2 = job.job-no2
          AND tt-mat.form-no  eq mat-act.s-num
          and tt-mat.blank-no eq mat-act.b-num
          and tt-mat.rm-i-no  eq mat-act.rm-i-no
        no-error.
    if not avail tt-mat then do:
      create tt-mat.
      assign
       tt-mat.job-no = job.job-no
       tt-mat.job-no2 = job.job-no2
       tt-mat.form-no  = mat-act.s-num
       tt-mat.blank-no = mat-act.b-num
       tt-mat.rm-i-no  = mat-act.rm-i-no
       tt-mat.qty-std  = 0
       tt-mat.cst-std  = 0
       v-qty        = 0
       v-cost       = 0.
       
      for each rm-rcpth
          where rm-rcpth.company   eq cocode
            and rm-rcpth.job-no    eq mat-act.job-no
            and rm-rcpth.job-no2   eq mat-act.job-no2
            and rm-rcpth.i-no      eq mat-act.i-no
            and rm-rcpth.rita-code eq "I"
          no-lock,
          
          each rm-rdtlh
          where rm-rdtlh.r-no eq rm-rcpth.r-no
          no-lock:
        
        assign
         v-qty  = v-qty  + rm-rdtlh.qty
         v-cost = v-cost + (rm-rdtlh.qty * rm-rdtlh.cost).
      end.
      
      v-cost = v-cost / v-qty.
    end.
    
   /* ELSE*/ DO:
     v-cost = mat-act.cost.

     IF tt-mat.cst-uom NE mat-act.qty-uom THEN
      if item.r-wid eq 0 then do:
        run sys/ref/convcuom.p(tt-mat.cst-uom,
                               mat-act.qty-uom,
                               (if tt-mat.basis-w   ne 0 then tt-mat.basis-w
                                else item.basis-w),
                               (if tt-mat.len       ne 0 then tt-mat.len
                                else item.s-len),
                               (if tt-mat.wid       ne 0 then tt-mat.wid
                                else item.s-wid),
                               item.s-dep, 
                               mat-act.cost,
                               output v-cost).
      end.

      else do:
        run sys/ref/convcuom.p(tt-mat.cst-uom,
                               mat-act.qty-uom,
                               (if tt-mat.basis-w   ne 0 then tt-mat.basis-w
                                else item.basis-w),
                               tt-mat.len,
                               (if tt-mat.wid       ne 0 then tt-mat.wid
                                else item.r-wid),
                               item.s-dep, 
                               mat-act.cost,
                               output v-cost).
      end.
    END.

    if v-cost eq ? then tt-mat.updatable = yes.

    tt-mat.qty-act = tt-mat.qty-act + (mat-act.qty * v-pct).

    IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
      tt-mat.cst-act = tt-mat.cst-act + (mat-act.qty * v-cost * v-pct).
    ELSE
      tt-mat.cst-act = tt-mat.cst-act + (mat-act.ext-cost * v-pct).
  end.

  for each tt-mat:
     assign
      li-seq      = li-seq + 1
      tt-mat.mat-seq = li-seq
      tt-mat.qty-var = tt-mat.qty-std - tt-mat.qty-act
      tt-mat.cst-var = tt-mat.cst-std - tt-mat.cst-act
      /*v-std-tot   = v-std-tot + tt-mat.cst-std
      v-act-tot   = v-act-tot + tt-mat.cst-act
      v-var-tot   = v-var-tot + tt-mat.cst-var.
      */
     .
      
     /*IF trim(tt-mat.job-no) = "w2297" AND tt-mat.job-no2 = 23 THEN
     MESSAGE "tt-mat:" tt-mat.job-no ":" tt-mat.job-no2 ":"
         tt-mat.Form-no ":" tt-mat.blank-no SKIP
           tt-mat.rm-i-no ":" tt-mat.mat-seq ":"
         tt-mat.cst-act ":" tt-mat.cst-std
           
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
         */
  end.
