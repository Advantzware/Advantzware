
DEF VAR v-n-out AS INT NO-UNDO.
DEF VAR v-item-multi-forms AS LOG NO-UNDO.
DEF VAR v-num-forms AS DEC NO-UNDO.
DEF VAR v-avg-qty AS DEC NO-UNDO.
DEF VAR v-most-sheets AS INT NO-UNDO.
DEF VAR v-non-zero-cost-found AS LOG NO-UNDO.

DEF BUFFER b-work-item FOR work-item.

EMPTY TEMP-TABLE work-mat.

FOR EACH work-item:
    IF CAN-FIND(FIRST b-work-item WHERE
       b-work-item.i-no EQ work-item.i-no AND
       b-work-item.form-no NE work-item.form-no) THEN
       DO:
          v-item-multi-forms = YES.
          LEAVE.
       END.
END.

for each job-mat
    where job-mat.company eq cocode
      and job-mat.job     eq job.job
    use-index seq-idx no-lock,
    
    first ITEM FIELDS(mat-type s-dep)
    where item.company eq cocode
      and item.i-no    eq job-mat.i-no
    use-index i-no no-lock
    
    BREAK BY job-mat.frm
          BY item.mat-type
          BY job-mat.j-no
          BY job-mat.rec_key:

    FIND FIRST est-prep NO-LOCK WHERE 
               est-prep.company = job.company AND 
               est-prep.est-no = job.est-no AND 
               est-prep.CODE = job-mat.i-no NO-ERROR.

    IF tb_exclude_prep AND 
        (AVAIL est-prep AND LOOKUP(est-prep.simon,"N,S") > 0) AND 
        LOOKUP(ITEM.mat-type,"7,8,O,X,Y") > 0 THEN do:
        NEXT.
    END.

  find first work-mat
      where work-mat.i-no    eq job-mat.i-no
        and work-mat.form-no eq job-mat.frm
        and work-mat.len     eq job-mat.len
        and work-mat.wid     eq job-mat.wid
        and work-mat.n-up    eq job-mat.n-up
      no-error.

  if not avail work-mat then do:
    create work-mat.
    assign
     work-mat.i-no    = job-mat.i-no
     work-mat.form-no = job-mat.frm
     work-mat.sc-uom  = job-mat.sc-uom
     work-mat.basis-w = job-mat.basis-w
     work-mat.len     = job-mat.len
     work-mat.wid     = job-mat.wid
     work-mat.n-up    = job-mat.n-up
     work-mat.board   = IF ITEM.mat-type EQ "B" THEN YES ELSE NO
     work-mat.mat-type = ITEM.mat-type.
  end.

  run sys/ref/convcuom.p(job-mat.sc-uom, job-mat.qty-uom, job-mat.basis-w,
                         job-mat.len, job-mat.wid, item.s-dep,
                         job-mat.std-cost, output v-op-cost).

  assign
   work-mat.est-qty     = work-mat.est-qty + job-mat.qty
   work-mat.est-qty-uom = job-mat.qty-uom
   work-mat.est-cost    = work-mat.est-cost + (job-mat.qty * v-op-cost).
   /*The following code commented out for ticket 23736.  
   Joe thinks it was originally added because a former customer used to issue board to one sheet only but since there is a 
   summarize materials, I believe this only causes problems now*/
/*   IF FIRST-OF(ITEM.mat-type) THEN                                                */
/*     v-non-zero-cost-found = FALSE.                                               */
/*                                                                                  */
/*  IF INDEX("1234BR",item.mat-type) GT 0 AND v-non-zero-cost-found EQ TRUE THEN DO:*/
/*     work-mat.est-cost = 0.                                                       */
/*  END.                                                                            */
/*  IF v-non-zero-cost-found EQ FALSE AND work-mat.est-cost GT 0 THEN               */
/*    v-non-zero-cost-found = TRUE.                                                 */

  IF work-mat.est-qty  EQ ? THEN work-mat.est-qty  = 0.
  IF work-mat.est-cost EQ ? THEN work-mat.est-cost = 0.

end.

for each mat-act
    where mat-act.company eq cocode
      and mat-act.job     eq job.job
    no-lock,
    
    first ITEM FIELDS(r-wid basis-w s-wid s-len s-dep)
    where item.company eq cocode
      and item.i-no    eq mat-act.i-no
    use-index i-no no-lock:

  find first work-mat
      where work-mat.i-no    eq mat-act.i-no
        and work-mat.form-no eq mat-act.s-num
      no-error.

  if not avail work-mat then do:

    FIND FIRST est-prep NO-LOCK WHERE 
               est-prep.company = job.company AND 
               est-prep.est-no = job.est-no AND 
               est-prep.CODE = mat-act.i-no NO-ERROR.

    /* Task 01051204 */
    IF tb_exclude_prep AND 
        (AVAIL est-prep AND LOOKUP(est-prep.simon,"S") = 0) AND 
        LOOKUP(ITEM.mat-type,"7,8,O,X,Y") > 0 THEN do: 
        NEXT.
    END.

    create work-mat.
    assign
     work-mat.i-no    = mat-act.i-no
     work-mat.form-no = mat-act.s-num
     work-mat.board   = IF ITEM.mat-type EQ "B" THEN YES ELSE NO
     work-mat.mat-type = ITEM.mat-type.
  end.

  v-ip-sc-uom = if work-mat.sc-uom gt "" then
                  work-mat.sc-uom else mat-act.qty-uom.

  if item.r-wid eq 0 then
    run sys/ref/convcuom.p(v-ip-sc-uom,
                           mat-act.qty-uom,
                           (if work-mat.basis-w ne 0 then work-mat.basis-w
                            else item.basis-w),
                           (if work-mat.len     ne 0 then work-mat.len
                            else item.s-len),
                           (if work-mat.wid     ne 0 then work-mat.wid
                            else item.s-wid),
                           item.s-dep, 
                           mat-act.cost,
                           output v-op-cost).

  else
    run sys/ref/convcuom.p(v-ip-sc-uom,
                           mat-act.qty-uom,
                           (if work-mat.basis-w ne 0 then work-mat.basis-w
                            else item.basis-w),
                           work-mat.len,
                           (if work-mat.wid     ne 0 then work-mat.wid
                            else item.r-wid),
                           item.s-dep, 
                           mat-act.cost,
                           output v-op-cost).

  assign
   work-mat.act-qty     = work-mat.act-qty + mat-act.qty
   work-mat.act-qty-uom = mat-act.qty-uom.

  IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
    work-mat.act-cost = work-mat.act-cost + (mat-act.qty * v-op-cost).
  ELSE
    work-mat.act-cost = work-mat.act-cost + mat-act.ext-cost.

  IF work-mat.act-cost EQ ? THEN work-mat.act-cost = 0.
end.

IF v-item-multi-forms EQ YES AND
   can-find(first work-item WHERE
   work-item.qty-prod gt 0) THEN
   
   for each work-item BY work-item.form-no:
      
       v-num-forms = 1.

       FOR EACH b-work-item WHERE
           b-work-item.i-no EQ work-item.i-no AND
           b-work-item.form-no NE work-item.form-no:

           v-num-forms = v-num-forms + 1.
       END.

       ASSIGN
          v-avg-qty = (work-item.qty-prod + work-item.est-spo) / v-num-forms
          v-num-up = 1.
       
       FIND FIRST eb WHERE
            eb.company EQ job.company AND
            eb.est-no EQ job.est-no AND
            eb.form-no EQ work-item.form-no AND
            eb.stock-no EQ work-item.i-no
            NO-LOCK NO-ERROR.

       IF AVAIL eb THEN
          v-num-up = eb.num-up.

       v-avg-qty = v-avg-qty / v-num-up.

       {sys/inc/roundup.i v-avg-qty}

       work-item.avg-qty = v-avg-qty.
   end.

for each work-mat break by work-mat.i-no:
  find first item
      where item.company eq cocode
        and item.i-no    eq work-mat.i-no
      use-index i-no no-lock no-error.

  if first-of(work-mat.i-no) then
  for each xwork-mat
      where xwork-mat.i-no    eq work-mat.i-no
        and xwork-mat.est-qty ne 0:

    if avail item and item.mat-type eq "B" then do:

       IF v-item-multi-forms EQ NO THEN
       DO:
          IF work-mat.n-up EQ 0 THEN DO:
             v-num-up = 1.
             find first ef
                 where ef.company   eq job.company
                   AND ef.est-no    EQ job.est-no
                   and ef.form-no eq xwork-mat.form-no
                 no-lock no-error.
             if avail ef then do:
               run sys/inc/numup.p (job.company, job.est-no, xwork-mat.form-no, output v-num-up).
               RUN est/ef-#out.p (ROWID(ef), OUTPUT v-n-out).
               v-num-up = v-num-up * v-n-out.
             end.
          END.
          ELSE
             v-num-up = work-mat.n-up.
         
          if can-find(first work-item
                      where work-item.form-no  eq xwork-mat.form-no
                        and work-item.qty-prod gt 0) THEN DO:
          
             for each work-item where work-item.form-no eq xwork-mat.form-no:
                 
                 work-mat.prd-qty = work-mat.prd-qty +
                                   ((work-item.qty-prod + work-item.est-spo) / v-num-up).
             end.
          END.
       END.
       ELSE /*item on multiple forms*/
       DO:
          if can-find(first work-item WHERE
             work-item.qty-prod gt 0) then
          DO:
             FOR EACH work-item
                 BREAK BY work-item.form-no:

                 IF FIRST-OF(work-item.form-no) THEN
                 DO:
                    v-most-sheets = -1.
                   
                    FOR EACH b-work-item WHERE
                        b-work-item.form-no EQ work-item.form-no:
                   
                        IF b-work-item.avg-qty GT v-most-sheets THEN
                           v-most-sheets = b-work-item.avg-qty.
                    END.

                    work-mat.prd-qty = work-mat.prd-qty + v-most-sheets.
                 END.
             END.
          END.
       END.
    end.

    if recid(xwork-mat) ne recid(work-mat) then do:
     
      ASSIGN
       work-mat.est-qty  = work-mat.est-qty  + xwork-mat.est-qty
       work-mat.est-cost = work-mat.est-cost + xwork-mat.est-cost
       work-mat.act-qty  = work-mat.act-qty  + xwork-mat.act-qty
       work-mat.act-cost = work-mat.act-cost + xwork-mat.act-cost.

      delete xwork-mat.
    end.
  end.
end.

