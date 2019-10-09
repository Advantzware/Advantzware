
EMPTY TEMP-TABLE work-mat.

assign
   v-est-mat-cost = 0
   v-act-mat-cost = 0
   v-est-board-mat-cost = 0
   v-act-board-mat-cost = 0
   v-est-other-mat-cost = 0
   v-act-other-mat-cost = 0.

IF rd_qty BEGINS "P" THEN DO:
  {jc/rep/job-summ.i}

  FOR EACH work-mat:
     FIND FIRST ITEM WHERE
          item.company EQ cocode AND
          item.i-no    EQ work-mat.i-no
          NO-LOCK NO-ERROR.

     v-est-mat-cost = v-est-mat-cost +  work-mat.est-cost . /* ticket 17911 */
   
     IF work-mat.prd-qty NE 0 AND
        work-mat.act-qty NE 0 AND
        work-mat.est-qty NE 0 THEN DO:
            
        IF work-mat.act-qty-uom NE "EA" THEN
           RUN sys/ref/convquom.p("EA", work-mat.act-qty-uom, work-mat.basis-w,
                                  work-mat.len, work-mat.wid, item.s-dep,
                                  work-mat.prd-qty, OUTPUT work-mat.prd-qty).
    
        /*work-mat.est-cost = work-mat.est-cost / work-mat.est-qty *
                            work-mat.prd-qty .*/ /* ticket 17911 */
     END.

    ASSIGN
    /* v-est-mat-cost = v-est-mat-cost + work-mat.est-cost*/
     v-act-mat-cost = v-act-mat-cost + work-mat.act-cost.

    IF tb_sep_board THEN
    DO:
       IF NOT work-mat.board THEN
          ASSIGN
             v-est-other-mat-cost = v-est-other-mat-cost + work-mat.est-cost
             v-act-other-mat-cost = v-act-other-mat-cost + work-mat.act-cost.
       ELSE
          ASSIGN
             v-est-board-mat-cost = v-est-board-mat-cost + work-mat.est-cost
             v-act-board-mat-cost = v-act-board-mat-cost + work-mat.act-cost.
    END.

    DELETE work-mat.
  END.
END.

ELSE DO:
  for each job-mat FIELDS(sc-uom qty-uom basis-w len wid std-cost qty)
      where job-mat.company eq cocode
        and job-mat.job     eq job.job
      use-index seq-idx no-lock,
    
      first ITEM FIELDS(s-dep mat-type)
      where item.company eq cocode
        and item.i-no    eq job-mat.i-no
      no-lock:


      /* stacey */

      FIND FIRST est-prep NO-LOCK WHERE 
                 est-prep.company = job.company AND 
                 est-prep.est-no = job.est-no AND 
                 est-prep.CODE = job-mat.i-no NO-ERROR.

/*       IF AVAIL est-prep THEN                            */
/*           MESSAGE "est-prep.CODE: " est-prep.CODE SKIP  */
/*                   "est-prep.simon: " est-prep.simon     */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.        */

      IF exclude-billed-prep AND 
          (AVAIL est-prep AND LOOKUP(est-prep.simon,"N,S") > 0) AND 
          LOOKUP(ITEM.mat-type,"7,8,O,X,Y") > 0 THEN do:
/*           MESSAGE "eliminate " job-mat.i-no       */
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.  */
          NEXT.
      END.
    
    run sys/ref/convcuom.p(job-mat.sc-uom, job-mat.qty-uom, job-mat.basis-w,
                           job-mat.len, job-mat.wid, item.s-dep,
                           job-mat.std-cost, output v-op-cost).
                         
    v-est-mat-cost = v-est-mat-cost + (job-mat.qty * v-op-cost).

    IF tb_sep_board THEN
    DO:
       IF ITEM.mat-type NE "B" THEN
          v-est-other-mat-cost = v-est-other-mat-cost + (job-mat.qty * v-op-cost).
       ELSE
          v-est-board-mat-cost = v-est-board-mat-cost + (job-mat.qty * v-op-cost).
    END.
  end.

  for each mat-act FIELDS(i-no job s-num b-num qty-uom ext-cost cost qty)
      where mat-act.company eq cocode
        and mat-act.job     eq job.job
      no-lock,
    
      first ITEM FIELDS(s-dep mat-type)
      where item.company eq cocode
        and item.i-no    eq mat-act.i-no
      no-lock:

    find first job-mat
        where job-mat.company  eq cocode
          and job-mat.job      eq mat-act.job
          and job-mat.frm      eq mat-act.s-num
          and job-mat.blank-no eq mat-act.b-num
          and job-mat.i-no     eq mat-act.i-no
        no-lock no-error.
      
    if available job-mat then
      assign
       v-ip-sc-uom  = job-mat.sc-uom
       v-ip-basis-w = job-mat.basis-w
       v-ip-len     = job-mat.len
       v-ip-wid     = job-mat.wid.
    else
      assign
       v-ip-sc-uom  = mat-act.qty-uom
       v-ip-basis-w = 0
       v-ip-len     = 0
       v-ip-wid     = 0.

    IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN DO:
       run sys/ref/convcuom.p(v-ip-sc-uom, mat-act.qty-uom, v-ip-basis-w,
                              v-ip-len, v-ip-wid, item.s-dep,
                              mat-act.cost, output v-op-cost).
                          
       v-act-mat-cost = v-act-mat-cost + (mat-act.qty * v-op-cost).
      
       IF tb_sep_board THEN
       DO:
          IF ITEM.mat-type NE "B" THEN
             v-act-other-mat-cost = v-act-other-mat-cost + (mat-act.qty * v-op-cost).
          ELSE
             v-act-board-mat-cost = v-act-board-mat-cost + (mat-act.qty * v-op-cost).
       END.
    END.

    ELSE
    DO:
       v-act-mat-cost = v-act-mat-cost + mat-act.ext-cost.

       IF tb_sep_board THEN
       DO:
          IF ITEM.mat-type NE "B" THEN
             v-act-other-mat-cost = v-act-other-mat-cost + mat-act.ext-cost.
          ELSE
             v-act-board-mat-cost = v-act-board-mat-cost + mat-act.ext-cost.
       END.
    END.
  end.
END.

