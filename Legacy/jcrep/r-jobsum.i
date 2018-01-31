
DEF VAR ll-act-rate AS LOG NO-UNDO.
DEF VAR ld-tot-rate AS DEC NO-UNDO. 
DEF VAR v-supress-est AS LOG NO-UNDO.
DEF VAR v-num-up AS INT NO-UNDO.
DEF VAR v-act-qty AS INT NO-UNDO.
DEF BUFFER bf-work-mat FOR work-mat.
DEF BUFFER bf-item FOR ITEM.
DEF VAR v-rm-qty AS INT EXTENT 2 NO-UNDO.
DEF VAR v-qty AS INT NO-UNDO.
DEF VAR part-no AS CHAR INIT "" NO-UNDO .
DEF VAR i-name AS CHAR INIT ""  NO-UNDO .
DEF VAR cat AS CHAR INIT "" NO-UNDO .
DEF  VAR acl-brd AS DEC INIT 0 NO-UNDO. 
DEF  VAR acl-oth AS DEC INIT 0 NO-UNDO.
DEF  VAR acl-lbr AS DEC INIT 0 NO-UNDO.
    

    put skip.
  
    for EACH job
        where job.company            eq cocode
          and job.job-no             ge SUBSTR(v-job-no[1],1,6)
          and job.job-no             le SUBSTR(v-job-no[2],1,6)
          AND fill(" ",6 - length(trim(job.job-no))) +
              trim(begin_job-no) + string(int(job.job-no2),"99") GE v-job-no[1]
          AND fill(" ",6 - length(trim(job.job-no))) +
              trim(begin_job-no) + string(int(job.job-no2),"99") LE v-job-no[2]
          and (v-stat                eq "A"                     or
               (v-stat               eq "O" and job.opened)     or
               (v-stat               eq "C" and NOT job.opened))
          and ((job.close-date       ge v-date[1] and
                job.close-date       le v-date[2] and
                NOT job.opened)                                 or
               (job.start-date       ge v-date[1] and
                job.start-date       le v-date[2] and
                job.opened)                                     OR
               v-job-no[1] EQ v-job-no[2])
          and (v-ind eq "B"                                     or
               (v-ind eq "F" and
                can-find(first est where est.company    EQ job.company
                                     AND est.est-no     EQ job.est-no
                                     and est.est-type le 4))    or
               (v-ind eq "C" and
                can-find(first est where est.company    EQ job.company
                                     AND est.est-no     EQ job.est-no
                                     and est.est-type gt 4)))
        no-lock
        break by job.job-no
              by job.job-no2:
         
      release cust.
      v-cust = "".
      find first job-hdr
          where job-hdr.company eq cocode
            and job-hdr.job     eq job.job
            and job-hdr.job-no  eq job.job-no
            and job-hdr.job-no2 eq job.job-no2
          no-lock no-error.
      if avail job-hdr then
      find first cust
          where cust.company eq cocode
            and cust.cust-no eq job-hdr.cust-no
          no-lock no-error.
      if avail cust then v-cust = cust.name.
        
      assign
       v-avg-prc   = 0
       v-t-ord     = 0
       v-t-prs     = 0
       v-t-prod    = 0
       v-t-inv-qty = 0
       v-t-est-spo = 0
       v-t-all     = 0
       v-sale      = 0
       v-mater     = 0
       v-prep      = 0
       v-labor     = 0
       v-lab-m     = 0
       v-comm      = 0
       v-frate     = 0
       v-act-spo   = 0
       v-est-spo   = 0
       v-misc-prep = 0.

      /***  Get the Item/Order Information  ***/
      run jc/rep/job-sumi.p (recid(job)).
      IF v-inv-tot-only  THEN
        v-misc-prep = getInvoiceTotal(job-hdr.ord-no, job.job-no, job.job-no2, "Prep").
      
      {jc/rep/job-summ.i}   /***  Get the Material Information ***/

      find first work-item no-error.
      if not avail work-item then next.
      IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
      put "Job Number: "
          trim(job.job-no) + "-" + string(job.job-no2,"99") +
          "     Closing Date: " +
          (IF job.close-date EQ ? THEN "        " ELSE
                                  STRING(job.close-date,"99/99/99"))
          FORMAT "X(36)" skip
          "Customer:   "
          v-cust.
     IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
     if not v-mch then do:
      put skip(1)
          "                                                SELLING <--------"
          "QUANTITY IN BLANKS--------------->  ACTUAL   ACT   "
          "ESTIMATE  EST   OVER-"
          skip
          "ITEM CODE    DESCRIPTION                        PRICE/M    ORDERED"
          "     POSTED   FINISHED    ALLOWED  SPOILAGE SPL%  "
          "SPOILAGE  SPL%  RUN %"
          skip
          fill("-",137) format "x(137)" skip.

      for each work-item BREAK BY work-item.i-no:
        assign
         v-act-spo = v-act-spo + work-item.act-spo
         v-est-spo = v-est-spo + work-item.est-spo
         v-qty-ord = v-qty-ord + work-item.qty-ord
         v-sale    = v-sale    + ((work-item.qty-ord / 1000) * work-item.price).

        IF LAST-OF(work-item.i-no) THEN DO:

          find first itemfg
              where itemfg.company eq cocode
                and itemfg.i-no    eq work-item.i-no
              no-lock.

          IF AVAIL itemfg THEN DO:
              ASSIGN 
                  part-no = itemfg.part-no 
                  i-name = itemfg.i-name
                  cat = itemfg.procat 
               /*   acl-brd = itemfg.std-mat-cost 
                  acl-oth = (itemfg.std-var-cost + itemfg.std-fix-cost)
                  acl-lbr = itemfg.std-lab-cost*/ .
          END.

          IF tb_waste-from-issued THEN DO:
              
              for each mat-act
                  where mat-act.company eq cocode
                    and mat-act.job     eq job.job
                    and mat-act.s-num   eq work-item.form-no
/*                     and mat-act.b-num   eq job-mat.blank-no */
/*                    and mat-act.i-no    eq work-item.i-no */
                  no-lock:
                  
                 FIND FIRST job-mat WHERE job-mat.company = mat-act.company
                                      AND job-mat.job     = mat-act.job
                                      AND job-mat.i-no    = mat-act.i-no
                                    NO-LOCK NO-ERROR.
                  IF AVAIL job-mat THEN DO:
                      find first work-mat
                          where work-mat.i-no    eq mat-act.i-no
                            and work-mat.form-no eq mat-act.s-num
                          no-error.                  
                      run sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                                             job-mat.len, job-mat.wid, item.s-dep,
                                             mat-act.qty, output v-qty).
                      IF AVAIL work-mat THEN
                        v-rm-qty[1] = v-rm-qty[1] + (v-qty * work-mat.n-up).
                      ELSE
                        v-rm-qty[1] = v-rm-qty[1] + (v-qty * job-mat.n-up).
                  END.
              end.
              IF v-rm-qty[1] EQ 0 AND AVAIL(job-mat) THEN      /* get sheets from slitter */
               RUN sys/inc/slitshts.p (ROWID(job), job-mat.frm, OUTPUT v-rm-qty[1]).

          END.
    
 
          ASSIGN
           v-qty-all = v-qty-all + work-item.qty-all
           work-item.press-1 = work-item.press-1 + v-act-spo
           v-a-spo-p         = v-act-spo / work-item.press-1 * 100.00
           v-e-spo-p         = (v-est-spo /
                                (work-item.qty-prod + v-est-spo)) * 100
           v-over-pct        = ((work-item.qty-prod - work-item.qty-ord) /
                                work-item.qty-ord) * 100.00.

          if v-a-spo-p = ? then v-a-spo-p = 0.
          if v-e-spo-p = ? then v-e-spo-p = 0.
          if v-over-pct = ? or v-over-pct lt 0 then v-over-pct = 0.    
           IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.       
          display work-item.i-no
                  itemfg.i-name         FORMAT "x(29)"
                  work-item.price       format ">>>>>9.99"   when v-tot
                  v-qty-ord             format ">>,>>>,>>9"
                  work-item.press-1     format ">>,>>>,>>9"
                  work-item.qty-prod    format ">>,>>>,>>9"
                  v-qty-all             format ">>,>>>,>>9"
                  v-act-spo WHEN NOT tb_waste-from-issued
                  v-a-spo-p WHEN NOT tb_waste-from-issued
                  v-est-spo             format ">>>>>>>9-"
                  space(0)
                  v-e-spo-p
                  space(0)
                  v-over-pct
              WITH FRAME det-item no-attr-space NO-LABELS NO-BOX STREAM-IO width 200 down.        

          ASSIGN
           v-t-ord     = v-t-ord + v-qty-ord
           v-t-prs     = v-t-prs + work-item.press-1
           v-t-prod    = v-t-prod + work-item.qty-prod
           v-t-est-spo = v-t-est-spo + v-est-spo
           v-t-all     = v-t-all + v-qty-all

           v-act-spo = 0
           v-est-spo = 0
           v-qty-ord = 0
           v-qty-all = 0.
          
        END.
      end.

      assign
       v-act-spo  = v-t-prs - v-t-prod
       v-a-spo-p  = v-act-spo / v-t-prs * 100.00
       v-e-spo-p  = (v-t-est-spo / (v-t-prod + v-t-est-spo)) * 100
       v-over-pct = ((v-t-prod - v-t-ord) / v-t-ord) * 100.00
       v-avg-prc  = v-sale / (v-t-ord / 1000).

      v-act-qty = 0. 
      FOR EACH work-mat WHERE .
          FIND FIRST ITEM 
              WHERE ITEM.i-no = work-mat.i-no NO-LOCK NO-ERROR.
          IF NOT ITEM.mat-type = 'B' THEN
              NEXT.          
          v-act-qty = v-act-qty + work-mat.act-qty * work-mat.n-up.
/*           v-num-up  = work-mat.n-up. */
      END.      
      /* 01301304 */
/*       IF tb_waste-from-issued THEN                                            */
/*           v-act-spo = IF (v-act-qty * v-num-up) GE v-t-prod THEN              */
/*                          0                                                    */
/*                          ELSE                                                 */
/*                          (v-act-qty * v-num-up) /* v-rm-qty[1] */ - v-t-prod. */
      IF tb_waste-from-issued THEN
          v-act-spo = v-act-qty  /* v-rm-qty[1] */ - v-t-prod.

      IF tb_waste-from-issued THEN
          v-a-spo-p = ( v-act-spo / v-act-qty /* v-rm-qty[1]*/) * 100.
/*       IF tb_waste-from-issued THEN                                                 */
/*           v-act-spo = (v-act-qty * v-num-up) /* v-rm-qty[1] */ - v-t-prod.         */
/*                                                                                    */
/*       IF tb_waste-from-issued THEN                                                 */
/*           v-a-spo-p = (v-act-spo / (v-act-qty * v-num-up) /* v-rm-qty[1]*/) * 100. */

      if v-a-spo-p = ? then v-a-spo-p = 0.
      if v-e-spo-p = ? then v-e-spo-p = 0.
      if v-over-pct = ? or v-over-pct lt 0 then v-over-pct = 0.
      
       IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.              
      put fill("-",90) format "x(90)" at 48 skip.

      if v-tot then put "ORDER TOTALS" at 5 v-avg-prc at 47.
      IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
      put v-t-ord at 57 v-t-prs at 68
          v-t-prod at 79 v-t-all at 90
          v-act-spo at 101 v-a-spo-p at 110
          v-t-est-spo at 117 v-e-spo-p at 126
          v-over-pct at 132 skip.

      if v-tot then
      put "SALES VALUE "
          v-sale format "$>>>>>>>>9.99" skip.
 
      else put skip(1).
     end.
  
     /* This procedure utilizes the shared temp-table work-mch */
     RUN jc/rep/job-sumr.p (INPUT ROWID(job),
                            INPUT cocode,
                            INPUT locode,
                            INPUT tb_exclude_run_if_no_prod,
                            INPUT v-tspost-val,
                            INPUT tb_curr,
                            INPUT tb_curr-crew).

      /* {jc/rep/job-sumr.i} */   /***  Get the Routing Information  ***/
      IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
      find first work-mch no-error.
      if avail work-mch then
        put skip(1)
            "MACH                    M QUANTITY  <------ESTIMATED----->"
            "  <-------ACTUAL-------->    COST      COST       "
            " <---SPOILAGE ANALYSIS---->" skip
            "CODE   DESCRIPTION      R PRODUCED   HOURS  SPEED   COST  "
            "   HOURS  SPEED   COST     VARIANCE    VAR%       "
            " EST QTY   ACTUAL     VAR %" skip
            fill("-", 135) format "x(135)" skip.

      assign
       v-t-est-hrs  = 0
       v-t-est-cost = 0
       v-t-act-hrs  = 0
       v-t-act-cost = 0
       acl-lbr = 0    .

      for each work-mch,
          first mach
          where mach.company eq cocode
            and mach.loc     eq locode
            and mach.m-code  eq work-mch.m-code
          no-lock
          break by work-mch.d-seq:

        assign
         v-mr-cost      = work-mch.mr-cost1  + work-mch.mr-cost2 +
                          work-mch.mr-cost3 
         v-run-cost     = work-mch.run-cost1 + work-mch.run-cost2 +
                          work-mch.run-cost3
         v-mr-cost-var  = work-mch.est-mr-cost - v-mr-cost
         v-run-cost-var = work-mch.est-run-cost - v-run-cost
         v-mr-prod-p    = v-mr-cost-var / work-mch.est-mr-cost * 100.00
         v-run-prod-p   = v-run-cost-var / work-mch.est-run-cost * 100.00
         v-mr-wst-var   = (work-mch.mr-waste - work-mch.mr-qty) /
                          work-mch.mr-waste * 100.00
         v-run-wst-var  = (work-mch.run-waste - work-mch.wst-qty) /
                          work-mch.run-waste * 100.00

         v-mr-cost-var  = if v-mr-cost-var  ne ? then v-mr-cost-var else 0
         v-run-cost-var = if v-run-cost-var ne ? then v-run-cost-var else 0
         v-mr-prod-p    = if v-mr-prod-p    ne ? then v-mr-prod-p else 0
         v-run-prod-p   = if v-run-prod-p   ne ? then v-run-prod-p else 0
         v-mr-wst-var   = if v-mr-wst-var   ne ? then v-mr-wst-var else 0
         v-run-wst-var  = if v-run-wst-var  ne ? then v-run-wst-var else 0.
         IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
        display work-mch.m-code
                mach.m-dscr format "x(16)"
                "M"
                round(work-mch.est-mr-hr,2) at 35 format ">>>9.99-"
                  when work-mch.est-mr-hr ne 0
                   and  work-mch.est-mr-hr ne ?
                work-mch.est-mr-cost at 51 format ">>>>,>>9-"
                  when work-mch.est-mr-cost ne 0
                   and  work-mch.est-mr-cost ne ?
                work-mch.mr-hr format ">>>9.9-"
                  when work-mch.mr-hr ne 0
                   and work-mch.mr-hr ne ?
                v-mr-cost at 77 format ">>>>,>>9-"
                  when v-mr-cost ne 0
                   and v-mr-cost ne ?
                v-mr-cost-var format ">,>>>,>>9-"
                  when v-mr-cost-var ne 0
                   and v-mr-cost-var ne ?
                v-mr-prod-p format ">>>9.9-"
                  when v-mr-prod-p ne 0
                   and v-mr-prod-p ne ?
                "     "
                work-mch.mr-waste format ">>>,>>9-"
                  when work-mch.mr-waste ne 0
                   and work-mch.mr-waste ne ?
                work-mch.mr-qty format ">>>,>>9-"
                  when work-mch.mr-qty ne 0
                   and work-mch.mr-qty ne ?
                v-mr-wst-var format ">>>>>9.9-"
                  when v-mr-wst-var ne 0
                   and v-mr-wst-var ne ? skip

            WITH FRAME det-r1 STREAM-IO WIDTH 200 NO-BOX DOWN NO-LABELS.

        IF v-lab                                                  AND
           (work-mch.est-mr-cost1 NE 0 OR work-mch.mr-cost1 NE 0) THEN do:

            acl-lbr =  acl-lbr + ROUND(work-mch.mr-cost1,0) .
          IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
          DISPLAY "Direct Labor"                                       AT 8
                  ROUND(work-mch.est-mr-cost1,0)    FORMAT "zzzz,zzz-" AT 51
                  ROUND(work-mch.mr-cost1,0)        FORMAT "zzzz,zzz-" AT 77
              WITH FRAME det-r1a NO-BOX NO-LABELS STREAM-IO WIDTH 87.
        END.
        
         IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.   
        IF v-foh                                                  AND
           (work-mch.est-mr-cost2 NE 0 OR work-mch.mr-cost2 NE 0) THEN
          DISPLAY "Fixed Overhead"                                     AT 8
                  ROUND(work-mch.est-mr-cost2,0)    FORMAT "zzzz,zzz-" AT 51
                  ROUND(work-mch.mr-cost2,0)        FORMAT "zzzz,zzz-" AT 77
              WITH FRAME det-r1b NO-BOX NO-LABELS STREAM-IO WIDTH 87.
        IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.    
        IF v-voh                                                  AND
           (work-mch.est-mr-cost3 NE 0 OR work-mch.mr-cost3 NE 0) THEN
          DISPLAY "Variable Overhead"                                  AT 8
                  ROUND(work-mch.est-mr-cost3,0)    FORMAT "zzzz,zzz-" AT 51
                  ROUND(work-mch.mr-cost3,0)        FORMAT "zzzz,zzz-" AT 75
              WITH FRAME det-r1c NO-BOX NO-LABELS STREAM-IO WIDTH 87.
        IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
        display "R" at 25
                work-mch.run-qty AT 26 format ">>>>>>>9-"
                round(work-mch.est-run-hr,2) at 35 format ">>>9.99-"
                  when work-mch.est-run-hr ne 0
                   and work-mch.est-run-hr ne ?
                work-mch.est-speed 
                  when work-mch.est-speed ne 0
                   and work-mch.est-speed ne ? format ">>>>>9-"
                work-mch.est-run-cost format ">>>,>>9-"
                  when work-mch.est-run-cost ne 0
                   and work-mch.est-run-cost ne ?
                work-mch.run-hr format ">>>9.9-"
                  when work-mch.run-hr ne 0
                   and work-mch.run-hr ne ?
                work-mch.run-speed 
                  when work-mch.run-speed ne 0
                   and work-mch.run-speed ne ? format ">>>>>9-"
                v-run-cost format ">>>>,>>9-"
                  when v-run-cost ne 0
                   and v-run-cost ne ?
                v-run-cost-var format ">,>>>,>>9-"
                  when v-run-cost-var ne 0
                   and v-run-cost-var ne ?
                v-run-prod-p format ">>>9.9-"
                  when v-run-prod-p ne 0
                   and v-run-prod-p ne ?
                "     "
                work-mch.run-waste format ">>>,>>9-"
                  when work-mch.run-waste ne 0
                   and work-mch.run-waste ne ?
                work-mch.wst-qty format ">>>,>>9-"
                  when work-mch.wst-qty ne 0
                   and work-mch.wst-qty ne ?
                v-run-wst-var format ">>>>>9.9-"
                  when v-run-wst-var ne 0
                   and v-run-wst-var ne ?

            WITH FRAME det-r2 STREAM-IO WIDTH 200 NO-BOX down NO-LABELS.

        IF v-lab                                                    AND
           (work-mch.est-run-cost1 NE 0 OR work-mch.run-cost1 NE 0) THEN do:

            acl-lbr =  acl-lbr + ROUND(work-mch.run-cost1,0) .
          IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
          DISPLAY "Direct Labor"                                       AT 8
                  ROUND(work-mch.est-run-cost1,0)   FORMAT "zzzz,zzz-" AT 51
                  ROUND(work-mch.run-cost1,0)       FORMAT "zzzz,zzz-" AT 77
              WITH FRAME det-r2a NO-BOX NO-LABELS STREAM-IO WIDTH 87.
        END.
         IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.   
        IF v-foh                                                    AND
           (work-mch.est-run-cost2 NE 0 OR work-mch.run-cost2 NE 0) THEN
          DISPLAY "Fixed Overhead"                                     AT 8
                  ROUND(work-mch.est-run-cost2,0)   FORMAT "zzzz,zzz-" AT 51
                  ROUND(work-mch.run-cost2,0)       FORMAT "zzzz,zzz-" AT 77
              WITH FRAME det-r2b NO-BOX NO-LABELS STREAM-IO WIDTH 87.
         IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.   
        IF v-voh                                                    AND
           (work-mch.est-run-cost3 NE 0 OR work-mch.run-cost3 NE 0) THEN
          DISPLAY "Variable Overhead"                                  AT 8
                  ROUND(work-mch.est-run-cost3,0)   FORMAT "zzzz,zzz-" AT 51
                  ROUND(work-mch.run-cost3,0)       FORMAT "zzzz,zzz-" AT 77
              WITH FRAME det-r2c NO-BOX NO-LABELS STREAM-IO WIDTH 87.

        assign
         v-t-est-hrs  = v-t-est-hrs +
                        if work-mch.est-mr-hr ne ? then work-mch.est-mr-hr
                        else 0
         v-t-est-hrs  = v-t-est-hrs +
                        if work-mch.est-run-hr ne ? then work-mch.est-run-hr
                        else 0

         v-t-est-cost = v-t-est-cost +
                        if work-mch.est-mr-cost ne ? then work-mch.est-mr-cost
                        else 0
         v-t-est-cost = v-t-est-cost +
                        if work-mch.est-run-cost ne ? then work-mch.est-run-cost
                        else 0

         v-t-act-hrs  = v-t-act-hrs +
                        if work-mch.mr-hr ne ? then work-mch.mr-hr
                        else 0
         v-t-act-hrs  = v-t-act-hrs +
                        if work-mch.run-hr ne ? then work-mch.run-hr
                        else 0

         v-t-act-cost = v-t-act-cost +
                        if v-mr-cost  ne ? then v-mr-cost  else 0
         v-t-act-cost = v-t-act-cost +
                        if v-run-cost ne ? then v-run-cost else 0.

        if last(work-mch.d-seq) then do:
          assign
           v-gt-est-hrs = v-gt-est-hrs + v-t-est-hrs
           v-gt-est-cst = v-gt-est-cst + v-t-est-cost
           v-gt-act-hrs = v-gt-act-hrs + v-t-act-hrs
           v-gt-act-cst = v-gt-act-cst + v-t-act-cost

           v-t-est-cost = round(v-t-est-cost,0)
           v-t-act-cost = round(v-t-act-cost,0)
           v-cst-var    = v-t-est-cost - v-t-act-cost
           v-cst-var    = round(v-cst-var,0)
           v-t-act-cost = round(v-t-act-cost,0)
           v-prod-p     = (v-cst-var / v-t-est-cost) * 100.00
           v-prod-p     = round(v-prod-p,1)

           v-t-est-hrs  = round(v-t-est-hrs,2)
           v-t-act-hrs  = round(v-t-act-hrs,1).

          if v-prod-p = ? then v-prod-p = 0.
         IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
          put "------" at 37 "--------  ------" at 51
              "--------  --------  ------" AT 78
              "        --------------------------" skip
              "  TOTAL CONVERSION (DIRECT) :"
              v-t-est-hrs at 35 v-t-est-cost at 50 " "
              v-t-act-hrs v-t-act-cost at 77 " " v-cst-var v-prod-p skip.
        end.        
      end.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED            
            '"' trim(job.job-no) + "-" + string(job.job-no2,"99") '",'
            '"' STRING(v-cust)                                    '",'
            '"' STRING(work-item.i-no)                            '",'
            '"' STRING(work-item.price,">>>>9.99")                '",'            
            '"' STRING(v-t-ord,">>,>>>,>>9")                      '",'
            '"' STRING(v-t-est-hrs)                               '",'
            '"' STRING(v-t-est-cost)                              '",'
            '"' STRING(v-t-act-hrs)                               '",'
            '"' STRING(v-t-act-cost)                              '",' .

      v-labor = v-t-act-cost.
     IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
     if v-mch then put skip(2).  

     else do:
      IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
      if can-find(FIRST work-mat) then
         put skip(1)
             "                                     <-----ESTIMATED------>"
             "  <-------ACTUAL--------->     COST      COST  " skip
             "ITEM CODE       DESCRIPTION          QUANTITY UM        COST"
             "  QUANTITY UM        COST   VARIANCE    VAR%   " skip
             fill("-",105) format "x(105)" skip.
      
      IF v-merge-mat THEN DO:
          FOR EACH work-mat BREAK BY work-mat.i-no:
             
              
              FIND FIRST bf-work-mat 
                   WHERE bf-work-mat.i-no EQ work-mat.i-no
                     AND ROWID(bf-work-mat) NE ROWID(work-mat)
                  NO-ERROR.
    
              FIND ITEM WHERE ITEM.company EQ cocode
                     AND ITEM.i-no EQ bf-work-mat.i-no
                  NO-LOCK NO-ERROR.
              
              IF AVAIL bf-work-mat AND AVAIL(ITEM) THEN DO:
                /* Add in values to original line and delete current */

                /* Convert quantities to match line being merged into */
                IF bf-work-mat.act-qty-uom NE work-mat.act-qty-uom AND bf-work-mat.act-qty-uom GT "" AND work-mat.act-qty-uom GT "" THEN
                    run sys/ref/convquom.p(work-mat.act-qty-uom, bf-work-mat.act-qty-uom, bf-work-mat.basis-w,
                            bf-work-mat.len, bf-work-mat.wid, item.s-dep,
                            work-mat.act-qty, output work-mat.act-qty).
    
                IF bf-work-mat.est-qty-uom NE work-mat.est-qty-uom AND bf-work-mat.est-qty-uom GT "" AND work-mat.est-qty-uom GT "" THEN
                    run sys/ref/convquom.p(work-mat.est-qty-uom, bf-work-mat.est-qty-uom, bf-work-mat.basis-w,
                            bf-work-mat.len, bf-work-mat.wid, item.s-dep,
                            work-mat.est-qty, output work-mat.est-qty).

                ASSIGN 
                  bf-work-mat.act-cost = bf-work-mat.act-cost + work-mat.act-cost
                  bf-work-mat.act-qty  = bf-work-mat.act-qty  + work-mat.act-qty
                  bf-work-mat.est-cost = bf-work-mat.est-cost + work-mat.est-cost
                  bf-work-mat.prd-qty  = bf-work-mat.prd-qty + work-mat.prd-qty
                  bf-work-mat.est-qty  = bf-work-mat.est-qty + work-mat.est-qty
                  .
                DELETE work-mat.
              END. /* if avail other item */
          END. /* each work-mat */
      
      END. /* if merge 'like' items */


      assign
       v-t-est-cost = 0
       v-t-act-cost = 0
        acl-brd = 0  .

      IF v-mat-sort EQ "Item Code" THEN
         for each work-mat break by work-mat.i-no:
            {jcrep\work-mat.i det-m}
         end.
      ELSE
         for each work-mat BREAK BY work-mat.mat-type by work-mat.i-no:          
            {jcrep\work-mat.i det-m2}
         end.

      IF tb_excel THEN
         PUT STREAM excel UNFORMATTED                       
            '"' STRING(v-t-est-cost,">>,>>>,>>9")                    '",'           
            '"' STRING(v-t-act-cost,">>,>>>,>>9")                    '",' .

      
      acl-oth = (v-t-act-cost - acl-brd) .

      v-mater = v-t-act-cost.

      {jc/rep/job-sump.i}   /***  Get the Prep/Misc Information ***/
      IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
      find first work-prep no-error.
      if avail work-prep then
        put skip(1)
                "EST"           to 47
                "ACT"           to 58       skip
                "MISC CODE"
                "DESCRIPTION"   at 13
                "COST"          to 47
                "COST"          to 58
                "VARIANCE"      to 69
                "VAR%"          to 79       skip
                fill("-",80) format "x(80)" skip.

      assign
       v-t-est-cost = 0
       v-t-act-cost = 0.

      for each work-prep BREAK BY work-prep.work-ml by work-prep.code:
          IF FIRST-OF(work-prep.work-ml) THEN
              ASSIGN
              v-sub-est-cost = 0
              v-sub-act-cost = 0
              v-sub-cst-var  = 0
              v-sub-prod-p   = 0 .
        assign
         v-cst-var = work-prep.est-cost - work-prep.act-cost
         v-prod-p  = (v-cst-var / work-prep.est-cost) * 100.00.

        if v-prod-p = ? then v-prod-p = 0.
        IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
        display work-prep.CODE format "x(11)"
                work-prep.dscr at 13
                work-prep.est-cost format ">,>>>,>>9-" to 48
                work-prep.act-cost format ">,>>>,>>9-" to 59
                v-cst-var format ">,>>>,>>9-" to 70
                v-prod-p format ">>>9.9-" to 80

            WITH FRAME det-p STREAM-IO WIDTH 200 NO-LABELS NO-BOX down.
        
        ASSIGN 
            v-sub-est-cost  = v-sub-est-cost + work-prep.est-cost 
            v-sub-act-cost  = v-sub-act-cost + work-prep.act-cost
            v-sub-cst-var   = v-sub-cst-var + v-cst-var 
            v-sub-prod-p    = /*v-sub-prod-p +*/ v-prod-p  .
        
    IF v-tot-mchg THEN do: /* task 04291401 */
        IF LAST-OF(work-prep.work-ml) AND work-prep.work-ml = "no" THEN DO:
           IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
            put "--------" to 47
              "--------" to 58
              "--------" to 69
              "------"   to 79 skip
              "  TOTAL MISC LABOR  :"
              v-sub-est-cost format ">,>>>,>>9-" to 48
              v-sub-act-cost format ">,>>>,>>9-" to 59
              v-sub-cst-var    to 70
              v-sub-prod-p     to 80
              skip(1).
        END.
        IF LAST-OF(work-prep.work-ml) AND work-prep.work-ml = "yes" THEN DO:
             IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
            put "--------" to 47
              "--------" to 58
              "--------" to 69
              "------"   to 79 skip
              "  TOTAL MISC MATERIALS   :"
              v-sub-est-cost format ">,>>>,>>9-" to 48
              v-sub-act-cost format ">,>>>,>>9-" to 59
              v-sub-cst-var  to 70
              v-sub-prod-p   to 80
              skip(1).
        END.
    END.

        assign
         v-t-est-cost = v-t-est-cost + work-prep.est-cost
         v-t-act-cost = v-t-act-cost + work-prep.act-cost.

        if last(work-prep.code) then do:
          assign
           v-cst-var = v-t-est-cost - v-t-act-cost.
           v-prod-p  = (v-cst-var / v-t-est-cost) * 100.00.

          if v-prod-p eq ? then v-prod-p = 0.
         IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
          put "--------" to 47
              "--------" to 58
              "--------" to 69
              "------"   to 79 skip
              "  TOTAL PREP/MISC  (DIRECT) :"
              v-t-est-cost format ">,>>>,>>9-" to 48
              v-t-act-cost format ">,>>>,>>9-" to 59
              v-cst-var    to 70
              v-prod-p     to 80
              skip.
        end.
      end.

      IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
      IF v-inv-tot-only THEN
        ASSIGN v-sale = getInvoiceTotal(job-hdr.ord-no, job-hdr.job-no, job-hdr.job-no2, "Price").
      ELSE
        ASSIGN v-sale = v-avg-prc * (v-t-prod / 1000).

      v-prep = v-t-act-cost.

      v-t-inv-qty = getInvoiceTotal(job-hdr.ord-no, job-hdr.job-no, job-hdr.job-no2, "Qty").
      /***  Print Grand Totals and Profit ***/
      if v-tot then DO:
        IF v-inv-tot-only THEN
          run jc/rep/job-sumt.p (/* 8/5/14 v-t-prod */ v-t-inv-qty, recid(job)).
        ELSE IF v-t-prod ne 0  THEN
          run jc/rep/job-sumt.p (v-t-prod, recid(job)).
      END.
        
      IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
      PUT skip(1)
          "Job Number: "
          trim(job.job-no) + "-" + string(job.job-no2,"99") +
          "     Closing Date: " +
          (IF job.close-date EQ ? THEN "        " ELSE
                         STRING(job.close-date,"99/99/99"))
         FORMAT "X(36)" SKIP
         "Customer:   "
         v-cust.

      page.
     end.

     if v-mch and last(job.job-no) then do:
       assign
        v-t-est-cost = round(v-gt-est-cst,0)
        v-t-act-cost = round(v-gt-act-cst,0)
        v-cst-var    = v-t-est-cost - v-t-act-cost
        v-cst-var    = round(v-cst-var,0)
        v-t-act-cost = round(v-t-act-cost,0)
        v-prod-p     = (v-cst-var / v-t-est-cost) * 100.00
        v-prod-p     = round(v-prod-p,1)

        v-t-est-hrs  = round(v-gt-est-hrs,2)
        v-t-act-hrs  = round(v-gt-act-hrs,1).

       if v-prod-p = ? then v-prod-p = 0.
        IF (LINE-COUNTER + 10) GT lines-per-page THEN PAGE.
       put skip(3)
           "------" at 37 "--------  ------" at 51
           "--------  --------  ------" at 76 skip
           "GRAND TOTAL CONVERSION (DIRECT) :"
           v-t-est-hrs at 35 v-t-est-cost at 50 " "
           v-t-act-hrs v-t-act-cost at 75 " " v-cst-var v-prod-p SKIP.
     end.   

     IF tb_excel THEN
         PUT STREAM excel UNFORMATTED                       
            '"' STRING(v-comm,"->>,>>>,>>9.99")                '",'           
            '"' STRING(v-frate,"->>,>>>,>>9.99")               '",'
            '"' STRING(v-sale,"->>,>>>,>>9.99")                '",' 
         '"' STRING(v-misc-prep,"->>,>>>,>>9.99")                '",'  
         '"' STRING(v-misc-prep + v-sale,"->>,>>>,>>9.99")                '",'  
            '"' STRING(v-total,"->>,>>>,>>9.99")               '",'
            '"' STRING(part-no)               '",'
            '"' STRING(i-name)               '",'
            '"' STRING(cat)               '",'
            '"' STRING(acl-brd,"->>,>>>,>>9")               '",'
            '"' STRING(acl-oth,"->>,>>>,>>9")               '",'
            '"' STRING(acl-lbr,"->>,>>>,>>9")               '",'
            '"' STRING(v-t-prod,"->>,>>>,>>9")               '",'
            SKIP.     
     
    end. /* each job */  
