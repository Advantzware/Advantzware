/****************************************************
jcrep\qbprpt.i
*****************************************************/    

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
          and ( (job.start-date = ? AND job.opened)     OR
                (job.close-date       ge v-date[1] and
                 job.close-date       le v-date[2] and
                 NOT job.opened)                        OR 
                (job.start-date       ge v-date[1] and
                 job.start-date       le v-date[2] and
                 job.opened)
               )
          and (v-ind eq "B"                                     or
               (v-ind eq "F" and
                can-find(first est where est.company    EQ job.company
                                     AND est.est-no     EQ job.est-no
                                     and est.est-type le 4))    or
               (v-ind eq "C" and
                can-find(first est where est.company    EQ job.company
                                     AND est.est-no     EQ job.est-no
                                     and est.est-type gt 4)))
        NO-LOCK,
         EACH job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
          NO-LOCK,
         EACH job-mat NO-LOCK
         WHERE job-mat.company EQ job.company
           AND job-mat.job     EQ job.job
           AND job-mat.job-no  EQ job.job-no
           AND job-mat.job-no2 EQ job.job-no2
           AND job-mat.all-flg = NO, /* exclude those that are already allocated */
        first item
        where item.company eq job-mat.company
          and item.i-no    eq job-mat.rm-i-no
          and (item.mat-type eq "B" OR item.mat-type eq "P")
               NO-LOCK 
        break by job.job-no
              by job.job-no2
              BY job-mat.frm
              BY job-mat.blank-no:
        
        {custom/statusMsg.i " 'Processing Job#  '  + string(job-hdr.job) "}

        FIND FIRST est where est.company    EQ job.company
                         AND est.est-no     EQ job.est-no
             NO-LOCK NO-ERROR.

        FIND first ef WHERE ef.company EQ job-hdr.company
                        AND ef.est-no  EQ job-hdr.est-no
                        AND ef.form-no EQ job-hdr.frm NO-LOCK NO-ERROR.
        /*IF AVAIL ef THEN DO:
            ASSIGN v-nsh-wid = ef.nsh-wid
                   v-nsh-len = ef.nsh-len.
        END.*/

        ASSIGN v-act-qty = 0.
        for each mat-act
            where mat-act.company eq cocode
              and mat-act.job     eq job-mat.job
              and mat-act.s-num   eq job-mat.frm
              and mat-act.b-num   eq job-mat.blank-no
              and mat-act.i-no    eq job-mat.i-no
            use-index job no-lock:
            ASSIGN v-act-qty = v-act-qty + mat-act.qty.
        END.

        IF v-act-qty > 0 THEN NEXT. /* exclude those which are issued, actual qty > 0 */

        ASSIGN v-qty = 0 v-lf = 0.

        IF v-sheet = "G" THEN 
            ASSIGN v-width = job-mat.wid
                   v-length = job-mat.len.
        ELSE
        IF v-sheet = "N" THEN 
            ASSIGN v-width = (IF AVAIL ef THEN ef.nsh-wid ELSE 0)
                   v-length = (IF AVAIL ef THEN ef.nsh-len ELSE 0).


        run sys/ref/convquom.p(job-mat.qty-uom, "EA", job-mat.basis-w,
                               v-length, v-width, item.s-dep,
                               job-mat.qty, output v-qty).
        run sys/ref/convquom.p(job-mat.qty-uom, "LF", job-mat.basis-w,
                               v-length, v-width, item.s-dep,
                               job-mat.qty, output v-lf).

        release cust.
        v-cust = "".

        find first cust
             where cust.company eq cocode
               and cust.cust-no eq job-hdr.cust-no
               no-lock no-error.
        if avail cust then v-cust = cust.name.

        ASSIGN v-job   = fill(" ",6 - length(trim(job-hdr.job-no))) +
                         trim(job-hdr.job-no) + "-" + string(job-hdr.job-no2,"99")
               v-lbs = job-mat.qty * (v-width * v-length / 144) / 1000 * ITEM.basis-w
               .

        IF FIRST-OF(job-mat.blank-no) THEN DO:
          display                                                            
             v-job
             job-mat.frm     
             job-mat.blank-no
             job-hdr.due-date FORM "99/99/99"
             job-hdr.cust-no FORM "x(11)"
             job-mat.rm-i-no  FORM "x(18)" SPACE(1)
             v-width  SPACE(8)
             v-length
             v-qty        FORM "->>,>>>,>>9"
             v-lbs        FORM "->>,>>>,>>9"
             v-lf         FORM "->>,>>>,>>9"
             with frame det STREAM-IO width 200 no-labels no-box down.
          DOWN WITH FRAME det.

       IF tb_excel THEN
        PUT STREAM excel UNFORMATTED
          '"' v-job               '",'
          '"' string(job-mat.frm)               '",'
          '"' string(job-mat.blank-no)               '",'
          '"' IF job-hdr.due-date NE ? THEN string(job-hdr.due-date) ELSE ""               '",'
          '"' job-hdr.cust-no               '",'
          '"' job-mat.rm-i-no               '",'
          '"' string(v-width)               '",'
          '"' string(v-length)               '",'
          '"' string(v-qty)               '",'
          '"' string(v-lbs)               '",'
          '"' string(v-lf)               '",'
          SKIP.

        END.
    end. /* each job */  
