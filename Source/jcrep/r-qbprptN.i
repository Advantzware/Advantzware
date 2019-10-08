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
            ASSIGN 
                cstat = "" .
            FIND FIRST oe-ord NO-LOCK WHERE oe-ord.company EQ cocode 
                                AND oe-ord.ord-no EQ job-hdr.ord-no NO-ERROR .  
            IF AVAIL oe-ord THEN ASSIGN cstat = oe-ord.stat .
            IF cstat NE "" THEN DO:
                DO i = 1 TO NUM-ENTRIES(vstatus,","):
                    IF cstat = TRIM(ENTRY(i,vstatus)) THEN DO:
                        ASSIGN cstat = TRIM(ENTRY(i,vstatus-desc)) .
                        LEAVE.
                    END.
                END.
            END.

            IF job.stat = "H" THEN DO: 
                FIND FIRST rejct-cd NO-LOCK WHERE rejct-cd.type = "JH" 
                    AND rejct-cd.code = job.reason  NO-ERROR.
                IF AVAIL rejct-cd THEN
                    ASSIGN
                    vHoldReason  =  rejct-cd.dscr.      
            END.
            ELSE 
                vHoldReason  = "".
         
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
         
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "job"    THEN cVarValue = IF v-job NE ? THEN string(v-job,"x(9)") ELSE "" .
                         WHEN "sn"   THEN cVarValue = string(job-mat.frm,">>>").
                         WHEN "bn"   THEN cVarValue = STRING(job-mat.blank-no,">>>").
                         WHEN "date"  THEN cVarValue = IF job-hdr.due-date NE ? THEN STRING(job-hdr.due-date,"99/99/9999") ELSE "".
                         WHEN "cust"   THEN cVarValue = STRING(job-hdr.cust-no,"x(11)") .
                         WHEN "board"  THEN cVarValue = STRING(job-mat.rm-i-no,"x(14)") .
                         WHEN "s-wid"   THEN cVarValue = IF v-width NE ? THEN STRING(v-width,"->>>,>>9.99") ELSE "" .
                         WHEN "s-len"  THEN cVarValue = IF v-length NE ? THEN STRING(v-length,"->>>>,>>9.99") ELSE "" .
                         WHEN "sheet"  THEN cVarValue = IF v-qty NE ? THEN STRING(v-qty,"->>,>>>,>>9") ELSE "".
                         WHEN "lbs"   THEN cVarValue = IF v-lbs NE ? THEN STRING(v-lbs,"->>,>>>,>>9") ELSE "".
                         WHEN "lf"  THEN cVarValue = IF v-lf NE ? THEN STRING(v-lf,"->>,>>>,>>9") ELSE "".
                         WHEN "job-rson"  THEN cVarValue = STRING(vHoldReason) .
                         WHEN "ord-sts"  THEN cVarValue =  STRING(cstat) .
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
             PUT STREAM excel UNFORMATTED  
                   cExcelDisplay SKIP.
            END.

        END.
    end. /* each job */  
