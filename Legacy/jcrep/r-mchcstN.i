
DEF VAR ll-act-rate AS LOG NO-UNDO.
DEF VAR ld-tot-rate AS DEC NO-UNDO.


    for each mch-act
        where mch-act.company eq cocode
          and mch-act.m-code  ge v-fmach
          and mch-act.m-code  le v-tmach
          and mch-act.job-no  ge substr(v-fjob,1,6)
          and mch-act.job-no  le substr(v-tjob,1,6)
          and fill(" ",6 - length(trim(mch-act.job-no))) +
              trim(mch-act.job-no) + string(mch-act.job-no2,"99")
                              ge v-fjob
          and fill(" ",6 - length(trim(mch-act.job-no))) +
              trim(mch-act.job-no) + string(mch-act.job-no2,"99")
                              le v-tjob
        use-index operation no-lock,

        first job
        where job.company            eq cocode
          and job.job                eq mch-act.job
          and (v-stat                eq "A"                     or
               (v-stat               eq "O" and job.opened)     or
               (v-stat               eq "C" and NOT job.opened))
          and ((job.close-date       ge v-fdate and
                job.close-date       le v-tdate and
                NOT job.opened)                                 or
               (job.start-date       ge v-fdate and
                job.start-date       le v-tdate and
                job.opened))
        no-lock,

        first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.job     eq job.job
          and job-hdr.job-no  eq job.job-no
          and job-hdr.job-no2 eq job.job-no2
        no-lock,

        first mach
        where mach.company eq cocode
          and mach.loc     eq locode
          and mach.m-code  eq mch-act.m-code
        no-lock,

        first job-code where job-code.code eq mch-act.code no-lock

        break by mch-act.m-code
              by mch-act.job-no
              by mch-act.job-no2
              by mch-act.frm
              by mch-act.blank-no
              by mch-act.pass:
      {custom/statusMsg.i " 'Processing Machine#  '  + mach.m-code "}
      if first-of(mch-act.m-code) then v-frst = yes.

      RUN jc/getactrt.p (ROWID(mch-act), OUTPUT ll-act-rate, OUTPUT ld-tot-rate).

      v-tot-fg[1] = v-tot-fg[1] + mch-act.qty + mch-act.waste.
                       
      if job-code.cat eq "RUN" or job-code.cat eq "DT" then
        v-run-fg[1] = v-run-fg[1] + mch-act.qty.

      if job-code.cat eq "MR" then
        assign
         v-mr-act-dl[1] = v-mr-act-dl[1] +
                          (mch-act.hours *
                           IF ll-act-rate THEN ld-tot-rate
                           ELSE (mach.mr-rate / 
                                 (IF mach.mr-crusiz EQ 0 THEN 1 ELSE mach.mr-crusiz) *
                                 mch-act.crew))
         v-mr-act-fo[1] = v-mr-act-fo[1] + (mach.mr-fixoh * mch-act.hours)
         v-mr-act-vo[1] = v-mr-act-vo[1] + (mach.mr-varoh * mch-act.hours).

      else
      if job-code.cat eq "RUN" or
         job-code.cat eq "DT"  then
        assign
         v-run-act-dl[1] = v-run-act-dl[1] +
                          (mch-act.hours *
                           IF ll-act-rate THEN ld-tot-rate
                           ELSE (mach.run-rate / 
                                 (IF mach.run-crusiz EQ 0 THEN 1 ELSE mach.run-crusiz) *
                                 mch-act.crew))
         v-run-act-fo[1] = v-run-act-fo[1] + (mach.mr-fixoh * mch-act.hours)
         v-run-act-vo[1] = v-run-act-vo[1] + (mach.mr-varoh * mch-act.hours).
        
      if last-of(mch-act.pass) then do:
        assign
         v-up     = 1
         v-out    = 1
         v-on     = 1.

        find est where est.company eq job.company
                 AND   est.est-no  EQ job.est-no
                no-lock no-error.

        if avail est and INDEX("AP",mach.p-type) LE 0 then do:
          find first ef
              where ef.company eq est.company
                AND ef.est-no  eq est.est-no
                and ef.form-no eq mch-act.frm
              no-lock no-error.

          IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-on).

          find first est-op
              where est-op.company eq est.company
                AND est-op.est-no  EQ est.est-no
                and est-op.s-num   eq mch-act.frm
                and est-op.b-num   eq mch-act.blank-no
                and est-op.m-code  eq mch-act.m-code
                and est-op.op-pass eq mch-act.pass
                and est-op.dept    eq mch-act.dept
                and est-op.line    lt 500
              no-lock no-error.

          if ((avail est-op) and est-op.op-sb)           or
             ((not avail est-op) and mach.p-type ne "B") then do:

            if avail est-op then
              run sys/inc/numout.p (recid(est-op), output v-out).

            else v-out = 1.
          end.

          v-on = v-on / v-out.

          if mach.p-type eq "B" then do:
            if est.est-type eq 4 or est.est-type eq 8 then do:
              find first eb
                  where eb.company  eq est.company
                    AND eb.est-no   EQ est.est-no
                    and eb.form-no  eq mch-act.frm
                    and eb.blank-no eq mch-act.blank-no
                  no-lock no-error.
              if avail eb then v-up = eb.num-up.
            end.

            else run sys/inc/numup.p (est.company, est.est-no, mch-act.frm, output v-up).
          end.

          v-on = v-on * v-up.
        end.

        v-up-hs = 1.

        if mch-act.dept eq "HS" then do:
          if avail est          and
             mach.therm         and
             mach.p-type eq "S" then
            run sys/inc/numup.p (est.company, est.est-no, mch-act.frm, output v-up-hs).

          assign
           v-est-fg    = v-est-fg / v-up-hs
           v-tot-fg[1] = v-tot-fg[1] / v-up-hs.
        end.

        assign
         v-est-fg    = v-est-fg / v-on
         v-tot-fg[1] = v-tot-fg[1] / v-on.

        for each job-mat
            where job-mat.company eq cocode
              and job-mat.job     eq job.job
              and job-mat.frm     eq mch-act.frm
            no-lock,

            first item
            where item.company  eq cocode
              and item.i-no     eq job-mat.i-no
              and item.mat-type eq "B"
            no-lock:

          run sys/ref/convcuom.p(job-mat.sc-uom, "MSH", job-mat.basis-w,
                                 job-mat.len, job-mat.wid, item.s-dep,
                                 job-mat.std-cost, output v-cost).

          assign
           v-mat-est[1] = v-mat-est[1] + if v-est-fg eq 0 then 0
                                         else (v-est-fg / 1000 * v-cost)
           v-tqty       = 0
           v-tcost      = 0.

          for each mat-act
              where mat-act.company eq cocode
                and mat-act.job     eq job-mat.job
                and mat-act.s-num   eq job-mat.frm
                and mat-act.b-num   eq job-mat.blank-no
                and mat-act.i-no    eq job-mat.i-no
              no-lock:

            if mat-act.qty-uom eq "EA" then
              v-qty = mat-act.qty.
            else
              run sys/ref/convcuom.p(mat-act.qty-uom, "EA", job-mat.basis-w,
                                     job-mat.len, job-mat.wid, item.s-dep,
                                     mat-act.qty, output v-qty).

            v-tqty  = v-tqty  + v-qty.

            if job-mat.sc-uom eq "EA" then
              v-cost = mat-act.cost.
            else
              run sys/ref/convcuom.p(job-mat.sc-uom, "EA", job-mat.basis-w,
                                     job-mat.len, job-mat.wid, item.s-dep,
                                     mat-act.cost, output v-cost).

            IF mat-act.ext-cost EQ 0 OR mat-act.ext-cost EQ ? THEN
              v-tcost = v-tcost + (v-qty * v-cost).
            ELSE
              v-tcost = v-tcost + mat-act.ext-cost.
          end.

          if v-tqty ne 0 and v-tcost ne 0 then
            v-mat-act[1] = v-mat-act[1] + (v-tcost / v-tqty * v-tot-fg[1]).
        end.

        assign
         v-speed  = 0
         v-run-hr = 0.

        /*for each job-mch
            where job-mch.company  eq cocode
              and job-mch.job      eq mch-act.job
              and job-mch.job-no   eq mch-act.job-no
              and job-mch.job-no2  eq mch-act.job-no2
              and job-mch.m-code   eq mch-act.m-code
              and job-mch.frm      eq mch-act.frm
              and job-mch.blank-no eq mch-act.blank-no
              and job-mch.pass     eq mch-act.pass
            no-lock:

          assign
           v-speed  = ((v-speed * v-run-hr) +
                       (job-mch.speed * job-mch.run-hr)) /
                       (v-run-hr + job-mch.run-hr)
           v-run-hr = v-run-hr + job-mch.run-hr.
        end.*/

        for each job-mch
            where job-mch.company  eq cocode
              and job-mch.job      eq mch-act.job
              and job-mch.job-no   eq mch-act.job-no
              and job-mch.job-no2  eq mch-act.job-no2
              and job-mch.m-code   eq mch-act.m-code
              and job-mch.frm      eq mch-act.frm
              and job-mch.blank-no eq mch-act.blank-no
              and job-mch.pass     eq mch-act.pass
            no-lock:

          assign
           /*v-est-fg = v-run-fg[2] +
                      (v-run-fg[2] * job-mch.wst-prct / 100) + job-mch.mr-waste*/
           /*v-est-hr = if v-speed eq 0 then 0
                      else (/*v-est-fg*/ v-run-fg[1] / v-speed)*/
           v-est-hr = v-run-fg[1] / job-mch.speed

           v-mr-est-dl[1] = v-mr-est-dl[1] + (mach.mr-rate  * job-mch.mr-hr)
           v-mr-est-fo[1] = v-mr-est-fo[1] + (mach.mr-fixoh * job-mch.mr-hr)
           v-mr-est-vo[1] = v-mr-est-vo[1] + (mach.mr-varoh * job-mch.mr-hr).
           
          if v-est-hr ne 0 then
            assign
             v-run-est-dl[1] = v-run-est-dl[1] + (mach.run-rate  * v-est-hr)
             v-run-est-fo[1] = v-run-est-fo[1] + (mach.run-fixoh * v-est-hr)
             v-run-est-vo[1] = v-run-est-vo[1] + (mach.run-varoh * v-est-hr).
        end.

        assign
         v-tot-fg[2] = v-tot-fg[2] + v-tot-fg[1]
         v-run-fg[2] = v-run-fg[2] + v-run-fg[1]

         v-tot-fg[1] = 0
         v-run-fg[1] = 0.
      end.

      if last-of(mch-act.job-no2) then do:
        find first itemfg
            where itemfg.company eq cocode
              and itemfg.i-no    eq job-hdr.i-no
            no-lock no-error.

        assign
         v-job = fill(" ",6 - length(trim(mch-act.job-no))) +
                 trim(mch-act.job-no) + "-" + string(mch-act.job-no2,"99")

         v-mr-act[1]  = v-mr-act-dl[1]  + v-mr-act-fo[1]  + v-mr-act-vo[1]
         v-run-act[1] = v-run-act-dl[1] + v-run-act-fo[1] + v-run-act-vo[1]

         v-mr-est[1]  = v-mr-est-dl[1]  + v-mr-est-fo[1]  + v-mr-est-vo[1]
         v-run-est[1] = v-run-est-dl[1] + v-run-est-fo[1] + v-run-est-vo[1]

         v-mr-var  = (v-mr-est[1]  - v-mr-act[1])  / v-mr-est[1]  * 100
         v-run-var = (v-run-est[1] - v-run-act[1]) / v-run-est[1] * 100
         v-mat-var = (v-mat-est[1] - v-mat-act[1]) / v-mat-est[1] * 100.

        if v-mr-var  = ? then v-mr-var  = 0.
        if v-run-var = ? then v-run-var = 0.
        if v-mat-var = ? then v-mat-var = 0.

     /*   display mch-act.m-code      column-label "MACH!CODE"
                                    when v-frst
                v-job               column-label "    JOB #"
                itemfg.i-name       format "x(20)"
                                    column-label "FG DESCRIPTION"
                                    when avail itemfg
                v-mr-est[1]         column-label "EST M/R"
                v-mr-act[1]         column-label "ACT M/R"
                v-mr-var            column-label "%VAR M/R"
                v-run-est[1]        column-label "EST RUN"
                v-run-act[1]        column-label "ACT RUN"
                v-run-var           column-label "%VAR RUN"
                v-mat-est[1]        column-label "EST BRD"
                v-mat-act[1]        column-label "ACT BRD"
                v-mat-var           column-label "%VAR MAT"
          with frame det STREAM-IO width 132 no-box down.

IF tb_excel THEN 
   EXPORT STREAM excel DELIMITER ","
        (IF v-frst THEN mch-act.m-code ELSE "")
        v-job 
        (IF AVAILABLE itemfg THEN itemfg.i-name ELSE "")
        v-mr-est[1]
        v-mr-act[1] 
        v-mr-var   
        v-run-est[1]
        v-run-act[1]
        v-run-var   
        v-mat-est[1]
        v-mat-act[1]
        v-mat-var
       SKIP. */

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = IF v-frst THEN mch-act.m-code ELSE "" .
                         WHEN "job"         THEN cVarValue = string(v-job) .
                         WHEN "fg-dsc"      THEN cVarValue = IF AVAILABLE itemfg THEN itemfg.i-name ELSE "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est[1] NE ? THEN string(v-mr-est[1],"->>>>>9.99")  ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act[1] NE ? THEN string(v-mr-act[1],"->>>>>9.99")  ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = string(v-mr-var,"->>>9.99") .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est[1] NE ? THEN string(v-run-est[1],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act[1] NE ? THEN string(v-run-act[1],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = string(v-run-var,"->>>9.99") .
                         WHEN "est-brd"     THEN cVarValue = IF v-mat-est[1] NE ? THEN string(v-mat-est[1],"->>>>>9.99") ELSE "" .
                         WHEN "act-brd"     THEN cVarValue = IF v-mat-act[1] NE ? THEN string(v-mat-act[1],"->>>>>9.99") ELSE "" .
                         WHEN "act-mat"     THEN cVarValue = string(v-mat-var,"->>>9.99") .
                         WHEN "fg-item"      THEN cVarValue = IF AVAILABLE itemfg THEN itemfg.i-no ELSE "" .
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

          
        if show-direct-labor then
        DO: /*with frame det:*/
          /*down.
          disp 'Direct Labor' @ itemfg.i-name
               v-mr-est-dl[1] when v-mr-est-dl[1] ne 0 @ v-mr-est[1]
               v-mr-act-dl[1] when v-mr-act-dl[1] ne 0 @ v-mr-act[1]
               v-run-est-dl[1] when v-run-est-dl[1] ne 0 @ v-run-est[1]
               v-run-act-dl[1] when v-run-act-dl[1] ne 0 @ v-run-act[1]. */

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-dl[1] NE ? THEN string(v-mr-est-dl[1],"->>>>>9.99") ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-dl[1] NE ? THEN  string(v-mr-act-dl[1],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-dl[1] NE ? THEN  string(v-run-est-dl[1],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-dl[1] NE ? THEN  string(v-run-act-dl[1],"->>>>>9.99") ELSE "".
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Direct Labor "   substring(cDisplay,24,250) SKIP.
        end.

        if show-fixed-overhead then
        do: /*with frame det:*/
         /* down.
          disp 'Fixed Overhead' @ itemfg.i-name
               v-mr-est-fo[1] when v-mr-est-fo[1] ne 0 @ v-mr-est[1]
               v-mr-act-fo[1] when v-mr-act-fo[1] ne 0 @ v-mr-act[1]
               v-run-est-fo[1] when v-run-est-fo[1] ne 0 @ v-run-est[1]
               v-run-act-fo[1] when v-run-act-fo[1] ne 0 @ v-run-act[1]. */
            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-fo[1] NE ? THEN string(v-mr-est-fo[1],"->>>>>9.99") ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-fo[1] NE ? THEN string(v-mr-act-fo[1],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-fo[1] NE ? THEN string(v-run-est-fo[1],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-fo[1] NE ? THEN string(v-run-act-fo[1],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Fixed Overhead "   substring(cDisplay,26,250) SKIP.

        end.

        if show-variable-overhead then
        do: /*with frame det:*/
          /*down.
          disp 'Variable Overhead' @ itemfg.i-name
               v-mr-est-vo[1] when v-mr-est-vo[1] ne 0 @ v-mr-est[1]
               v-mr-act-vo[1] when v-mr-act-vo[1] ne 0 @ v-mr-act[1]
               v-run-est-vo[1] when v-run-est-vo[1] ne 0 @ v-run-est[1]
               v-run-act-vo[1] when v-run-act-vo[1] ne 0 @ v-run-act[1]. */

            ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-vo[1] NE ? THEN string(v-mr-est-vo[1],"->>>>>9.99") ELSE "" .
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-vo[1] NE ? THEN string(v-mr-act-vo[1],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-vo[1] NE ? THEN string(v-run-est-vo[1],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-vo[1] NE ? THEN string(v-run-act-vo[1],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Variable Overhead "   substring(cDisplay,29,250) SKIP.

        end.

        assign
         v-mr-est[2]  = v-mr-est[2]  + v-mr-est[1]
         v-mr-act[2]  = v-mr-act[2]  + v-mr-act[1]
         v-run-est[2] = v-run-est[2] + v-run-est[1]
         v-run-act[2] = v-run-act[2] + v-run-act[1]
         v-mat-est[2] = v-mat-est[2] + v-mat-est[1]
         v-mat-act[2] = v-mat-act[2] + v-mat-act[1]
         v-prd-fg[2]  = v-prd-fg[2]  + v-prd-fg[1]
         v-tot-rm[2]  = v-tot-rm[2]  + v-tot-rm[1]

         v-tot-fg[2]   = 0
         v-run-fg[2]   = 0
         v-mr-est[1]   = 0
         v-mr-act[1]   = 0
         v-run-est[1]  = 0
         v-run-act[1]  = 0
         v-mat-est[1]  = 0
         v-mat-act[1]  = 0
         v-prd-fg[1]   = 0
         v-tot-rm[1]   = 0

         v-mr-act-dl[2] = v-mr-act-dl[2] + v-mr-act-dl[1]
         v-mr-act-fo[2] = v-mr-act-fo[2] + v-mr-act-fo[1]
         v-mr-act-vo[2] = v-mr-act-vo[2] + v-mr-act-vo[1]
         v-mr-est-dl[2] = v-mr-est-dl[2] + v-mr-est-dl[1]
         v-mr-est-fo[2] = v-mr-est-fo[2] + v-mr-est-fo[1]
         v-mr-est-vo[2] = v-mr-est-vo[2] + v-mr-est-vo[1]
         v-run-act-dl[2] = v-run-act-dl[2] + v-run-act-dl[1]
         v-run-act-fo[2] = v-run-act-fo[2] + v-run-act-fo[1]
         v-run-act-vo[2] = v-run-act-vo[2] + v-run-act-vo[1]
         v-run-est-dl[2] = v-run-est-dl[2] + v-run-est-dl[1]
         v-run-est-fo[2] = v-run-est-fo[2] + v-run-est-fo[1]
         v-run-est-vo[2] = v-run-est-vo[2] + v-run-est-vo[1]

         v-mr-act-dl[1]  = 0
         v-mr-act-fo[1]  = 0
         v-mr-act-vo[1]  = 0
         v-mr-est-dl[1]  = 0
         v-mr-est-fo[1]  = 0
         v-mr-est-vo[1]  = 0
         v-run-act-dl[1] = 0
         v-run-act-fo[1] = 0
         v-run-act-vo[1] = 0
         v-run-est-dl[1] = 0
         v-run-est-fo[1] = 0
         v-run-est-vo[1] = 0.
      end.

      if last-of(mch-act.m-code) then do:
        if not v-frst then do:
          put skip(1).

          assign
           v-mr-var  = (v-mr-est[2]  - v-mr-act[2])  / v-mr-est[2]  * 100
           v-run-var = (v-run-est[2] - v-run-act[2]) / v-run-est[2] * 100
           v-mat-var = (v-mat-est[2] - v-mat-act[2]) / v-mat-est[2] * 100.

          if v-mr-var  = ? then v-mr-var  = 0.
          if v-run-var = ? then v-run-var = 0.
          if v-mat-var = ? then v-mat-var = 0.

          /*display ""                  @ v-job
                  "Machine Totals"    @ itemfg.i-name
                  v-mr-est[2]         @ v-mr-est[1]
                  v-mr-act[2]         @ v-mr-act[1]
                  v-mr-var
                  v-run-est[2]        @ v-run-est[1]
                  v-run-act[2]        @ v-run-act[1]
                  v-run-var
                  v-mat-est[2]        @ v-mat-est[1]
                  v-mat-act[2]        @ v-mat-act[1]
                  v-mat-var

              with frame det STREAM-IO width 132 no-box down. */
          PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est[2] NE ? THEN string(v-mr-est[2],"->>>>>9.99")  ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act[2] NE ? THEN string(v-mr-act[2],"->>>>>9.99")  ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = string(v-mr-var,"->>>9.99") .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est[2] NE ? THEN string(v-run-est[2],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act[2] NE ? THEN string(v-run-act[2],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = string(v-run-var,"->>>9.99") .
                         WHEN "est-brd"     THEN cVarValue = IF v-mat-est[2] NE ? THEN string(v-mat-est[2],"->>>>>9.99") ELSE "" .
                         WHEN "act-brd"     THEN cVarValue = IF v-mat-act[2] NE ? THEN string(v-mat-act[2],"->>>>>9.99") ELSE "" .
                         WHEN "act-mat"     THEN cVarValue = string(v-mat-var,"->>>9.99") .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Machine Totals "   substring(cDisplay,26,250) SKIP.

          if show-direct-labor then
          do: /*with frame det:*/
            /*down.
            disp 'Direct Labor' @ itemfg.i-name
                 v-mr-est-dl[2] when v-mr-est-dl[2] ne 0 @ v-mr-est[1]
                 v-mr-act-dl[2] when v-mr-act-dl[2] ne 0 @ v-mr-act[1]
                 v-run-est-dl[2] when v-run-est-dl[2] ne 0 @ v-run-est[1]
                 v-run-act-dl[2] when v-run-act-dl[2] ne 0 @ v-run-act[1].*/

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-dl[2] NE ? THEN string(v-mr-est-dl[2],"->>>>>9.99") ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-dl[2] NE ? THEN  string(v-mr-act-dl[2],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-dl[2] NE ? THEN  string(v-run-est-dl[2],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-dl[2] NE ? THEN  string(v-run-act-dl[2],"->>>>>9.99") ELSE "".
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Direct Labor "   substring(cDisplay,24,250) SKIP.

          end.
          if show-fixed-overhead then
          do: /*with frame det: */
            /*down.
            disp 'Fixed Overhead' @ itemfg.i-name
                 v-mr-est-fo[2] when v-mr-est-fo[2] ne 0 @ v-mr-est[1]
                 v-mr-act-fo[2] when v-mr-act-fo[2] ne 0 @ v-mr-act[1]
                 v-run-est-fo[2] when v-run-est-fo[2] ne 0 @ v-run-est[1]
                 v-run-act-fo[2] when v-run-act-fo[2] ne 0 @ v-run-act[1]. */

               ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-fo[2] NE ? THEN string(v-mr-est-fo[2],"->>>>>9.99") ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-fo[2] NE ? THEN string(v-mr-act-fo[2],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-fo[2] NE ? THEN string(v-run-est-fo[2],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-fo[2] NE ? THEN string(v-run-act-fo[2],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Fixed Overhead "   substring(cDisplay,26,250) SKIP.
          end.
          if show-variable-overhead then
          do: /*with frame det:*/
            /*down.
            disp 'Variable Overhead' @ itemfg.i-name
                 v-mr-est-vo[2] when v-mr-est-vo[2] ne 0 @ v-mr-est[1]
                 v-mr-act-vo[2] when v-mr-act-vo[2] ne 0 @ v-mr-act[1]
                 v-run-est-vo[2] when v-run-est-vo[2] ne 0 @ v-run-est[1]
                 v-run-act-vo[2] when v-run-act-vo[2] ne 0 @ v-run-act[1]. */

              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-vo[2] NE ? THEN string(v-mr-est-vo[2],"->>>>>9.99") ELSE "" .
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-vo[2] NE ? THEN string(v-mr-act-vo[2],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-vo[2] NE ? THEN string(v-run-est-vo[2],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-vo[2] NE ? THEN string(v-run-act-vo[2],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Variable Overhead "   substring(cDisplay,29,250) SKIP.

          end.
        end.

        put skip(2).

        assign
         v-mr-est[3]  = v-mr-est[3]  + v-mr-est[2]
         v-mr-act[3]  = v-mr-act[3]  + v-mr-act[2]
         v-run-est[3] = v-run-est[3] + v-run-est[2]
         v-run-act[3] = v-run-act[3] + v-run-act[2]
         v-mat-est[3] = v-mat-est[3] + v-mat-est[2]
         v-mat-act[3] = v-mat-act[3] + v-mat-act[2]
         v-prd-fg[3]  = v-prd-fg[3]  + v-prd-fg[2]
         v-tot-rm[3]  = v-tot-rm[3]  + v-tot-rm[2]

         v-mr-est[2]  = 0
         v-mr-act[2]  = 0
         v-run-est[2] = 0
         v-run-act[2] = 0
         v-mat-est[2] = 0
         v-mat-act[2] = 0
         v-prd-fg[2]  = 0
         v-tot-rm[2]  = 0

         v-mr-act-dl[3] = v-mr-act-dl[3] + v-mr-act-dl[2]
         v-mr-act-fo[3] = v-mr-act-fo[3] + v-mr-act-fo[2]
         v-mr-act-vo[3] = v-mr-act-vo[3] + v-mr-act-vo[2]
         v-mr-est-dl[3] = v-mr-est-dl[3] + v-mr-est-dl[2]
         v-mr-est-fo[3] = v-mr-est-fo[3] + v-mr-est-fo[2]
         v-mr-est-vo[3] = v-mr-est-vo[3] + v-mr-est-vo[2]
         v-run-act-dl[3] = v-run-act-dl[3] + v-run-act-dl[2]
         v-run-act-fo[3] = v-run-act-fo[3] + v-run-act-fo[2]
         v-run-act-vo[3] = v-run-act-vo[3] + v-run-act-vo[2]
         v-run-est-dl[3] = v-run-est-dl[3] + v-run-est-dl[2]
         v-run-est-fo[3] = v-run-est-fo[3] + v-run-est-fo[2]
         v-run-est-vo[3] = v-run-est-vo[3] + v-run-est-vo[2]

         v-mr-act-dl[2] = 0
         v-mr-act-fo[2] = 0
         v-mr-act-vo[2] = 0
         v-mr-est-dl[2] = 0
         v-mr-est-fo[2] = 0
         v-mr-est-vo[2] = 0
         v-run-act-dl[2] = 0
         v-run-act-fo[2] = 0
         v-run-act-vo[2] = 0
         v-run-est-dl[2] = 0
         v-run-est-fo[2] = 0
         v-run-est-vo[2] = 0.
      end.

      if last-of(mch-act.job-no2) then v-frst = no.

      if last(mch-act.m-code) then do:
        put skip(1).

        assign
         v-mr-var  = (v-mr-est[3]  - v-mr-act[3])  / v-mr-est[3]  * 100
         v-run-var = (v-run-est[3] - v-run-act[3]) / v-run-est[3] * 100
         v-mat-var = (v-mat-est[3] - v-mat-act[3]) / v-mat-est[3] * 100.

        if v-mr-var  = ? then v-mr-var  = 0.
        if v-run-var = ? then v-run-var = 0.
        if v-mat-var = ? then v-mat-var = 0.

        /*display ""                  @ mch-act.m-code
                ""                  @ v-job
                "Grand Totals"      @ itemfg.i-name
                v-mr-est[3]         @ v-mr-est[1]
                v-mr-act[3]         @ v-mr-act[1]
                v-mr-var
                v-run-est[3]        @ v-run-est[1]
                v-run-act[3]        @ v-run-act[1]
                v-run-var
                v-mat-est[3]        @ v-mat-est[1]
                v-mat-act[3]        @ v-mat-act[1]
                v-mat-var

            with frame det STREAM-IO width 132 no-box down. */

        PUT SKIP str-line SKIP .
          ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est[3] NE ? THEN string(v-mr-est[3],"->>>>>9.99")  ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act[3] NE ? THEN string(v-mr-act[3],"->>>>>9.99")  ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = string(v-mr-var,"->>>9.99") .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est[3] NE ? THEN string(v-run-est[3],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act[3] NE ? THEN string(v-run-act[3],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = string(v-run-var,"->>>9.99") .
                         WHEN "est-brd"     THEN cVarValue = IF v-mat-est[3] NE ? THEN string(v-mat-est[3],"->>>>>9.99") ELSE "" .
                         WHEN "act-brd"     THEN cVarValue = IF v-mat-act[3] NE ? THEN string(v-mat-act[3],"->>>>>9.99") ELSE "" .
                         WHEN "act-mat"     THEN cVarValue = string(v-mat-var,"->>>9.99") .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Grand Totals "   substring(cDisplay,24,250) SKIP.

          if show-direct-labor then
          do: /*with frame det:*/
            /*down.
            disp 'Direct Labor' @ itemfg.i-name
                 v-mr-est-dl[3] when v-mr-est-dl[3] ne 0 @ v-mr-est[1]
                 v-mr-act-dl[3] when v-mr-act-dl[3] ne 0 @ v-mr-act[1]
                 v-run-est-dl[3] when v-run-est-dl[3] ne 0 @ v-run-est[1]
                 v-run-act-dl[3] when v-run-act-dl[3] ne 0 @ v-run-act[1].*/

               ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-dl[3] NE ? THEN string(v-mr-est-dl[3],"->>>>>9.99") ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-dl[3] NE ? THEN  string(v-mr-act-dl[3],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-dl[3] NE ? THEN  string(v-run-est-dl[3],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-dl[3] NE ? THEN  string(v-run-act-dl[3],"->>>>>9.99") ELSE "".
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Direct Labor "   substring(cDisplay,24,250) SKIP.
          end.
          if show-fixed-overhead then
          do: /*with frame det:*/
            /*down.
            disp 'Fixed Overhead' @ itemfg.i-name
                 v-mr-est-fo[3] when v-mr-est-fo[3] ne 0 @ v-mr-est[1]
                 v-mr-act-fo[3] when v-mr-act-fo[3] ne 0 @ v-mr-act[1]
                 v-run-est-fo[3] when v-run-est-fo[3] ne 0 @ v-run-est[1]
                 v-run-act-fo[3] when v-run-act-fo[3] ne 0 @ v-run-act[1]. */
               ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-fo[3] NE ? THEN string(v-mr-est-fo[3],"->>>>>9.99") ELSE "".
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-fo[3] NE ? THEN string(v-mr-act-fo[3],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-fo[3] NE ? THEN string(v-run-est-fo[3],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-fo[3] NE ? THEN string(v-run-act-fo[3],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Fixed Overhead "   substring(cDisplay,26,250) SKIP.
          end.
          if show-variable-overhead then
          do: /*with frame det:*/
           /* down.
            disp 'Variable Overhead' @ itemfg.i-name
                 v-mr-est-vo[3] when v-mr-est-vo[3] ne 0 @ v-mr-est[1]
                 v-mr-act-vo[3] when v-mr-act-vo[3] ne 0 @ v-mr-act[1]
                 v-run-est-vo[3] when v-run-est-vo[3] ne 0 @ v-run-est[1]
                 v-run-act-vo[3] when v-run-act-vo[3] ne 0 @ v-run-act[1]. */
              ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cd"      THEN cVarValue = "" .
                         WHEN "job"         THEN cVarValue = "" .
                         WHEN "fg-dsc"      THEN cVarValue = "" .
                         WHEN "est-mr"      THEN cVarValue = IF v-mr-est-vo[3] NE ? THEN string(v-mr-est-vo[3],"->>>>>9.99") ELSE "" .
                         WHEN "act-mr"      THEN cVarValue = IF v-mr-act-vo[3] NE ? THEN string(v-mr-act-vo[3],"->>>>>9.99") ELSE "" .
                         WHEN "var-mr"      THEN cVarValue = "" .
                         WHEN "est-run"     THEN cVarValue = IF v-run-est-vo[3] NE ? THEN string(v-run-est-vo[3],"->>>>>9.99") ELSE "" .
                         WHEN "act-run"     THEN cVarValue = IF v-run-act-vo[3] NE ? THEN string(v-run-act-vo[3],"->>>>>9.99") ELSE "" .
                         WHEN "var-run"     THEN cVarValue = "" .
                         WHEN "est-brd"     THEN cVarValue = "" .
                         WHEN "act-brd"     THEN cVarValue = "" .
                         WHEN "act-mat"     THEN cVarValue = "" .
                         WHEN "fg-item"     THEN cVarValue = "" .
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED "          Variable Overhead "   substring(cDisplay,29,250) SKIP.
          end.
      end.
    end.
