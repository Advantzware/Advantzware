   
   DEFINE VARIABLE tot-mch-run-std AS DECIMAL FORMAT ">>>>9.9" NO-UNDO.
   DEFINE VARIABLE tot-mch-run-act AS DECIMAL FORMAT ">>>>9.9" NO-UNDO.
   DEFINE VARIABLE gr-tot-mch-run-std AS DECIMAL FORMAT ">>>>9.9" NO-UNDO.
   DEFINE VARIABLE gr-tot-mch-run-act AS DECIMAL FORMAT ">>>>9.9" NO-UNDO.
   DEFINE VARIABLE dpt-tot-mch-run-act AS DECIMAL FORMAT ">>>>9.9" NO-UNDO.
   DEFINE VARIABLE dpt-tot-mch-run-std AS DECIMAL FORMAT ">>>>9.9" NO-UNDO.
   DEFINE VARIABLE iTotalUp AS INTEGER NO-UNDO.

   EMPTY TEMP-TABLE tt-srt.

   FOR EACH mch-act NO-LOCK
       WHERE mch-act.company EQ cocode
         AND mch-act.op-date GE v-date[1]
         AND mch-act.op-date LE v-date[2]
         AND mch-act.shift   GE v-shift[1]
         AND mch-act.shift   LE v-shift[2]
       USE-INDEX dte-idx,

       FIRST mach NO-LOCK
       WHERE mach.company EQ cocode
         AND mach.loc     EQ locode
         AND mach.m-code  EQ mch-act.m-code:
     
     IF NOT ((tb_sched                     AND
              mach.sch-m-code NE ""        AND
              mach.sch-m-code GE v-mach[1] AND
              mach.sch-m-code LE v-mach[2])        OR 
             ((NOT tb_sched OR
               mach.sch-m-code EQ "")  AND
              mach.m-code GE v-mach[1] AND
              mach.m-code LE v-mach[2])) THEN
        NEXT.
     
     IF (mch-act.dept GE v-dept[1] AND
         mch-act.dept LE v-dept[2])                                 OR
        (mach.dept[2] NE ""        AND
         mach.dept[2] GE v-dept[1] AND
         mach.dept[2] LE v-dept[2] AND
         NOT CAN-FIND(FIRST b-mch-act
                      WHERE b-mch-act.company  EQ mch-act.company
                        AND b-mch-act.job      EQ mch-act.job
                        AND b-mch-act.job-no   EQ mch-act.job-no
                        AND b-mch-act.job-no2  EQ mch-act.job-no2
                        AND b-mch-act.frm      EQ mch-act.frm
                        AND b-mch-act.m-code   NE mch-act.m-code
                        AND b-mch-act.dept     EQ mach.dept[2]))   OR
        (mach.dept[3] NE ""        AND
         mach.dept[3] GE v-dept[1] AND
         mach.dept[3] LE v-dept[2] AND
         NOT CAN-FIND(FIRST b-mch-act
                      WHERE b-mch-act.company  EQ mch-act.company
                        AND b-mch-act.job      EQ mch-act.job
                        AND b-mch-act.job-no   EQ mch-act.job-no
                        AND b-mch-act.job-no2  EQ mch-act.job-no2
                        AND b-mch-act.frm      EQ mch-act.frm
                        AND b-mch-act.m-code   NE mch-act.m-code
                        AND b-mch-act.dept     EQ mach.dept[3]))   OR
        (mach.dept[4] NE ""        AND
         mach.dept[4] GE v-dept[1] AND
         mach.dept[4] LE v-dept[2] AND
         NOT CAN-FIND(FIRST b-mch-act
                      WHERE b-mch-act.company  EQ mch-act.company
                        AND b-mch-act.job      EQ mch-act.job
                        AND b-mch-act.job-no   EQ mch-act.job-no
                        AND b-mch-act.job-no2  EQ mch-act.job-no2
                        AND b-mch-act.frm      EQ mch-act.frm
                        AND b-mch-act.m-code   NE mch-act.m-code
                        AND b-mch-act.dept     EQ mach.dept[4]))   THEN DO:
       FIND FIRST tt-srt
           WHERE tt-srt.dept       EQ mch-act.dept
             AND tt-srt.m-code     EQ (IF tb_sched              AND
                                          mach.sch-m-code NE "" THEN mach.sch-m-code
                                                                ELSE mach.m-code)
             AND tt-srt.shift      EQ mch-act.shift
             AND tt-srt.job-no     EQ mch-act.job-no
             AND tt-srt.job-no2    EQ mch-act.job-no2
             AND tt-srt.frm        EQ mch-act.frm
             AND tt-srt.blank-no   EQ mch-act.blank-no
             AND tt-srt.pass       EQ mch-act.pass
             AND tt-srt.act-m-code EQ mch-act.m-code
           NO-ERROR.

       FIND job-code WHERE job-code.code EQ mch-act.code NO-LOCK NO-ERROR.
       IF NOT AVAILABLE job-code THEN NEXT.

       IF NOT AVAILABLE tt-srt THEN DO:
         CREATE tt-srt.
         ASSIGN tt-srt.dept       = mch-act.dept
                tt-srt.m-code     = (IF tb_sched              AND
                                        mach.sch-m-code NE "" THEN mach.sch-m-code
                                                              ELSE mach.m-code)
                tt-srt.shift      = mch-act.shift
                tt-srt.job        = mch-act.job
                tt-srt.job-no     = mch-act.job-no
                tt-srt.job-no2    = mch-act.job-no2
                tt-srt.frm        = mch-act.frm
                tt-srt.blank-no   = mch-act.blank-no
                tt-srt.pass       = mch-act.pass
                tt-srt.act-m-code = mch-act.m-code.
         RUN pro-rate-mr.
       END.
      
      
       IF job-code.cat EQ "RUN" THEN DO:

          ASSIGN
             tt-srt.run-act-hr = tt-srt.run-act-hr + mch-act.hours
             tt-srt.qty-prod   = tt-srt.qty-prod +
                                 IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty
                               .

             IF tb_tonmsf AND mch-act.qty <> ? AND mch-act.qty <> 0 THEN DO:             
                 FIND FIRST job-hdr WHERE job-hdr.company = mch-act.company
                                         AND job-hdr.job-no = mch-act.job-no
                                         AND job-hdr.job-no2 = mch-act.job-no2
                                         AND job-hdr.frm = mch-act.frm NO-LOCK NO-ERROR.
                 FIND eb WHERE eb.company = job-hdr.company
                                 AND eb.est-no = job-hdr.est-no
                                 AND eb.form-no = job-hdr.frm
                                 AND eb.blank-no = job-hdr.blank-no NO-LOCK NO-ERROR.
                 FIND ef OF eb NO-LOCK NO-ERROR.

                 IF AVAILABLE ef THEN
                 iTotalUP = IF ef.spare-int-1 = 0 THEN ef.n-out * ef.n-out-l * ef.n-out-d
                            ELSE ef.spare-int-1.

                IF (mach.p-type = "R" OR mach.p-type = "S" OR mach.p-type = "B") THEN DO:   
                  FOR EACH job-mat WHERE job-mat.company = mch-act.company
                                     AND job-mat.job = mch-act.job
                                     AND job-mat.job-no = mch-act.job-no
                                     AND job-mat.job-no2 = mch-act.job-no2
                                     AND job-mat.frm = mch-act.frm
                                         USE-INDEX seq-idx NO-LOCK,
                     FIRST item OF job-mat WHERE item.company EQ job-mat.company 
                             AND item.i-no EQ job-mat.rm-i-no
                             AND (ITEM.mat-type = "B" /* OR ITEM.mat-type = "G" */) NO-LOCK:



                      IF mach.p-type = "R" OR mach.p-type = "S" THEN DO:
                        IF mach.dept[1] = "PR" OR mach.dept[2] = "PR" OR mach.dept[3] = "PR" OR mach.dept[4] = "PR" THEN
                           ASSIGN tt-srt.qty-ton = tt-srt.qty-ton +
                                    (mch-act.qty * job-mat.wid * job-mat.len / 144000 * ITEM.basis-w / 2000)
                                  tt-srt.qty-msf = tt-srt.qty-msf + (mch-act.qty * job-mat.wid * job-mat.len / iTotalUp / 144000)
                                    .
                        ELSE
                        ASSIGN tt-srt.qty-ton = tt-srt.qty-ton +
                                    /*IF rd_tonmsf = "m" THEN  tt-srt.qty-prod * ITEM.s-wid * ITEM.s-len / 144000
                                    ELSE tt-srt.qty-prod * ITEM.s-wid * ITEM.s-len / 144000 * ITEM.basis-w / 2000
                                    */
                                    (mch-act.qty * job-mat.wid * job-mat.len / 144000 * ITEM.basis-w / 2000)
                               tt-srt.qty-msf = tt-srt.qty-msf + (mch-act.qty * job-mat.wid * job-mat.len / 144000)
                                    .
                      END.
                      ELSE DO:
                          
                          FIND itemfg WHERE itemfg.company = job-hdr.company
                                        AND itemfg.i-no = job-hdr.i-no NO-LOCK NO-ERROR.
                          IF AVAILABLE itemfg THEN
                              ASSIGN tt-srt.qty-msf = tt-srt.qty-msf + mch-act.qty * itemfg.t-sqin / 144000
                              tt-srt.qty-ton = tt-srt.qty-ton + (mch-act.qty * itemfg.t-sqin / 144000 * ITEM.basis-w / 2000) 
                              .               
                      END.                                   
                      LEAVE.
                  END.
                END.            
             END.
       END.
       ELSE
       IF job-code.cat EQ "MR" THEN 
         tt-srt.mr-act-hr  = tt-srt.mr-act-hr + mch-act.hours.

       ELSE
         tt-srt.act-dt-hr  = tt-srt.act-dt-hr + mch-act.hours.
     END.
   END.

   FOR EACH tt-srt,
       FIRST job
       WHERE job.company EQ cocode
         AND job.job     EQ tt-srt.job
         AND job.job-no  EQ tt-srt.job-no
         AND job.job-no2 EQ tt-srt.job-no2
       USE-INDEX job-no NO-LOCK:

      FIND FIRST job-mch WHERE job-mch.company  = cocode AND
                               job-mch.job      EQ tt-srt.job AND
                               job-mch.job-no  EQ tt-srt.job-no AND
                               job-mch.job-no2 EQ tt-srt.job-no2 AND
                               job-mch.frm      = tt-srt.frm AND
                               (job-mch.blank-no = tt-srt.blank-no OR
                                tt-srt.blank-no = 0) AND
                               job-mch.m-code   = tt-srt.act-m-code AND
                               job-mch.pass     = tt-srt.pass
                               NO-LOCK NO-ERROR.
      IF NOT AVAILABLE job-mch THEN
      FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                               job-mch.job      EQ tt-srt.job AND
                               job-mch.job-no  EQ tt-srt.job-no AND
                               job-mch.job-no2 EQ tt-srt.job-no2 AND
                               job-mch.frm      EQ tt-srt.frm AND
                               (job-mch.blank-no = tt-srt.blank-no OR
                                tt-srt.blank-no = 0) AND
                               job-mch.m-code   EQ tt-srt.act-m-code
                               NO-LOCK NO-ERROR.
      IF NOT AVAILABLE job-mch THEN
      FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                               job-mch.job     EQ tt-srt.job AND
                               job-mch.job-no  EQ tt-srt.job-no AND
                               job-mch.job-no2 EQ tt-srt.job-no2 AND
                               job-mch.frm     EQ tt-srt.frm AND
                               job-mch.m-code  EQ tt-srt.act-m-code AND
                               job-mch.speed   NE 0
                               NO-LOCK NO-ERROR.
      IF NOT AVAILABLE job-mch THEN
      FIND FIRST job-mch WHERE job-mch.company EQ cocode AND
                               job-mch.job     EQ tt-srt.job AND
                               job-mch.job-no  EQ tt-srt.job-no AND
                               job-mch.job-no2 EQ tt-srt.job-no2 AND
                               job-mch.frm     EQ tt-srt.frm AND
                               job-mch.m-code  EQ tt-srt.act-m-code
                               NO-LOCK NO-ERROR.

      IF AVAILABLE job-mch THEN
      DO:
         IF tt-srt.qty-prod NE 0 THEN
         DO:
            IF CAN-FIND(FIRST mach WHERE
               mach.company EQ cocode AND
               mach.loc     EQ locode AND
               mach.m-code  EQ job-mch.m-code AND
               mach.therm   EQ YES AND
               (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
               FOR EACH job-mat FIELDS(i-no len) WHERE
                   job-mat.company EQ cocode AND
                   job-mat.job = job.job AND
                   job-mat.frm EQ job-mch.frm AND
                   job-mat.frm GT 0 AND
                   job-mat.len GT 0
                   NO-LOCK,
                   FIRST ITEM FIELDS(mat-type) WHERE
                         item.company EQ cocode AND
                         item.i-no EQ job-mat.i-no
                         NO-LOCK
         
                   BREAK BY job-mat.frm
                         BY item.mat-type
                         BY job-mat.j-no
                         BY job-mat.rec_key:

                   tt-srt.run-std-hr = (tt-srt.qty-prod * job-mat.len / 12) / job-mch.speed.
                   LEAVE.
               END.
            ELSE
                tt-srt.run-std-hr = tt-srt.qty-prod / job-mch.speed.
         END.
         ELSE
            tt-srt.run-std-hr = job-mch.run-hr.
                                    /*(IF tt-srt.qty-prod NE 0 then
                                      tt-srt.qty-prod / job-mch.speed
                                     ELSE job-mch.run-hr) *
                                    (tt-srt.run-act-hr / tt-srt.tot-run-hours)*/


         ASSIGN tt-srt.mr-std-hr  = job-mch.mr-hr *
                                    (tt-srt.mr-act-hr / tt-srt.tot-mr-hours)
                tt-srt.qty-expect = IF job-mch.speed NE 0 THEN
                                      (IF tt-srt.run-act-hr NE 0
                                       THEN tt-srt.run-act-hr
                                       ELSE tt-srt.run-std-hr) * job-mch.speed
                                    ELSE job-mch.run-qty.

            IF rd_alptime EQ "TM" THEN DO:  
                ASSIGN
                    tt-srt.start-date = job-mch.start-date
                    tt-srt.start-time = job-mch.start-time . /* task 02271403  */ 
            END. 
      END.

      IF tt-srt.run-std-hr EQ ? THEN tt-srt.run-std-hr = 0.
      IF tt-srt.mr-std-hr  EQ ? THEN tt-srt.mr-std-hr  = 0.
      
      IF TB_round = YES THEN DO:
         {sys/inc/roundup.i tt-srt.qty-expect}
      END.
   END.
      
   FOR EACH tt-srt USE-INDEX dept-idx
                    BREAK BY tt-srt.dept
                          BY tt-srt.shift
                          BY tt-srt.m-code
                          BY tt-srt.start-time 
                          BY tt-srt.start-date DESCENDING  /* task 02271403  */
                          BY tt-srt.job-no
                          BY tt-srt.job-no2:

       
      IF tt-srt.run-act-hr EQ 0 THEN
         tt-srt.run-std-hr = 0.

   /*   IF NOT v-show AND tb_excel THEN DO:*/
          ASSIGN
            mr-eff  = (tt-srt.mr-std-hr  / tt-srt.mr-act-hr)  * 100.00
            run-eff = (tt-srt.run-std-hr / tt-srt.run-act-hr) * 100.00
            tot-std-hrs = tt-srt.mr-std-hr + tt-srt.run-std-hr
            tot-act-hrs = tt-srt.mr-act-hr + tt-srt.run-act-hr
            tot-eff = (tot-std-hrs / tot-act-hrs) * 100.00
            dt-eff = (tt-srt.act-dt-hr / tot-act-hrs) * 100.00.

         IF mr-eff = ? THEN mr-eff = 0.
         IF run-eff = ? THEN run-eff = 0.
         IF tot-eff = ? THEN tot-eff = 0.
         IF dt-eff = ? THEN dt-eff = 0.
   
         
         ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "mch-cod"          THEN cVarValue = STRING(tt-srt.m-code) .
                         WHEN "job"              THEN cVarValue = STRING(tt-srt.job-no) .
                         WHEN "job2"             THEN cVarValue = STRING(tt-srt.job-no2) .
                         WHEN "shift"            THEN cVarValue = STRING(tt-srt.shift,">>>>") .
                         WHEN "mr-stnd"          THEN cVarValue = STRING(tt-srt.mr-std-hr,"->>>9.99") .
                         WHEN "mr-act"           THEN cVarValue = STRING(tt-srt.mr-act-hr,"->>>9.99") .
                         WHEN "mr-eff"           THEN cVarValue = STRING(mr-eff,"->>>9.99") .

                         WHEN "run-hrs-std"      THEN cVarValue = STRING(tt-srt.run-std-hr,"->>>9.99") .
                         WHEN "run-hrs-act"      THEN cVarValue =  STRING(tt-srt.run-act-hr,"->>>9.99") .
                         WHEN "run-hrs-eff"      THEN cVarValue = STRING(run-eff,"->>>9.99") .

                         WHEN "mr-rn-hrs-std"    THEN cVarValue = STRING(tot-std-hrs,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-act"    THEN cVarValue = STRING(tot-act-hrs,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-eff"    THEN cVarValue = STRING(tot-eff,"->>>,>>9.99") .

                         WHEN "dt-hrs-act"       THEN cVarValue = STRING(tt-srt.act-dt-hr,"->>9.99") .
                         WHEN "dt-hrs-eff"       THEN cVarValue = STRING(dt-eff,"->>9.99") .

                         WHEN "act-qty"          THEN cVarValue = STRING(tt-srt.qty-prod,"->,>>>,>>9") .
                         WHEN "act-ton"          THEN cVarValue = STRING(tt-srt.qty-ton,"->>,>>9.99") .
                         WHEN "act-msf"          THEN cVarValue = STRING(tt-srt.qty-msf,"->>,>>9.99") .
                         WHEN "exp-qty"          THEN cVarValue = STRING(tt-srt.qty-expect,"->>>,>>>,>>9") .
                         
                    END CASE.
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
            PUT UNFORMATTED cDisplay SKIP.
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED  
                       cExcelDisplay SKIP.
             END.
             IF FIRST-OF(tt-srt.job-no2) THEN shf-jobs = shf-jobs + 1.

             ASSIGN 
                 mch-mr-std     = mch-mr-std + tt-srt.mr-std-hr
                 mch-mr-act     = mch-mr-act + tt-srt.mr-act-hr
                 mch-run-std    = mch-run-std + tt-srt.run-std-hr
                 mch-run-act    = mch-run-act + tt-srt.run-act-hr
                 mch-dt-act     = mch-dt-act + tt-srt.act-dt-hr
                 tot-mch-run-std = tot-mch-run-std + tot-std-hrs
                 tot-mch-run-act = tot-mch-run-act + tot-act-hrs
                 mch-qty-prod   = mch-qty-prod + tt-srt.qty-prod
                 mch-qty-expect = mch-qty-expect + tt-srt.qty-expect
                 mch-qty-msf = mch-qty-msf + tt-srt.qty-msf
                 mch-qty-ton = mch-qty-ton + tt-srt.qty-ton
                 .
             IF LAST-OF(tt-srt.m-code) THEN
                 DO:
                 FIND mach WHERE mach.company = cocode AND
                     mach.loc     = locode AND
                     mach.m-code  = tt-srt.m-code
                     NO-LOCK NO-ERROR.
                 {pc/rep/mchprdhr.i "mch"}
                 PUT SKIP SPACE(6) str-line SKIP .
                 ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".
                 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                     cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField: 
                         /*PUT "pinky" mch-mr-std mch-mr-act SKIP.*/
                         WHEN "mch-cod"       THEN cVarValue =  "" .
                         WHEN "job"           THEN cVarValue = "" .
                         WHEN "job2"          THEN cVarValue = "" .
                         WHEN "shift"         THEN cVarValue = STRING(tt-srt.shift,">>>>") .
                         WHEN "mr-stnd"       THEN cVarValue = STRING(mch-mr-std,"->>>9.99") .
                         WHEN "mr-act"        THEN cVarValue = STRING(mch-mr-act,"->>>9.99") .
                         WHEN "mr-eff"        THEN cVarValue = STRING(mr-eff,"->>>9.99") .
                         WHEN "run-hrs-std"   THEN cVarValue = STRING(mch-run-std,"->>>9.99") .
                         WHEN "run-hrs-act"   THEN cVarValue =  STRING(mch-run-act,"->>>9.99") .
                         WHEN "run-hrs-eff"   THEN cVarValue = STRING(run-eff,"->>>9.99") .
                         WHEN "mr-rn-hrs-std" THEN cVarValue = STRING(tot-mch-run-std,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-act" THEN cVarValue = STRING(tot-mch-run-act,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-eff" THEN cVarValue = STRING(tot-eff,"->>>,>>9.99") .
                         WHEN "dt-hrs-act"    THEN cVarValue = STRING(mch-dt-act,"->>9.99") .
                         WHEN "dt-hrs-eff"    THEN cVarValue = STRING(dt-eff,"->>9.99") .
                         WHEN "act-qty"       THEN cVarValue = STRING(mch-qty-prod,"->,>>>,>>9") .
                         WHEN "act-ton"       THEN cVarValue = STRING(mch-qty-ton,"->>,>>9.99") .
                         WHEN "act-msf"       THEN cVarValue = STRING(mch-qty-msf,"->>,>>9.99") .
                         WHEN "exp-qty"       THEN cVarValue = STRING(mch-qty-expect,"->>>,>>>,>>9") .
                         
                    END CASE.
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
               
            PUT UNFORMATTED "       " + (IF AVAILABLE mach THEN STRING(mach.m-dscr,"x(10)") ELSE "          " )  + substring(cDisplay,18,500) SKIP.
            PUT SKIP SPACE(6) str-line SKIP .
           
            
            IF tb_excel THEN DO:
                 PUT STREAM excel UNFORMATTED "," + ( IF AVAILABLE mach THEN STRING(mach.m-dscr) ELSE "          " ) + " ," +
                       substring(cExcelDisplay,9,500) SKIP.
             END.

            ASSIGN 
                shf-mr-std     = shf-mr-std + mch-mr-std
                shf-mr-act     = shf-mr-act + mch-mr-act
                shf-run-std    = shf-run-std + mch-run-std
                shf-run-act    = shf-run-act + mch-run-act
                shf-dt-act     = shf-dt-act + mch-dt-act
                shf-qty-prod   = shf-qty-prod + mch-qty-prod
                shf-qty-expect = shf-qty-expect + mch-qty-expect
                shf-qty-msf = shf-qty-msf + mch-qty-msf
                shf-qty-ton = shf-qty-ton + mch-qty-ton
                gr-tot-mch-run-std = gr-tot-mch-run-std + tot-mch-run-std
                gr-tot-mch-run-act = gr-tot-mch-run-act + tot-mch-run-act.
            ASSIGN 
                mch-mr-std = 0
                mch-mr-act = 0
                mch-run-std = 0
                mch-run-act = 0
                mch-dt-act = 0
                mch-qty-prod = 0
                mch-qty-expect = 0
                mch-qty-msf = 0
                mch-qty-msf = 0
                mch-qty-ton = 0
                tot-mch-run-std = 0
                tot-mch-run-act = 0.
             END.
             IF LAST-OF(tt-srt.shift) THEN
                 DO:
                 {pc/rep/mchprdhr.i "shf"}
                 
                 ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".
                 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                     cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField: 
                         
                         /*PUT "pinky" mch-mr-std mch-mr-act SKIP.*/
                         WHEN "mch-cod"           THEN cVarValue =  /* STRING(mach.m-dscr) */ "" .
                         WHEN "job"               THEN cVarValue = "" .
                         WHEN "job2"              THEN cVarValue = "" .
                         WHEN "shift"             THEN cVarValue = STRING(tt-srt.shift,">>>>") .
                         WHEN "mr-stnd"           THEN cVarValue = STRING(shf-mr-std,"->>>9.99") .
                         WHEN "mr-act"            THEN cVarValue = STRING(shf-mr-act,"->>>9.99") .
                         WHEN "mr-eff"            THEN cVarValue = STRING(mr-eff,"->>>9.99") .
                         WHEN "run-hrs-std"       THEN cVarValue = STRING(shf-run-std,"->>>9.99") .
                         WHEN "run-hrs-act"       THEN cVarValue =  STRING(shf-run-act,"->>>9.99") .
                         WHEN "run-hrs-eff"       THEN cVarValue = STRING(run-eff,"->>>9.99") .
                         WHEN "mr-rn-hrs-std"     THEN cVarValue = STRING(gr-tot-mch-run-std,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-act"     THEN cVarValue = STRING(gr-tot-mch-run-act,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-eff"     THEN cVarValue = STRING(tot-eff,"->>>,>>9.99") .
                         WHEN "dt-hrs-act"        THEN cVarValue = STRING(shf-dt-act,"->>9.99") .
                         WHEN "dt-hrs-eff"        THEN cVarValue = STRING(dt-eff,"->>9.99") .
                         WHEN "act-qty"           THEN cVarValue = STRING(shf-qty-prod,"->,>>>,>>9") .
                         WHEN "act-ton"           THEN cVarValue = STRING(shf-qty-ton,"->>,>>9.99") .
                         WHEN "act-msf"           THEN cVarValue = STRING(shf-qty-msf,"->>,>>9.99") .
                         WHEN "exp-qty"           THEN cVarValue = STRING(shf-qty-expect,"->>>,>>>,>>9") .
                         
                    END CASE.
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                 END.
                 PUT UNFORMATTED "       " + "SHIFT TOT"  + substring(cDisplay,17,500) SKIP.
                 PUT SKIP SPACE(6) str-line SKIP .
                 
                 IF tb_excel THEN DO:
                    PUT STREAM excel UNFORMATTED "," + "SHIFT TOT" + " ," +
                       substring(cExcelDisplay,9,500) SKIP.
                 END.
                 
                 ASSIGN
                     dpt-mr-std     = dpt-mr-std + shf-mr-std
                     dpt-run-std    = dpt-run-std + shf-run-std
                     dpt-mr-act     = dpt-mr-act + shf-mr-act
                     dpt-run-act    = dpt-run-act + shf-run-act
                     dpt-dt-act     = dpt-dt-act + shf-dt-act
                     dpt-qty-prod   = dpt-qty-prod + shf-qty-prod
                     dpt-qty-expect = dpt-qty-expect + shf-qty-expect
                     dpt-qty-msf = dpt-qty-msf + shf-qty-msf
                     dpt-qty-ton = dpt-qty-ton + shf-qty-ton
                     dpt-tot-mch-run-std = dpt-tot-mch-run-std + gr-tot-mch-run-std
                     dpt-tot-mch-run-act = dpt-tot-mch-run-act + gr-tot-mch-run-act.

                 ASSIGN
                     shf-mr-std     = 0
                     shf-run-std    = 0
                     shf-mr-act     = 0
                     shf-run-act    = 0
                     shf-dt-act     = 0
                     shf-qty-prod   = 0
                     shf-qty-expect = 0
                     shf-qty-msf    = 0
                     shf-qty-ton    = 0
                     gr-tot-mch-run-std = 0
                     gr-tot-mch-run-act = 0.
             END.

             IF LAST-OF(tt-srt.dept) THEN
                 DO:
                 FIND dept WHERE dept.company = cocode AND
                     dept.cod     = tt-srt.dept
                     NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE dept THEN
                     FIND dept WHERE dept.company = "" AND
                     dept.cod     = tt-srt.dept
                     NO-LOCK NO-ERROR.
                 {pc/rep/mchprdhr.i "dpt"}
                     ASSIGN cDisplay = ""
                     cTmpField = ""
                     cVarValue = ""
                     cExcelDisplay = ""
                     cExcelVarValue = "".
                 DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
                     cTmpField = ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                     CASE cTmpField: 
                         
                         /*PUT "pinky" mch-mr-std mch-mr-act SKIP.*/
                         WHEN "mch-cod"           THEN cVarValue =  /* STRING(mach.m-dscr) */ "" .
                         WHEN "job"               THEN cVarValue = ""  .
                         WHEN "job2"              THEN cVarValue = "" .
                         WHEN "shift"             THEN cVarValue = "" .
                         WHEN "mr-stnd"           THEN cVarValue = STRING(dpt-mr-std,"->>>9.99") .
                         WHEN "mr-act"            THEN cVarValue = STRING(dpt-mr-act,"->>>9.99") .
                         WHEN "mr-eff"            THEN cVarValue = STRING(mr-eff,"->>>9.99") .
                         WHEN "run-hrs-std"       THEN cVarValue = STRING(dpt-run-std,"->>>9.99") .
                         WHEN "run-hrs-act"       THEN cVarValue =  STRING(dpt-run-act,"->>>9.99") .
                         WHEN "run-hrs-eff"       THEN cVarValue = STRING(run-eff,"->>>9.99") .
                         WHEN "mr-rn-hrs-std"     THEN cVarValue = STRING(dpt-tot-mch-run-std,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-act"     THEN cVarValue = STRING(dpt-tot-mch-run-act,"->>,>>9.99") .
                         WHEN "mr-rn-hrs-eff"     THEN cVarValue = STRING(tot-eff,"->>>,>>9.99") .
                         WHEN "dt-hrs-act"        THEN cVarValue = STRING(dpt-dt-act,"->>9.99") .
                         WHEN "dt-hrs-eff"        THEN cVarValue = STRING(dt-eff,"->>9.99") .
                         WHEN "act-qty"           THEN cVarValue = STRING(dpt-qty-prod,"->,>>>,>>9") .
                         WHEN "act-ton"           THEN cVarValue = STRING(dpt-qty-ton,"->>,>>9.99") .
                         WHEN "act-msf"           THEN cVarValue = STRING(dpt-qty-msf,"->>,>>9.99") .
                         WHEN "exp-qty"           THEN cVarValue = STRING(dpt-qty-expect,"->>>,>>>,>>9") .
                         
                    END CASE.
                    
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(ENTRY(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
                 END.
                 /*PUT UNFORMATTED cDisplay SKIP.*/
                 PUT UNFORMATTED "       " + (IF AVAILABLE dept THEN STRING(dept.dscr,"x(10)") ELSE "          ")  +  substring(cDisplay,18,350) SKIP.

                 PUT SKIP SPACE(6) str-line SKIP .
                 
                 IF tb_excel THEN DO:
                        PUT STREAM excel UNFORMATTED "," + (IF AVAILABLE dept THEN STRING(dept.dscr) ELSE "") + " ," +
                       substring(cExcelDisplay,9,500) SKIP.
                 END.
                 ASSIGN
                     dpt-mr-std     = 0
                     dpt-run-std    = 0
                     dpt-mr-act     = 0
                     dpt-run-act    = 0
                     dpt-dt-act     = 0
                     dpt-qty-prod   = 0
                     dpt-qty-expect = 0
                     dpt-qty-msf = 0
                     dpt-qty-ton = 0
                     dpt-tot-mch-run-std = 0
                     dpt-tot-mch-run-act = 0.
            
             END.
     
   END. /* each item */
   
   
