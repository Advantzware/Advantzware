   DEF VAR iTotalUp AS INT NO-UNDO.

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
       find first tt-srt
           where tt-srt.dept       eq mch-act.dept
             and tt-srt.m-code     eq (IF tb_sched              AND
                                          mach.sch-m-code NE "" THEN mach.sch-m-code
                                                                ELSE mach.m-code)
             and tt-srt.shift      eq mch-act.shift
             and tt-srt.job-no     eq mch-act.job-no
             and tt-srt.job-no2    eq mch-act.job-no2
             and tt-srt.frm        eq mch-act.frm
             and tt-srt.blank-no   eq mch-act.blank-no
             and tt-srt.pass       eq mch-act.pass
             and tt-srt.act-m-code eq mch-act.m-code
           no-error.

       find job-code where job-code.code eq mch-act.code no-lock no-error.
       if not avail job-code then next.

       if not avail tt-srt then do:
         create tt-srt.
         assign tt-srt.dept       = mch-act.dept
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
       end.
      
      
       if job-code.cat eq "RUN" THEN DO:

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

                 iTotalUP = IF ef.spare-int-1 = 0 THEN ef.n-out * ef.n-out-l * ef.n-out-d
                            ELSE ef.spare-int-1.

                IF (mach.p-type = "R" OR mach.p-type = "S" OR mach.p-type = "B") THEN DO:   
                  FOR each job-mat WHERE job-mat.company = mch-act.company
                                     AND job-mat.job = mch-act.job
                                     AND job-mat.job-no = mch-act.job-no
                                     AND job-mat.job-no2 = mch-act.job-no2
                                     AND job-mat.frm = mch-act.frm
                                         use-index seq-idx NO-LOCK,
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
                          ASSIGN tt-srt.qty-msf = tt-srt.qty-msf + mch-act.qty * itemfg.t-sqin / 144000
                                 tt-srt.qty-ton = tt-srt.qty-ton + (mch-act.qty * itemfg.t-sqin / 144000 * ITEM.basis-w / 2000)
                                    .               
                      END.                                   
                      LEAVE.
                  END.
                END.            
             END.
       END.
       else
       if job-code.cat eq "MR" then 
         tt-srt.mr-act-hr  = tt-srt.mr-act-hr + mch-act.hours.

       else
         tt-srt.act-dt-hr  = tt-srt.act-dt-hr + mch-act.hours.
     END.
   end.

   for each tt-srt,
       first job
       where job.company eq cocode
         and job.job     eq tt-srt.job
         and job.job-no  eq tt-srt.job-no
         and job.job-no2 eq tt-srt.job-no2
       use-index job-no no-lock:

      find first job-mch where job-mch.company  = cocode and
                               job-mch.job      eq tt-srt.job and
                               job-mch.job-no  eq tt-srt.job-no and
                               job-mch.job-no2 eq tt-srt.job-no2 and
                               job-mch.frm      = tt-srt.frm and
                               (job-mch.blank-no = tt-srt.blank-no or
                                tt-srt.blank-no = 0) and
                               job-mch.m-code   = tt-srt.act-m-code and
                               job-mch.pass     = tt-srt.pass
                               no-lock no-error.
      if not avail job-mch then
      find first job-mch where job-mch.company eq cocode and
                               job-mch.job      eq tt-srt.job and
                               job-mch.job-no  eq tt-srt.job-no and
                               job-mch.job-no2 eq tt-srt.job-no2 and
                               job-mch.frm      eq tt-srt.frm and
                               (job-mch.blank-no = tt-srt.blank-no or
                                tt-srt.blank-no = 0) and
                               job-mch.m-code   eq tt-srt.act-m-code
                               no-lock no-error.
      if not avail job-mch then
      find first job-mch where job-mch.company eq cocode and
                               job-mch.job     eq tt-srt.job and
                               job-mch.job-no  eq tt-srt.job-no and
                               job-mch.job-no2 eq tt-srt.job-no2 and
                               job-mch.frm     eq tt-srt.frm and
                               job-mch.m-code  eq tt-srt.act-m-code and
                               job-mch.speed   ne 0
                               no-lock no-error.
      if not avail job-mch then
      find first job-mch where job-mch.company eq cocode and
                               job-mch.job     eq tt-srt.job and
                               job-mch.job-no  eq tt-srt.job-no and
                               job-mch.job-no2 eq tt-srt.job-no2 and
                               job-mch.frm     eq tt-srt.frm and
                               job-mch.m-code  eq tt-srt.act-m-code
                               no-lock no-error.

      if available job-mch then
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
                   job-mat.company eq cocode AND
                   job-mat.job = job.job AND
                   job-mat.frm EQ job-mch.frm AND
                   job-mat.frm GT 0 AND
                   job-mat.len GT 0
                   no-lock,
                   first ITEM FIELDS(mat-type) WHERE
                         item.company eq cocode AND
                         item.i-no eq job-mat.i-no
                         no-lock
         
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

            IF rd_alptime EQ "TM" THEN do:  
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
   end.
      
   for each tt-srt use-index dept-idx
                    BREAK by tt-srt.dept
                          by tt-srt.shift
                          by tt-srt.m-code
                          BY tt-srt.start-time 
                          BY tt-srt.start-date DESC  /* task 02271403  */
                          by tt-srt.job-no
                          by tt-srt.job-no2:

       
      IF tt-srt.run-act-hr EQ 0 THEN
         tt-srt.run-std-hr = 0.

      IF NOT v-show AND tb_excel THEN DO:
          ASSIGN
            mr-eff  = (tt-srt.mr-std-hr  / tt-srt.mr-act-hr)  * 100.00
            run-eff = (tt-srt.run-std-hr / tt-srt.run-act-hr) * 100.00
            tot-std-hrs = tt-srt.mr-std-hr + tt-srt.run-std-hr
            tot-act-hrs = tt-srt.mr-act-hr + tt-srt.run-act-hr
            tot-eff = (tot-std-hrs / tot-act-hrs) * 100.00
            dt-eff = (tt-srt.act-dt-hr / tot-act-hrs) * 100.00.

         if mr-eff = ? then mr-eff = 0.
         if run-eff = ? then run-eff = 0.
         if tot-eff = ? then tot-eff = 0.
         if dt-eff = ? then dt-eff = 0.

      END.

      if v-show then
      do:
         ASSIGN
            mr-eff  = (tt-srt.mr-std-hr  / tt-srt.mr-act-hr)  * 100.00
            run-eff = (tt-srt.run-std-hr / tt-srt.run-act-hr) * 100.00
            tot-std-hrs = tt-srt.mr-std-hr + tt-srt.run-std-hr
            tot-act-hrs = tt-srt.mr-act-hr + tt-srt.run-act-hr
            tot-eff = (tot-std-hrs / tot-act-hrs) * 100.00
            dt-eff = (tt-srt.act-dt-hr / tot-act-hrs) * 100.00.

         if mr-eff = ? then mr-eff = 0.
         if run-eff = ? then run-eff = 0.
         if tot-eff = ? then tot-eff = 0.
         if dt-eff = ? then dt-eff = 0.
      
         IF TB_round = YES THEN DO:
            IF tb_tonmsf AND rd_tonmsfqty = "TM" THEN
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23
                 tt-srt.mr-act-hr
                 mr-eff
                 tt-srt.run-std-hr at 48
                 tt-srt.run-act-hr
                 run-eff
                 tot-std-hrs at 73
                 tot-act-hrs
                 tot-eff
                 tt-srt.act-dt-hr at 98
                 dt-eff
                 tt-srt.qty-ton /*tt-srt.qty-prod*/
                 tt-srt.qty-msf /*tt-srt.qty-expect*/
                 /*tt-srt.qty-tonmsf WHEN tb_tonmsf  @ tt-srt.qty-prod format ">>,>>9.99"*/
                 with frame detton STREAM-IO width 132 no-labels no-box down.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "QM" THEN
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23
                 tt-srt.mr-act-hr
                 mr-eff
                 tt-srt.run-std-hr at 48
                 tt-srt.run-act-hr
                 run-eff
                 tot-std-hrs at 73
                 tot-act-hrs
                 tot-eff
                 tt-srt.act-dt-hr at 98
                 dt-eff
                 tt-srt.qty-prod
                 tt-srt.qty-msf 
                 /*tt-srt.qty-ton tt-srt.qty-expect*/
                 /*tt-srt.qty-tonmsf WHEN tb_tonmsf  @ tt-srt.qty-prod format ">>,>>9.99"*/
                 with frame detton1 STREAM-IO width 132 no-labels no-box down.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "QT" THEN
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23
                 tt-srt.mr-act-hr
                 mr-eff
                 tt-srt.run-std-hr at 48
                 tt-srt.run-act-hr
                 run-eff
                 tot-std-hrs at 73
                 tot-act-hrs
                 tot-eff
                 tt-srt.act-dt-hr at 98
                 dt-eff
                 tt-srt.qty-prod tt-srt.qty-ton 
                 /*tt-srt.qty-msf tt-srt.qty-expect*/
                 /*tt-srt.qty-tonmsf WHEN tb_tonmsf  @ tt-srt.qty-prod format ">>,>>9.99"*/
                 with frame detton2 STREAM-IO width 132 no-labels no-box down.
            
            ELSE
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23
                 tt-srt.mr-act-hr
                 mr-eff
                 tt-srt.run-std-hr at 48
                 tt-srt.run-act-hr
                 run-eff
                 tot-std-hrs at 73
                 tot-act-hrs
                 tot-eff
                 tt-srt.act-dt-hr at 98
                 dt-eff
                 tt-srt.qty-prod 
                 tt-srt.qty-expect 
                 with frame det STREAM-IO width 132 no-labels no-box down.
         END.
         ELSE DO:
            IF tb_tonmsf AND rd_tonmsfqty = "tm" THEN
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23 FORMAT ">>>9.99"
                 tt-srt.mr-act-hr       FORMAT ">>>9.99"
                 mr-eff                 FORMAT ">>>9.99"
                 tt-srt.run-std-hr at 48 FORMAT ">>>9.99"
                 tt-srt.run-act-hr      FORMAT ">>>9.99"
                 run-eff                FORMAT ">>9.99"
                 tot-std-hrs at 73      FORMAT ">>>9.99"
                 tot-act-hrs            FORMAT ">>>9.99"
                 tot-eff                FORMAT ">>9.99"
                 tt-srt.act-dt-hr at 98 FORMAT ">>>9.99"
                 dt-eff                 FORMAT ">>9.99"
                 tt-srt.qty-ton
                 tt-srt.qty-msf 
                 /*tt-srt.qty-expect*/
                 /*tt-srt.qty-tonmsf WHEN tb_tonmsf @ tt-srt.qty-prod format ">>,>>9.99"*/
                 with frame det2ton STREAM-IO width 132 no-labels no-box down.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "qm" THEN
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23 FORMAT ">>>9.99"
                 tt-srt.mr-act-hr       FORMAT ">>>9.99"
                 mr-eff                 FORMAT ">>>9.99"
                 tt-srt.run-std-hr at 48 FORMAT ">>>9.99"
                 tt-srt.run-act-hr      FORMAT ">>>9.99"
                 run-eff                FORMAT ">>9.99"
                 tot-std-hrs at 73      FORMAT ">>>9.99"
                 tot-act-hrs            FORMAT ">>>9.99"
                 tot-eff                FORMAT ">>9.99"
                 tt-srt.act-dt-hr at 98 FORMAT ">>>9.99"
                 dt-eff                 FORMAT ">>9.99"
                 tt-srt.qty-prod 
                 tt-srt.qty-msf 
                 /*tt-srt.qty-expect*/
                 /*tt-srt.qty-tonmsf WHEN tb_tonmsf @ tt-srt.qty-prod format ">>,>>9.99"*/
                 with frame det2ton1 STREAM-IO width 132 no-labels no-box down.
           ELSE IF tb_tonmsf AND rd_tonmsfqty = "qt" THEN
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23 FORMAT ">>>9.99"
                 tt-srt.mr-act-hr       FORMAT ">>>9.99"
                 mr-eff                 FORMAT ">>>9.99"
                 tt-srt.run-std-hr at 48 FORMAT ">>>9.99"
                 tt-srt.run-act-hr      FORMAT ">>>9.99"
                 run-eff                FORMAT ">>9.99"
                 tot-std-hrs at 73      FORMAT ">>>9.99"
                 tot-act-hrs            FORMAT ">>>9.99"
                 tot-eff                FORMAT ">>9.99"
                 tt-srt.act-dt-hr at 98 FORMAT ">>>9.99"
                 dt-eff                 FORMAT ">>9.99"
                 tt-srt.qty-prod 
                 tt-srt.qty-ton
                 /*tt-srt.qty-msf */
                 /*tt-srt.qty-expect*/
                 /*tt-srt.qty-tonmsf WHEN tb_tonmsf @ tt-srt.qty-prod format ">>,>>9.99"*/
                 with frame det2ton2 STREAM-IO width 132 no-labels no-box down.
            ELSE
            display tt-srt.m-code
                 tt-srt.job-no space(0) "-" space(0)
                 tt-srt.job-no2
                 tt-srt.shift at 20 format ">>"
                 tt-srt.mr-std-hr at 23 FORMAT ">>>9.99"
                 tt-srt.mr-act-hr       FORMAT ">>>9.99"
                 mr-eff                 FORMAT ">>>9.99"
                 tt-srt.run-std-hr at 48 FORMAT ">>>9.99"
                 tt-srt.run-act-hr      FORMAT ">>>9.99"
                 run-eff                FORMAT ">>9.99"
                 tot-std-hrs at 73      FORMAT ">>>9.99"
                 tot-act-hrs            FORMAT ">>>9.99"
                 tot-eff                FORMAT ">>9.99"
                 tt-srt.act-dt-hr at 98 FORMAT ">>>9.99"
                 dt-eff                 FORMAT ">>9.99"
                 tt-srt.qty-prod
                 tt-srt.qty-expect
                 with frame det2 STREAM-IO width 132 no-labels no-box down.
         END.

      end.

      IF tb_excel THEN  DO:
      
         PUT STREAM excel UNFORMATTED
            '"' tt-srt.m-code '",'         
            '"' tt-srt.job-no '",'  
            '"' tt-srt.job-no2 '",'
            '"' tt-srt.shift '",'
            '"' tt-srt.mr-std-hr '",'
            '"' tt-srt.mr-act-hr '",'
            '"' mr-eff '",'
            '"' tt-srt.run-std-hr '",'
            '"' tt-srt.run-act-hr '",'
            '"' run-eff '",'
            '"' tot-std-hrs '",'
            '"' tot-act-hrs '",'
            '"' tot-eff '",'
            '"' tt-srt.act-dt-hr '",'
            '"' dt-eff '",'
            '"' tt-srt.qty-prod '",'
            '"' tt-srt.qty-ton '",'
            '"' tt-srt.qty-msf '",'
            '"' tt-srt.qty-expect '",'      /*Task# 10301311*/
             SKIP.

/*             IF tb_tonmsf AND rd_tonmsfqty = "TM" THEN                                                   */
/*               PUT STREAM excel UNFORMATTED '"' tt-srt.qty-ton '",'  '"' tt-srt.qty-msf '",' SKIP.       */
/*             IF tb_tonmsf AND rd_tonmsfqty = "QM" THEN                                                   */
/*               PUT STREAM excel UNFORMATTED '"' tt-srt.qty-prod '",'  '"' tt-srt.qty-msf '",' SKIP.      */
/*             IF tb_tonmsf AND rd_tonmsfqty = "QT" THEN                                                   */
/*               PUT STREAM excel UNFORMATTED '"' tt-srt.qty-prod '",'  '"' tt-srt.qty-ton '",' SKIP.      */
/*             ELSE PUT STREAM excel UNFORMATTED '"' tt-srt.qty-prod '",' '"' tt-srt.qty-expect '",' skip.     */
      END.
      if first-of(tt-srt.job-no2) then shf-jobs = shf-jobs + 1.

      assign mch-mr-std     = mch-mr-std + tt-srt.mr-std-hr
             mch-run-std    = mch-run-std + tt-srt.run-std-hr
             mch-mr-act     = mch-mr-act + tt-srt.mr-act-hr
             mch-run-act    = mch-run-act + tt-srt.run-act-hr
             mch-dt-act     = mch-dt-act + tt-srt.act-dt-hr
             mch-qty-prod   = mch-qty-prod + tt-srt.qty-prod
             mch-qty-expect = mch-qty-expect + tt-srt.qty-expect
             mch-qty-msf = mch-qty-msf + tt-srt.qty-msf
             mch-qty-ton = mch-qty-ton + tt-srt.qty-ton
             dpt-mr-std     = dpt-mr-std + tt-srt.mr-std-hr
             dpt-run-std    = dpt-run-std + tt-srt.run-std-hr
             dpt-mr-act     = dpt-mr-act + tt-srt.mr-act-hr
             dpt-run-act    = dpt-run-act + tt-srt.run-act-hr
             dpt-dt-act     = dpt-dt-act + tt-srt.act-dt-hr
             dpt-qty-prod   = dpt-qty-prod + tt-srt.qty-prod
             dpt-qty-expect = dpt-qty-expect + tt-srt.qty-expect
             dpt-qty-msf = dpt-qty-msf + tt-srt.qty-msf
             dpt-qty-ton = dpt-qty-ton + tt-srt.qty-ton
             shf-mr-std     = shf-mr-std + tt-srt.mr-std-hr
             shf-run-std    = shf-run-std + tt-srt.run-std-hr
             shf-mr-act     = shf-mr-act + tt-srt.mr-act-hr
             shf-run-act    = shf-run-act + tt-srt.run-act-hr
             shf-dt-act     = shf-dt-act + tt-srt.act-dt-hr
             shf-qty-prod   = shf-qty-prod + tt-srt.qty-prod
             shf-qty-expect = shf-qty-expect + tt-srt.qty-expect
             shf-qty-msf = shf-qty-msf + tt-srt.qty-msf
             shf-qty-ton = shf-qty-ton + tt-srt.qty-ton
             .

      if last-of(tt-srt.m-code) then
      do:
         find mach where mach.company = cocode and
                         mach.loc     = locode and
                         mach.m-code  = tt-srt.m-code
                         no-lock no-error.
         {pc/rep/mchprdhr.i "mch"}
         IF TB_round = YES THEN DO:
            IF tb_tonmsf AND rd_tonmsfqty = "TM" THEN
             display fill("-", 126) format "x(126)" at 7 skip
                    "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                    tt-srt.shift at 20 format ">>"
                    mch-mr-std at 23
                    mch-mr-act
                    mr-eff
                    mch-run-std at 48
                    mch-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    mch-dt-act at 98
                    dt-eff
                    mch-qty-ton /*mch-qty-prod*/
                    mch-qty-msf
                    /*mch-qty-expect*/
                    /*mch-qty-tonmsf WHEN tb_tonmsf @ mch-qty-prod format ">>,>>9.99" */ SKIP
                    fill("-", 126) format "x(126)" at 7
                    with frame detmton STREAM-IO width 132 no-labels no-box.

          ELSE IF tb_tonmsf AND rd_tonmsfqty = "QM" THEN
            display fill("-", 126) format "x(126)" at 7 skip
                      "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                     tt-srt.shift at 20 format ">>"
                     mch-mr-std at 23
                     mch-mr-act
                    mr-eff
                    mch-run-std at 48
                    mch-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    mch-dt-act at 98
                    dt-eff
                    mch-qty-prod
                    mch-qty-msf
                    /*mch-qty-expect*/
               /*mch-qty-tonmsf WHEN tb_tonmsf @ mch-qty-prod format ">>,>>9.99" */ SKIP
              fill("-", 126) format "x(126)" at 7
            with frame detmton1 STREAM-IO width 132 no-labels no-box.

            ELSE IF tb_tonmsf AND rd_tonmsfqty = "QT" THEN
             display fill("-", 126) format "x(126)" at 7 skip
                    "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                    tt-srt.shift at 20 format ">>"
                    mch-mr-std at 23
                    mch-mr-act
                    mr-eff
                    mch-run-std at 48
                    mch-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    mch-dt-act at 98
                    dt-eff
                    mch-qty-prod
                    mch-qty-ton
                    /*mch-qty-expect*/
                    /*mch-qty-tonmsf WHEN tb_tonmsf @ mch-qty-prod format ">>,>>9.99" */ SKIP
                    fill("-", 126) format "x(126)" at 7
                    with frame detmton2 STREAM-IO width 132 no-labels no-box.                    

              ELSE
                  display fill("-", 126) format "x(126)" at 7 skip
                    "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                    tt-srt.shift at 20 format ">>"
                    mch-mr-std at 23
                    mch-mr-act
                    mr-eff
                    mch-run-std at 48
                    mch-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    mch-dt-act at 98
                    dt-eff
                    mch-qty-prod
                    mch-qty-expect SKIP
                    fill("-", 126) format "x(126)" at 7
                    with frame detm STREAM-IO width 132 no-labels no-box.

         END.
         ELSE DO:
            IF tb_tonmsf AND rd_tonmsfqty = "TM" THEN
            display fill("-", 126) format "x(126)" at 7 skip
                    "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                    tt-srt.shift at 20 format ">>"
                    mch-mr-std at 23 FORMAT ">>>9.99"
                    mch-mr-act       FORMAT ">>>9.99"
                    mr-eff           FORMAT ">>>9.99"
                    mch-run-std at 48 FORMAT ">>>9.99"
                    mch-run-act      FORMAT ">>>9.99"
                    run-eff          FORMAT ">>>9.99"
                    tot-std-hrs at 73 FORMAT ">>>9.99"
                    tot-act-hrs      FORMAT ">>>9.99"
                    tot-eff          FORMAT ">>9.99"
                    mch-dt-act at 98 FORMAT ">>>9.99"
                    dt-eff           FORMAT ">>9.99"
                    mch-qty-ton
                    mch-qty-msf /*mch-qty-prod*/
                    /*mch-qty-expect*/
                    /*mch-qty-tonmsf WHEN tb_tonmsf @ mch-qty-prod format ">>,>>9.99" */ skip
                    fill("-", 126) format "x(126)" at 7
                    with frame detm2ton STREAM-IO width 132 no-labels no-box.

             ELSE IF tb_tonmsf AND rd_tonmsfqty = "QM" THEN
                display fill("-", 126) format "x(126)" at 7 skip
                        "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                        tt-srt.shift at 20 format ">>"
                        mch-mr-std at 23 FORMAT ">>>9.99"
                        mch-mr-act       FORMAT ">>>9.99"
                        mr-eff           FORMAT ">>>9.99"
                        mch-run-std at 48 FORMAT ">>>9.99"
                        mch-run-act      FORMAT ">>>9.99"
                        run-eff          FORMAT ">>>9.99"
                        tot-std-hrs at 73 FORMAT ">>>9.99"
                        tot-act-hrs      FORMAT ">>>9.99"
                        tot-eff          FORMAT ">>9.99"
                        mch-dt-act at 98 FORMAT ">>>9.99"
                        dt-eff           FORMAT ">>9.99"
                        mch-qty-prod 
                        mch-qty-msf /*mch-qty-prod*/
                        /*mch-qty-expect*/
                        /*mch-qty-tonmsf WHEN tb_tonmsf @ mch-qty-prod format ">>,>>9.99" */ skip
                        fill("-", 126) format "x(126)" at 7
                        with frame detm2ton1 STREAM-IO width 132 no-labels no-box.

              ELSE IF tb_tonmsf AND rd_tonmsfqty = "QT" THEN
                  display fill("-", 126) format "x(126)" at 7 skip
                      "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                       tt-srt.shift at 20 format ">>"
                       mch-mr-std at 23 FORMAT ">>>9.99"
                       mch-mr-act       FORMAT ">>>9.99"
                       mr-eff           FORMAT ">>>9.99"
                       mch-run-std at 48 FORMAT ">>>9.99"
                       mch-run-act      FORMAT ">>>9.99"
                       run-eff          FORMAT ">>>9.99"
                       tot-std-hrs at 73 FORMAT ">>>9.99"
                       tot-act-hrs      FORMAT ">>>9.99"
                       tot-eff          FORMAT ">>9.99"
                       mch-dt-act at 98 FORMAT ">>>9.99"
                       dt-eff           FORMAT ">>9.99"
                       mch-qty-prod
                       mch-qty-ton
                      /*mch-qty-expect*/
                      /*mch-qty-tonmsf WHEN tb_tonmsf @ mch-qty-prod format ">>,>>9.99" */ skip
                      fill("-", 126) format "x(126)" at 7
                      with frame detm2ton2 STREAM-IO width 132 no-labels no-box.

              ELSE display fill("-", 126) format "x(126)" at 7 skip
                    "*" at 5 mach.m-dscr at 7 format "x(10)" when avail mach
                    tt-srt.shift at 20 format ">>"
                    mch-mr-std at 23 FORMAT ">>>9.99"
                    mch-mr-act       FORMAT ">>>9.99"
                    mr-eff           FORMAT ">>>9.99"
                    mch-run-std at 48 FORMAT ">>>9.99"
                    mch-run-act      FORMAT ">>>9.99"
                    run-eff          FORMAT ">>>9.99"
                    tot-std-hrs at 73 FORMAT ">>>9.99"
                    tot-act-hrs      FORMAT ">>>9.99"
                    tot-eff          FORMAT ">>9.99"
                    mch-dt-act at 98 FORMAT ">>>9.99"
                    dt-eff           FORMAT ">>9.99"
                    mch-qty-prod
                    mch-qty-expect
                    SKIP
                    fill("-", 126) format "x(126)" at 7
                    with frame detm2 STREAM-IO width 132 no-labels no-box.
         END.
         assign mch-mr-std = 0
                mch-mr-act = 0
                mch-run-std = 0
                mch-run-act = 0
                mch-dt-act = 0
                mch-qty-prod = 0
                mch-qty-expect = 0
                mch-qty-msf = 0
                mch-qty-msf = 0
                mch-qty-ton = 0.
      end.

      if last-of(tt-srt.shift) then
      do:
         {pc/rep/mchprdhr.i "shf"}
         IF TB_round = YES THEN DO:
            IF tb_tonmsf AND rd_tonmsfqty = "tm" THEN
            display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23
                    shf-mr-act
                    mr-eff
                    shf-run-std at 48
                    shf-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    shf-dt-act at 98
                    dt-eff
                    shf-qty-ton /*shf-qty-prod*/
                    shf-qty-msf
                    /*shf-qty-expect*/
                    /*shf-qty-tonmsf WHEN tb_tonmsf  @ shf-qty-prod format ">>,>>9.99" */ skip
                    fill("=", 126) format "x(126)" at 7
                    with frame detston STREAM-IO width 132 no-labels no-box.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "qm" THEN
            display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23
                    shf-mr-act
                    mr-eff
                    shf-run-std at 48
                    shf-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    shf-dt-act at 98
                    dt-eff
                    shf-qty-prod
                    shf-qty-msf
                    /*shf-qty-expect*/
                    /*shf-qty-tonmsf WHEN tb_tonmsf  @ shf-qty-prod format ">>,>>9.99" */ skip
                    fill("=", 126) format "x(126)" at 7
                    with frame detston1 STREAM-IO width 132 no-labels no-box.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "qt" THEN
            display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23
                    shf-mr-act
                    mr-eff
                    shf-run-std at 48
                    shf-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    shf-dt-act at 98
                    dt-eff
                    shf-qty-prod
                    shf-qty-ton
                    /*shf-qty-expect*/
                    /*shf-qty-tonmsf WHEN tb_tonmsf  @ shf-qty-prod format ">>,>>9.99" */ skip
                    fill("=", 126) format "x(126)" at 7
                    with frame detston2 STREAM-IO width 132 no-labels no-box.


            ELSE display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23
                    shf-mr-act
                    mr-eff
                    shf-run-std at 48
                    shf-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    shf-dt-act at 98
                    dt-eff
                    shf-qty-prod
                    shf-qty-expect
                    SKIP
                    fill("=", 126) format "x(126)" at 7
                    with frame dets STREAM-IO width 132 no-labels no-box.
         END.
         ELSE DO:
            IF tb_tonmsf AND rd_tonmsfqty = "tm" THEN
            display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23 FORMAT ">>>9.99"
                    shf-mr-act       FORMAT ">>>9.99"
                    mr-eff           FORMAT ">>>9.99"
                    shf-run-std at 48 FORMAT ">>>9.99"
                    shf-run-act      FORMAT ">>>9.99"
                    run-eff          FORMAT ">>>9.99"
                    tot-std-hrs at 73 FORMAT ">>>9.99"
                    tot-act-hrs      FORMAT ">>>9.99"
                    tot-eff          FORMAT ">>9.99"
                    shf-dt-act at 98 FORMAT ">>>9.99"
                    dt-eff           FORMAT ">>9.99"
                    shf-qty-ton /*shf-qty-prod*/
                    shf-qty-msf
                    /*shf-qty-expect */
                    /*shf-qty-tonmsf WHEN tb_tonmsf @ shf-qty-prod format ">>,>>9.99" */ skip
                    fill("=", 126) format "x(126)" at 7
                    with frame dets2ton STREAM-IO width 132 no-labels NO-BOX.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "qm" THEN
            display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23 FORMAT ">>>9.99"
                    shf-mr-act       FORMAT ">>>9.99"
                    mr-eff           FORMAT ">>>9.99"
                    shf-run-std at 48 FORMAT ">>>9.99"
                    shf-run-act      FORMAT ">>>9.99"
                    run-eff          FORMAT ">>>9.99"
                    tot-std-hrs at 73 FORMAT ">>>9.99"
                    tot-act-hrs      FORMAT ">>>9.99"
                    tot-eff          FORMAT ">>9.99"
                    shf-dt-act at 98 FORMAT ">>>9.99"
                    dt-eff           FORMAT ">>9.99"
                    shf-qty-prod
                    shf-qty-msf
                    /*shf-qty-expect */
                    /*shf-qty-tonmsf WHEN tb_tonmsf @ shf-qty-prod format ">>,>>9.99" */ skip
                    fill("=", 126) format "x(126)" at 7
                    with frame dets2ton1 STREAM-IO width 132 no-labels NO-BOX.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "qt" THEN
            display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23 FORMAT ">>>9.99"
                    shf-mr-act       FORMAT ">>>9.99"
                    mr-eff           FORMAT ">>>9.99"
                    shf-run-std at 48 FORMAT ">>>9.99"
                    shf-run-act      FORMAT ">>>9.99"
                    run-eff          FORMAT ">>>9.99"
                    tot-std-hrs at 73 FORMAT ">>>9.99"
                    tot-act-hrs      FORMAT ">>>9.99"
                    tot-eff          FORMAT ">>9.99"
                    shf-dt-act at 98 FORMAT ">>>9.99"
                    dt-eff           FORMAT ">>9.99"
                    shf-qty-prod
                    shf-qty-ton
                    /*shf-qty-expect */
                    /*shf-qty-tonmsf WHEN tb_tonmsf @ shf-qty-prod format ">>,>>9.99" */ skip
                    fill("=", 126) format "x(126)" at 7
                    with frame dets2ton2 STREAM-IO width 132 no-labels NO-BOX.

             ELSE display "**" at 4
                    "SHIFT TOT" at 7
                    tt-srt.shift at 20 format ">>"
                    shf-mr-std at 23 FORMAT ">>>9.99"
                    shf-mr-act       FORMAT ">>>9.99"
                    mr-eff           FORMAT ">>>9.99"
                    shf-run-std at 48 FORMAT ">>>9.99"
                    shf-run-act      FORMAT ">>>9.99"
                    run-eff          FORMAT ">>>9.99"
                    tot-std-hrs at 73 FORMAT ">>>9.99"
                    tot-act-hrs      FORMAT ">>>9.99"
                    tot-eff          FORMAT ">>9.99"
                    shf-dt-act at 98 FORMAT ">>>9.99"
                    dt-eff           FORMAT ">>9.99"
                    shf-qty-prod
                    shf-qty-expect 
                    SKIP
                    fill("=", 126) format "x(126)" at 7
                    with frame dets2 STREAM-IO width 132 no-labels NO-BOX.
         END.
         if v-show1 then
            put "** SHIFT JOBS" at 4
                shf-jobs        at 18 format ">>>>".
               
         put fill("=", 126) format "x(126)" at 7.

         assign shf-mr-std = 0
                shf-mr-act = 0
                shf-run-std = 0
                shf-run-act = 0
                shf-dt-act = 0
                shf-qty-prod = 0
                shf-qty-expect = 0
                shf-jobs = 0
                shf-qty-msf = 0
                shf-qty-ton = 0.
      end.

      if last-of(tt-srt.dept) then
      do:
         find dept where dept.company = cocode and
                         dept.cod     = tt-srt.dept
                         no-lock no-error.
         if not avail dept then
            find dept where dept.company = "" and
                            dept.cod     = tt-srt.dept
                            no-lock no-error.
         {pc/rep/mchprdhr.i "dpt"}
         IF TB_round = YES THEN DO:
            IF tb_tonmsf AND rd_tonmsfqty = "tm" THEN
            display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                    dpt-mr-std at 23
                    dpt-mr-act
                    mr-eff
                    dpt-run-std at 48
                    dpt-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    dpt-dt-act at 98
                    dt-eff
                    dpt-qty-ton 
                    dpt-qty-msf /*dpt-qty-prod*/
                    /*dpt-qty-expect*/
                    /*dpt-qty-tonmsf WHEN tb_tonmsf @ dpt-qty-prod */ skip
                    fill("*", 132) format "x(132)"
                    with frame detdton STREAM-IO width 132 no-labels no-box.

           ELSE IF tb_tonmsf AND rd_tonmsfqty = "qm" THEN
              display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                                                                         dpt-mr-std at 23
                                                                         dpt-mr-act
                                                                         mr-eff
                                                                         dpt-run-std at 48
                                                                         dpt-run-act
                                                                         run-eff
                                                                         tot-std-hrs at 73
                                                                         tot-act-hrs
                                                                         tot-eff
                                                                         dpt-dt-act at 98
                                                                         dt-eff
                                                                         dpt-qty-prod 
                                                                         dpt-qty-msf 
                                                                         /*dpt-qty-expect*/
                                                                         /*dpt-qty-tonmsf WHEN tb_tonmsf @ dpt-qty-prod */ skip
                                                                         fill("*", 132) format "x(132)"
                                                                         with frame detdton1 STREAM-IO width 132 no-labels no-box.
           ELSE IF tb_tonmsf AND rd_tonmsfqty = "qt" THEN
              display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                                                                           dpt-mr-std at 23
                                                                           dpt-mr-act
                                                                           mr-eff
                                                                           dpt-run-std at 48
                                                                           dpt-run-act
                                                                           run-eff
                                                                           tot-std-hrs at 73
                                                                           tot-act-hrs
                                                                           tot-eff
                                                                           dpt-dt-act at 98
                                                                           dt-eff
                                                                            dpt-qty-prod
                                                                           dpt-qty-ton 
                                                                           /*dpt-qty-expect*/
                                                                           /*dpt-qty-tonmsf WHEN tb_tonmsf @ dpt-qty-prod */ skip
                                                                           fill("*", 132) format "x(132)"
                                                                           with frame detdton2 STREAM-IO width 132 no-labels no-box.

               
          ELSE display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                    dpt-mr-std at 23
                    dpt-mr-act
                    mr-eff
                    dpt-run-std at 48
                    dpt-run-act
                    run-eff
                    tot-std-hrs at 73
                    tot-act-hrs
                    tot-eff
                    dpt-dt-act at 98
                    dt-eff
                    dpt-qty-prod
                    dpt-qty-expect
                    SKIP
                    fill("*", 132) format "x(132)"
                    with frame detd STREAM-IO width 132 no-labels no-box.
         END.
         ELSE DO:
            IF tb_tonmsf AND rd_tonmsfqty = "tm" THEN
            display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                    dpt-mr-std at 23   FORMAT ">>>9.99"
                    dpt-mr-act         FORMAT ">>>9.99"
                    mr-eff             FORMAT ">>>9.99"
                    dpt-run-std at 48  FORMAT ">>>9.99"
                    dpt-run-act        FORMAT ">>>9.99"
                    run-eff            FORMAT ">>9.99"
                    tot-std-hrs at 73  FORMAT ">>>9.99"
                    tot-act-hrs        FORMAT ">>>9.99"
                    tot-eff            FORMAT ">>9.99"
                    dpt-dt-act at 98   FORMAT ">>>9.99"
                    dt-eff             FORMAT ">>9.99"
                    dpt-qty-ton /*dpt-qty-prod       */
                    dpt-qty-msf
                    /*dpt-qty-expect*/
                    /*dpt-qty-tonmsf WHEN tb_tonmsf @ dpt-qty-prod*/  skip
                    fill("*", 132) format "x(132)"
                    with frame detd2ton STREAM-IO width 132 no-labels NO-BOX.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "qm" THEN
            display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                    dpt-mr-std at 23   FORMAT ">>>9.99"
                    dpt-mr-act         FORMAT ">>>9.99"
                    mr-eff             FORMAT ">>>9.99"
                    dpt-run-std at 48  FORMAT ">>>9.99"
                    dpt-run-act        FORMAT ">>>9.99"
                    run-eff            FORMAT ">>9.99"
                    tot-std-hrs at 73  FORMAT ">>>9.99"
                    tot-act-hrs        FORMAT ">>>9.99"
                    tot-eff            FORMAT ">>9.99"
                    dpt-dt-act at 98   FORMAT ">>>9.99"
                    dt-eff             FORMAT ">>9.99"
                    dpt-qty-prod      
                    dpt-qty-msf
                    /*dpt-qty-expect*/
                    /*dpt-qty-tonmsf WHEN tb_tonmsf @ dpt-qty-prod*/  skip
                    fill("*", 132) format "x(132)"
                    with frame detd2ton1 STREAM-IO width 132 no-labels NO-BOX.
            ELSE IF tb_tonmsf AND rd_tonmsfqty = "qt" THEN
            display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                    dpt-mr-std at 23   FORMAT ">>>9.99"
                    dpt-mr-act         FORMAT ">>>9.99"
                    mr-eff             FORMAT ">>>9.99"
                    dpt-run-std at 48  FORMAT ">>>9.99"
                    dpt-run-act        FORMAT ">>>9.99"
                    run-eff            FORMAT ">>9.99"
                    tot-std-hrs at 73  FORMAT ">>>9.99"
                    tot-act-hrs        FORMAT ">>>9.99"
                    tot-eff            FORMAT ">>9.99"
                    dpt-dt-act at 98   FORMAT ">>>9.99"
                    dt-eff             FORMAT ">>9.99"
                    dpt-qty-prod       
                    dpt-qty-ton 
                    /*dpt-qty-msf*/
                    /*dpt-qty-expect*/
                    /*dpt-qty-tonmsf WHEN tb_tonmsf @ dpt-qty-prod*/  skip
                    fill("*", 132) format "x(132)"
                    with frame detd2ton2 STREAM-IO width 132 no-labels NO-BOX.

             ELSE display "***" at 3 dept.dscr at 7 format "x(10)" when avail dept
                    dpt-mr-std at 23   FORMAT ">>>9.99"
                    dpt-mr-act         FORMAT ">>>9.99"
                    mr-eff             FORMAT ">>>9.99"
                    dpt-run-std at 48  FORMAT ">>>9.99"
                    dpt-run-act        FORMAT ">>>9.99"
                    run-eff            FORMAT ">>9.99"
                    tot-std-hrs at 73  FORMAT ">>>9.99"
                    tot-act-hrs        FORMAT ">>>9.99"
                    tot-eff            FORMAT ">>9.99"
                    dpt-dt-act at 98   FORMAT ">>>9.99"
                    dt-eff             FORMAT ">>9.99"
                    dpt-qty-prod       
                    dpt-qty-expect
                    SKIP          
                    fill("*", 132) format "x(132)"
                    with frame detd2 STREAM-IO width 132 no-labels NO-BOX.
         END.
         assign dpt-mr-std = 0
                dpt-mr-act = 0
                dpt-run-std = 0
                dpt-run-act = 0
                dpt-dt-act = 0
                dpt-qty-prod = 0
                dpt-qty-expect = 0
                dpt-qty-msf = 0
                dpt-qty-ton = 0.
         page.
      end.
   end. /* each item */

   IF v-tot-uni-jobs THEN
   DO:
      for each tt-srt use-index dept-idx
                    break by tt-srt.job-no
                          by tt-srt.job-no2:

         IF FIRST-OF(tt-srt.job-no2) THEN
            tot-jobs = tot-jobs + 1.
      END.

      PUT "** TOTAL JOBS" AT 4
          tot-jobs        AT 18 FORMAT ">>>>" SKIP.
   END.
