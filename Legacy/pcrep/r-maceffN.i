DEF VAR v-up-tmp AS INT NO-UNDO.
def var v-out-tmp as INT NO-UNDO.
DEF VAR v-qty AS DEC NO-UNDO.
DEF BUFFER b-mach FOR mach.
DEF VAR v-date2 AS DATE NO-UNDO.
def var v-blanks    as   dec format "->>>>,>>>,>>9" .

EMPTY TEMP-TABLE work-tmp.
EMPTY TEMP-TABLE work-rep.
EMPTY TEMP-TABLE work-rep-copy.

FOR EACH b-mach FIELDS(m-code) WHERE
    b-mach.company EQ cocode AND
    b-mach.loc EQ locode AND
    b-mach.m-code GE v-mach[1] AND
    b-mach.m-code LE v-mach[2]
    NO-LOCK:

    DO v-date2 = v-date[1] TO v-date[2]:
    
       FOR EACH mch-act WHERE
           mch-act.company EQ cocode AND
           mch-act.m-code  EQ b-mach.m-code AND
           mch-act.op-date EQ v-date2 AND
           mch-act.dept    GE v-dept[1] AND
           mch-act.dept    LE v-dept[2] AND
           (NOT ll-shifts OR
              CAN-DO(lv-shifts,TRIM(STRING(mch-act.shift,">>>>>"))))
           NO-LOCK
           USE-INDEX dly-idx:
            {custom/statusMsg.i " 'Processing Machine#  '  + mch-act.m-code "}
      
           {pc/rep/mch-eff.i}
       END.
    END. /*do v-date*/
END.


FOR EACH work-tmp
    break by work-tmp.m-code
          by work-tmp.shift-sort
          by work-tmp.job
          by work-tmp.job-no
          by work-tmp.job-no2
          by work-tmp.frm
          by work-tmp.blank-no:

     {custom/statusMsg.i " 'Processing Machine#  '  + work-tmp.m-code "}

    if last-of(work-tmp.blank-no) then do:

       for each job-hdr FIELDS(job job-no job-no2 i-no)
          where job-hdr.company   eq cocode
            and job-hdr.job       eq work-tmp.job
            and job-hdr.job-no    eq work-tmp.job-no
            and job-hdr.job-no2   eq work-tmp.job-no2
            and job-hdr.frm       eq work-tmp.frm
            and (job-hdr.blank-no eq work-tmp.blank-no or
                 work-tmp.blank-no eq 0)
            no-lock:

            for each fg-act FIELDS(qty)
               where fg-act.company eq cocode
                 and fg-act.job     eq job-hdr.job
                 and fg-act.job-no  eq job-hdr.job-no
                 and fg-act.job-no2 eq job-hdr.job-no2
                 and fg-act.i-no    eq job-hdr.i-no
                 no-lock:
             
                RUN fg/GetProductionQty.p (INPUT cocode,
                                INPUT work-tmp.job-no,
                                INPUT work-tmp.job-no2,
                                INPUT work-tmp.i-no,
                                INPUT NO,
                                OUTPUT v-blanks).

                 work-tmp.qty-fg-rec = work-tmp.qty-fg-rec + v-blanks /*fg-act.qty*/ .
        
                 find first itemfg
                      where itemfg.company eq cocode
                        and itemfg.i-no    eq job-hdr.i-no
                      no-lock no-error.

                 work-tmp.msf-fg-rec = work-tmp.msf-fg-rec
                                     + (v-blanks /*fg-act.qty*/ *
                                        (if avail itemfg then itemfg.t-sqft
                                         else 1) / 1000).
                 
          end. /*end for each fg-act*/
       END. /*each job-hdr*/

       RELEASE est.

       FIND FIRST job
            where job.company            eq cocode
              and job.job                eq work-tmp.job
              and job.job-no             eq work-tmp.job-no
              and job.job-no2            eq work-tmp.job-no2
              NO-LOCK NO-ERROR.

       IF AVAIL job THEN
          find first est
              where est.company eq job.company
                and est.est-no  eq job.est-no
              no-lock no-error.

       if avail est then
          for each eb FIELDS(num-up)
             where eb.company   eq est.company
               and eb.est-no    eq est.est-no
               and eb.form-no   eq work-tmp.frm
               and (eb.blank-no eq work-tmp.blank-no or work-tmp.blank-no eq 0)
             no-lock:
        
             v-up-tmp = v-up-tmp + eb.num-up.
          end.
      
      else v-up-tmp = v-up-tmp + 1.
    END. /*last of work-tmp.blank-no*/

    if last-of(work-tmp.frm) then do:

       for each job-mat
          where job-mat.company eq cocode
            and job-mat.job     eq work-tmp.job
            and job-mat.job-no  eq work-tmp.job-no
            and job-mat.job-no2 eq work-tmp.job-no2
            and job-mat.frm     eq work-tmp.frm
          use-index seq-idx no-lock,
          
          first ITEM FIELDS(s-dep)
          where item.company  eq cocode
            and item.i-no     eq job-mat.i-no
            and item.mat-type eq "B"
          no-lock,

          each mat-act FIELDS(qty)
          where mat-act.company eq cocode
            and mat-act.job     eq job-mat.job
            and mat-act.job-no  eq job-mat.job-no
            and mat-act.job-no2 eq job-mat.job-no2
            and mat-act.s-num   eq job-mat.frm
            and mat-act.b-num   eq job-mat.blank-no
            and mat-act.i-no    eq job-mat.i-no
          use-index job no-lock:
          
          run sys/ref/convquom.p(job-mat.qty-uom, "EA",
                                 job-mat.basis-w, job-mat.len,
                                 job-mat.wid, item.s-dep,
                                 mat-act.qty, output v-qty).
                               
          work-tmp.qty-sheets = work-tmp.qty-sheets + v-qty.

          run sys/ref/convquom.p(job-mat.qty-uom, "MSF",
                                 job-mat.basis-w, job-mat.len,
                                 job-mat.wid, item.s-dep,
                                 mat-act.qty, output v-qty).
                               
          work-tmp.msf-sheets = work-tmp.msf-sheets + v-qty.
      end.

      IF work-tmp.qty-sheets EQ 0 THEN      /* get sheets from slitter */
         FOR EACH mch-act FIELDS(waste qty)
            WHERE mch-act.company EQ cocode
              AND mch-act.job     EQ work-tmp.job
              AND mch-act.job-no  EQ work-tmp.job-no
              AND mch-act.job-no2 EQ work-tmp.job-no2
              AND mch-act.frm     EQ work-tmp.frm
              AND mch-act.dept    EQ "RC"
              AND mch-act.pass    LE 1
            NO-LOCK:

            work-tmp.qty-sheets = work-tmp.qty-sheets
                                + (mch-act.waste + mch-act.qty).
         END.

      IF work-tmp.msf-sheets EQ 0 THEN      /* get sheets from slitter */
         FOR EACH mch-act FIELDS(waste qty)
            WHERE mch-act.company EQ cocode
              AND mch-act.job     EQ work-tmp.job
              AND mch-act.job-no  EQ work-tmp.job-no
              AND mch-act.job-no2 EQ work-tmp.job-no2
              AND mch-act.frm     EQ work-tmp.frm
              AND mch-act.dept    EQ "RC"
              AND mch-act.pass    LE 1
            NO-LOCK:

            work-tmp.msf-sheets = work-tmp.msf-sheets
                                + (mch-act.waste + mch-act.qty).
         END.

      release ef.
      
      if avail est then
         find first ef
              where ef.company eq est.company
                and ef.est-no  eq est.est-no
                and ef.form-no eq work-tmp.frm
              no-lock no-error.

      {sys/inc/roundup.i work-tmp.qty-sheets}
      
      if v-up-tmp eq 0 then v-up-tmp = 1.

      IF AVAIL ef THEN
         RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out-tmp).
      
      ASSIGN
         v-up-tmp  = v-up-tmp * v-out-tmp
         work-tmp.qty-scrap-rec = work-tmp.qty-sheets -
                                   (work-tmp.qty-fg-rec / v-up-tmp)
         work-tmp.msf-scrap-rec = work-tmp.msf-sheets -
                                   (work-tmp.msf-fg-rec)
         v-up-tmp = 0.
     
    END. /*last-of(mch-act.frm)*/

END. /*each work-tmp*/

for each work-tmp BREAK BY work-tmp.sort-field
                        BY work-tmp.job-no 
                        BY work-tmp.job-no2:
   find first job-mch where job-mch.company  = cocode and
                            job-mch.job      eq work-tmp.job and
                            job-mch.frm      = work-tmp.frm and
                            (job-mch.blank-no = work-tmp.blank-no or
                             work-tmp.blank-no = 0) and
                            job-mch.m-code   = work-tmp.m-code and
                            job-mch.pass     = work-tmp.pass
                            no-lock no-error.
   if not avail job-mch then
   find first job-mch where job-mch.company eq cocode and
                            job-mch.job      eq work-tmp.job and
                            job-mch.frm      eq work-tmp.frm and
                            (job-mch.blank-no = work-tmp.blank-no or
                             work-tmp.blank-no = 0) and
                            job-mch.m-code   eq work-tmp.m-code
                            no-lock no-error.
   if not avail job-mch then
   find first job-mch where job-mch.company eq cocode and
                            job-mch.job     eq work-tmp.job and
                            job-mch.frm     eq work-tmp.frm and
                            job-mch.m-code  eq work-tmp.m-code and
                            job-mch.speed   ne 0
                            no-lock no-error.
   if not avail job-mch then
   find first job-mch where job-mch.company eq cocode and
                            job-mch.job     eq work-tmp.job and
                            job-mch.frm     eq work-tmp.frm and
                            job-mch.m-code  eq work-tmp.m-code
                            no-lock no-error.
   if available job-mch then
   do:
      IF work-tmp.qty NE 0 THEN
      DO:
         IF job-mch.speed NE 0 THEN
         DO:
            IF CAN-FIND(FIRST mach WHERE
               mach.company EQ cocode AND
               mach.loc     EQ locode AND
               mach.m-code  EQ job-mch.m-code AND
               mach.therm   EQ YES AND
               (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
               FOR EACH job-mat FIELDS(i-no len) WHERE
                   job-mat.company eq cocode AND
                   job-mat.job = work-tmp.job AND
                   job-mat.frm EQ work-tmp.frm AND
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
         
                 run-hr = (work-tmp.qty * job-mat.len / 12) / job-mch.speed.
                 LEAVE.
               END.
            ELSE
               run-hr = work-tmp.qty / job-mch.speed.
         END.
         ELSE
            run-hr = 0.
      END.
      ELSE
         run-hr = job-mch.run-hr.

      IF work-tmp.tot-mr-hours NE 0 THEN
         mr-hr  = job-mch.mr-hr * (work-tmp.m-act-hrs / work-tmp.tot-mr-hours).
      ELSE
         mr-hr  = 0.
   end.
   else
      assign run-hr = 0
             mr-hr  = 0.
   
   find first work-rep where work-rep.sort-field EQ work-tmp.sort-field and
                             work-rep.dept   = work-tmp.dept and
                             work-rep.m-code = work-tmp.m-code
                             no-error.
   if not available work-rep then
   do:
      create work-rep.
      assign work-rep.sort-field = work-tmp.sort-field
             work-rep.dept     = work-tmp.dept
             work-rep.m-code   = work-tmp.m-code
             work-rep.sch-m-code = work-tmp.sch-m-code.
   end.
   assign work-rep.r-std-hrs = work-rep.r-std-hrs + run-hr
          work-rep.r-act-hrs = work-rep.r-act-hrs + work-tmp.r-act-hrs
          work-rep.m-std-hrs = work-rep.m-std-hrs + mr-hr
          work-rep.m-act-hrs = work-rep.m-act-hrs + work-tmp.m-act-hrs
          work-rep.dt-chg-hrs = work-rep.dt-chg-hrs + work-tmp.dt-chg-hrs
          work-rep.dt-nochg-hrs = work-rep.dt-nochg-hrs +
                                  work-tmp.dt-nochg-hrs
          work-rep.qty          = work-rep.qty + work-tmp.qty
          work-rep.msf          = work-rep.msf + work-tmp.msf

          work-rep.qty-fg-rec = work-rep.qty-fg-rec + work-tmp.qty-fg-rec
          work-rep.msf-fg-rec = work-rep.msf-fg-rec + work-tmp.msf-fg-rec
          work-rep.qty-scrap-rec = work-rep.qty-scrap-rec
                                 + work-tmp.qty-scrap-rec
          work-rep.msf-scrap-rec = work-rep.msf-scrap-rec
                                 + work-tmp.msf-scrap-rec
          work-rep.qty-sheets    = work-rep.qty-sheets
                                 + work-tmp.qty-sheets
          work-rep.perc-total-scrap = work-rep.qty-scrap-rec / work-rep.qty-sheets
                                    * 100.

   IF LAST-OF(work-tmp.job-no2) THEN DO:
        work-rep.no-jobs = work-rep.no-jobs + 1.
        FOR EACH mch-act
            WHERE mch-act.company EQ cocode
              AND mch-act.job     EQ work-tmp.job
              AND mch-act.job-no  EQ work-tmp.job-no
              AND mch-act.job-no2 EQ work-tmp.job-no2
              AND mch-act.m-code  EQ work-tmp.m-code
              AND mch-act.shift   EQ INT(work-tmp.shift-sort)
              AND CAN-FIND(FIRST job-code 
                           WHERE job-code.CODE EQ mch-act.CODE
                             AND job-code.cat EQ "MR")
            NO-LOCK
            BREAK BY mch-act.m-code:
            IF FIRST-OF(mch-act.m-code) THEN
                work-rep.no-setups = work-rep.no-setups + 1.
        END.
   END.

          
end. /*end each work-temp*/

IF rs_machine EQ "Schedule" THEN
DO:
   FOR EACH work-rep:

       FIND FIRST work-rep-copy WHERE
            work-rep-copy.m-code EQ work-rep.sch-m-code AND
            work-rep-copy.sort-field EQ work-rep.sort-field AND
            work-rep-copy.dept EQ work-rep.dept
            NO-ERROR.

       IF NOT AVAIL work-rep-copy THEN
       DO:
          CREATE work-rep-copy.
          ASSIGN work-rep-copy.m-code = work-rep.sch-m-code
                 work-rep-copy.sort-field = work-rep.sort-field
                 work-rep-copy.dept = work-rep.dept.
       END.

       ASSIGN
          work-rep-copy.no-jobs = work-rep-copy.no-jobs + work-rep.no-jobs
          work-rep-copy.no-setups = work-rep-copy.no-setups + work-rep.no-setups
          work-rep-copy.r-std-hrs = work-rep-copy.r-std-hrs + work-rep.r-std-hrs
          work-rep-copy.r-act-hrs = work-rep-copy.r-act-hrs + work-rep.r-act-hrs
          work-rep-copy.m-std-hrs = work-rep-copy.m-std-hrs + work-rep.m-std-hrs
          work-rep-copy.m-act-hrs = work-rep-copy.m-act-hrs + work-rep.m-act-hrs
          work-rep-copy.dt-chg-hrs = work-rep-copy.dt-chg-hrs + work-rep.dt-chg-hrs
          work-rep-copy.dt-nochg-hrs = work-rep-copy.dt-nochg-hrs + work-rep.dt-nochg-hrs
          work-rep-copy.qty = work-rep-copy.qty + work-rep.qty
          work-rep-copy.msf = work-rep-copy.msf + work-rep.msf
          work-rep-copy.qty-fg-rec = work-rep-copy.qty-fg-rec + work-rep.qty-fg-rec
          work-rep-copy.msf-fg-rec = work-rep-copy.msf-fg-rec + work-rep.msf-fg-rec
          work-rep-copy.qty-scrap-rec = work-rep-copy.qty-scrap-rec + work-rep.qty-scrap-rec
          work-rep-copy.msf-scrap-received = work-rep-copy.msf-scrap-received + work-rep.msf-scrap-received
          work-rep-copy.qty-sheets = work-rep-copy.qty-sheets + work-rep.qty-sheets.

       RELEASE work-rep-copy.

       DELETE work-rep.
   END.

   FOR EACH work-rep-copy:
       CREATE work-rep.
       BUFFER-COPY work-rep-copy TO work-rep
          ASSIGN work-rep.perc-total-scrap = work-rep.qty-scrap-rec / work-rep.qty-sheets
                                             * 100.
       RELEASE work-rep.
       DELETE work-rep-copy.
   END.
END.

FOR EACH work-rep BREAK BY work-rep.sort-field
                        BY work-rep.dept
                        BY work-rep.m-code:

   IF rd_sort NE "Dept" AND FIRST-OF(work-rep.sort-field) THEN DO:
      lv-sort = TRIM(rd_sort) + ": " + TRIM(work-rep.sort-field).

      IF FIRST(work-rep.sort-field) THEN VIEW FRAME r-top2.

      PAGE.
   END.

   ASSIGN
      qty-hr = work-rep.qty / work-rep.r-act-hrs
      chg-hrs = work-rep.r-act-hrs + work-rep.m-act-hrs +
                work-rep.dt-chg-hrs
      tot-hrs = chg-hrs + work-rep.dt-nochg-hrs
      std-hrs = work-rep.r-std-hrs + work-rep.m-std-hrs
      eff-pct = (std-hrs / chg-hrs) * 100.00
      pct-utl = (std-hrs / tot-hrs) * 100.00
      pct-dt  = ((work-rep.dt-nochg-hrs + work-rep.dt-chg-hrs) / tot-hrs)
                * 100.00.

   IF qty-hr = ? THEN qty-hr = 0.
   if eff-pct = ? then eff-pct = 0.
   if pct-utl = ? then pct-utl = 0.
   if pct-dt = ? then pct-dt = 0.

   FIND FIRST mach WHERE
        mach.company = cocode AND
        mach.m-code  = work-rep.m-code
        NO-LOCK.
         {custom/statusMsg.i " 'Processing Machine#  '  + work-rep.m-code "}
   /*IF tb_msf THEN DO:*/
     dt-hrs = work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs.

     /*display work-rep.m-code          COLUMN-LABEL "MACH!CODE"
             mach.m-dscr              column-label "DESCRIPTION"
             work-rep.qty             column-label "QUANTITY"
             work-rep.msf             column-label "MSF"
             qty-hr                   column-label "QTY!HOUR"
             work-rep.r-act-hrs       column-label "RUN!HOURS"
             work-rep.m-act-hrs       column-label "MR!HOURS"
             dt-hrs                   column-label "D/T"
             chg-hrs                  column-label "TOTAL!CHARGE"
             tot-hrs                  column-label "TOTAL!HOURS"
             std-hrs                  column-label "STD!HOURS"
             eff-pct                  column-label "EFFIC!PERCENT"
             pct-utl                  column-label "PERCENT!UTILIZED"
             pct-dt                   column-label "D/T!PERCENT"
           with frame det1 STREAM-IO width 200 no-box down.
     
     down with frame det1.*/

        ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "sft"    THEN cVarValue = string(work-rep.sort-field,"x(5)") .
                         WHEN "mach-code"   THEN cVarValue = string(work-rep.m-code,"x(6)").
                         WHEN "desc"   THEN cVarValue = STRING(mach.m-dscr,"x(20)").
                         WHEN "qty"  THEN cVarValue = STRING(work-rep.qty,"->,>>>,>>9") .
                         WHEN "msf"   THEN cVarValue = STRING(work-rep.msf,"->>>>9.999<<") .
                         WHEN "qty-hr"  THEN cVarValue = IF qty-hr <> ? THEN  STRING(qty-hr,"->>>,>>9") ELSE "" .
                         WHEN "run-hrs"   THEN cVarValue = STRING(work-rep.r-act-hrs,"->>>9.99") .
                         WHEN "mr-hrs"  THEN cVarValue = STRING(work-rep.m-act-hrs,"->>>9.99") .

                         WHEN "dt-chg"    THEN cVarValue = string(work-rep.dt-chg-hrs,"->>>9.99") .
                         WHEN "tot-crg"   THEN cVarValue = string(chg-hrs,"->>>9.99").
                         WHEN "dt-crg"   THEN cVarValue = STRING(work-rep.dt-nochg-hrs,"->>>9.99").
                         WHEN "tot-hrs"  THEN cVarValue = STRING(tot-hrs,"->>>9.99") .
                         WHEN "std-hrs"   THEN cVarValue = STRING(std-hrs,"->>>9.99") .

                         WHEN "eff-per"  THEN cVarValue = STRING(eff-pct,"->>>9.99") .
                         WHEN "per-util"   THEN cVarValue = STRING(pct-utl,"->>>9.99") .
                         WHEN "dt-per"  THEN cVarValue = STRING(pct-dt,"->>>9.99") .
                         WHEN "qty-fg-rec"    THEN cVarValue = IF work-rep.qty-fg-rec <> ? THEN string(work-rep.qty-fg-rec,"->>>>>>>>9") ELSE "".
                         WHEN "msf-fg-rec"   THEN cVarValue = IF work-rep.msf-fg-rec <> ? THEN string(work-rep.msf-fg-rec,"->>>>>.999") ELSE "".

                         WHEN "scr-qty"   THEN cVarValue = IF work-rep.qty-scrap-rec <> ? THEN STRING(work-rep.qty-scrap-rec,"->>>>>>>>9") ELSE "".
                         WHEN "scr-msf"  THEN cVarValue = IF work-rep.msf-scrap-rec <> ? THEN STRING(work-rep.msf-scrap-rec,"->>>>>.999") ELSE "".
                         WHEN "tot-scrap"   THEN cVarValue = IF work-rep.perc-total-scrap <> ? THEN STRING(work-rep.perc-total-scrap,"->>>>9.99") ELSE "" .
                         
                         
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
     
    

   assign sort-run-hrs = sort-run-hrs + work-rep.r-act-hrs
          sort-mr-hrs  = sort-mr-hrs  + work-rep.m-act-hrs
          sort-chg-hrs = sort-chg-hrs + work-rep.dt-chg-hrs
          sort-nochg-hrs = sort-nochg-hrs + work-rep.dt-nochg-hrs
          sort-std-hrs = sort-std-hrs + std-hrs
          sort-qty     = sort-qty + work-rep.qty
          sort-qty-sheets = sort-qty-sheets + work-rep.qty-sheets
          sort-msf     = sort-msf + work-rep.msf
          sort-qty-fg-rec = sort-qty-fg-rec + work-rep.qty-fg-rec
          sort-msf-fg-rec = sort-msf-fg-rec + work-rep.msf-fg-rec
          sort-qty-scrap-rec = sort-qty-scrap-rec + work-rep.qty-scrap-rec
          sort-msf-scrap-rec = sort-msf-scrap-rec + work-rep.msf-scrap-rec
          sort-shift-jobs = sort-shift-jobs + work-rep.no-jobs
          sort-shift-setups = sort-shift-setups + work-rep.no-setups
          dpt-run-hrs = dpt-run-hrs + work-rep.r-act-hrs
          dpt-mr-hrs  = dpt-mr-hrs  + work-rep.m-act-hrs
          dpt-chg-hrs = dpt-chg-hrs + work-rep.dt-chg-hrs
          dpt-nochg-hrs = dpt-nochg-hrs + work-rep.dt-nochg-hrs
          dpt-std-hrs = dpt-std-hrs + std-hrs
          dpt-qty     = dpt-qty + work-rep.qty
          dpt-msf     = dpt-msf + work-rep.msf
          rep-run-hrs = rep-run-hrs + work-rep.r-act-hrs
          rep-mr-hrs  = rep-mr-hrs  + work-rep.m-act-hrs
          rep-chg-hrs = rep-chg-hrs + work-rep.dt-chg-hrs
          rep-nochg-hrs = rep-nochg-hrs + work-rep.dt-nochg-hrs
          rep-std-hrs = rep-std-hrs + std-hrs
          rep-qty     = rep-qty  + work-rep.qty
          rep-msf     = rep-msf  + work-rep.msf.

   if last-of(work-rep.dept) then
   do:
      {pc/rep/mcheffhr.i "dpt"}

      find dept where dept.company = cocode and
                      dept.code    = work-rep.dept
                      no-lock no-error.
      if not available dept then
         find dept where dept.company = "" and
                         dept.code    = work-rep.dept
                         no-lock no-error.
      PUT str-line /*fill("-", 130) format 'x(130)' at 8*/ skip.

       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "sft"    THEN cVarValue = "" .
                         WHEN "mach-code"   THEN cVarValue = "".
                         WHEN "desc"   THEN cVarValue = STRING(dept.dscr,"x(20)").
                         WHEN "qty"  THEN cVarValue = STRING(dpt-qty,"->,>>>,>>9") .
                         WHEN "msf"   THEN cVarValue = STRING(dpt-msf,"->>>>9.999<<") .
                         WHEN "qty-hr"  THEN cVarValue = IF qty-hr <> ? THEN  STRING(qty-hr,"->>>,>>9") ELSE "" .
                         WHEN "run-hrs"   THEN cVarValue = STRING(dpt-run-hrs,"->>>9.99") .
                         WHEN "mr-hrs"  THEN cVarValue = STRING(dpt-mr-hrs,"->>>9.99") .

                         WHEN "dt-chg"    THEN cVarValue = string(dpt-chg-hrs + dpt-nochg-hrs,"->>>9.99") .
                         WHEN "tot-crg"   THEN cVarValue = string(chg-hrs,"->>>9.99").
                         WHEN "dt-crg"   THEN cVarValue = STRING(dpt-nochg-hrs,"->>>9.99").
                         WHEN "tot-hrs"  THEN cVarValue = STRING(tot-hrs,"->>>9.99") .
                         WHEN "std-hrs"   THEN cVarValue = STRING(std-hrs,"->>>9.99") .

                         WHEN "eff-per"  THEN cVarValue = STRING(eff-pct,"->>>9.99") .
                         WHEN "per-util"   THEN cVarValue = STRING(pct-utl,"->>>9.99") .
                         WHEN "dt-per"  THEN cVarValue = STRING(pct-dt,"->>>9.99") .
                         WHEN "qty-fg-rec"    THEN cVarValue = "" .
                         WHEN "msf-fg-rec"   THEN cVarValue = "".

                         WHEN "scr-qty"   THEN cVarValue = "".
                         WHEN "scr-msf"  THEN cVarValue = "" .
                         WHEN "tot-scrap"   THEN cVarValue = "" .
                         
                         
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

      

      PUT str-line /*fill("-", 137) format 'x(137)'*/ skip.

      assign dpt-qty = 0
             dpt-msf = 0
             dpt-run-hrs = 0
             dpt-mr-hrs = 0
             dpt-chg-hrs = 0
             dpt-nochg-hrs = 0
             dpt-std-hrs = 0.
   end. /*last-of(work-rep.dept)*/

   if rd_sort NE "Dept" AND LAST-OF(work-rep.sort-field) THEN
   do:
      {pc/rep/mcheffhr.i "sort"}

      put skip(1).

     /* IF tb_msf THEN DO:
        display lv-sort                       @ mach.m-dscr
                sort-qty                      @ work-rep.qty
                sort-msf                      @ work-rep.msf
                qty-hr
                sort-run-hrs                  @ work-rep.r-act-hrs
                sort-mr-hrs                   @ work-rep.m-act-hrs
                sort-chg-hrs + sort-nochg-hrs @ dt-hrs
                chg-hrs
                tot-hrs
                sort-std-hrs                  @ std-hrs
                eff-pct
                pct-utl
                pct-dt
            with frame det1 STREAM-IO width 200 no-box down.
        down with frame det1.*/
       ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "sft"    THEN cVarValue = "" .
                         WHEN "mach-code"   THEN cVarValue = "".
                         WHEN "desc"   THEN cVarValue = STRING(lv-sort,"x(20)").
                         WHEN "qty"  THEN cVarValue = STRING(sort-qty,"->,>>>,>>9") .
                         WHEN "msf"   THEN cVarValue = STRING(sort-msf,"->>>>9.999<<") .
                         WHEN "qty-hr"  THEN cVarValue = IF qty-hr <> ? THEN  STRING(qty-hr,"->>>,>>9")  ELSE "".
                         WHEN "run-hrs"   THEN cVarValue = STRING(sort-run-hrs,"->>>9.99") .
                         WHEN "mr-hrs"  THEN cVarValue = STRING(sort-mr-hrs,"->>>9.99") .

                         WHEN "dt-chg"    THEN cVarValue = string(sort-chg-hrs + sort-nochg-hrs,"->>>9.99") .
                         WHEN "tot-crg"   THEN cVarValue = string(chg-hrs,"->>>9.99").
                         WHEN "dt-crg"   THEN cVarValue = STRING(sort-nochg-hrs,"->>>9.99").
                         WHEN "tot-hrs"  THEN cVarValue = STRING(tot-hrs,"->>>9.99") .
                         WHEN "std-hrs"   THEN cVarValue = STRING(sort-std-hrs,"->>>9.99") .

                         WHEN "eff-per"  THEN cVarValue = STRING(eff-pct,"->>>9.99") .
                         WHEN "per-util"   THEN cVarValue = STRING(pct-utl,"->>>9.99") .
                         WHEN "dt-per"  THEN cVarValue = STRING(pct-dt,"->>>9.99") .
                         WHEN "qty-fg-rec"    THEN cVarValue = IF sort-qty-fg-rec <> ? THEN string(sort-qty-fg-rec,"->>>>>>>>9") ELSE "".
                         WHEN "msf-fg-rec"   THEN cVarValue = IF sort-msf-fg-rec <> ? THEN string(sort-msf-fg-rec,"->>>>>.999") ELSE "".

                         WHEN "scr-qty"   THEN cVarValue = IF sort-qty-scrap-rec <> ? THEN STRING(sort-qty-scrap-rec,"->>>>>>>>9") ELSE "".
                         WHEN "scr-msf"  THEN cVarValue = IF sort-msf-scrap-rec <> ? THEN STRING(sort-msf-scrap-rec,"->>>>>.999") ELSE "".
                         WHEN "tot-scrap"   THEN cVarValue = IF tot-scrap-pct <> ? THEN STRING(tot-scrap-pct,"->>>>9.99") ELSE "" .
                         
                         
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

     
           IF sort-qty-sheets NE 0 THEN
              tot-scrap-pct = sort-qty-scrap-rec / sort-qty-sheets * 100.
           ELSE
              tot-scrap-pct = 0.

           

      PUT str-line /*fill("*", 137) format 'x(137)'*/ skip.

      assign sort-qty = 0
             sort-qty-sheets = 0
             sort-msf = 0
             sort-run-hrs = 0
             sort-mr-hrs = 0
             sort-chg-hrs = 0
             sort-nochg-hrs = 0
             sort-std-hrs = 0
             sort-qty-fg-rec = 0
             sort-msf-fg-rec = 0
             sort-qty-scrap-rec = 0
             sort-msf-scrap-rec = 0
             sort-shift-jobs = 0
             sort-shift-setups = 0.

   end. /*rd_sort NE "Dept" AND LAST-OF(work-rep.sort-field)*/

   if last(work-rep.sort-field) then
   do:
      {pc/rep/mcheffhr.i "rep"}

      put skip(1).
     
      ASSIGN cDisplay = ""
                   cTmpField = ""
                   cVarValue = ""
                   cExcelDisplay = ""
                   cExcelVarValue = "".
          
            DO i = 1 TO NUM-ENTRIES(cSelectedlist):                             
               cTmpField = entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldListToSelect).
                    CASE cTmpField:             
                         WHEN "sft"    THEN cVarValue = "" .
                         WHEN "mach-code"   THEN cVarValue = "".
                         WHEN "desc"   THEN cVarValue = STRING("REPORT TOTALS","x(20)").
                         WHEN "qty"  THEN cVarValue = STRING(rep-qty,"->,>>>,>>9") .
                         WHEN "msf"   THEN cVarValue = STRING(rep-msf,"->>>>9.999<<") .
                         WHEN "qty-hr"  THEN cVarValue = IF qty-hr <> ? THEN STRING(qty-hr,"->>>,>>9") ELSE "" .
                         WHEN "run-hrs"   THEN cVarValue = STRING(rep-run-hrs,"->>>9.99") .
                         WHEN "mr-hrs"  THEN cVarValue = STRING(rep-mr-hrs,"->>>9.99") .

                         WHEN "dt-chg"    THEN cVarValue = string(rep-chg-hrs + rep-nochg-hrs,"->>>9.99") .
                         WHEN "tot-crg"   THEN cVarValue = string(chg-hrs,"->>>9.99").
                         WHEN "dt-crg"   THEN cVarValue = STRING(rep-nochg-hrs,"->>>9.99").
                         WHEN "tot-hrs"  THEN cVarValue = STRING(tot-hrs,"->>>9.99") .
                         WHEN "std-hrs"   THEN cVarValue = STRING(rep-std-hrs,"->>>9.99") .

                         WHEN "eff-per"  THEN cVarValue = STRING(eff-pct,"->>>9.99") .
                         WHEN "per-util"   THEN cVarValue = STRING(pct-utl,"->>>9.99") .
                         WHEN "dt-per"  THEN cVarValue = STRING(pct-dt,"->>>9.99") .
                         WHEN "qty-fg-rec"    THEN cVarValue = "" .
                         WHEN "msf-fg-rec"   THEN cVarValue = "".

                         WHEN "scr-qty"   THEN cVarValue = "".
                         WHEN "scr-msf"  THEN cVarValue = "" .
                         WHEN "tot-scrap"   THEN cVarValue = "".
                         
                         
                    END CASE.
                      
                    cExcelVarValue = cVarValue.
                    cDisplay = cDisplay + cVarValue +
                               FILL(" ",int(entry(getEntryNumber(INPUT cTextListToSelect, INPUT ENTRY(i,cSelectedList)), cFieldLength)) + 1 - LENGTH(cVarValue)). 
                    cExcelDisplay = cExcelDisplay + quoter(cExcelVarValue) + ",".            
            END.
          
            PUT UNFORMATTED cDisplay SKIP.

      put fill("*", 137) format 'x(137)'.

      ASSIGN rep-qty = 0
             rep-msf = 0
             qty-hr = 0
             rep-run-hrs = 0
             rep-chg-hrs = 0
             chg-hrs = 0
             rep-nochg-hrs = 0
             tot-hrs = 0
             rep-std-hrs = 0
             eff-pct = 0
             pct-utl = 0
             pct-dt = 0
             dpt-run-hrs = 0
             dpt-mr-hrs  = 0
             dpt-chg-hrs = 0
             dpt-nochg-hrs = 0
             dpt-std-hrs = 0
             dpt-qty     = 0
             dpt-msf     = 0
             rep-mr-hrs  = 0
             rep-nochg-hrs = 0
             rep-std-hrs = 0.

   end. /*last(work-rep.sort-field)*/
end. /* EACH work-rep */
