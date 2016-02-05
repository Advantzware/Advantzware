
find first work-tmp where work-tmp.job = mch-act.job and
	 work-tmp.frm = mch-act.frm and
	 work-tmp.blank-no = mch-act.blank-no and
	 work-tmp.dept = mch-act.dept and
	 work-tmp.m-code = mch-act.m-code and
	 work-tmp.pass = mch-act.pass AND
     (rd_sort NE "Shift" OR
      work-tmp.sort-field EQ TRIM(STRING(mch-act.shift,">>>>>")))
	 no-error.

find first mach
    where mach.company eq cocode
	  and mach.m-code  eq mch-act.m-code
    no-lock no-error.

IF NOT AVAIL work-tmp AND
   (NOT AVAIL mach OR
    (mach.industry EQ ""  AND (tb_fold OR tb_corr)) OR
    (mach.industry EQ "1" AND tb_fold)              OR
    (mach.industry EQ "2" AND tb_corr)              OR
    (mach.industry EQ "X" AND tb_fold AND tb_corr)) THEN
   DO:
      CREATE work-tmp.
      ASSIGN
       work-tmp.job      = mch-act.job
       work-tmp.i-no      = mch-act.i-no
       work-tmp.job-no   = mch-act.job-no
       work-tmp.job-no2  = mch-act.job-no2
       work-tmp.frm      = mch-act.frm
       work-tmp.blank-no = mch-act.blank-no
       work-tmp.dept     = IF mch-act.dept NE "" THEN mch-act.dept
                           ELSE IF AVAIL mach THEN mach.dept[1] ELSE ""
       work-tmp.m-code   = mch-act.m-code
       work-tmp.pass     = mch-act.pass
/*        work-tmp.shift-sort = IF rd_sort NE "Shift" THEN TRIM(STRING(mch-act.shift,">>>>>")) */
/*                              ELSE ""                                                        */
       work-tmp.shift-sort = TRIM(STRING(mch-act.shift,">>>>>"))
       work-tmp.sch-m-code = mach.sch-m-code.
      
      IF rd_sort NE "Dept" THEN
         work-tmp.sort-field = IF rd_sort EQ "Shift" THEN
                                  TRIM(STRING(mch-act.shift,">>>>>"))
                               ELSE
                                  IF AVAIL mach THEN
                                     ENTRY(LOOKUP(mach.industry,lv-industries),lv-ind-list)
                               ELSE "".

      FOR EACH b-mch-act FIELDS(CODE hours) WHERE
          b-mch-act.company  EQ mch-act.company AND
          b-mch-act.job      EQ mch-act.job AND
          b-mch-act.job-no   EQ mch-act.job-no AND
          b-mch-act.job-no2  EQ mch-act.job-no2 AND
          b-mch-act.m-code   EQ mch-act.m-code AND
          b-mch-act.dept     EQ mch-act.dept AND
          b-mch-act.pass     EQ mch-act.pass AND
          b-mch-act.frm      EQ mch-act.frm AND
          b-mch-act.blank-no EQ mch-act.blank-no
          NO-LOCK,
          FIRST b-job-cod WHERE
                b-job-cod.code EQ b-mch-act.CODE AND
                b-job-cod.cat EQ "MR"
                NO-LOCK:

             work-tmp.tot-mr-hours  = work-tmp.tot-mr-hours + b-mch-act.hours.
      END.
   END.

RELEASE job-code.
IF AVAIL work-tmp THEN
   FIND job-code WHERE job-code.code EQ mch-act.code NO-LOCK NO-ERROR.

IF AVAIL job-code THEN
   IF job-code.cat eq "RUN" THEN DO:
      ASSIGN
         work-tmp.r-act-hrs = work-tmp.r-act-hrs    + mch-act.hours
         work-tmp.qty       = work-tmp.qty          + 
                              (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty).
     
      IF work-tmp.qty EQ ? THEN work-tmp.qty = 0.
     
      IF tb_msf THEN
         for each job-hdr
             where job-hdr.company   eq cocode
               and job-hdr.job       eq work-tmp.job
               and job-hdr.frm       eq work-tmp.frm
               and (job-hdr.blank-no eq mch-act.blank-no or mch-act.blank-no eq 0)
             no-lock,
             first itemfg
             where itemfg.company eq cocode
               and itemfg.i-no    eq job-hdr.i-no
             no-lock:
        
             assign
               v-on  = 1
               v-out = 1.
             
             RELEASE eb.

             if avail mach and index("APB",mach.p-type) le 0 then do:
                find first eb
                     where eb.company   eq job-hdr.company
                       and eb.est-no    EQ job-hdr.est-no
                       and eb.form-no   eq job-hdr.frm
                       and (eb.blank-no eq job-hdr.blank-no or job-hdr.blank-no eq 0)
                     no-lock no-error.
               
                if avail eb then v-up = eb.num-up.
               
                if job-hdr.n-on ne 0 then v-up = job-hdr.n-on.
               
                find first ef
                    where ef.company eq job-hdr.company
                      and ef.est-no  EQ job-hdr.est-no
                      and ef.form-no eq job-hdr.frm
                    no-lock no-error.
               
                IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).
               
                v-on = v-up * v-out.
                 
                find first est-op
                    where est-op.company eq job-hdr.company
                      AND est-op.est-no  EQ job-hdr.est-no
                      and est-op.s-num   eq mch-act.frm
                      and (est-op.b-num  eq mch-act.blank-no or
                           mch-act.blank-no eq 0)
                      and est-op.m-code  eq mch-act.m-code
                      and est-op.op-pass eq mch-act.pass
                      and est-op.dept    eq mch-act.dept
                      and est-op.line    lt 500
                    no-lock no-error.

                if not avail est-op then
                   find first est-op
                        where est-op.company eq job-hdr.company
                          AND est-op.est-no  EQ job-hdr.est-no
                          and est-op.s-num   eq mch-act.frm
                          and (est-op.b-num  eq mch-act.blank-no or
                               mch-act.blank-no eq 0)
                          and est-op.op-pass eq mch-act.pass
                          and est-op.dept    eq mch-act.dept
                          and est-op.line    lt 500
                        no-lock no-error.
               
                if avail est-op then
                   run sys/inc/numout.p (recid(est-op), output v-out).
                else
                   v-out = 1.
               
                v-on = v-on / v-out.
             end.
             
             IF NOT AVAIL eb THEN
                find first eb
                     where eb.company   eq job-hdr.company
                       and eb.est-no    EQ job-hdr.est-no
                       and eb.form-no   eq job-hdr.frm
                       and (eb.blank-no eq job-hdr.blank-no or job-hdr.blank-no eq 0)
                     no-lock no-error.

             IF AVAIL eb THEN
                v-t-sqft = if v-corr then round(eb.t-sqin * 0.007,4) else round(eb.t-sqin / 144,4).
             ELSE
                v-t-sqft = itemfg.t-sqft.

             work-tmp.msf = work-tmp.msf + (mch-act.qty * v-t-sqft * v-on / 1000).
         END. /*each job-hdr*/
   END.
  
   ELSE
   IF job-code.cat EQ "MR" THEN
      work-tmp.m-act-hrs    = work-tmp.m-act-hrs    + mch-act.hours.
   ELSE
   IF job-code.cat EQ "DT" THEN
      work-tmp.dt-chg-hrs   = work-tmp.dt-chg-hrs   + mch-act.hours.
   ELSE
      work-tmp.dt-nochg-hrs = work-tmp.dt-nochg-hrs + mch-act.hours.
