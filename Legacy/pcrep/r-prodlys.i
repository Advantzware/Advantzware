DEF    VAR      iTotalUp    AS INT     NO-UNDO.
DEF    VAR      v-checkcust AS CHAR    NO-UNDO .
DEFINE VARIABLE dMSF        AS DECIMAL NO-UNDO.
DEFINE BUFFER bf-itemfg FOR itemfg.
   
EMPTY TEMP-TABLE tt-srt.
FOR EACH ttCustList 
    WHERE ttCustList.log-fld
    NO-LOCK:
    v-checkcust  = v-checkcust + ttCustList.cust-no + "," .
END.
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

    FIND FIRST job-hdr WHERE job-hdr.company = mch-act.company
        AND job-hdr.job-no = mch-act.job-no
        AND job-hdr.job-no2 = mch-act.job-no2
        AND job-hdr.cust-no GE fcust
        AND job-hdr.cust-no LE tcust
        AND (if lselected then can-find(first ttCustList where ttCustList.cust-no eq job-hdr.cust-no
        AND ttCustList.log-fld no-lock) else true)
        AND job-hdr.frm = mch-act.frm NO-LOCK NO-ERROR.

    /*IF AVAIL job-hdr AND lookup(job-hdr.cust,v-checkcust) EQ 0 THEN NEXT .*/
     
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
        AND b-mch-act.dept     EQ mach.dept[4]))   THEN 
    DO:
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

        if not avail tt-srt then 
        do:
            create tt-srt.
            assign 
                tt-srt.dept       = mch-act.dept
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
      
      
        if job-code.cat eq "RUN" THEN 
        DO:

            ASSIGN
                tt-srt.run-act-hr = tt-srt.run-act-hr + mch-act.hours
                tt-srt.qty-prod   = tt-srt.qty-prod +
                                 IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty
                .

            IF /*tb_tonmsf AND*/ mch-act.qty <> ? AND mch-act.qty <> 0 THEN 
            DO:             
                FIND FIRST job-hdr WHERE job-hdr.company = mch-act.company
                    AND job-hdr.job-no = mch-act.job-no
                    AND job-hdr.job-no2 = mch-act.job-no2
                    AND job-hdr.frm = mch-act.frm NO-LOCK NO-ERROR.
                FIND eb WHERE eb.company = job-hdr.company
                    AND eb.est-no = job-hdr.est-no
                    AND eb.form-no = job-hdr.frm
                    AND eb.blank-no = job-hdr.blank-no NO-LOCK NO-ERROR.
                FIND ef OF eb NO-LOCK NO-ERROR.

                IF AVAIL ef THEN
                    iTotalUP = IF ef.spare-int-1 = 0 THEN ef.n-out * ef.n-out-l * ef.n-out-d
                    ELSE ef.spare-int-1.

                FOR each job-mat WHERE job-mat.company = mch-act.company
                    AND job-mat.job = mch-act.job
                    AND job-mat.job-no = mch-act.job-no
                    AND job-mat.job-no2 = mch-act.job-no2
                    AND job-mat.frm = mch-act.frm
                    use-index seq-idx NO-LOCK,
                    FIRST item OF job-mat WHERE item.company EQ job-mat.company 
                    AND item.i-no EQ job-mat.rm-i-no
                    AND (ITEM.mat-type = "B" /* OR ITEM.mat-type = "G" */) NO-LOCK:

                    IF mach.p-type = "R" OR mach.p-type = "S" THEN 
                    DO:
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
                    END. /*p-type eq R,S*/
                    ELSE 
                    DO:
                        dMSF = 0.
                        IF mach.p-type EQ "A" OR mach.p-type  EQ "P" THEN  /*if assembly use job header*/
                            FIND FIRST itemfg NO-LOCK 
                                WHERE itemfg.company EQ job-hdr.company
                                AND itemfg.i-no EQ job-hdr.i-no 
                                NO-ERROR.
                        ELSE  /*use machine specific item*/
                            FIND FIRST itemfg NO-LOCK 
                                WHERE itemfg.company EQ mch-act.company
                                AND itemfg.i-no EQ mch-act.i-no
                                NO-ERROR.
                        IF NOT AVAILABLE itemfg THEN /*if the above didn't find the correct item use job-hdr*/
                            FIND FIRST itemfg NO-LOCK 
                                WHERE itemfg.company EQ job-hdr.company
                                AND itemfg.i-no EQ job-hdr.i-no 
                                NO-ERROR.
                        IF AVAIL itemfg THEN 
                            RUN fg/GetFGArea.p (ROWID(itemfg), "MSF", OUTPUT dMSF).
                        ASSIGN
                            tt-srt.qty-msf = tt-srt.qty-msf + mch-act.qty * dMSF
                            tt-srt.qty-ton = tt-srt.qty-ton + (mch-act.qty * dMSF * ITEM.basis-w / 2000)
                            .
                    END.  /*P-type = A,P,B*/                                   
                    LEAVE.
                END. /*each job-mat*/
            END.  /*mch-act.qty <>0*/
        END.  /*job-code = "RUN"*/
        ELSE 
            IF job-code.cat eq "MR" then 
                tt-srt.mr-act-hr  = tt-srt.mr-act-hr + mch-act.hours.
            ELSE 
                tt-srt.act-dt-hr  = tt-srt.act-dt-hr + mch-act.hours.
    END.  /*large condition*/
END.  /*each mch-act*/

for each tt-srt,
    first job
    where job.company eq cocode
    and job.job     eq tt-srt.job
    and job.job-no  eq tt-srt.job-no
    and job.job-no2 eq tt-srt.job-no2
    use-index job-no no-lock:
    b = "" .
    for each job-hdr
        where job-hdr.company eq cocode
        and job-hdr.job     eq job.job
        and job-hdr.job-no  eq job.job-no
        and job-hdr.job-no2 eq job.job-no2
        no-lock
               
        by job-hdr.blank-no desc
        by job-hdr.frm      desc:
              
        b = job-hdr.i-no.
        if job-hdr.frm       eq tt-srt.frm           and
            (job-hdr.blank-no eq tt-srt.blank-no or
            job-hdr.blank-no eq 0)                    then leave.
    end.

    ASSIGN 
        tt-srt.i-no = b .


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


        ASSIGN 
            tt-srt.mr-std-hr  = job-mch.mr-hr *
                                    (tt-srt.mr-act-hr / tt-srt.tot-mr-hours)
            tt-srt.qty-expect = IF job-mch.speed NE 0 THEN
                                      (IF tt-srt.run-act-hr NE 0
                                       THEN tt-srt.run-act-hr
                                       ELSE tt-srt.run-std-hr) * job-mch.speed
                                    ELSE job-mch.run-qty.

        IF rd_alptime EQ "TM" THEN 
        do:  
            ASSIGN
                tt-srt.start-date = job-mch.start-date
                tt-srt.start-time = job-mch.start-time . /* task 02271403  */ 
        END. 
    END.

    IF tt-srt.run-std-hr EQ ? THEN tt-srt.run-std-hr = 0.
    IF tt-srt.mr-std-hr  EQ ? THEN tt-srt.mr-std-hr  = 0.
      
    IF TB_round = YES THEN 
    DO:
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

    find first itemfg
        where itemfg.company eq cocode
        and itemfg.i-no    eq tt-srt.i-no
        no-lock no-error.

    v-calmsf = /*v-calmsf
                                     +*/ (tt-srt.qty-prod *
        (if avail itemfg then itemfg.t-sqft
        else 1) / 1000).

    FIND FIRST job where 
        job.company eq cocode and
        job.job-no  eq SUBSTRING(tt-srt.job-no,1,6) and
        job.job-no2 eq tt-srt.job-no2 NO-LOCK NO-ERROR .
    ASSIGN 
        v-cust-no = "" .
    IF AVAIL job THEN 
    do:
             
        FIND FIRST eb WHERE eb.company EQ cocode
            AND eb.est-no EQ job.est-no
            AND eb.stock-no EQ tt-srt.i-no NO-LOCK NO-ERROR .
        IF AVAIL eb THEN
            ASSIGN v-num-up = eb.num-up .
        ELSE 
            v-num-up = 0 .
         
        for each misc-act FIELDS(cost)
            where misc-act.company eq cocode
            and misc-act.job     eq job.job
            no-lock:
                     
        /*v-act-lab-cost = v-act-lab-cost + misc-act.cost.*/
         
        END.
        FIND FIRST job-hdr
            where job-hdr.company eq cocode
            and job-hdr.i-no     eq tt-srt.i-no
            and job-hdr.job-no  eq job.job-no
            and job-hdr.job-no2 eq job.job-no2 NO-LOCK NO-ERROR .
        IF AVAIL job-hdr THEN
            v-cust-no = job-hdr.cust-no  .

    END.

       
    IF tt-srt.run-act-hr EQ 0 THEN
        tt-srt.run-std-hr = 0.
     
    /*   IF NOT v-show AND tb_excel THEN DO:*/                                                            
    ASSIGN                                                                                         
        mr-eff      = (tt-srt.mr-std-hr  / tt-srt.mr-act-hr)  * 100.00                                   
        run-eff     = (tt-srt.run-std-hr / tt-srt.run-act-hr) * 100.00                  
        tot-std-hrs = tt-srt.mr-std-hr + tt-srt.run-std-hr
        tot-act-hrs = tt-srt.mr-act-hr + tt-srt.run-act-hr
        tot-eff     = (tot-std-hrs / tot-act-hrs) * 100.00
        dt-eff      = (tt-srt.act-dt-hr / tot-act-hrs) * 100.00.

    if mr-eff = ? then mr-eff = 0.
    if run-eff = ? then run-eff = 0.
    if tot-eff = ? then tot-eff = 0.
    if dt-eff = ? then dt-eff = 0.

    /* {pcrep/r-prodlyst.i}*/
    ASSIGN 
        inrowcount = inrowcount + 1.

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C1".
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                                    
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.m-code).                                          
                                                                                                                       
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C2".                                                             
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                                    
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.job-no) .                                         
                                                                                                                       
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C3".                                                                                                                   
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.job-no2) .                                
    /* shift*/                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C4".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.shift,">>>>").                            
                                                                                                               
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C5".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.mr-std-hr,"->>>9.99") .                   
    /* mr-act*/                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C6".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.mr-act-hr,"->>>9.99") .                   
                                                                                                               
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C7".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(mr-eff,"->>>9.99").
    /* run-hrs-std */
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C8".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.run-std-hr,"->>>9.99") .
         
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C9".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.run-act-hr,"->>>9.99") .

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C10".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(run-eff,"->>>9.99").
         
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C11".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.run-std-hr,"->>,>>9.99") .
         
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C12".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.run-act-hr,"->>,>>9.99")  .

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C13".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tot-eff,"->>>,>>9.99")  .

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C14".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.act-dt-hr,"->>9.99") .

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C15".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(dt-eff,"->>9.99").
         
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C16".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.qty-prod,"->,>>>,>>9")  .

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C17".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.qty-ton,"->>,>>9.99")  .
         
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C18".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.qty-msf,"->>,>>9.99")  .

    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C19".
    chExcelApplication:Goto(v-cell) NO-ERROR.
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(tt-srt.qty-expect,"->>>,>>>,>>9")  .


    find first job-mch where job-mch.company  = cocode  and
        job-mch.job      eq tt-srt.job  and 
        job-mch.job-no  EQ SUBSTRING(tt-srt.job-no,1,6)  and
        job-mch.job-no2 eq tt-srt.job-no2  and 
        job-mch.frm      = tt-srt.frm and 
        (job-mch.blank-no = tt-srt.blank-no or
        mch-srt.blank-no = 0) and              
        job-mch.m-code   = tt-srt.m-code and 
        job-mch.pass     = tt-srt.pass 
        no-lock no-error.
    if not avail job-mch then
        find first job-mch where job-mch.company eq cocode and
            job-mch.job      eq tt-srt.job and
            job-mch.job-no  eq SUBSTRING(tt-srt.job-no,1,6) and
            job-mch.job-no2 eq tt-srt.job-no2 and 
            job-mch.frm      eq tt-srt.frm and
            (job-mch.blank-no = tt-srt.blank-no or
            mch-srt.blank-no = 0) and
            job-mch.m-code   eq tt-srt.m-code
            no-lock no-error.
    if not avail job-mch then
        find first job-mch where job-mch.company eq cocode and
            job-mch.job     eq tt-srt.job and
            job-mch.job-no  eq SUBSTRING(tt-srt.job-no,1,6) and
            job-mch.job-no2 eq tt-srt.job-no2 and 
            job-mch.frm     eq tt-srt.frm and
            job-mch.m-code  eq tt-srt.m-code and
            job-mch.speed   ne 0
            no-lock no-error.
    if not avail job-mch then
        find first job-mch where job-mch.company eq cocode and
            job-mch.job     eq tt-srt.job and 
            job-mch.job-no  eq SUBSTRING(tt-srt.job-no,1,6)  and
            job-mch.job-no2 eq tt-srt.job-no2 and  
            job-mch.frm     eq tt-srt.frm and
            job-mch.m-code  eq tt-srt.m-code        
            no-lock no-error.  
    if available job-mch then
    DO:
        ASSIGN                                   
            v-mrcomp  = string(job-mch.mr-complete)
            v-runcomp = string(job-mch.run-complete) .
        
    END.

    ASSIGN 
        v-runwaste     = 0 
        v-mrwaste      = 0 
        v-act-lab-cost = 0
        v-crew-size    = 0. 
    FOR EACH bf-mch-act WHERE
        bf-mch-act.company EQ cocode AND
        bf-mch-act.dept EQ tt-srt.dept AND
        bf-mch-act.m-code EQ tt-srt.m-code AND
        bf-mch-act.job EQ tt-srt.job AND 
        bf-mch-act.job-no EQ SUBSTRING(tt-srt.job-no,1,6) AND
        bf-mch-act.job-no2 EQ tt-srt.job-no2 AND
        bf-mch-act.frm EQ tt-srt.frm AND
        (bf-mch-act.blank-no = tt-srt.blank-no  OR
        mach.p-type ne "B" OR
        bf-mch-act.blank-no EQ 0) AND
        bf-mch-act.pass eq tt-srt.pass  NO-LOCK:

        find job-code where job-code.code eq bf-mch-act.CODE no-lock no-error.

        if not available job-code then next.
        if job-code.cat eq "RUN" then
        do:
            v-runwaste = v-runwaste + bf-mch-act.waste .
        end.
        else if job-code.cat eq "MR" then
                v-mrwaste = v-mrwaste + bf-mch-act.waste .

        v-act-lab-cost = v-act-lab-cost + (bf-mch-act.hours * bf-mch-act.crew) .
        v-crew-size = v-crew-size + bf-mch-act.crew .

    END. /* FOR EACH bf-mch-act W */

    assign 
        job-mr-std     = /*job-mr-std +*/ tt-srt.mr-std-hr
        job-run-std    = /*job-run-std +*/ tt-srt.run-std-hr
        job-mr-act     = /*job-mr-act +*/ tt-srt.mr-act-hr
        job-run-act    = /*job-run-act +*/ tt-srt.run-act-hr
        job-dt-act     = /*job-dt-act +*/ tt-srt.act-dt-hr
        job-qty-prod   = /*job-qty-prod +*/ tt-srt.qty-prod
        job-qty-expect = /*job-qty-expect +*/ tt-srt.qty-expect.

    /* IF LAST-OF(tt-srt.job-no) THEN DO:*/
    IF job-run-act EQ 0 THEN
        job-run-std = 0.
    /*if v-show then do:*/
    mr-eff  = (job-mr-std  / job-mr-act)  * 100.00.
    if mr-eff eq ? then mr-eff = 0.
    run-eff = (job-run-std / job-run-act) * 100.00.
    if run-eff eq ? then run-eff = 0.
    ASSIGN
        tot-std-hrs = job-mr-std + job-run-std
        tot-act-hrs = job-mr-act + job-run-act
        tot-eff     = (tot-std-hrs / tot-act-hrs) * 100.00.
    if tot-eff eq ? then tot-eff = 0.
    dt-eff = (job-dt-act / tot-act-hrs) * 100.00.
    if dt-eff eq ? then dt-eff = 0.
          
    IF v-num-up = 0 THEN ASSIGN v-num-up = 1 .
    ASSIGN
        v-pic-per-hrs = (job-qty-prod / (job-mr-act + job-run-act + job-dt-act))
        v-msf-per-hrs = (v-calmsf / (job-mr-act + job-run-act + job-dt-act))
        v-kik-per-hrs = ( /*job-qty-prod*/ v-pic-per-hrs  / v-num-up)
        v-wst         = ((v-mrwaste + v-runwaste) / ( job-qty-prod)) * 100
        v-per-man-hrs = (job-qty-prod / v-act-lab-cost) .

    /* msf per hours  */
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C20".
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                                    
    ASSIGN 
        chExcelApplication:ActiveCell:Value = IF v-msf-per-hrs NE ? THEN STRING(v-msf-per-hrs,">,>>>,>>9.99") ELSE "".                                         
    /* total machine hours */                                                                                                             
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C21".                                                             
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                                    
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING((job-mr-act + job-run-act + job-dt-act),"->>,>>>,>>>,>>9.99")  .                                         
    /*Pieces per hours*/                                                                                                   
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C22".                                                                                                                   
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = IF v-pic-per-hrs NE ? THEN STRING(v-pic-per-hrs,"->>>,>>>,>>9.99") ELSE "".                              
    /* Number on */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C23".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(v-num-up,">>,>>>,>>9")  . 

    /*Pieces per man hours */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C24".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = IF v-per-man-hrs NE ? THEN STRING(v-per-man-hrs,"->>,>>>,>>>,>>9.99") ELSE "" .       

    /*Kicks per hours */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C25".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = IF v-kik-per-hrs NE ? THEN STRING(v-kik-per-hrs,"->>,>>>,>>9.99") ELSE "".
               
    /* Total Waste */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C26".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(v-mrwaste + v-runwaste,"->>,>>>,>>9.99") .

    /* % Waste*/                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C27".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = IF v-wst NE ? THEN STRING(v-wst,"->,>>9.99") ELSE "".

    /* FG Item  */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C28".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = string(tt-srt.i-no) .

    /* Cust # */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C29".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = IF v-cust-no <> ? THEN string(v-cust-no) ELSE "" .

    /* MR & run std */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C30".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = string(tot-std-hrs,"->>>9.9") . 

    /* mr & run acl */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C31".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = string(tot-act-hrs,"->>>9.9") . 

    /* mr & run eff% */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C32".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = string(tot-eff,"->>>>9.9") .    

    /* MR / C */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C33".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = string(v-mrcomp).

    /* Run \ C  */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C34".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = string(v-runcomp).

    /* Total Labor hours   */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C35".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(v-act-lab-cost,"->,>>>,>>>,>>9.99") .

    /* Mr Waste   */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C36".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(v-mrwaste,"->,>>9.99") . 

    /* Run Waste   */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C37".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(v-runwaste,"->,>>9.99") .

    /* crew   */                                                                                           
    ASSIGN 
        v-cell = "R" + STRING(inrowcount) + "C38".                                                     
    chExcelApplication:Goto(v-cell) NO-ERROR.                                                            
    ASSIGN 
        chExcelApplication:ActiveCell:Value = STRING(v-crew-size,"->,>>>,>>9.99") .

    ASSIGN 
        job-mr-std     = 0
        job-mr-act     = 0
        job-run-std    = 0
        job-run-act    = 0
        job-dt-act     = 0
        job-qty-prod   = 0
        job-qty-expect = 0.

/*         END.*/
        
  
end. /* each item */

 
