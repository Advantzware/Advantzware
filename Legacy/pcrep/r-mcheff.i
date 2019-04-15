DEFINE VARIABLE iTotalUp   AS INTEGER NO-UNDO.
DEFINE VARIABLE lGotRmRct  AS LOG     NO-UNDO.
DEFINE VARIABLE dQty       AS DECIMAL NO-UNDO.
DEFINE VARIABLE dMSF       AS DECIMAL NO-UNDO.
DEFINE VARIABLE dSqFtBlank AS DECIMAL NO-UNDO.

EMPTY TEMP-TABLE tt-srt.

FOR EACH mch-act NO-LOCK
    WHERE mch-act.company EQ cocode
    AND (mch-act.op-date GT v-date[1] OR (mch-act.op-date EQ v-date[1] AND mch-act.start GE v-time[1])) 
    AND (mch-act.op-date LT v-date[2] OR (mch-act.op-date EQ v-date[2] AND mch-act.start LE v-time[2]))        
    AND mch-act.shift   GE v-shift[1]
    AND mch-act.shift   LE v-shift[2]
    USE-INDEX dte-idx,

    FIRST mach NO-LOCK
    WHERE mach.company EQ cocode
    AND mach.loc     EQ locode
    AND mach.m-code  EQ mch-act.m-code
    BY mch-act.job-no
    BY mch-act.job-no2:
     
    IF NOT ((
        mach.sch-m-code NE ""        AND
        mach.sch-m-code GE v-mach[1] AND
        mach.sch-m-code LE v-mach[2])        OR 
        ((mach.sch-m-code EQ "")  AND
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
        AND b-mch-act.job        EQ mch-act.job
        AND b-mch-act.job-no     EQ mch-act.job-no
        AND b-mch-act.job-no2    EQ mch-act.job-no2
        AND b-mch-act.frm        EQ mch-act.frm
        AND b-mch-act.m-code     NE mch-act.m-code
        AND b-mch-act.dept       EQ mach.dept[4]))   THEN 
    DO:
        FIND job-code NO-LOCK  
            WHERE job-code.code EQ mch-act.code NO-ERROR.
        IF NOT AVAILABLE job-code THEN NEXT.
        
        /*find a matching report record if it exists*/
        FIND FIRST tt-srt
            WHERE tt-srt.dept     EQ mch-act.dept
            AND tt-srt.m-code     EQ (IF mach.sch-m-code NE "" THEN mach.sch-m-code ELSE mach.m-code)
            AND tt-srt.job-no     EQ mch-act.job-no
            AND tt-srt.job-no2    EQ mch-act.job-no2
            AND tt-srt.frm        EQ mch-act.frm
            AND tt-srt.blank-no   EQ mch-act.blank-no
            AND tt-srt.pass       EQ mch-act.pass
            AND tt-srt.act-m-code EQ mch-act.m-code
            AND tt-srt.job-date   EQ mch-act.op-date
            AND (tt-srt.shift     EQ mch-act.shift OR NOT v-show)
            NO-ERROR.
        /*if it doesn't exist, create it*/
        IF NOT AVAILABLE tt-srt THEN 
        DO:
            CREATE tt-srt.
            ASSIGN 
                tt-srt.dept           = mch-act.dept
                tt-srt.m-code         = (IF mach.sch-m-code NE "" THEN mach.sch-m-code
                                                              ELSE mach.m-code)
                tt-srt.shift          = mch-act.shift
                tt-srt.job            = mch-act.job
                tt-srt.job-no         = mch-act.job-no
                tt-srt.job-no2        = mch-act.job-no2
                tt-srt.frm            = mch-act.frm
                tt-srt.blank-no       = mch-act.blank-no
                tt-srt.pass           = mch-act.pass
                tt-srt.act-m-code     = mch-act.m-code
                tt-srt.job-date       = mch-act.op-date
                tt-srt.run-start-time = ?
                tt-srt.run-end-time   = ?
                tt-srt.mr-start-time  = ?
                tt-srt.mr-end-time    = ?
                tt-srt.gotReceipts    = NO
                lGotRmRct             = NO
                .
            
            RUN pro-rate-mr.
        END. /*not avail tt-srt*/     
      
        IF job-code.cat EQ "RUN" THEN 
        DO:
            /*Add to hours total*/
            IF mch-act.hours NE ? THEN 
                tt-srt.run-act-hr = tt-srt.run-act-hr + mch-act.hours.
            IF mch-act.qty NE ? THEN 
                tt-srt.qty-prod   = tt-srt.qty-prod + mch-act.qty.
                
            /*Test for new start and end times*/   
            IF tt-srt.run-start-time = ? THEN 
                tt-srt.run-start-time = mch-act.START.
            ELSE IF mch-act.START LT tt-srt.run-start-time THEN
                    tt-srt.run-start-time = mch-act.START.
            IF tt-srt.run-end-time = ? THEN 
                tt-srt.run-end-time = mch-act.stopp.
            ELSE IF mch-act.stopp GT tt-srt.run-end-time THEN
                    tt-srt.run-end-time = mch-act.stopp.
            
            /*If Run Qty exists...*/    
            IF mch-act.qty NE ? AND mch-act.qty NE 0 THEN 
            DO:
                FIND FIRST job NO-LOCK 
                    WHERE job.company EQ mch-act.company
                    AND job.job-no EQ mch-act.job-no
                    AND job.job-no2 EQ mch-act.job-no2
                    NO-ERROR.
                IF mch-act.i-no NE "" THEN 
                    FIND itemfg NO-LOCK 
                        WHERE itemfg.company EQ mch-act.company
                        AND itemfg.i-no EQ mch-act.i-no
                        NO-ERROR.
                ELSE DO:                
                    IF AVAILABLE job THEN 
                        FIND FIRST ef NO-LOCK 
                            WHERE ef.company EQ job.company
                            AND ef.est-no EQ job.est-no
                            AND ef.form-no EQ mch-act.frm
                            NO-ERROR.
                    IF AVAILABLE ef AND NOT CAN-FIND(FIRST eb OF ef WHERE eb.blank-no GT 1) THEN DO:                     
                        FIND FIRST job-hdr NO-LOCK
                            WHERE job-hdr.company EQ mch-act.company
                            AND job-hdr.job-no EQ mch-act.job-no
                            AND job-hdr.job-no2 EQ mch-act.job-no2
                            AND job-hdr.frm EQ mch-act.frm
                            AND job-hdr.blank-no EQ mch-act.blank-no
                            NO-ERROR. 
                        IF NOT AVAILABLE job-hdr AND mch-act.blank-no EQ 0 THEN
                            FIND FIRST job-hdr NO-LOCK
                                WHERE job-hdr.company EQ mch-act.company
                                AND job-hdr.job-no EQ mch-act.job-no
                                AND job-hdr.job-no2 EQ mch-act.job-no2
                                AND job-hdr.frm EQ mch-act.frm
                                AND job-hdr.blank-no EQ mch-act.blank-no
                                NO-ERROR.
                        IF AVAILABLE job-hdr THEN 
                            FIND FIRST itemfg NO-LOCK 
                                WHERE itemfg.company EQ job-hdr.company
                                AND itemfg.i-no EQ job-hdr.i-no
                                NO-ERROR.
                    END.
                END.
                IF AVAILABLE itemfg THEN DO:  /*itemfg won't be available if combo form and i-no is blank*/                    
                    IF NOT tt-srt.gotReceipts THEN
                    /*find FG receipts for itemfg for job*/
                        FOR EACH fg-rcpth NO-LOCK 
                            WHERE fg-rcpth.company EQ mch-act.company
                            AND fg-rcpth.job-no EQ mch-act.job-no
                            AND fg-rcpth.job-no2 EQ mch-act.job-no2
                            AND fg-rcpth.rita-code EQ "R"
                            AND fg-rcpth.i-no EQ itemfg.i-no ,
                        EACH fg-rdtlh NO-LOCK 
                            WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no  
                            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                            :
                            tt-srt.qty-rcv = tt-srt.qty-rcv + fg-rdtlh.qty.                                  
                        END. /*each fg-rcpth,fg-rdtlh*/
                    
                    /*Get qty finished*/
                    IF CAN-DO("B,A,P",mach.p-type) THEN
                        tt-srt.qty-finished = tt-srt.qty-finished + mch-act.qty.
                    ELSE DO: /*Sheet or roll fed*/
                        /*First find total multiplier by getting the specific blank and form for the machine actual*/   
                        
                        IF AVAILABLE job THEN DO: 
                            FIND eb NO-LOCK  /*specific blank based on i-no*/
                                WHERE eb.company EQ  job.company
                                AND eb.est-no EQ job.est-no
                                AND eb.stock-no EQ itemfg.i-no
                                NO-ERROR.
                            IF NOT AVAILABLE eb AND mch-act.blank-no NE 0 THEN 
                                FIND eb NO-LOCK  /*specific blank based on form-blank*/
                                    WHERE eb.company EQ  job.company
                                    AND eb.est-no EQ job.est-no
                                    AND eb.form-no EQ mch-act.frm
                                    AND eb.blank-no EQ mch-act.blank-no 
                                    NO-ERROR.
                            IF NOT AVAILABLE eb AND mch-act.blank-no EQ 0 THEN 
                                FIND eb NO-LOCK  /*specific blank based on form-blank*/
                                    WHERE eb.company EQ  job.company
                                    AND eb.est-no EQ job.est-no
                                    AND eb.form-no EQ mch-act.frm
                                    AND eb.blank-no EQ 1 
                                    NO-ERROR.
                        END.
                        FIND ef OF eb NO-LOCK NO-ERROR.
                        IF AVAILABLE ef AND ef.spare-int-1 GT 0 THEN 
                            iTotalUp = ef.spare-int-1.
                        ELSE IF AVAILABLE ef THEN 
                            iTotalUp = ef.n-out * ef.n-out-l * ef.n-out-d.
                        ELSE 
                            iTotalUp = 1.
                        
                        tt-srt.qty-finished = tt-srt.qty-finished +
                            IF (mach.dept[1] = "RS" OR mach.dept[2] = "RS" OR mach.dept[3] = "RS" OR mach.dept[4] = "RS" OR
                            mach.dept[1] = "RC" OR mach.dept[2] = "RC" OR mach.dept[3] = "RC" OR mach.dept[4] = "RC"  OR
                            mach.dept[1] = "GU" OR mach.dept[2] = "GU" OR mach.dept[3] = "GU" OR mach.dept[4] = "GU" OR
                            mach.dept[1] = "SS" OR mach.dept[2] = "SS" OR mach.dept[3] = "SS" OR mach.dept[4] = "SS" 
                            )  
                            THEN eb.num-up * iTotalUp * mch-act.qty
                          ELSE IF mach.p-type = "S" THEN eb.num-up * mch-act.qty
                          ELSE mch-act.qty         
                        .
                    END.                                    
                END.  /*mch-act.i-no ne ""*/
                
                /*get msf and tons processed*/
                IF CAN-DO("R,S,B,A,P",mach.p-type) THEN 
                DO:   
                    FOR EACH job-mat WHERE job-mat.company = mch-act.company
                        AND job-mat.job = mch-act.job
                        AND job-mat.job-no = mch-act.job-no
                        AND job-mat.job-no2 = mch-act.job-no2
                        AND job-mat.frm = mch-act.frm
                        USE-INDEX seq-idx NO-LOCK,
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
                                     (mch-act.qty * job-mat.wid * job-mat.len / 144000 * ITEM.basis-w / 2000)
                                    tt-srt.qty-msf = tt-srt.qty-msf + (mch-act.qty * job-mat.wid * job-mat.len / 144000)
                                    .
                        END.
                        ELSE  
                        DO:
                            dMSF = 0.
                           
                            IF AVAILABLE itemfg THEN 
                                RUN fg/GetFGArea.p (ROWID(itemfg), "MSF", OUTPUT dMSF).
  
                            ASSIGN 
                                tt-srt.qty-msf = tt-srt.qty-msf + mch-act.qty * dMSF
                                tt-srt.qty-ton = tt-srt.qty-ton + (mch-act.qty * dMSF * ITEM.basis-w / 2000)
                                .               
                        END.         
                        dSqFtBlank = 0.
                        IF AVAILABLE itemfg THEN 
                        DO:
                            RUN fg/GetFGArea.p (ROWID(itemfg), "SF", OUTPUT dSqFtBlank).
                            tt-srt.sqFeet-Blank = ROUND(dSqFtBlank,4).
                        END. 
                        IF NOT tt-srt.gotReceipts THEN
                            FOR EACH rm-rcpth NO-LOCK WHERE rm-rcpth.company = job-mat.company
                                AND rm-rcpth.job-no = job-mat.job-no
                                AND rm-rcpth.job-no2 = job-mat.job-no2
                                AND rm-rcpth.i-no = job-mat.rm-i-no
                                /*rm-rcpth.trans-date GE fi_date     */
                                AND rm-rcpth.rita-code = "I",
                                EACH rm-rdtlh NO-LOCK WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no 
                                AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code:
                                RUN sys/ref/convquom.p (INPUT rm-rcpth.pur-uom,
                                    INPUT "EA",
                                    INPUT job-mat.basis-w,
                                    INPUT job-mat.len,
                                    INPUT job-mat.wid,
                                    INPUT job-mat.dep,
                                    INPUT rm-rdtlh.qty,
                                    OUTPUT dQty).

                                ASSIGN 
                                    tt-srt.sheet-rcv  = tt-srt.sheet-rcv + dQty
                                    tt-srt.sqfeet-rcv = tt-srt.sqFeet-rcv +
                                                         ROUND(dQty * job-mat.wid * job-mat.len / 144,0) .
                            END. /*each rm-rcpth,rm-rdtlh*/
                        tt-srt.gotReceipts = YES.
                        lGotRmRct= YES.
                        LEAVE.
                    END. /*each job-mat*/
                END. /*(mach.p-type = "R" OR mach.p-type = "S" OR mach.p-type = "B")*/            
            END. /*mch-act.qty <> ? AND mch-act.qty <> 0*/
        END. /*job-code.cat eq "RUN"*/
        ELSE
            IF job-code.cat EQ "MR" THEN 
            DO: 
                /*Add to hours total*/
                IF mch-act.hours NE ? THEN 
                    tt-srt.mr-act-hr  = tt-srt.mr-act-hr + mch-act.hours.
                
                /*Test for new start and end times*/            
                IF tt-srt.mr-start-time = ? THEN 
                    tt-srt.mr-start-time = mch-act.START.
                ELSE IF mch-act.START LT tt-srt.mr-start-time THEN
                        tt-srt.mr-start-time = mch-act.START.
                IF tt-srt.mr-end-time = ? THEN 
                    tt-srt.mr-end-time = mch-act.stopp.
                ELSE IF mch-act.stopp GT tt-srt.mr-end-time THEN
                        tt-srt.mr-end-time = mch-act.stopp.

            END. /*job-code.cat eq "MR"*/
            ELSE
                IF mch-act.hours NE ? THEN 
                    tt-srt.act-dt-hr  = tt-srt.act-dt-hr + mch-act.hours.
    END. /*main condition*/
END. /* each mch-act*/

/*Calculate standards based on gathered, summarized actuals*/
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
 
        ASSIGN 
            tt-srt.mr-std-hr  = job-mch.mr-hr *
                                    (tt-srt.mr-act-hr / tt-srt.tot-mr-hours)
            tt-srt.qty-expect = IF job-mch.speed NE 0 THEN
                                      (IF tt-srt.run-act-hr NE 0
                                       THEN tt-srt.run-act-hr
                                       ELSE tt-srt.run-std-hr) * job-mch.speed
                                    ELSE job-mch.run-qty.
    END.

    IF tt-srt.run-std-hr EQ ? THEN tt-srt.run-std-hr = 0.
    IF tt-srt.mr-std-hr  EQ ? THEN tt-srt.mr-std-hr  = 0.
      
      
END.
      
FOR EACH tt-srt USE-INDEX dept-idx
    BREAK /*by tt-srt.dept
                          by tt-srt.shift */
    BY tt-srt.m-code
    BY tt-srt.job-date
    BY tt-srt.job-no
    BY tt-srt.job-no2:
     
    IF tt-srt.run-act-hr EQ 0 THEN
        tt-srt.run-std-hr = 0.

    /* if v-show then*/
    DO:
        ASSIGN
            mr-eff      = (tt-srt.mr-std-hr  / tt-srt.mr-act-hr)  * 100.00
            run-eff     = (tt-srt.run-std-hr / tt-srt.run-act-hr) * 100.00
            tot-std-hrs = tt-srt.mr-std-hr + tt-srt.run-std-hr
            tot-act-hrs = tt-srt.mr-act-hr + tt-srt.run-act-hr
            tot-eff     = (tot-std-hrs / tot-act-hrs) * 100.00
            dt-eff      = (tt-srt.act-dt-hr / tot-act-hrs) * 100.00.
        IF mr-eff = ? THEN mr-eff = 0.
        IF run-eff = ? THEN run-eff = 0.
        IF tot-eff = ? THEN tot-eff = 0.
        IF dt-eff = ? THEN dt-eff = 0.
    END.

    IF tb_excel THEN  
    DO:
        tt-srt.sqFeet-Prod = tt-srt.qty-msf * 1000.
        PUT STREAM excel UNFORMATTED
            '"' tt-srt.m-code '",'
            '"' tt-srt.job-date '",'
            '"' STRING(tt-srt.mr-start-time,"HH:MM") '",'
            '"' STRING(tt-srt.mr-end-time,"HH:MM") '",'
            '"' STRING(tt-srt.run-start-time,"HH:MM") '",'
            '"' STRING(tt-srt.run-end-time,"HH:MM") '",'
            '"' tt-srt.job-no + "-" + string(tt-srt.job-no2,"99") '",'  
            '"' tt-srt.sqfeet-blank '",'
            '"' tt-srt.mr-act-hr '",'
            '"' tt-srt.run-act-hr '",'
            '"' tt-srt.sqfeet-rcv '",'
            '"' tt-srt.sheet-rcv '",'
            '"' tt-srt.qty-prod '",'
            '"' tt-srt.qty-finished '",'
            '"' tt-srt.qty-rcv '",'
            '"' ROUND((tt-srt.qty-rcv - tt-srt.qty-finished) / tt-srt.qty-rcv,2) * 100 FORM "->>>>9%" '",'
            '"' tt-srt.sqfeet-prod '",'
            '"' tt-srt.sqfeet-rcv - tt-srt.sqfeet-prod '",'
            '"' ROUND((tt-srt.sqFeet-rcv - tt-srt.sqFeet-prod) / tt-srt.sqFeet-rcv * 100,2) '",'
            '"' ROUND(tt-srt.qty-prod / tt-srt.run-act-hr,0) '",'
            '"' mr-eff FORM "->>>>9.99%" '",'
            '"' run-eff FORM "->>>>9.99%" '",'
            '"' STRING(tt-srt.shift,">>>>") '",'
            SKIP.
          
    END.
            
END. /* each tt-srt */

