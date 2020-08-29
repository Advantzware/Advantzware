/*------------------------------------------------------------------------
  File:         MachineEfficiency.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 8.4.2020
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttMachineEfficiency
DEFINE TEMP-TABLE ttMachineEfficiency NO-UNDO
    FIELD shift    AS CHARACTER LABEL "SHIFT" FORMAT "x(5)"  
    FIELD machCode AS CHARACTER LABEL "MACH CODE" FORMAT "X(6)"
    FIELD cDESC    AS CHARACTER LABEL "DESCRIPTION" FORMAT "X(30)"
    FIELD qty      AS INTEGER   LABEL "QUANTITY" FORMAT "->>,>>>,>>9"
    FIELD msf      AS DECIMAL   LABEL "MSF" FORMAT "->>>>9.999<<"
    FIELD qtyHr    AS INTEGER   LABEL "QTY HOUR" FORMAT "->>>,>>9"
    FIELD runHrs   AS DECIMAL   LABEL "RUN HOURS" FORMAT "->>>9.99"
    FIELD mrHrs    AS DECIMAL   LABEL "MR HOURS" FORMAT "->>>9.99"
    FIELD dtChg    AS DECIMAL   LABEL "D/T CHGBL" FORMAT "->>>9.99"
    FIELD totCrg   AS DECIMAL   LABEL "TOTAL CHARGE" FORMAT "->>>9.99"
    FIELD dtCrg    AS DECIMAL   LABEL "D/T No CHARGE" FORMAT "->>>9.99"
    FIELD totHrs   AS DECIMAL   LABEL "TOTAL HOURS" FORMAT "->>>9.99"
    FIELD stdHrs   AS DECIMAL   LABEL "STD HOURS" FORMAT "->>>9.99"
    FIELD effPer   AS DECIMAL   LABEL "EFFIC PERCENT" FORMAT "->>>9.99"
    FIELD perUtil  AS DECIMAL   LABEL "PERCENT UTILIZED" FORMAT "->>>9.99"
    FIELD dtPer    AS DECIMAL   LABEL "D/T PERCENT" FORMAT "->>>9.99"
    FIELD qtyFgRec AS INTEGER   LABEL "QTY FG RECEIVED" FORMAT "->>>>>>>>9"
    FIELD msfFgRec AS DECIMAL   LABEL "MSF FG RECEIVED" FORMAT "->>>>>.999"
    FIELD scrQty   AS INTEGER   LABEL "SCRAP QTY" FORMAT "->>>>>>>>9"
    FIELD scrMsf   AS DECIMAL   LABEL "SCRAP MSF" FORMAT "->>>>>.999"
    FIELD totScrap AS DECIMAL   LABEL "% OF TOT SCRAP" FORMAT "->>>>9.99"
    FIELD job      AS CHARACTER LABEL "Job#" FORMAT "x(6)"
    FIELD job2     AS INTEGER   LABEL "Job2" FORMAT ">>9"
    .
        
 
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.
{pc/rep/ttMachEfficiency.i}
 
/* Parameters Definitions ---                                           */

&Scoped-define subjectID 139
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:

    
    DEFINE VARIABLE rd_sort AS CHARACTER INITIAL "Dept" NO-UNDO.
    DEFINE VARIABLE tb_fold AS LOGICAL   INITIAL YES NO-UNDO.
    DEFINE VARIABLE tb_corr AS LOGICAL   INITIAL YES NO-UNDO.  
    DEFINE VARIABLE tb_msf  AS LOGICAL   INITIAL YES NO-UNDO.    
 
    DEFINE BUFFER b-mch-act FOR mch-act.
    DEFINE BUFFER b-job-cod FOR job-code.
 
    DEFINE VARIABLE v-t-sqft      LIKE itemfg.t-sqft NO-UNDO.   
    DEFINE VARIABLE mr-hr         LIKE job-mch.mr-hr NO-UNDO.
    DEFINE VARIABLE run-hr        LIKE job-mch.run-hr NO-UNDO.
    DEFINE VARIABLE chg-hrs       AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE dt-hrs        AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE tot-hrs       AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE eff-pct       AS DECIMAL   FORMAT '->>>9.99' NO-UNDO.
    DEFINE VARIABLE pct-utl       AS DECIMAL   FORMAT '->>>9.99' NO-UNDO.
    DEFINE VARIABLE pct-dt        AS DECIMAL   FORMAT '->>>9.99' NO-UNDO.
    DEFINE VARIABLE qty-hr        AS INTEGER   FORMAT '>>>>>>9' NO-UNDO.
    DEFINE VARIABLE std-hrs       AS DECIMAL   FORMAT '>>>>9.99' NO-UNDO.
    DEFINE VARIABLE v-up          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-on          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-out         AS INTEGER   NO-UNDO.   
    DEFINE VARIABLE lv-industries AS CHARACTER INIT ",1,2,X" NO-UNDO.  
    DEFINE VARIABLE lv-ind-list   AS CHARACTER INIT "Both,Folding,Corrugated,eXclude" NO-UNDO. 
    DEFINE VARIABLE iUpTmp        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iOutTmp       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dQty          AS DECIMAL   NO-UNDO.   
    DEFINE VARIABLE dtDate        AS DATE      NO-UNDO.
    DEFINE VARIABLE dBlanks       AS DECIMAL   FORMAT "->>>>,>>>,>>9" .  
 
    DEFINE BUFFER b-mach FOR mach.

    EMPTY TEMP-TABLE work-tmp.
    EMPTY TEMP-TABLE work-rep.
    EMPTY TEMP-TABLE work-rep-copy.

    rd_sort =  IF cSortBy EQ "1" THEN "Dept" ELSE IF cSortBy EQ "2" THEN "Industry" ELSE "Shift" .
    tb_fold = lFolding .
    tb_corr = lCorrugated .
    cocode =  cCompany .

    FOR EACH b-mach FIELDS(m-code) WHERE
        b-mach.company EQ cCompany AND    
        b-mach.m-code GE cStartMachine AND
        b-mach.m-code LE cEndMachine
        NO-LOCK:

        DO dtDate = dtStartTransDate TO dtEndTransDate:
    
            FOR EACH mch-act WHERE
                mch-act.company EQ cCompany AND
                mch-act.m-code  EQ b-mach.m-code AND
                mch-act.op-date EQ dtDate AND
                mch-act.dept    GE cStartDeptNo AND
                mch-act.dept    LE cEndDeptNo AND
                mch-act.shift   GE integer(cStartShift) AND
                mch-act.shift   LE integer(cEndShift)                  
                NO-LOCK
                USE-INDEX dly-idx:                  
      
                {pc/rep/mch-eff.i}
               
            END.
        END. /*do v-date*/
    END.

    FOR EACH work-tmp
        BREAK BY work-tmp.m-code
        BY work-tmp.shift-sort
        BY work-tmp.job
        BY work-tmp.job-no
        BY work-tmp.job-no2
        BY work-tmp.frm
        BY work-tmp.blank-no:          

        IF LAST-OF(work-tmp.blank-no) THEN 
        DO:

            FOR EACH job-hdr FIELDS(job job-no job-no2 i-no)
                WHERE job-hdr.company   EQ cCompany
                AND job-hdr.job       EQ work-tmp.job
                AND job-hdr.job-no    EQ work-tmp.job-no
                AND job-hdr.job-no2   EQ work-tmp.job-no2
                AND job-hdr.frm       EQ work-tmp.frm
                AND (job-hdr.blank-no EQ work-tmp.blank-no OR
                work-tmp.blank-no EQ 0)
                NO-LOCK:
             
                RUN fg/GetProductionQty.p (INPUT cCompany,
                    INPUT work-tmp.job-no,
                    INPUT work-tmp.job-no2,
                    INPUT work-tmp.i-no,
                    INPUT NO,
                    OUTPUT dBlanks).

                work-tmp.qty-fg-rec = work-tmp.qty-fg-rec + dBlanks /*fg-act.qty*/ .
        
                FIND FIRST itemfg
                    WHERE itemfg.company EQ cCompany
                    AND itemfg.i-no    EQ job-hdr.i-no
                    NO-LOCK NO-ERROR.

                work-tmp.msf-fg-rec = work-tmp.msf-fg-rec
                    + (dBlanks /*fg-act.qty*/ *
                    (IF AVAILABLE itemfg THEN itemfg.t-sqft
                    ELSE 1) / 1000).
            END. /*each job-hdr*/

            RELEASE est.

            FIND FIRST job
                WHERE job.company            EQ cCompany
                AND job.job                EQ work-tmp.job
                AND job.job-no             EQ work-tmp.job-no
                AND job.job-no2            EQ work-tmp.job-no2
                NO-LOCK NO-ERROR.

            IF AVAILABLE job THEN
                FIND FIRST est
                    WHERE est.company EQ job.company
                    AND est.est-no  EQ job.est-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE est THEN
                FOR EACH eb FIELDS(num-up)
                    WHERE eb.company   EQ est.company
                    AND eb.est-no    EQ est.est-no
                    AND eb.form-no   EQ work-tmp.frm
                    AND (eb.blank-no EQ work-tmp.blank-no OR work-tmp.blank-no EQ 0)
                    NO-LOCK:
        
                    iUpTmp = iUpTmp + eb.num-up.
                END.
      
            ELSE iUpTmp = iUpTmp + 1.
        END. /*last of work-tmp.blank-no*/

        IF LAST-OF(work-tmp.frm) THEN 
        DO:

            FOR EACH job-mat
                WHERE job-mat.company EQ cCompany
                AND job-mat.job     EQ work-tmp.job
                AND job-mat.job-no  EQ work-tmp.job-no
                AND job-mat.job-no2 EQ work-tmp.job-no2
                AND job-mat.frm     EQ work-tmp.frm
                USE-INDEX seq-idx NO-LOCK,
          
                FIRST ITEM FIELDS(s-dep)
                WHERE item.company  EQ cCompany
                AND item.i-no     EQ job-mat.i-no
                AND item.mat-type EQ "B"
                NO-LOCK,

                EACH mat-act FIELDS(qty)
                WHERE mat-act.company EQ cCompany
                AND mat-act.job     EQ job-mat.job
                AND mat-act.job-no  EQ job-mat.job-no
                AND mat-act.job-no2 EQ job-mat.job-no2
                AND mat-act.s-num   EQ job-mat.frm
                AND mat-act.b-num   EQ job-mat.blank-no
                AND mat-act.i-no    EQ job-mat.i-no
                USE-INDEX job NO-LOCK:
          
                RUN sys/ref/convquom.p(job-mat.qty-uom, "EA",
                    job-mat.basis-w, job-mat.len,
                    job-mat.wid, item.s-dep,
                    mat-act.qty, OUTPUT dQty).
                               
                work-tmp.qty-sheets = work-tmp.qty-sheets + dQty.

                RUN sys/ref/convquom.p(job-mat.qty-uom, "MSF",
                    job-mat.basis-w, job-mat.len,
                    job-mat.wid, item.s-dep,
                    mat-act.qty, OUTPUT dQty).
                               
                work-tmp.msf-sheets = work-tmp.msf-sheets + dQty.
            END.

            IF work-tmp.qty-sheets EQ 0 THEN      /* get sheets from slitter */
                FOR EACH mch-act FIELDS(waste qty)
                    WHERE mch-act.company EQ cCompany
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
                    WHERE mch-act.company EQ cCompany
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

            RELEASE ef.
      
            IF AVAILABLE est THEN
                FIND FIRST ef
                    WHERE ef.company EQ est.company
                    AND ef.est-no  EQ est.est-no
                    AND ef.form-no EQ work-tmp.frm
                    NO-LOCK NO-ERROR.

            {sys/inc/roundup.i work-tmp.qty-sheets}
      
            IF iUpTmp EQ 0 THEN iUpTmp = 1.

            IF AVAILABLE ef THEN
                RUN est/ef-#out.p (ROWID(ef), OUTPUT iOutTmp).
      
            ASSIGN
                iUpTmp                 = iUpTmp * iOutTmp
                work-tmp.qty-scrap-rec = work-tmp.qty-sheets -
                                   (work-tmp.qty-fg-rec / iUpTmp)
                work-tmp.msf-scrap-rec = work-tmp.msf-sheets -
                                   (work-tmp.msf-fg-rec)
                iUpTmp                 = 0.
     
        END. /*last-of(mch-act.frm)*/                     
    END. /*each work-tmp*/

    FOR EACH work-tmp BREAK BY work-tmp.sort-field
        BY work-tmp.job-no 
        BY work-tmp.job-no2:
        FIND FIRST job-mch WHERE job-mch.company  = cCompany AND
            job-mch.job      EQ work-tmp.job AND
            job-mch.frm      = work-tmp.frm AND
            (job-mch.blank-no = work-tmp.blank-no OR
            work-tmp.blank-no = 0) AND
            job-mch.m-code   = work-tmp.m-code AND
            job-mch.pass     = work-tmp.pass
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job      EQ work-tmp.job AND
                job-mch.frm      EQ work-tmp.frm AND
                (job-mch.blank-no = work-tmp.blank-no OR
                work-tmp.blank-no = 0) AND
                job-mch.m-code   EQ work-tmp.m-code
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job     EQ work-tmp.job AND
                job-mch.frm     EQ work-tmp.frm AND
                job-mch.m-code  EQ work-tmp.m-code AND
                job-mch.speed   NE 0
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job     EQ work-tmp.job AND
                job-mch.frm     EQ work-tmp.frm AND
                job-mch.m-code  EQ work-tmp.m-code
                NO-LOCK NO-ERROR.
        IF AVAILABLE job-mch THEN
        DO:
            IF work-tmp.qty NE 0 THEN
            DO:
                IF job-mch.speed NE 0 THEN
                DO:
                    IF CAN-FIND(FIRST mach WHERE
                        mach.company EQ cCompany AND               
                        mach.m-code  EQ job-mch.m-code AND
                        mach.therm   EQ YES AND
                        (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
                        FOR EACH job-mat FIELDS(i-no len) WHERE
                            job-mat.company EQ cCompany AND
                            job-mat.job = work-tmp.job AND
                            job-mat.frm EQ work-tmp.frm AND
                            job-mat.frm GT 0 AND
                            job-mat.len GT 0
                            NO-LOCK,
                            FIRST ITEM FIELDS(mat-type) WHERE
                            item.company EQ cCompany AND
                            item.i-no EQ job-mat.i-no
                            NO-LOCK
         
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
        END.
        ELSE
            ASSIGN run-hr = 0
                mr-hr  = 0.
        IF lJobDetail THEN
        DO: 
            FIND FIRST work-rep WHERE work-rep.sort-field EQ work-tmp.sort-field AND
                work-rep.dept   = work-tmp.dept AND
                work-rep.m-code = work-tmp.m-code AND
                work-rep.job-no = work-tmp.job-no AND
                work-rep.job-no2 = work-tmp.job-no2
                NO-ERROR.
        END.                             
        ELSE
        DO:
            FIND FIRST work-rep WHERE work-rep.sort-field EQ work-tmp.sort-field AND
                work-rep.dept   = work-tmp.dept AND
                work-rep.m-code = work-tmp.m-code 
                NO-ERROR.
        END.
        IF NOT AVAILABLE work-rep THEN
        DO:
            CREATE work-rep.
            ASSIGN 
                work-rep.sort-field = work-tmp.sort-field
                work-rep.dept       = work-tmp.dept
                work-rep.m-code     = work-tmp.m-code
                work-rep.sch-m-code = work-tmp.sch-m-code
                work-rep.job-no     = work-tmp.job-no
                work-rep.job-no2    = work-tmp.job-no2
                .
        END.
        ASSIGN 
            work-rep.r-std-hrs        = work-rep.r-std-hrs + run-hr
            work-rep.r-act-hrs        = work-rep.r-act-hrs + work-tmp.r-act-hrs
            work-rep.m-std-hrs        = work-rep.m-std-hrs + mr-hr
            work-rep.m-act-hrs        = work-rep.m-act-hrs + work-tmp.m-act-hrs
            work-rep.dt-chg-hrs       = work-rep.dt-chg-hrs + work-tmp.dt-chg-hrs
            work-rep.dt-nochg-hrs     = work-rep.dt-nochg-hrs +
                                  work-tmp.dt-nochg-hrs
            work-rep.qty              = work-rep.qty + work-tmp.qty
            work-rep.msf              = work-rep.msf + work-tmp.msf

            work-rep.qty-fg-rec       = work-rep.qty-fg-rec + work-tmp.qty-fg-rec
            work-rep.msf-fg-rec       = work-rep.msf-fg-rec + work-tmp.msf-fg-rec
            work-rep.qty-scrap-rec    = work-rep.qty-scrap-rec
                                 + work-tmp.qty-scrap-rec
            work-rep.msf-scrap-rec    = work-rep.msf-scrap-rec
                                 + work-tmp.msf-scrap-rec
            work-rep.qty-sheets       = work-rep.qty-sheets
                                 + work-tmp.qty-sheets
            work-rep.perc-total-scrap = work-rep.qty-scrap-rec / work-rep.qty-sheets
                                    * 100.

        IF LAST-OF(work-tmp.job-no2) THEN 
        DO:
            work-rep.no-jobs = work-rep.no-jobs + 1.
            FOR EACH mch-act
                WHERE mch-act.company EQ cCompany
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

          
    END. /*end each work-temp*/

    IF cTotalBy EQ "2" THEN
    DO:
        FOR EACH work-rep:

            IF lJobDetail THEN
            DO:       
                FIND FIRST work-rep-copy WHERE
                    work-rep-copy.m-code EQ work-rep.sch-m-code AND
                    work-rep-copy.sort-field EQ work-rep.sort-field AND
                    work-rep-copy.dept EQ work-rep.dept AND
                    work-rep-copy.job-no EQ work-tmp.job-no AND
                    work-rep-copy.job-no2 EQ work-tmp.job-no2
                    NO-ERROR.
            END.
            ELSE 
            DO:
                FIND FIRST work-rep-copy WHERE
                    work-rep-copy.m-code EQ work-rep.sch-m-code AND
                    work-rep-copy.sort-field EQ work-rep.sort-field AND
                    work-rep-copy.dept EQ work-rep.dept 
                    NO-ERROR.
            END.

            IF NOT AVAILABLE work-rep-copy THEN
            DO:
                CREATE work-rep-copy.
                ASSIGN 
                    work-rep-copy.m-code     = work-rep.sch-m-code
                    work-rep-copy.sort-field = work-rep.sort-field
                    work-rep-copy.dept       = work-rep.dept
                    work-rep-copy.job-no     = work-rep.job-no 
                    work-rep-copy.job-no2    = work-rep.job-no2.
            END.

            ASSIGN
                work-rep-copy.no-jobs            = work-rep-copy.no-jobs + work-rep.no-jobs
                work-rep-copy.no-setups          = work-rep-copy.no-setups + work-rep.no-setups
                work-rep-copy.r-std-hrs          = work-rep-copy.r-std-hrs + work-rep.r-std-hrs
                work-rep-copy.r-act-hrs          = work-rep-copy.r-act-hrs + work-rep.r-act-hrs
                work-rep-copy.m-std-hrs          = work-rep-copy.m-std-hrs + work-rep.m-std-hrs
                work-rep-copy.m-act-hrs          = work-rep-copy.m-act-hrs + work-rep.m-act-hrs
                work-rep-copy.dt-chg-hrs         = work-rep-copy.dt-chg-hrs + work-rep.dt-chg-hrs
                work-rep-copy.dt-nochg-hrs       = work-rep-copy.dt-nochg-hrs + work-rep.dt-nochg-hrs
                work-rep-copy.qty                = work-rep-copy.qty + work-rep.qty
                work-rep-copy.msf                = work-rep-copy.msf + work-rep.msf
                work-rep-copy.qty-fg-rec         = work-rep-copy.qty-fg-rec + work-rep.qty-fg-rec
                work-rep-copy.msf-fg-rec         = work-rep-copy.msf-fg-rec + work-rep.msf-fg-rec
                work-rep-copy.qty-scrap-rec      = work-rep-copy.qty-scrap-rec + work-rep.qty-scrap-rec
                work-rep-copy.msf-scrap-received = work-rep-copy.msf-scrap-received + work-rep.msf-scrap-received
                work-rep-copy.qty-sheets         = work-rep-copy.qty-sheets + work-rep.qty-sheets.

            RELEASE work-rep-copy.

            DELETE work-rep.
        END.

        FOR EACH work-rep-copy:
            CREATE work-rep.
            BUFFER-COPY work-rep-copy TO work-rep
                ASSIGN 
                work-rep.perc-total-scrap = work-rep.qty-scrap-rec / work-rep.qty-sheets
                                             * 100.
            RELEASE work-rep.
            DELETE work-rep-copy.
        END.    
    END.

    FOR EACH work-rep BREAK BY work-rep.sort-field
        BY work-rep.dept
        BY work-rep.m-code:  
       
        ASSIGN
            qty-hr  = work-rep.qty / work-rep.r-act-hrs
            chg-hrs = work-rep.r-act-hrs + work-rep.m-act-hrs +
                work-rep.dt-chg-hrs
            tot-hrs = chg-hrs + work-rep.dt-nochg-hrs
            std-hrs = work-rep.r-std-hrs + work-rep.m-std-hrs
            eff-pct = (std-hrs / chg-hrs) * 100.00
            pct-utl = (std-hrs / tot-hrs) * 100.00
            pct-dt  = ((work-rep.dt-nochg-hrs + work-rep.dt-chg-hrs) / tot-hrs)
                * 100.00.

        IF qty-hr = ? THEN qty-hr = 0.
        IF eff-pct = ? THEN eff-pct = 0.
        IF pct-utl = ? THEN pct-utl = 0.
        IF pct-dt = ? THEN pct-dt = 0.

        FIND FIRST mach WHERE
            mach.company = cCompany AND
            mach.m-code  = work-rep.m-code
            NO-LOCK.
                
        dt-hrs = work-rep.dt-chg-hrs + work-rep.dt-nochg-hrs.
     
        CREATE ttMachineEfficiency .
        ASSIGN
            ttMachineEfficiency.shift    = work-rep.sort-field
            ttMachineEfficiency.machCode = work-rep.m-code
            ttMachineEfficiency.cDESC    = IF AVAIL mach THEN mach.m-dscr ELSE ""
            ttMachineEfficiency.qty      = work-rep.qty
            ttMachineEfficiency.msf      = work-rep.msf
            ttMachineEfficiency.qtyHr    = IF qty-hr <> ? THEN  qty-hr ELSE 0 
            ttMachineEfficiency.runHrs   = work-rep.r-act-hrs
            ttMachineEfficiency.mrHrs    = work-rep.m-act-hrs
            ttMachineEfficiency.dtChg    = work-rep.dt-chg-hrs
            ttMachineEfficiency.totCrg   = chg-hrs
            ttMachineEfficiency.dtCrg    = work-rep.dt-nochg-hrs
            ttMachineEfficiency.totHrs   = tot-hrs
            ttMachineEfficiency.stdHrs   = std-hrs
            ttMachineEfficiency.effPer   = eff-pct
            ttMachineEfficiency.perUtil  = pct-utl
            ttMachineEfficiency.dtPer    = pct-dt
            ttMachineEfficiency.qtyFgRec = IF work-rep.qty-fg-rec <> ? THEN work-rep.qty-fg-rec  ELSE 0
            ttMachineEfficiency.msfFgRec = IF work-rep.msf-fg-rec <> ? THEN work-rep.msf-fg-rec ELSE 0
            ttMachineEfficiency.scrQty   = IF work-rep.qty-scrap-rec <> ? THEN work-rep.qty-scrap-rec ELSE 0
            ttMachineEfficiency.scrMsf   = IF work-rep.msf-scrap-rec <> ? THEN work-rep.msf-scrap-rec ELSE 0
            ttMachineEfficiency.totScrap = IF work-rep.perc-total-scrap <> ? THEN work-rep.perc-total-scrap ELSE 0 
            ttMachineEfficiency.job      = work-rep.job-no
            ttMachineEfficiency.job2     = work-rep.job-no2 .       
              
    END. /* EACH work-rep */  
 
    

    
END PROCEDURE.
