/*------------------------------------------------------------------------
  File:         r-promac.p
  Description:  Business Logic
  Author:       Sewa Singh
  Date Created: 06.03.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttProductivityMachine 
{aoa/tempTable/ttProductivityMachine.i}

{sys/ref/CustList.i NEW}
{sys/inc/var.i new shared}
/* Local Variable Definitions ---                                       */

DEFINE TEMP-TABLE tt-mch-srt NO-UNDO LIKE mch-srt 
    FIELD i-no       LIKE mch-srt.job-no
    FIELD start-time AS INTEGER
    FIELD start-date AS DATE .

&Scoped-define subjectID 207
{AOA/includes/subjectID{&subjectID}Defs.i} 

/* subject business logic */
/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE BUFFER b-mch-srt  FOR mch-srt.
    DEFINE BUFFER bf-mch-act FOR mch-act .    
    
    DEFINE VARIABLE dJobMrStd     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dJobRunStd    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dJobMrAct     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dJobRunAct    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dJobDtAct     AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dJobQtyProd   AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE dJobQtyExpect AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.     
    DEFINE VARIABLE dMrEff        AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE dRunEff       AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE dTotEff       AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE dDtEff        AS DECIMAL   FORMAT "->>>>9.9" NO-UNDO.
    DEFINE VARIABLE dTotStdHrs    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE dTotActHrs    AS DECIMAL   FORMAT "->>>9.9" NO-UNDO.
    DEFINE VARIABLE cJobNoCom     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemName     AS CHARACTER NO-UNDO.       
    DEFINE VARIABLE cMrComp       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRunComp      AS CHARACTER NO-UNDO .
    DEFINE VARIABLE dMrWaste      AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dRunWaste     AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dtDate        AS DATE      NO-UNDO .
    DEFINE VARIABLE cUserId       AS CHARACTER NO-UNDO .
    DEFINE VARIABLE iShift        AS INTEGER   NO-UNDO .    
    DEFINE VARIABLE dCalMsf       AS DECIMAL   FORMAT ">>>>>>.99" NO-UNDO .
    DEFINE VARIABLE iNumUp        AS INTEGER   FORMAT ">>>,>>9" NO-UNDO .
    DEFINE VARIABLE dActLabCost   AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE dPicPerHrs    AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dMsfPerHrs    AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dKikPerHrs    AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dPerWst       AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE dPerManHrs    AS DECIMAL   NO-UNDO .
    DEFINE VARIABLE cCustNo       AS CHARACTER NO-UNDO .
    DEFINE VARIABLE iJobQtyProd   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobQtyExpect AS INTEGER   NO-UNDO.     
    
    DEFINE VARIABLE iCount        AS INTEGER   NO-UNDO.
    
    FOR EACH mch-srt:
        DELETE mch-srt.
    END.
    FOR EACH tt-mch-srt:
        DELETE tt-mch-srt.
    END.
  
    cocode = cCompany.
    iCount = 0.
    RUN spGetSessionParam("Location", OUTPUT locode).     
    FOR EACH mch-act WHERE
        mch-act.company EQ cCompany AND
        mch-act.m-code  GE cStartMachine AND
        mch-act.m-code  LE cEndMachine AND
        mch-act.op-date GE dtStartDate AND
        mch-act.op-date LE dtEndDate AND
        mch-act.dept    GE cStartDeptNo AND
        mch-act.dept    LE cEndDeptNo AND
        mch-act.USER-ID GE cStartUserID AND
        mch-act.USER-ID LE cEndUserID  
        USE-INDEX dly-idx
        NO-LOCK,
        FIRST mach WHERE
        mach.company EQ cCompany AND
        mach.loc     EQ locode AND
        mach.m-code  EQ mch-act.m-code
        NO-LOCK:
        {custom/statusMsg.i " 'Processing Machine#  '  + mch-act.m-code "} 
        iCount = iCount + 1.            
        FIND FIRST mch-srt WHERE
            mch-srt.dept EQ mch-act.dept AND
            mch-srt.m-code EQ mch-act.m-code AND
            mch-srt.job-no EQ mch-act.job-no AND
            mch-srt.job-no2 EQ mch-act.job-no2 AND
            mch-srt.frm EQ mch-act.frm AND
            (mch-srt.blank-no EQ mch-act.blank-no OR
            mach.p-type NE "B" OR
            mch-act.blank-no EQ 0) AND
            mch-srt.pass EQ mch-act.pass
            NO-ERROR.
        IF NOT AVAILABLE mch-srt THEN
        DO:
            CREATE mch-srt.
            ASSIGN 
                mch-srt.dept     = mch-act.dept
                mch-srt.m-code   = mch-act.m-code
                mch-srt.job      = mch-act.job
                mch-srt.job-no   = mch-act.job-no
                mch-srt.job-no2  = mch-act.job-no2
                mch-srt.frm      = mch-act.frm
                mch-srt.blank-no = IF mach.p-type EQ "B"    AND
                                     mch-act.blank-no EQ 0 THEN 1
                                                           ELSE mch-act.blank-no
                mch-srt.pass     = mch-act.pass.
        
        END.
        FIND job-code WHERE job-code.code EQ mch-act.code
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-code THEN NEXT.
        IF job-code.cat EQ "RUN" THEN
        DO:
            mch-srt.run-act-hr = mch-srt.run-act-hr + mch-act.hours.
            mch-srt.qty-prod   = mch-srt.qty-prod +
                IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.
        END.
        ELSE IF job-code.cat EQ "MR" THEN
                mch-srt.mr-act-hr  = mch-srt.mr-act-hr + mch-act.hours.
            ELSE
                mch-srt.act-dt-hr  = mch-srt.act-dt-hr + mch-act.hours.    
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).        
    END.
    
    ASSIGN
        dPicPerHrs = 0
        dMsfPerHrs = 0
        dKikPerHrs = 0
        dPerWst    = 0
        dPerManHrs = 0
        iCount     = 0.
            
    FOR EACH mch-srt,

        FIRST job WHERE   /* CTS added join */
        job.company EQ cCompany AND
        job.job-no  EQ mch-srt.job-no AND
        job.job-no2 EQ mch-srt.job-no2 NO-LOCK,
        FIRST mach FIELDS(p-type) WHERE
        mach.company EQ cCompany AND
        mach.loc     EQ locode AND
        mach.m-code  EQ mch-srt.m-code
        NO-LOCK:
        iCount = iCount + 1.           

        FIND FIRST job-mch WHERE job-mch.company  = cCompany AND
            job-mch.job      EQ mch-srt.job AND
            job-mch.job-no  EQ mch-srt.job-no AND
            job-mch.job-no2 EQ mch-srt.job-no2 AND
            job-mch.frm      = mch-srt.frm AND
            (job-mch.blank-no = mch-srt.blank-no OR
            mch-srt.blank-no = 0) AND
            job-mch.m-code   = mch-srt.m-code AND
            job-mch.pass     = mch-srt.pass
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job      EQ mch-srt.job AND
                job-mch.job-no  EQ mch-srt.job-no AND
                job-mch.job-no2 EQ mch-srt.job-no2 AND
                job-mch.frm      EQ mch-srt.frm AND
                (job-mch.blank-no = mch-srt.blank-no OR
                mch-srt.blank-no = 0) AND
                job-mch.m-code   EQ mch-srt.m-code
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job     EQ mch-srt.job AND
                job-mch.job-no  EQ mch-srt.job-no AND
                job-mch.job-no2 EQ mch-srt.job-no2 AND
                job-mch.frm     EQ mch-srt.frm AND
                job-mch.m-code  EQ mch-srt.m-code AND
                job-mch.speed   NE 0
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job     EQ mch-srt.job AND
                job-mch.job-no  EQ mch-srt.job-no AND
                job-mch.job-no2 EQ mch-srt.job-no2 AND
                job-mch.frm     EQ mch-srt.frm AND
                job-mch.m-code  EQ mch-srt.m-code
                NO-LOCK NO-ERROR.
        IF AVAILABLE job-mch THEN
        DO:
            IF mch-srt.qty-prod NE 0 THEN
            DO:
                IF CAN-FIND(FIRST mach WHERE
                    mach.company EQ cCompany AND
                    mach.loc     EQ locode AND
                    mach.m-code  EQ job-mch.m-code AND
                    mach.therm   EQ YES AND
                    (mach.p-type EQ "R" OR mach.dept[1] EQ "LM")) THEN
                    FOR EACH job-mat FIELDS(i-no len) WHERE
                        job-mat.company EQ cCompany AND
                        job-mat.job = mch-srt.job AND
                        job-mat.frm EQ mch-srt.frm AND
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
         
                        mch-srt.run-std-hr = (mch-srt.qty-prod * job-mat.len / 12) / job-mch.speed.
                        LEAVE.
                    END.
                ELSE
                    ASSIGN mch-srt.run-std-hr = mch-srt.qty-prod / job-mch.speed.
            END.
            ELSE
                ASSIGN mch-srt.run-std-hr = job-mch.run-hr.

            IF NOT( lIncludedonotcarry AND
                CAN-FIND(FIRST mch-act WHERE
                mch-act.company EQ job-mch.company AND
                mch-act.dept EQ mch-srt.dept AND
                mch-act.m-code EQ mch-srt.m-code AND
                mch-act.job EQ mch-srt.job AND 
                mch-act.job-no EQ mch-srt.job-no AND
                mch-act.job-no2 EQ mch-srt.job-no2 AND
                mch-act.frm EQ mch-srt.frm AND
                (mch-srt.blank-no = IF mach.p-type EQ "B"    AND
                mch-act.blank-no EQ 0 THEN 1
            ELSE mch-act.blank-no) AND
                mch-act.pass EQ mch-srt.pass AND
                mch-act.op-date LT dtStartDate)) THEN
                mch-srt.mr-std-hr  = job-mch.mr-hr.

            mch-srt.qty-expect = IF job-mch.speed NE 0 THEN
                (IF mch-srt.run-act-hr NE 0
                THEN mch-srt.run-act-hr
                ELSE mch-srt.run-std-hr) * job-mch.speed
                ELSE job-mch.run-qty.
        END.
    
     

        {sys/inc/roundup.i mch-srt.qty-expect}

        IF mch-srt.run-std-hr EQ ? THEN mch-srt.run-std-hr = 0.
                                    
        cJobNoCom = STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', mch-srt.job-no, mch-srt.job-no2)) .
                     
        FOR EACH job-hdr
            WHERE job-hdr.company EQ cCompany
            AND job-hdr.job     EQ mch-srt.job
            AND job-hdr.job-no  EQ mch-srt.job-no
            AND job-hdr.job-no2 EQ mch-srt.job-no2
            NO-LOCK
               
            BY job-hdr.blank-no DESCENDING
            BY job-hdr.frm      DESCENDING:
              
            cItemName = job-hdr.i-no.
            IF job-hdr.frm       EQ mch-srt.frm           AND
                (job-hdr.blank-no EQ mch-srt.blank-no OR
                job-hdr.blank-no EQ 0)                    THEN LEAVE.
        END.

        mch-srt.job-no = cJobNoCom.
        CREATE tt-mch-srt .
        BUFFER-COPY mch-srt TO tt-mch-srt .
        ASSIGN
            tt-mch-srt.i-no = cItemName            
            .
        IF cJobNumber-DateTime EQ "2" THEN 
        DO:   
            IF AVAILABLE job-mch THEN
                ASSIGN
                    tt-mch-srt.start-date = job-mch.start-date
                    tt-mch-srt.start-time = job-mch.start-time . 
        END. 
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
    END.
  
    PUT SKIP.
     
    ASSIGN 
        cMrComp     = "" 
        cRunComp    = "" 
        dCalMsf     = 0
        iNumUp      = 0 
        dActLabCost = 0 . 

    FOR EACH tt-mch-srt USE-INDEX dept-idx
        BREAK BY tt-mch-srt.dept
        BY tt-mch-srt.m-code
        BY tt-mch-srt.start-date
        BY tt-mch-srt.start-time
        BY tt-mch-srt.job-no :        

        FIND FIRST itemfg
            WHERE itemfg.company EQ cCompany
            AND itemfg.i-no    EQ tt-mch-srt.i-no
            NO-LOCK NO-ERROR.

        dCalMsf = /*dCalMsf
                                     +*/ (tt-mch-srt.qty-prod *
            (IF AVAILABLE itemfg THEN itemfg.t-sqft
            ELSE 1) / 1000).

        FIND FIRST job WHERE 
            job.company EQ cCompany AND
            job.job-no  EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
            job.job-no2 EQ tt-mch-srt.job-no2 NO-LOCK NO-ERROR .
        ASSIGN 
            cCustNo = "" .
        IF AVAILABLE job THEN 
        DO:
             
            FIND FIRST eb WHERE eb.company EQ cCompany
                AND eb.est-no EQ job.est-no
                AND eb.stock-no EQ tt-mch-srt.i-no NO-LOCK NO-ERROR .
            IF AVAILABLE eb THEN
                ASSIGN iNumUp = eb.num-up .
            ELSE 
                iNumUp = 0 .
         
            FOR EACH misc-act FIELDS(cost)
                WHERE misc-act.company EQ cCompany
                AND misc-act.job     EQ job.job
                NO-LOCK:
                     
            /*dActLabCost = dActLabCost + misc-act.cost.*/
         
            END.
            FIND FIRST job-hdr
                WHERE job-hdr.company EQ cCompany
                AND job-hdr.i-no     EQ tt-mch-srt.i-no
                AND job-hdr.job-no  EQ job.job-no
                AND job-hdr.job-no2 EQ job.job-no2 NO-LOCK NO-ERROR .
            IF AVAILABLE job-hdr THEN
                cCustNo = job-hdr.cust-no  .

        END.

        FIND FIRST job-mch WHERE job-mch.company  = cCompany  AND
            job-mch.job      EQ tt-mch-srt.job  AND 
            job-mch.job-no   EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen)  AND
            job-mch.job-no2  EQ tt-mch-srt.job-no2  AND 
            job-mch.frm      = tt-mch-srt.frm AND 
            (job-mch.blank-no = tt-mch-srt.blank-no OR
            mch-srt.blank-no = 0) AND              
            job-mch.m-code   = tt-mch-srt.m-code AND 
            job-mch.pass     = tt-mch-srt.pass 
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job      EQ tt-mch-srt.job AND
                job-mch.job-no   EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
                job-mch.job-no2  EQ tt-mch-srt.job-no2 AND 
                job-mch.frm      EQ tt-mch-srt.frm AND
                (job-mch.blank-no = tt-mch-srt.blank-no OR
                mch-srt.blank-no = 0) AND
                job-mch.m-code   EQ tt-mch-srt.m-code
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job     EQ tt-mch-srt.job AND
                job-mch.job-no  EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
                job-mch.job-no2 EQ tt-mch-srt.job-no2 AND 
                job-mch.frm     EQ tt-mch-srt.frm AND
                job-mch.m-code  EQ tt-mch-srt.m-code AND
                job-mch.speed   NE 0
                NO-LOCK NO-ERROR.
        IF NOT AVAILABLE job-mch THEN
            FIND FIRST job-mch WHERE job-mch.company EQ cCompany AND
                job-mch.job     EQ tt-mch-srt.job AND 
                job-mch.job-no  EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen)  AND
                job-mch.job-no2 EQ tt-mch-srt.job-no2 AND  
                job-mch.frm     EQ tt-mch-srt.frm AND
                job-mch.m-code  EQ tt-mch-srt.m-code        
                NO-LOCK NO-ERROR.  
        IF AVAILABLE job-mch THEN
        DO:
            ASSIGN                                   
                cMrComp  = STRING(job-mch.mr-complete)
                cRunComp = STRING(job-mch.run-complete) .
        
        END.

        ASSIGN 
            dRunWaste   = 0 
            dMrWaste    = 0 
            dActLabCost = 0
            dtDate      = ? 
            cUserId     = ""
            iShift      = 0.
            
        FOR EACH bf-mch-act WHERE
            bf-mch-act.company EQ cCompany AND
            bf-mch-act.dept EQ tt-mch-srt.dept AND
            bf-mch-act.m-code EQ tt-mch-srt.m-code AND
            bf-mch-act.job EQ tt-mch-srt.job AND 
            bf-mch-act.job-no EQ SUBSTRING(tt-mch-srt.job-no,1,iJobLen) AND
            bf-mch-act.job-no2 EQ tt-mch-srt.job-no2 AND
            bf-mch-act.frm EQ tt-mch-srt.frm AND
            (bf-mch-act.blank-no = tt-mch-srt.blank-no  OR
            mach.p-type NE "B" OR
            bf-mch-act.blank-no EQ 0) AND
            bf-mch-act.pass EQ tt-mch-srt.pass  NO-LOCK:

            FIND job-code WHERE job-code.code EQ bf-mch-act.CODE NO-LOCK NO-ERROR.

            IF NOT AVAILABLE job-code THEN NEXT.
            IF job-code.cat EQ "RUN" THEN
            DO:
                dRunWaste = dRunWaste + bf-mch-act.waste .
            END.
            ELSE IF job-code.cat EQ "MR" THEN
                    dMrWaste = dMrWaste + bf-mch-act.waste .

            dActLabCost = dActLabCost + (bf-mch-act.hours * bf-mch-act.crew) .
            dtDate = bf-mch-act.op-date .
            cUserId = IF AVAILABLE bf-mch-act THEN  bf-mch-act.USER-ID ELSE "".
            iShift  = IF AVAILABLE bf-mch-act THEN  bf-mch-act.shift ELSE 0.

        END. /* FOR EACH bf-mch-act W */

        ASSIGN 
            dJobMrStd     = dJobMrStd + tt-mch-srt.mr-std-hr
            dJobRunStd    = dJobRunStd + tt-mch-srt.run-std-hr
            dJobMrAct     = dJobMrAct + tt-mch-srt.mr-act-hr
            dJobRunAct    = dJobRunAct + tt-mch-srt.run-act-hr
            dJobDtAct     = dJobDtAct + tt-mch-srt.act-dt-hr
            dJobQtyProd   = dJobQtyProd + tt-mch-srt.qty-prod
            dJobQtyExpect = dJobQtyExpect + tt-mch-srt.qty-expect.

        IF LAST-OF(tt-mch-srt.job-no) THEN 
        DO:
            IF dJobRunAct EQ 0 THEN
                dJobRunStd = 0.
           
            dMrEff  = (dJobMrStd  / dJobMrAct)  * 100.00.
            IF dMrEff EQ ? THEN dMrEff = 0.
            dRunEff = (dJobRunStd / dJobRunAct) * 100.00.
            IF dRunEff EQ ? THEN dRunEff = 0.
            ASSIGN
                dTotStdHrs = dJobMrStd + dJobRunStd
                dTotActHrs = dJobMrAct + dJobRunAct
                dTotEff    = (dTotStdHrs / dTotActHrs) * 100.00.
            IF dTotEff EQ ? THEN dTotEff = 0.
            dDtEff = (dJobDtAct / dTotActHrs) * 100.00.
            IF dDtEff EQ ? THEN dDtEff = 0.
          
            IF iNumUp = 0 THEN ASSIGN iNumUp = 1 .
            ASSIGN
                dPicPerHrs = (iJobQtyProd / (dJobMrAct + dJobRunAct + dJobDtAct))
                dMsfPerHrs = (dCalMsf / (dJobMrAct + dJobRunAct + dJobDtAct))
                dKikPerHrs = ( /*iJobQtyProd*/ dPicPerHrs  / iNumUp)
                dPerWst    = ((dMrWaste + dRunWaste) / ( iJobQtyProd)) * 100
                dPerManHrs = (iJobQtyProd / dActLabCost) .
               
            CREATE ttProductivityMachine.
            ASSIGN
                ttProductivityMachine.cMCode        = tt-mch-srt.m-code
                ttProductivityMachine.cFgItem       = STRING(tt-mch-srt.i-no)
                ttProductivityMachine.cJobNo        = STRING(tt-mch-srt.job-no)
                ttProductivityMachine.cCustNo       = IF cCustNo NE ? THEN STRING(cCustNo) ELSE ""
                ttProductivityMachine.dMrStd        = dJobMrStd
                ttProductivityMachine.dMrAcl        = dJobMrAct
                ttProductivityMachine.dMrEff        = dMrEff
                ttProductivityMachine.dRunStd       = dJobRunStd
                ttProductivityMachine.dRunAcl       = dJobRunAct
                ttProductivityMachine.dRunEff       = dRunEff
                ttProductivityMachine.dMrRunStd     = dTotStdHrs
                ttProductivityMachine.dMrRunAcl     = dTotActHrs
                ttProductivityMachine.dMrRunEff     = dTotEff
                ttProductivityMachine.dDtAcl        = dJobDtAct
                ttProductivityMachine.dDtEff        = dDtEff
                ttProductivityMachine.iAclQty       = iJobQtyProd
                ttProductivityMachine.iExptdQty     = iJobQtyExpect
                ttProductivityMachine.cMrComp       = cMrComp
                ttProductivityMachine.cRunComp      = cRunComp
                ttProductivityMachine.dTtlMchHrs    = dJobMrAct + dJobRunAct + dJobDtAct
                ttProductivityMachine.dTtlLbrHrs    = dActLabCost
                ttProductivityMachine.dPicPerHrs    = dPicPerHrs
                ttProductivityMachine.dMsf          = dCalMsf
                ttProductivityMachine.dMsfPerHrs    = dMsfPerHrs
                ttProductivityMachine.iNumberOn     = iNumUp
                ttProductivityMachine.dKikPerHrs    = dKikPerHrs
                ttProductivityMachine.dPicPerManHrs = dPerManHrs
                ttProductivityMachine.dMrWaste      = dMrWaste
                ttProductivityMachine.dRunWaste     = dRunWaste
                ttProductivityMachine.dTotalWaste   = dMrWaste + dRunWaste
                ttProductivityMachine.dPercentWaste = dPerWst
                ttProductivityMachine.dtDate        = dtDate 
                ttProductivityMachine.cUserId       = cUserId 
                ttProductivityMachine.iShift        = iShift  
                iCount                              = iCount + 1
                .      
                                        
            IF lProgressBar THEN
                RUN spProgressBar (cProgressBar, iCount, ?).               
        END.                 
    END. /* each item */

    FIND CURRENT mch-srt NO-LOCK NO-ERROR.
    
END PROCEDURE.

{AOA/dynBL/pBuildCustList.i}
