/* scheduleProcs.i - rstark - 8.15.2020    */
/* used in capacityPage.w and jobDueDate.p */

/* **********************  Internal Procedures  *********************** */

PROCEDURE calcEnd:
    DEFINE INPUT PARAMETER ipDate AS DATE    NO-UNDO.
    DEFINE INPUT PARAMETER ipTime AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipMR   AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipRun  AS DECIMAL NO-UNDO.

    DEFINE OUTPUT PARAMETER opDate AS DATE    NO-UNDO.
    DEFINE OUTPUT PARAMETER opTime AS INTEGER NO-UNDO.

    DEFINE VARIABLE totalTime AS INTEGER NO-UNDO.
    DEFINE VARIABLE days      AS INTEGER NO-UNDO.

    IF ipTime EQ ? THEN ipTime = 0.
    IF ipMR EQ ? THEN ipMR     = 0.
    IF ipRun EQ ? THEN ipRun   = 0.
    ASSIGN
        totalTime = ipTime + ipMR * 3600 + ipRun * 3600
        days      = TRUNCATE(totalTime / 86400,0)
        opDate    = ipDate + days
        opTime    = totalTime - days * 86400
        .
    IF opDate EQ ? THEN opDate = ipDate.
    IF opTime EQ ? THEN opTime = ipTime.

END PROCEDURE.

PROCEDURE pBuildTTJob:
    DEFINE INPUT PARAMETER ipcType    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iprRowID   AS ROWID     NO-UNDO.
    
    DEFINE VARIABLE cMachine    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dRunHours   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iDie        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE dOnForm     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dOnSheet    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iOut        AS INTEGER   NO-UNDO INITIAL 1.
    DEFINE VARIABLE iNumUp      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lPrintedLit AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dSheetLen   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lUnitize    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE dPalletQty  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dParts      AS DECIMAL   NO-UNDO.

    DEFINE BUFFER beb FOR eb.

    lContinue = YES.
    EMPTY TEMP-TABLE ttJob.    
    /* order not yet implemented */
    IF ipcType EQ "Order" THEN DO:
        FIND FIRST oe-ord NO-LOCK 
             WHERE ROWID(oe-ord) EQ iprRowID
             NO-ERROR.
        IF NOT AVAILABLE oe-ord OR oe-ord.job-no EQ "" THEN
        ASSIGN
            cErrorMsg = "No Job Exists for this Order."
            lContinue = NO
            .
        FIND FIRST job NO-LOCK
             WHERE job.company EQ oe-ord.company
               AND job.job     EQ oe-ord.j-no
             NO-ERROR.
        IF NOT AVAILABLE job THEN
        ASSIGN
            cErrorMsg = cErrorMsg + "Unable to locate Job for this Order." + CHR(10)
            lContinue = NO
            .
        ASSIGN
            iprRowID = ROWID(job)
            ipcType  = "Job"
            .
    END. /* type eq order */

    IF lContinue AND ipcType EQ "Job" AND iprRowID NE ? THEN DO: 
        FIND job NO-LOCK WHERE ROWID(job) EQ iprRowID NO-ERROR.
        IF NOT AVAILABLE job THEN
        ASSIGN
            cErrorMsg = cErrorMsg + "Job Record Missing." + CHR(10)
            lContinue = NO
            .
        IF lContinue THEN DO:
            FIND FIRST job-hdr OF job NO-LOCK NO-ERROR.
            IF NOT AVAILABLE job THEN
            ASSIGN
                cErrorMsg = cErrorMsg + "Unable to locate Job Header." + CHR(10)
                lContinue = NO
                .
            ELSE IF NOT job-hdr.opened THEN
                 cErrorMsg = cErrorMsg + "Job is Closed." + CHR(10).
            baseOnText = "Based on Job #" + TRIM(job.job-no + "-" + STRING(job.job-no2)).
            FOR EACH job-mch NO-LOCK
                WHERE job-mch.company      EQ job.company
                  AND job-mch.job          EQ job.job
                  AND job-mch.run-complete EQ NO,
                FIRST mach NO-LOCK
                WHERE mach.company EQ job-mch.company
                  AND mach.loc     EQ job.loc
                  AND mach.m-code  EQ job-mch.m-code
                :
                IF job-mch.start-date-su NE ? THEN
                cErrorMsg = cErrorMsg + job-mch.m-code + " Routing already Scheduled." + CHR(10).
                IF job-mch.run-complete THEN
                cErrorMsg = cErrorMsg + job-mch.m-code + " Routing already Run Completed." + CHR(10).
                cMachine = IF mach.sch-m-code NE "" THEN mach.sch-m-code ELSE mach.m-code.
                CREATE ttJob.
                BUFFER-COPY job-mch TO ttJob
                    ASSIGN 
                      ttJob.rRowID = ROWID(job-mch)
                      ttJob.d-seq  = mach.d-seq
                      ttJob.m-seq  = mach.m-seq
                      ttJob.m-code = cMachine
                      ttJob.m-dscr = mach.m-dscr
                      . 
            END. /* each job-mch */
        END. /* if lcontinue */
    END. /* job and iprrowid ne ? */
    
    IF ipcType EQ "Est" AND iprRowID NE ? THEN DO: 
        FIND est NO-LOCK WHERE ROWID(est) EQ iprRowID NO-ERROR.
        IF NOT AVAILABLE est THEN
        ASSIGN
            cErrorMsg = "Estimate Record Missing."
            lContinue = NO
            .
        baseOnText = "Based on Est #" + TRIM(est.est-no).
        FIND FIRST eb NO-LOCK 
             WHERE eb.company EQ est.company
               AND eb.est-no  EQ est.est-no
               AND eb.form-no EQ 1
             NO-ERROR.
        IF AVAILABLE eb THEN DO:
            lPrintedLit = CAN-FIND(FIRST prodl
                                   WHERE prodl.company EQ est.company
                                     AND prodl.prolin  EQ 'Printed'
                                     AND prodl.procat  EQ eb.procat).        
            FOR EACH est-op NO-LOCK
                WHERE est-op.company EQ est.company
                  AND est-op.est-no  EQ est.est-no
                  AND est-op.line    LT 500,
                FIRST ef NO-LOCK
                WHERE ef.company EQ est.company
                  AND ef.est-no  EQ est-op.est-no
                  AND ef.form-no EQ est-op.s-num
                   BY est-op.s-num
                   BY est-op.b-num
                   BY est-op.d-seq
                   BY est-op.op-pass
                :
                cMachine  = est-op.m-code.
                FIND FIRST mach NO-LOCK
                     WHERE mach.company EQ est.company
                       AND mach.loc     EQ est.loc
                       AND mach.m-code  EQ cMachine
                     NO-ERROR.
                IF NOT AVAILABLE mach THEN NEXT.
                IF mach.sch-m-code NE "" AND mach.sch-m-code NE mach.m-code THEN DO:
                    cMachine = mach.sch-m-code. 
                    FIND FIRST mach NO-LOCK
                         WHERE mach.company EQ est.company
                           AND mach.loc     EQ est.loc
                           AND mach.m-code  EQ cMachine
                         NO-ERROR.
                    IF NOT AVAILABLE mach THEN NEXT.
                END. /* if sch-m-code ne m-code */
                IF est.est-type EQ 6 THEN DO:
                    dParts = 0.
                    FOR EACH beb FIELDS(quantityPerSet) NO-LOCK
                        WHERE beb.company EQ est.company
                          AND beb.est-no  EQ est.est-no
                          AND beb.form-no NE 0
                        :
                        dParts = dParts
                               + (IF beb.quantityPerSet LT 0 THEN (-1 / beb.quantityPerSet)
                                  ELSE beb.quantityPerSet)
                               .
                    END. /* each beb */
                END. /* if est-type eq 6 */
                ASSIGN
                    dSheetLen = IF est-op.dept EQ "LM" THEN ef.nsh-len ELSE ef.gsh-len
                    dRunHours = 0
                    .
                CASE est.est-type:
                    WHEN 1 OR WHEN 3 THEN DO:
                        RUN sys/inc/numout.p (RECID(est-op), OUTPUT dOnForm).                    
                        IF est-op.op-speed GT 0 THEN DO:
                            IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                            dRunHours = ((est-op.num-sh * dOnForm * iOut) - est-op.op-waste)
                                      * (dSheetLen / 12)
                                      .
                            ELSE IF est-op.op-sb THEN
                                 dRunHours = (est-op.num-sh * dOnForm * iOut) - est-op.op-waste.
                                 ELSE IF NOT lPrintedLit OR iOut EQ 1 THEN
                                 dRunHours = (est-op.num-sh * iNumUp
                                           * (IF ef.n-out   EQ 0 THEN 1 ELSE ef.n-out)
                                           * (IF ef.n-out-l EQ 0 THEN 1 ELSE ef.n-out-l))
                                           - est-op.op-waste
                                           .
                                      ELSE
                                      dRunHours = (est-op.num-sh * iOut) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 1 or 3 */
                    WHEN 2 OR WHEN 4 THEN DO:
                        RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT iNumUp).
                        RUN sys/inc/numout.p (RECID(est-op), OUTPUT dOnForm).            
                        IF est-op.dept EQ "DC" AND est-op.n-out GT 0 THEN DO:
                            FIND FIRST ef-nsh OF ef NO-LOCK 
                                 WHERE ef-nsh.pass-no EQ est-op.op-pass
                                   AND ef-nsh.dept    EQ est-op.dept
                                 NO-ERROR.
                            IF AVAILABLE ef-nsh THEN DO:
                                RUN cec/foamplus.p (ROWID(ef-nsh), OUTPUT iDie).
                                dOnForm = dOnForm * (est-op.n-out + INT(iDie GT 0)).
                            END. /* avail ef-nsh */
                        END. /* if dc */            
                        IF est-op.op-speed GT 0 THEN DO:
                            IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                            dRunHours = ((est-op.num-sh * dOnForm * iOut) - est-op.op-waste)
                                      * (dSheetLen / 12)
                                      .
                            ELSE IF est-op.op-sb THEN
                                 dRunHours = (est-op.num-sh * dOnForm * iOut) - est-op.op-waste.
                                 ELSE IF NOT lPrintedLit OR iOut EQ 1 THEN
                                      dRunHours = (est-op.num-sh * iNumUp * dOnForm) - est-op.op-waste.
                                      ELSE
                                      dRunHours = (est-op.num-sh * iOut * dOnForm) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 2 or 4 */
                    WHEN 5 THEN DO:
                        lUnitize = NO.
                        FOR EACH mstd NO-LOCK
                            WHERE mstd.company EQ mach.company
                              AND mstd.loc     EQ mach.loc
                              AND mstd.m-code  EQ mach.m-code
                            BREAK BY mstd.style DESCENDING
                            :
                            IF LAST(mstd.style) OR mstd.style EQ eb.style THEN DO:
                                lUnitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
                                LEAVE.
                            END. /* last(mstd.style) */
                        END. /* each mstd */
                        IF est-op.op-speed GT 0 THEN DO:
                            IF lUnitize THEN 
                            dRunHours = dPalletQty. /* not set, calc in cec/pr4-cas.p as p-qty */
                            ELSE IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                                 dRunHours = ((est-op.num-sh * dOnForm) - est-op.op-waste)
                                           * (dSheetLen / 12)
                                           .
                                ELSE IF mach.p-type EQ "A" THEN
                                     dRunHours = est.est-qty[1].
                                    ELSE IF est-op.op-sb THEN
                                         dRunHours = (est-op.num-sh * dOnForm) - est-op.op-waste.
                                         ELSE 
                                         dRunHours = (est-op.num-sh * dOnSheet) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 5 */
                    WHEN 6 THEN DO:
                        lUnitize = NO.
                        FOR EACH mstd NO-LOCK
                            WHERE mstd.company EQ mach.company
                              AND mstd.loc     EQ mach.loc
                              AND mstd.m-code  EQ mach.m-code
                            BREAK BY mstd.style DESCENDING
                            :
                            IF LAST(mstd.style) OR mstd.style EQ eb.style THEN DO:
                                lUnitize = mstd.rs-x EQ 98 OR mstd.rs-y EQ 98.
                                LEAVE.
                            END. /* last(mstd.style) */
                        END. /* each mstd */
                        IF lUnitize THEN DO:
                        END. /* if lunitize */
                        ELSE IF mach.p-type EQ "P" THEN
                             dRunHours = (est-op.num-sh - est-op.op-waste) * dParts.
                            ELSE IF mach.therm AND (mach.p-type NE "A" OR est-op.dept EQ "LM") THEN
                                 dRunHours = ((est-op.num-sh * dOnForm) - est-op.op-waste)
                                           * (dSheetLen / 12)
                                           .
                                ELSE IF mach.p-type EQ "A" THEN
                                     dRunHours = est.est-qty[1].
                                    ELSE IF est-op.op-sb THEN
                                         dRunHours = (est-op.num-sh * dOnForm) - est-op.op-waste.
                                         ELSE 
                                         dRunHours = (est-op.num-sh * dOnSheet) - est-op.op-waste.
                    END. /* 6 */
                    WHEN 8 THEN DO:
                        RUN sys/inc/numup.p (ef.company, ef.est-no, ef.form-no, OUTPUT iNumUp).
                        RUN sys/inc/numout.p (RECID(est-op), OUTPUT dOnForm).            
                        IF est-op.op-speed GT 0 THEN DO:
                            IF mach.therm AND (mach.p-type EQ "R" OR est-op.dept EQ "LM") THEN
                            dRunHours = ((est-op.num-sh * dOnForm) - est-op.op-waste)
                                      * (dSheetLen / 12)
                                      .
                            ELSE IF est-op.op-sb THEN
                                 dRunHours = (est-op.num-sh * dOnForm) - est-op.op-waste.
                                 ELSE 
                                 dRunHours = (est-op.num-sh * iNumUp * dOnForm) - est-op.op-waste.
                        END. /* if op-speed */
                    END. /* 8 */
                END CASE.
                dRunHours = dRunHours / est-op.op-speed.
                IF est-op.n_out_div GT 0 THEN 
                dRunHours = dRunHours / est-op.n_out_div.
                IF dRunHours LT 0 THEN 
                dRunHours = 0.
                CREATE ttJob.
                ASSIGN 
                    ttJob.rRowID   = ROWID(est-op)
                    ttJob.company  = ipcCompany
                    ttJob.m-code   = cMachine
                    ttJob.m-dscr   = mach.m-dscr
                    ttJob.frm      = est-op.s-num
                    ttJob.blank-no = IF (mach.p-type  EQ "B" OR
                                        (est.est-type EQ  3 AND
                                         est-op.dept  EQ "PR")) THEN est-op.b-num
                                     ELSE 0
                    ttJob.pass     = est-op.op-pass
                    ttJob.d-seq    = mach.d-seq
                    ttJob.m-seq    = mach.m-seq
                    ttJob.mr-hr    = est-op.op-mr
                    ttJob.run-hr   = dRunHours
                    . 
            END. /* each est-op */
        END. /* if avail eb */
    END. /* est and iprrowid ne ? */    
    {&OPEN-QUERY-ttJob}

END PROCEDURE.
    
PROCEDURE pBuildTTMachine:
    FOR EACH mach NO-LOCK
        WHERE mach.company EQ ipcCompany
        BREAK BY mach.sch-m-code
        :
        IF FIRST-OF(mach.sch-m-code) THEN DO:
            CREATE ttMachine.
            ASSIGN
                ttMachine.m-code = mach.m-code
                ttMachine.m-dscr = mach.m-dscr
                ttMachine.d-seq  = mach.d-seq
                ttMachine.m-seq  = mach.m-seq
                .
        END. /* first-of */
    END. /* each mach */
    {&OPEN-QUERY-ttMachine}

END PROCEDURE.
    
PROCEDURE pCreateTtblDowntime:
    IF CAN-FIND(FIRST ttblDowntime
                WHERE ttblDowntime.dayID     EQ tempDowntime.dayID
                  AND ttblDowntime.resource  EQ tempDowntime.resource
                  AND ttblDowntime.startDate EQ tempDowntime.startDate
                  AND ttblDowntime.startTime EQ tempDowntime.startTime
                  AND ttblDowntime.endTime   EQ tempDowntime.endTime) THEN
    RETURN.
    CREATE ttblDowntime.
    BUFFER-COPY tempDowntime TO ttblDowntime.
    ttblDowntime.startDateTime = numericDateTime(ttblDowntime.startDate,ttblDowntime.startTime).
    ttblDowntime.endDateTime   = numericDateTime(ttblDowntime.startDate,ttblDowntime.endTime).

END PROCEDURE.

PROCEDURE pLoadDowntime:
    DEFINE VARIABLE cFileName     AS CHARACTER NO-UNDO FORMAT "X(60)".
    DEFINE VARIABLE cAttrList     AS CHARACTER NO-UNDO FORMAT "X(4)".
    DEFINE VARIABLE cListItems    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResource     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cResourceList AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bttFolder FOR ttFolder.

    CREATE ttFolder.
    ttFolder.folderName = ".\schedule\data\ASI".
    FOR EACH ttFolder
        WHERE ttFolder.searched EQ NO
        :
        INPUT FROM OS-DIR(ttFolder.folderName) NO-ECHO.
        REPEAT:
            SET cFileName ^ cAttrList.
            IF cAttrList EQ "d" THEN DO:
                IF cFileName EQ "." OR cFileName EQ ".." THEN NEXT.
                CREATE bttFolder.
                bttFolder.folderName = ttFolder.folderName + "\" + cFileName.
            END.
            ELSE
            IF cAttrList EQ "f" AND
               INDEX(cFileName,"downtimes.") NE 0 AND
               INDEX(cFileName,".dat") NE 0 THEN
            cListItems = cListItems + ttFolder.folderName + "\" + cFileName + ",".
        END. /* repeat */
        INPUT CLOSE.
        ttFolder.searched = YES.
    END. /* each ttfolder */
    cListItems = TRIM(cListItems,",").

    EMPTY TEMP-TABLE ttblDowntime.
    DO idx = 1 TO NUM-ENTRIES(cListItems):
        EMPTY TEMP-TABLE ttResource.
        cResourceList = REPLACE(ENTRY(idx,cListItems),"downtimes.Actual","ResourceList").
        INPUT FROM VALUE(SEARCH(cResourceList)) NO-ECHO.
        IMPORT ^.
        REPEAT:
            IMPORT cResource.
            CREATE ttResource.
            ttResource.resource = cResource.
        END. /* repeat */
        INPUT CLOSE.

        INPUT FROM VALUE(SEARCH(ENTRY(idx,cListItems))) NO-ECHO.
        REPEAT:
            IMPORT tempDowntime.
            tempDowntime.dayID = tempDowntime.dayID MODULO 7.
            IF tempDowntime.dayID EQ 0 THEN
            tempDowntime.dayID = 7.
            IF tempDowntime.resource EQ "<Calendar>" THEN DO:
                FOR EACH ttResource
                    :
                    tempDowntime.resource = ttResource.resource.
                    RUN pCreateTtblDowntime.
                END. /* each ttbljob */
            END.
            ELSE
            IF CAN-FIND(FIRST ttResource
                        WHERE ttResource.resource EQ tempDowntime.resource) THEN
            RUN pCreateTtblDowntime.
        END. /* repeat */
        INPUT CLOSE.
    END. /* do idx */

END PROCEDURE.

PROCEDURE pRemoveUnusedDowntime:
    FOR EACH ttblDowntime
        :
        IF NOT CAN-FIND(FIRST ttJob
                        WHERE ttJob.m-code EQ ttblDowntime.resource) THEN
        DELETE ttblDowntime.
    END. /* each ttbldowntime */

END PROCEDURE.

PROCEDURE pScheduleJob:
    DEFINE INPUT PARAMETER iprRowID AS ROWID     NO-UNDO.

    DEFINE VARIABLE lvStartDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE lvStartTime     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lvEndDate       AS DATE      NO-UNDO.
    DEFINE VARIABLE lvEndTime       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lvStartDateTime AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lvEndDateTime   AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lvTimeSpan      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lvEndDateMR     AS DATE      NO-UNDO.
    DEFINE VARIABLE lvEndTimeMR     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lvMachine       AS CHARACTER NO-UNDO.

    ASSIGN
        lvStartDate = TODAY
        lvStartTime = TIME
        .
    EMPTY TEMP-TABLE ttblJob.
    FOR EACH ttJob USE-INDEX ttJob:
        RUN ttblJobCreate (ttJob.company,ttJob.m-code,ROWID(ttJob)).
        RUN calcEnd (lvStartDate,lvStartTime,ttJob.mr-hr,ttJob.run-hr,
                     OUTPUT lvEndDate,OUTPUT lvEndTime).
        ASSIGN
            lvStartDateTime = numericDateTime(lvStartDate,lvStartTime)
            lvEndDateTime   = numericDateTime(lvEndDate,lvEndTime)
            lvTimeSpan      = timeSpan(lvStartDate,lvStartTime,lvEndDate,lvEndTime)
            .
        RUN firstAvailable (
            ttJob.m-code,
            lvTimeSpan,
            lvStartDateTime,
            lvEndDateTime,
            OUTPUT lvStartDate,
            OUTPUT lvStartTime,
            OUTPUT lvEndDate,
            OUTPUT lvEndTime
            ).
        RUN calcEnd (lvStartDate,lvStartTime,ttJob.mr-hr,0,
            OUTPUT lvEndDateMR,OUTPUT lvEndTimeMR).
        CREATE ttblJob.
        ASSIGN
            ttblJob.m-code        = ttJob.m-code
            ttblJob.job           = ttJob.job-no + '-'
                                  + STRING(ttJob.job-no2) + '.'
                                  + STRING(ttJob.frm)
            ttblJob.frm           = ttJob.frm
            ttblJob.blank-no      = ttJob.blank-no
            ttblJob.pass          = ttJob.pass
            ttblJob.startDate     = lvStartDate
            ttblJob.startTime     = lvStartTime
            ttblJob.endDate       = lvEndDate
            ttblJob.endTime       = lvEndTime
            ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime)
            ttblJob.endDateTime   = lvEndDateTime
            ttblJob.newJob        = YES
            lvStartDate           = lvEndDate
            lvStartTime           = lvEndTime
            .
    END. /* each ttjob */

END PROCEDURE.

PROCEDURE ttblJobCreate:
    DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipMachine AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipRowID   AS ROWID     NO-UNDO.

    DEFINE VARIABLE lvStartDate     AS DATE    NO-UNDO.
    DEFINE VARIABLE lvStartTime     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lvEndDate       AS DATE    NO-UNDO.
    DEFINE VARIABLE lvEndTime       AS INTEGER NO-UNDO.
    DEFINE VARIABLE lvStartDateTime AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lvEndDateTime   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lvDowntimeSpan  AS INTEGER NO-UNDO.
    DEFINE VARIABLE lvTimeSpan      AS INTEGER NO-UNDO.

    lvStartDateTime = numericDateTime(TODAY,TIME).
    FOR EACH mach NO-LOCK
        WHERE mach.company    EQ ipCompany
          AND mach.sch-m-code EQ ipMachine
        :
        FOR EACH bJobMch NO-LOCK
            WHERE bJobMch.company       EQ mach.company
              AND bJobMch.m-code        EQ mach.m-code
              AND bJobMch.run-complete  EQ NO
              AND bJobMch.start-date-su NE ?
              AND ROWID(bJobMch)        NE ipRowId
               BY bJobMch.start-date-su
               BY bJobMch.start-time-su
               BY bJobMch.end-date
               BY bJobMch.end-time
            :
            IF CAN-FIND(FIRST job-hdr
                WHERE job-hdr.company EQ bJobMch.company
                  AND job-hdr.job     EQ bJobMch.job
                  AND job-hdr.opened  EQ NO) THEN NEXT.
            ASSIGN
                lvEndDate     = bJobMch.end-date
                lvEndTime     = fixTime(bJobMch.end-time)
                lvEndDateTime = numericDateTime(lvEndDate,lvEndTime)
                .
            IF lvStartDateTime GT lvEndDateTime THEN NEXT.
            ASSIGN
                lvStartDate = bJobMch.start-date-su
                lvStartTime = fixTime(bJobMch.start-time-su)
                .
            CREATE ttblJob.
            ASSIGN
                ttblJob.m-code        = ipMachine
                ttblJob.job           = bJobMch.job-no + '-'
                                      + STRING(bJobMch.job-no2) + '.'
                                      + STRING(bJobMch.frm)
                ttblJob.frm           = bJobMch.frm
                ttblJob.blank-no      = bJobMch.blank-no
                ttblJob.pass          = bJobMch.pass
                ttblJob.startDate     = lvStartDate
                ttblJob.startTime     = lvStartTime
                ttblJob.endDate       = lvEndDate
                ttblJob.endTime       = lvEndTime
                ttblJob.endDateTime   = lvEndDateTime
                ttblJob.startDateTime = numericDateTime(ttblJob.startDate,ttblJob.startTime)
                ttblJob.newJob        = NO
                .
        END. /* each bjobmch */
    END. /* each mach */

END PROCEDURE.
