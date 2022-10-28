/*------------------------------------------------------------------------
  File:         dmiTran.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 9.8.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttblProdAce
{AOA/tempTable/ttblProdAce.i}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 44
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

DEFINE VARIABLE lvProdAceDat           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceDir           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceType          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvEmpLogin             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvProdAceBlankEmployee AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvImportDir            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvResourceList         AS CHARACTER NO-UNDO.

DEFINE STREAM sProdAce.
DEFINE STREAM sProcessed.
DEFINE STREAM sError.
DEFINE STREAM sHold.

/* **********************  Internal Procedures  *********************** */

{system/fSuperRunning.i}
&Scoped-define dmiTran
{AOA/dynBL/dmiTran.i}

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cArchive         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cChargeCode      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFile            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cHoldFile        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobNo           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProcessed       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceBarScan  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceBlank    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceData     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceForm     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceJob      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceOperator AS CHARACTER NO-UNDO EXTENT 10.
    DEFINE VARIABLE cProdAcePass     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cResource        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cState           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cTemp            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDate      AS DATE      NO-UNDO.
    DEFINE VARIABLE idx              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iProdAceDMIID    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lProdAceBarScan  AS LOGICAL   NO-UNDO.

    RUN sys/ref/nk1look.p (
        cCompany,"ProdAceBarScan","L",NO,NO,"","",
         OUTPUT cProdAceBarScan,OUTPUT lProdAceBarScan
        ).
    ASSIGN
        lProdAceBarScan = lProdAceBarScan AND cProdAceBarScan EQ "YES"
        lvProdAceDat    = SEARCH("schedule\data\" + cSBID + "\ProdAce.dat")
        .
    IF lvProdAceDat NE ? THEN DO:
        RUN getProdAceDatValues.
        lvProdAceDat = SEARCH(lvProdAceDir + "\adware.dat").
        IF lvProdAceDat NE ? THEN DO:
            ASSIGN
                cFile      = lvProdAceDat
                cTemp      = REPLACE(cFile,'.dat','.tmp')
                cHoldFile  = REPLACE(cFile,'.dat','.hold')
                cProcessed = lvProdAceDir
                           + '/processed/adware.'
                           + STRING(YEAR(TODAY),'9999')
                           + STRING(MONTH(TODAY),'99')
                           + STRING(DAY(TODAY),'99')
                           + '.' + STRING(TIME,'99999')
                           + '.dat'
                cArchive   = REPLACE(cProcessed,'processed','archive')
                .        
            /* move transactions to tmp file */
            OS-COPY VALUE(cFile) VALUE(cArchive).
            OS-RENAME VALUE(cFile) VALUE(cTemp).
            /* append hold records */
            OS-APPEND VALUE(cHoldFile) VALUE(cTemp).
            EMPTY TEMP-TABLE ttblProdAce.
            INPUT STREAM sProdAce FROM VALUE(cTemp).
            OUTPUT STREAM sHold TO VALUE(cHoldFile).
            OUTPUT STREAM sProcessed TO VALUE(cProcessed).
            REPEAT:
                IMPORT STREAM sProdAce UNFORMATTED cProdAceData.
                cProdAceData = REPLACE(cProdAceData,"'","").
                IF NUM-ENTRIES(cProdAceData) LT 2 THEN NEXT.
                IF ENTRY(2,cProdAceData) EQ 'n/f' THEN NEXT.
                dtStartDate = DATE(ENTRY(10,cProdAceData)).
                IF ((cShift NE 'All' AND
                     cShift NE ENTRY(6,cProdAceData)) OR
                    dtStartDate LT dtStartTransDate OR
                    dtStartDate GT dtEndTransDate) THEN DO:
                    PUT STREAM sHold UNFORMATTED cProdAceData SKIP.
                    NEXT.
                END. /* if prodAceshift ne */
                ASSIGN
                    iProdAceDMIID = INTEGER(ENTRY(1,cProdAceData))
                    cProdAceJob   = ENTRY(2,cProdAceData)
                    .
                IF lProdAceBarScan THEN DO:
                    cJobNo = cProdAceJob.
                    IF NUM-ENTRIES(cJobNo,".") GT 1 THEN DO:
                        iJobMchID = INTEGER(ENTRY(2,cJobNo,'.')).
                        FIND FIRST job-mch NO-LOCK
                             WHERE job-mch.job-mchID EQ iJobMchID
                             NO-ERROR.
                        IF AVAILABLE job-mch THEN
                        ASSIGN
                            cProdAceJob   = LEFT-TRIM(job-mch.job-no) + '-'
                                          + STRING(job-mch.job-no2) + '.'
                                          + STRING(job-mch.frm)
                            cProdAceForm  = STRING(job-mch.frm)
                            cProdAceBlank = STRING(job-mch.blank-no)
                            cProdAcePass  = STRING(job-mch.pass)
                            .
                    END. // if num-entries gt 1
                END. /* if prod ace bar scanning */
                ELSE
                ASSIGN
                    cProdAceForm  = ENTRY(2,cProdAceJob,'.')
                    cProdAceBlank = ENTRY(3,cProdAceJob,'.')
                    cProdAcePass  = ENTRY(4,cProdAceJob,'.')
                    cResource     = ENTRY(5,cProdAceJob,'.')
                    cProdAceJob   = ENTRY(1,cProdAceJob,'.') + '.'
                                  + cProdAceForm
                                  .
                ASSIGN
                    cState           = ENTRY(15,cProdAceData)
                    cChargeCode      = cState
                    cProdAceOperator = ''
                    .
                DO idx = 19 TO NUM-ENTRIES(cProdAceData):
                    cProdAceOperator[idx - 18] = IF ENTRY(idx,cProdAceData) EQ '' THEN lvProdAceBlankEmployee
                                                 ELSE ENTRY(idx,cProdAceData).
                    IF ENTRY(idx,cProdAceData) EQ '' THEN LEAVE.
                END. /* do idx */
                /* get charge code for non run and mr */    
                IF cState EQ 'DT' AND INTEGER(ENTRY(16,cProdAceData)) NE 0 THEN DO: 
                    FIND FIRST job-code NO-LOCK 
                         WHERE job-code.dmiID EQ INTEGER(ENTRY(16,cProdAceData))
                         NO-ERROR.
                    IF AVAILABLE job-code THEN
                    ASSIGN 
                        cState      = job-code.cat
                        cChargeCode = job-code.code
                        .
                END. /* if dt and dt reason given */
                IF NOT AVAILABLE job-mch THEN
                FIND FIRST mach NO-LOCK
                     WHERE mach.company     EQ cCompany
                       AND mach.spare-int-2 EQ iProdAceDMIID
                     NO-ERROR.
                CREATE ttblProdAce.
                ASSIGN
                    ttblProdAce.prodAceResource      = IF AVAILABLE job-mch THEN job-mch.m-code
                                                  ELSE IF AVAILABLE mach    THEN mach.m-code
                                                  ELSE "Missing"
                    ttblProdAce.prodAceDMIID         = iProdAceDMIID
                    ttblProdAce.prodAceJob           = cProdAceJob
                    ttblProdAce.prodAceItem          = ENTRY(3,cProdAceData)
                    ttblProdAce.prodAceSeq           = INTEGER(ENTRY(5,cProdAceData))
                    ttblProdAce.prodAceShift         = ENTRY(6,cProdAceData)
                    ttblProdAce.prodAceShiftDate     = DATE(ENTRY(7,cProdAceData))
                    ttblProdAce.prodAceStartDate     = DATE(ENTRY(10,cProdAceData))
                    ttblProdAce.prodAceStartTime     = INTEGER(ENTRY(11,cProdAceData))
                    ttblProdAce.prodAceEndDate       = DATE(ENTRY(8,cProdAceData))
                    ttblProdAce.prodAceEndTime       = INTEGER(ENTRY(9,cProdAceData))
                    ttblProdAce.prodAceTranRunQty    = INTEGER(ENTRY(12,cProdAceData))
                    ttblProdAce.prodAceTranRejectQty = INTEGER(ENTRY(13,cProdAceData))
                    ttblProdAce.prodAceQtyDue        = INTEGER(ENTRY(14,cProdAceData))
                    ttblProdAce.prodAceState         = cState
                    ttblProdAce.prodAceChargeCode    = cChargeCode
                    ttblProdAce.prodAceOperator      = cProdAceOperator
                    ttblProdAce.prodAceDuration      = INTEGER(ENTRY(17,cProdAceData)) * 60
                                                     + INTEGER(ENTRY(18,cProdAceData)) * 60
                    ttblProdAce.prodAceRunComplete   = ENTRY(19,cProdAceData) EQ "C"
                    ttblProdAce.prodAceData          = cProdAceData
                    .
                PUT STREAM sProcessed UNFORMATTED cProdAceData SKIP.
                IF CAN-FIND(FIRST dmiTrans
                            WHERE dmiTrans.dmiID     EQ ttblProdAce.prodAceDMIID
                              AND dmiTrans.jobID     EQ ENTRY(2,cProdAceData)
                              AND dmiTrans.productID EQ ttblProdAce.prodAceItem
                              AND dmiTrans.shift     EQ ttblProdAce.prodAceShift
                              AND dmiTrans.shiftDate EQ ttblProdAce.prodAceShiftDate
                              AND dmiTrans.startDate EQ ttblProdAce.prodAceStartDate
                              AND dmiTrans.startTime EQ ttblProdAce.prodAceStartTime
                    ) THEN NEXT.
                DO TRANSACTION:
                    CREATE dmiTrans.
                    ASSIGN
                        dmiTrans.dmiID          = ttblProdAce.prodAceDMIID
                        dmiTrans.jobID          = ENTRY(2,cProdAceData)
                        dmiTrans.productID      = ttblProdAce.prodAceItem
                        dmiTrans.seq            = ttblProdAce.prodAceSeq
                        dmiTrans.shift          = ttblProdAce.prodAceShift
                        dmiTrans.shiftDate      = ttblProdAce.prodAceShiftDate
                        dmiTrans.startDate      = ttblProdAce.prodAceStartDate
                        dmiTrans.startTime      = ttblProdAce.prodAceStartTime
                        dmiTrans.tranDate       = ttblProdAce.prodAceEndDate
                        dmiTrans.tranTime       = ttblProdAce.prodAceEndTime
                        dmiTrans.tranRunQty     = ttblProdAce.prodAceTranRunQty
                        dmiTrans.tranRejectQty  = ttblProdAce.prodAceTranRejectQty
                        dmiTrans.qtyDue         = ttblProdAce.prodAceQtyDue
                        dmiTrans.transState     = ttblProdAce.prodAceState
                        dmiTrans.jobCodeDMIID   = INTEGER(ENTRY(16,cProdAceData))
                        dmiTrans.downTime       = INTEGER(ENTRY(17,cProdAceData))
                        dmiTrans.runTime        = INTEGER(ENTRY(18,cProdAceData))
                        dmiTrans.jobStatus      = ENTRY(19,cProdAceData)
                        dmiTrans.operator       = ENTRY(20,cProdAceData)
                        dmiTrans.posted         = NO
                        ttblProdAce.dmiTransAdd = YES
                        .
                END. /* do trans */
                RELEASE job-mch.
                RELEASE mach.
            END. /* repeat */
            OUTPUT STREAM sHold CLOSE.
            OUTPUT STREAM sProcessed CLOSE.
            INPUT STREAM sProdAce CLOSE.
        END. /* if lvProdAceDat */
    END. /* if lvProdAceDat */
    RUN pGetDMITrans.
    RUN prodAceDetail.
    // RUN prodAceSummary.
    RELEASE machtran.
    RELEASE emplogin.
    RELEASE machemp.
END PROCEDURE.

PROCEDURE pGetDMITrans:
    DEFINE VARIABLE cCalcShift            AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cChargeCode           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cMissingShift         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceBlankEmployee AS CHARACTER NO-UNDO INITIAL "ProdAce".
    DEFINE VARIABLE cShiftValue           AS CHARACTER NO-UNDO EXTENT 2.
    DEFINE VARIABLE cState                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cType                 AS CHARACTER NO-UNDO EXTENT 2 INIT ["Start","End"].
    DEFINE VARIABLE dtDate                AS DATE      NO-UNDO.
    DEFINE VARIABLE dtEndDate             AS DATE      NO-UNDO.
    DEFINE VARIABLE dtStartDate           AS DATE      NO-UNDO.
    DEFINE VARIABLE idx                   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobMchID             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iEndTime              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iETime                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iStartTime            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iSTime                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iTime                 AS INTEGER   NO-UNDO EXTENT 2.
    
    DEFINE BUFFER bDMITrans FOR dmiTrans.

    EMPTY TEMP-TABLE ttblProdAce.
    FOR EACH dmiTrans NO-LOCK
        WHERE ((dmiTrans.startDate GE dtStartTransDate
          AND   dmiTrans.startDate LE dtEndTransDate)
           OR  (dmiTrans.tranDate  GE dtStartTransDate
          AND   dmiTrans.tranDate  LE dtEndTransDate))
          AND  (dmiTrans.shift     EQ cShift OR cShift EQ "All")
          AND   dmiTrans.posted    EQ NO
           BY dmiTrans.dmiID
           BY dmiTrans.startDate
           BY dmiTrans.startTime
        :
        idx = idx + 1.
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, idx, ?).
        /* check if valid entry to process */
        IF INDEX(dmiTrans.jobID,"Invalid WO") NE 0 THEN NEXT.
        /* check if valid entry to process */
        IF INDEX(dmiTrans.jobID,"n/f") NE 0 THEN NEXT.
        /* make sure job is in long format */
        IF NUM-ENTRIES(dmiTrans.jobID,".") LT 2 THEN NEXT.
        /* filter zero records */
        IF dmiTrans.startDate     EQ dmiTrans.tranDate AND
           dmiTrans.startTime     EQ dmiTrans.tranTime AND
           dmiTrans.tranRunQty    EQ 0 AND
           dmiTrans.tranRejectQty EQ 0 THEN NEXT.
        FIND FIRST mach NO-LOCK
             WHERE mach.company     EQ cCompany
               AND mach.spare-int-2 EQ dmiTrans.dmiID
             NO-ERROR.
        IF NOT AVAILABLE mach THEN NEXT.
        /* get charge code for non run and mr */
        ASSIGN
            cState      = dmiTrans.transState
            cChargeCode = cState
            . 
        IF dmiTrans.transState EQ 'DT' AND dmiTrans.jobCodeDMIID NE 0 THEN DO: 
            FIND FIRST job-code NO-LOCK 
                 WHERE job-code.dmiID EQ dmiTrans.jobCodeDMIID
                 NO-ERROR.
            IF AVAILABLE job-code THEN
            ASSIGN 
                cState      = job-code.cat
                cChargeCode = job-code.code
                .
        END. /* if dt and dt reason given */
        ASSIGN
            dtStartDate = dmiTrans.startDate
            dtEndDate   = dmiTrans.tranDate
            iStartTime  = dmiTrans.startTime
            iEndTime    = dmiTrans.tranTime
            .
        DO dtDate = dtStartDate TO dtEndDate:
            ASSIGN
                iTime[1]      = iStartTime
                iTime[2]      = iEndTime
                cMissingShift = ""
                .
            IF dtDate NE dtStartDate THEN
                iTime[1] = 0.
            IF dtDate NE dtEndDate THEN
                iTime[2] = 86399.
        
            RUN Get-Shift (mach.company, mach.m-code, iTime[1], cType[1], OUTPUT cShiftValue[1]).
            RUN Get-Shift (mach.company, mach.m-code, iTime[2], cType[2], OUTPUT cShiftValue[2]).
            IF cShiftValue[1] NE cShiftValue[2] THEN DO:
                RUN Shift-Data (mach.company, mach.m-code, cShiftValue[1], OUTPUT iSTime, OUTPUT iETime).
                RUN pCreateTtblProdAce (
                    mach.m-code,
                    dmiTrans.dmiID,
                    dmiTrans.jobID,
                    dmiTrans.productID,
                    dmiTrans.seq,
                    cShiftValue[1],
                    dtDate,
                    iTime[1],
                    iETime,
                    0,
                    0,
                    0,
                    cState,
                    cChargeCode,
                    dmiTrans.jobStatus EQ "C",
                    dmiTrans.operator
                    ).
                RUN Shift-Data (mach.company, mach.m-code, cShiftValue[2], OUTPUT iSTime, OUTPUT iETime).
                RUN pCreateTtblProdAce (
                    mach.m-code,
                    dmiTrans.dmiID,
                    dmiTrans.jobID,
                    dmiTrans.productID,
                    dmiTrans.seq,
                    cShiftValue[2],
                    dtDate,
                    iSTime,
                    iTime[2],
                    IF dtDate EQ dtEndDate THEN dmiTrans.tranRunQty ELSE 0,
                    IF dtDate EQ dtEndDate THEN dmiTrans.tranRejectQty ELSE 0,
                    IF dtDate EQ dtEndDate THEN dmiTrans.qtyDue ELSE 0,
                    cState,
                    cChargeCode,
                    dmiTrans.jobStatus EQ "C",
                    dmiTrans.operator
                    ).
                RUN Missing-Shift(mach.company, mach.m-code, cShiftValue[1], cShiftValue[2], OUTPUT cMissingShift).
                IF cMissingShift NE "" THEN DO:
                    RUN Shift-Data (mach.company, mach.m-code, cMissingShift, OUTPUT iSTime, OUTPUT iETime).
                    RUN pCreateTtblProdAce (
                        mach.m-code,
                        dmiTrans.dmiID,
                        dmiTrans.jobID,
                        dmiTrans.productID,
                        dmiTrans.seq,
                        cMissingShift,
                        dtDate,
                        iSTime,
                        iETime,
                        0,
                        0,
                        0,
                        cState,
                        cChargeCode,
                        dmiTrans.jobStatus EQ "C",
                        dmiTrans.operator
                        ).
                END. /* if missing shift */
            END. /* if span multi shifts */
            ELSE DO: /* single shift */
                /* check if shift spans multiple shifts */
                DO WHILE TRUE:
                    RUN Shift-Data (mach.company, mach.m-code, cShiftValue[1], OUTPUT iSTime, OUTPUT iETime).
                    /* shift spans midnight, if before midnight then done */
                    IF iSTime GT iETime AND 86400 GE iTime[2] THEN LEAVE.
                    /* if shift end time ge end time then done */
                    IF iETime GE iTime[2] THEN LEAVE.
                    RUN pCreateTtblProdAce (
                        mach.m-code,
                        dmiTrans.dmiID,
                        dmiTrans.jobID,
                        dmiTrans.productID,
                        dmiTrans.seq,
                        cShiftValue[1],
                        dtDate,
                        iTime[1],
                        iETime,
                        0,
                        0,
                        0,
                        cState,
                        cChargeCode,
                        dmiTrans.jobStatus EQ "C",
                        dmiTrans.operator
                        ).
                    /* set the start time to shift end time plus 1 second */
                    iTime[1] = iETime + 1.
                    /* check if crossing over midnight */
                    IF iTime[1] GE 86400 THEN iTime[1] = 0.
                    RUN Get-Shift (mach.company, mach.m-code, iTime[1], cType[1], OUTPUT cShiftValue[1]).
                    /* if start and end shift the same, we've come full circle, done */
                    IF cShiftValue[1] EQ cShiftValue[2] THEN LEAVE.            
                END. /* do while */
                RUN pCreateTtblProdAce (
                    mach.m-code,
                    dmiTrans.dmiID,
                    dmiTrans.jobID,
                    dmiTrans.productID,
                    dmiTrans.seq,
                    cShiftValue[1],
                    dtDate,
                    iTime[1],
                    iTime[2],
                    IF dtDate EQ dtEndDate THEN dmiTrans.tranRunQty ELSE 0,
                    IF dtDate EQ dtEndDate THEN dmiTrans.tranRejectQty ELSE 0,
                    IF dtDate EQ dtEndDate THEN dmiTrans.qtyDue ELSE 0,
                    cState,
                    cChargeCode,                    
                    dmiTrans.jobStatus EQ "C",
                    dmiTrans.operator
                    ).
            END. /* else */
        END. /* do dtdate */
        DO TRANSACTION:
            FIND FIRST bDMITrans EXCLUSIVE-LOCK
                 WHERE ROWID(bDMITrans) EQ ROWID(dmiTrans).
            bDMITrans.posted = YES.
        END. /* do trans */
    END. /* each dmitrans */

END PROCEDURE.

PROCEDURE pCreateTtblProdAce:
    DEFINE INPUT PARAMETER ipcResource    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiDMIID       AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProductID   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSeq         AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcShift       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDate       AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiStartTime   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiEndTime     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiRunQty      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiRejectQty   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiQtyDue      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcState       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcChargeCode  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplRunComplete AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperator    AS CHARACTER NO-UNDO.

    DEFINE VARIABLE idx AS INTEGER NO-UNDO.

    CREATE ttblProdAce.
    ASSIGN
        ttblProdAce.prodAceResource      = ipcResource
        ttblProdAce.prodAceDMIID         = ipiDMIID
        ttblProdAce.prodAceJob           = ipcJobID
        ttblProdAce.prodAceItem          = ipcProductID
        ttblProdAce.prodAceSeq           = ipiSeq
        ttblProdAce.prodAceShift         = ipcShift
        ttblProdAce.prodAceShiftDate     = ipdtDate
        ttblProdAce.prodAceStartDate     = ipdtDate
        ttblProdAce.prodAceStartTime     = ipiStartTime
        ttblProdAce.prodAceEndDate       = ipdtDate
        ttblProdAce.prodAceEndTime       = ipiEndTime
        ttblProdAce.prodAceDuration      = ttblProdAce.prodAceEndTime
                                         + ((ttblProdAce.prodAceEndDate - ttblProdAce.prodAceStartDate)
                                         * 86400 - ttblProdAce.prodAceStartTime)
        ttblProdAce.prodAceTranRunQty    = ipiRunQty
        ttblProdAce.prodAceTranRejectQty = ipiRejectQty
        ttblProdAce.prodAceQtyDue        = ipiQtyDue
        ttblProdAce.prodAceState         = ipcState
        ttblProdAce.prodAceChargeCode    = ipcChargeCode
        ttblProdAce.prodAceRunComplete   = iplRunComplete
        .
    DO idx = 1 TO NUM-ENTRIES(ipcOperator):
        ttblProdAce.prodAceOperator[idx] = ENTRY(idx,ipcOperator).
    END. /* do idx */
END PROCEDURE.

{custom/shftproc.i}
