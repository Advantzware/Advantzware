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

/* **********************  Internal Procedures  *********************** */

{system/fSuperRunning.i}
&Scoped-define dmiTran
{AOA/dynBL/dmiTran.i}

PROCEDURE pBusinessLogic:
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
    DEFINE INPUT PARAMETER ipcResource   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiDMIID      AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcProductID  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiSeq        AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcShift      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipdtDate      AS DATE      NO-UNDO.
    DEFINE INPUT PARAMETER ipiStartTime  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiEndTime    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiRunQty     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiRejectQty  AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiQtyDue     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcState      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcChargeCode AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcOperator   AS CHARACTER NO-UNDO.

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
        ttblProdAce.prodAceRunComplete   = ttblProdAce.prodAceState EQ "RUN" AND
                                           CAN-FIND(FIRST dmiJobStatus
                                                    WHERE dmiJobStatus.dmiID     EQ ttblProdAce.prodAceDMIID
                                                      AND dmiJobStatus.jobID     EQ ttblProdAce.prodAceJob
                                                      AND dmiJobStatus.productID EQ ttblProdAce.prodAceItem
                                                      AND dmiJobStatus.runID     EQ ttblProdAce.prodAceSeq
                                                      AND dmiJobStatus.jobStatus EQ "C")
        .
    DO idx = 1 TO NUM-ENTRIES(ipcOperator):
        ttblProdAce.prodAceOperator[idx] = ENTRY(idx,ipcOperator).
    END. /* do idx */
END PROCEDURE.

{custom/shftproc.i}
