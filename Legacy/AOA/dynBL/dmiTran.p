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

PROCEDURE createTtblProdAce:
    DEFINE VARIABLE cChargeCode           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProdAceBlankEmployee AS CHARACTER NO-UNDO INITIAL "ProdAce".
    DEFINE VARIABLE cState                AS CHARACTER NO-UNDO.
    DEFINE VARIABLE idx                   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobMchID             AS INTEGER   NO-UNDO.
    
    DEFINE BUFFER bDMITrans FOR dmiTrans.

    EMPTY TEMP-TABLE ttblProdAce.
    FOR EACH dmiTrans NO-LOCK
        WHERE dmiTrans.startDate GE dtStartTransDate
          AND dmiTrans.startDate LE dtEndTransDate
          AND (dmiTrans.shift    EQ cShift OR cShift EQ "All")
        :
        /* check if valid entry to process */
        IF INDEX(dmiTrans.jobID,"Invalid WO") NE 0 THEN NEXT.
        /* check if valid entry to process */
        IF INDEX(dmiTrans.jobID,"n/f") NE 0 THEN NEXT.
        /* make sure job is in long format */
        IF NUM-ENTRIES(ENTRY(2,dmiTrans.jobID),".") LT 2 THEN NEXT.
        FIND FIRST mach NO-LOCK
             WHERE mach.spare-int-2 EQ dmiTrans.dmiID
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
        CREATE ttblProdAce.
        ASSIGN
            ttblProdAce.prodAceResource      = mach.m-code
            ttblProdAce.prodAceDMIID         = dmiTrans.dmiID
            ttblProdAce.prodAceJob           = dmiTrans.jobID
            ttblProdAce.prodAceItem          = dmiTrans.productID
            ttblProdAce.prodAceSeq           = dmiTrans.seq
            ttblProdAce.prodAceShift         = dmiTrans.shift
            ttblProdAce.prodAceShiftDate     = dmiTrans.shiftDate
            ttblProdAce.prodAceStartDate     = dmiTrans.startDate
            ttblProdAce.prodAceStartTime     = dmiTrans.startTime
            ttblProdAce.prodAceTranRunQty    = dmiTrans.tranRunQty
            ttblProdAce.prodAceTranRejectQty = dmiTrans.tranRejectQty
            ttblProdAce.prodAceQtyDue        = dmiTrans.qtyDue
            ttblProdAce.prodAceState         = cState
            ttblProdAce.prodAceChargeCode    = cChargeCode
            ttblProdAce.prodAceDuration      = dmiTrans.downTime + dmiTrans.runTime
            ttblProdAce.prodAceRunComplete   = ttblProdAce.prodAceState EQ "RUN" AND
                                               CAN-FIND(FIRST dmiJobStatus
                                                        WHERE dmiJobStatus.dmiID       EQ ttblProdAce.prodAceDMIID
                                                          AND dmiJobStatus.jobID       EQ ttblProdAce.prodAceJob
                                                          AND dmiJobStatus.productID   EQ ttblProdAce.prodAceItem
                                                          AND dmiJobStatus.runID       EQ ttblProdAce.prodAceSeq
                                                          AND dmiJobStatus.jobStatus   EQ "C")
            .
        DO idx = 1 TO NUM-ENTRIES(dmiTrans.operator):
            ttblProdAce.prodAceOperator[idx] = ENTRY(idx,dmiTrans.operator).
        END. /* do idx */
        DO TRANSACTION:
            FIND FIRST bDMITrans EXCLUSIVE-LOCK
                 WHERE ROWID(bDMITrans) EQ ROWID(dmiTrans).
            bDMITrans.posted = YES.
        END. /* do trans */
    END. /* each dmitrans */
END PROCEDURE.

PROCEDURE pBusinessLogic:
    RUN createTtblProdAce.
    RUN prodAceSummary.
END PROCEDURE.
