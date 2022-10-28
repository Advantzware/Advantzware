/*------------------------------------------------------------------------
  File:         dmiJobCodeAnalysis.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 6.27.2022
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttDMIJobCodeAnalysis
DEFINE TEMP-TABLE ttDMIJobCodeAnalysis NO-UNDO LIKE dmiTrans
    FIELD totTime AS DECIMAL   FORMAT ">,>>>,>>9.99" LABEL "Total Time"
    FIELD itemNo  AS CHARACTER FORMAT "x(15)"        LABEL "Product"
    FIELD code LIKE job-code.code
    FIELD cat  LIKE job-code.cat
    FIELD dscr LIKE job-code.dscr
    .

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 208
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE hDynCalcField   AS HANDLE    NO-UNDO.
    DEFINE VARIABLE iJobMachID      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iShiftStartTime AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iShiftEndTime   AS INTEGER   NO-UNDO INITIAL 86400000.

    RUN AOA/spDynCalcField.p PERSISTENT SET hDynCalcField.
    SESSION:ADD-SUPER-PROCEDURE (hDynCalcField).

    RUN calcShiftStartTime (
        cCompany,
        lUseTimes,
        cStartShift,
        cStartTime,
        OUTPUT iShiftStartTime
        ).
    RUN calcShiftEndTime (
        cCompany,
        lUseTimes,
        cStartShift,
        cEndShift,
        cEndTime,
        OUTPUT iShiftEndTime
        ).
    IF dtStartTransDate EQ dtEndTransDate  AND
       iShiftEndTime    LT iShiftStartTime THEN
    iShiftEndTime = 86400000.

    FOR EACH dmiTrans NO-LOCK
        WHERE dmiTrans.dmiID GE iStartDMIID
          AND dmiTrans.dmiID LE iEndDMIID
          AND dmiTrans.shift GE cStartShift
          AND dmiTrans.shift LE cEndShift
          AND DATETIME(dmiTrans.startDate, dmiTrans.startTime * 1000) GE DATETIME(dtStartTransDate, iShiftStartTime)
          AND DATETIME(dmiTrans.tranDate,  dmiTrans.tranTime  * 1000) LE DATETIME(dtEndTransDate,   iShiftEndTime)
          AND (cPostedType   EQ "All" OR dmiTrans.posted     EQ (cPostedType EQ "yes"))
          AND (cDMIStateType EQ "All" OR dmiTrans.transState EQ cDMIStateType)
        :
        IF dmiTrans.transState EQ "DT" THEN
        FIND FIRST job-code NO-LOCK
             WHERE job-code.dmiID EQ dmiTrans.jobCodeDMIID
             NO-ERROR.
        ELSE IF dmiTrans.transState EQ "RUN" THEN
        FIND FIRST job-code NO-LOCK
             WHERE job-code.code EQ "RUN"
               AND job-code.cat  EQ "RUN"
             NO-ERROR.
        FIND FIRST ttDMIJobCodeAnalysis
             WHERE ttDMIJobCodeAnalysis.dmiID        EQ dmiTrans.dmiID
               AND ttDMIJobCodeAnalysis.jobCodeDMIID EQ dmiTrans.jobCodeDMIID
             NO-ERROR.
        IF NOT AVAILABLE ttDMIJobCodeAnalysis THEN DO:
            CREATE ttDMIJobCodeAnalysis.
            ASSIGN
                ttDMIJobCodeAnalysis.dmiID        = dmiTrans.dmiID
                ttDMIJobCodeAnalysis.jobCodeDMIID = dmiTrans.jobCodeDMIID
                .
            IF AVAILABLE job-code THEN
            ASSIGN
                ttDMIJobCodeAnalysis.code = job-code.code
                ttDMIJobCodeAnalysis.cat  = job-code.cat
                ttDMIJobCodeAnalysis.dscr = job-code.dscr
                .
        END. // if not can-find
        ttDMIJobCodeAnalysis.totTime = ttDMIJobCodeAnalysis.totTime
                                     + DYNAMIC-FUNCTION("fTimeSpan",
                                       dmiTrans.startDate,
                                       dmiTrans.startTime,
                                       dmiTrans.tranDate,
                                       dmiTrans.tranTime
                                     ).
    END. // each dmitran
    FOR EACH ttDMIJobCodeAnalysis:
        ttDMIJobCodeAnalysis.totTime = ttDMIJobCodeAnalysis.totTime / 3600.
    END. // each ttDMIJobCodeAnalysis

END PROCEDURE.
