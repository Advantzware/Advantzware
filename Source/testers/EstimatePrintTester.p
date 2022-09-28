
/*------------------------------------------------------------------------
    File        : EstimatePrintTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Thu Jan 24 16:45:11 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE VARIABLE ghSession           AS HANDLE.
DEFINE VARIABLE ghEstimateCalcProcs AS HANDLE.
DEFINE VARIABLE giTimer             AS INTEGER   NO-UNDO.

DEFINE VARIABLE gcOutputFile        AS CHARACTER INITIAL "C:\temp\estPrintOut.xpr".
DEFINE VARIABLE gcProfilerFile      AS CHARACTER INITIAL "C:\temp\estCalcProfile.prof".
DEFINE VARIABLE gcCompany           AS CHARACTER INITIAL "001".
DEFINE VARIABLE gcEstimate          AS CHARACTER INITIAL "  104159".
DEFINE VARIABLE glDoJob             AS LOGICAL   INITIAL NO.
DEFINE VARIABLE glDoCalc            AS LOGICAL   INITIAL NO.
DEFINE VARIABLE glPurge             AS LOGICAL   INITIAL YES.
DEFINE VARIABLE glAlt               AS LOGICAL   INITIAL NO.
DEFINE VARIABLE glDoPrompts         AS LOGICAL   INITIAL NO.
DEFINE VARIABLE glShowWeights       AS LOGICAL   INITIAL NO.
DEFINE VARIABLE glPrint             AS LOGICAL   INITIAL YES.

DEFINE VARIABLE cJobID              AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobID2             AS INTEGER   NO-UNDO.
DEFINE VARIABLE iEstCostHeaderID    AS INT64.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).

IF glAlt THEN 
    RUN est\EstimateCalcProcsAlt.p PERSISTENT SET ghEstimateCalcProcs.
ELSE 
    RUN est\EstimateCalcProcs.p PERSISTENT SET ghEstimateCalcProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghEstimateCalcProcs).

RUN spSetSessionParam ("Company",  gcCompany).

FIND FIRST job NO-LOCK 
    WHERE job.company EQ gcCompany
    AND job.est-no EQ gcEstimate
    NO-ERROR.
RUN pOnOffProfiler.
IF AVAILABLE job AND glDoJob THEN 
    ASSIGN 
        cJobID  = job.job-no
        iJobID2 = job.job-no2
        .
IF glDoCalc THEN 
    IF glDoJob THEN 
        IF glDoPrompts THEN
            RUN CalculateJobWithPrompts(gcCompany,gcEstimate, cJobID, iJobID2, 0, glPurge, OUTPUT iEstCostHeaderID).
        ELSE  
            RUN CalculateJob(gcCompany,gcEstimate, cJobID, iJobID2, 0, glPurge, OUTPUT iEstCostHeaderID).
    ELSE 
        IF glDoPrompts THEN
            RUN CalculateEstimateWithPrompts(gcCompany,gcEstimate, glPurge).
        ELSE 
            RUN CalculateEstimate(gcCompany,gcEstimate, glPurge).

RUN pOnOffProfiler.

IF glPurge AND NOT glAlt THEN 
    FIND FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ gcCompany
        AND estCostHeader.estimateNo EQ gcEstimate
        AND estCostHeader.jobID EQ cJobID
        AND estCostHeader.jobID2 EQ iJobID2
        NO-ERROR.
IF AVAILABLE estCostHeader AND glPrint THEN 
    RUN est\EstimatePrint.p (estCostHeader.estCostHeaderID, gcOutputFile).

IF AVAILABLE estCostHeader AND glShowWeights THEN 
    FOR EACH estCostMaterial NO-LOCK 
    WHERE estCostMaterial.estCostHeaderID EQ estCostHeader.estCostHeaderID:
        DISPLAY estCostMaterial.itemID estCostMaterial.weightTotal estCostMaterial.addToWeightNet estCostMaterial.addToWeightTare.
    END.    
/* **********************  Internal Procedures  *********************** */

PROCEDURE pOnOffProfiler :
    /*------------------------------------------------------------------------------
         Purpose:
         Notes:
        ------------------------------------------------------------------------------*/
    
    DEFINE VARIABLE lProfile          AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iProfileStartTime AS INTEGER NO-UNDO.
    
    IF PROFILER:ENABLED THEN 
    DO:
        ASSIGN 
            PROFILER:PROFILING = FALSE                         
            PROFILER:ENABLED   = FALSE
            iProfileStartTime  = TIME                 
            . 
        PROFILER:WRITE-DATA().
    END.
    ELSE 
    DO:
        ASSIGN  
            PROFILER:ENABLED      = TRUE
            PROFILER:DESCRIPTION  = STRING(TODAY,"999999") + "_" + STRING(TIME, "HH:MM:SS")
            PROFILER:FILE-NAME    = gcProfilerFile
            PROFILER:PROFILING    = TRUE
            PROFILER:TRACE-FILTER = "*"
            iProfileStartTime     = TIME 
            .
    END. 
   

END PROCEDURE.
