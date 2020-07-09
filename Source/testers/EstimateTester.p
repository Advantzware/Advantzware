
/*------------------------------------------------------------------------
    File        : EstimateTester.p
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
DEFINE VARIABLE gcEstimate          AS CHARACTER INITIAL "  100386".
DEFINE VARIABLE glDoJob             AS LOGICAL   INITIAL NO.
DEFINE VARIABLE glPurge             AS LOGICAL   INITIAL YES.

DEFINE VARIABLE cJobID AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobID2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
RUN est\EstimateCalcProcsAlt.p PERSISTENT SET ghEstimateCalcProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghEstimateCalcProcs).

FIND FIRST job NO-LOCK 
    WHERE job.company EQ gcCompany
    AND job.est-no EQ gcEstimate
    NO-ERROR.
RUN pOnOffProfiler.
IF AVAILABLE job AND glDoJob THEN 
    ASSIGN 
        cJobID = job.job-no
        iJobID2 = job.job-no2
        .

RUN CalculateEstimate(gcCompany, gcEstimate, OUTPUT lError, OUTPUT cMessage). 
RUN ExportAll("C:\Temp\test.json","JSON").

RUN pOnOffProfiler.


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
