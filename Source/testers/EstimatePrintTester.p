
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
DEFINE VARIABLE gcEstimate          AS CHARACTER INITIAL "   12236".
//DEFINE VARIABLE gcEstimate     AS CHARACTER INITIAL "   14058".
//DEFINE VARIABLE gcEstimate     AS CHARACTER INITIAL "   13675".

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
RUN est\EstimateCalcProcs.p PERSISTENT SET ghEstimateCalcProcs.
SESSION:ADD-SUPER-PROCEDURE (ghEstimateCalcProcs).
RUN pOnOffProfiler.
RUN CalculateEstimate(gcCompany,gcEstimate, YES).
RUN pOnOffProfiler.
FIND FIRST estCostHeader NO-LOCK
    WHERE estCostHeader.company EQ gcCompany
    AND estCostHeader.estimateNo EQ gcEstimate
//    AND estCostHeader.quantityMaster EQ 10000
    NO-ERROR.
RUN est\EstimatePrint.p (estCostHeader.estCostHeaderID, gcOutputFile, "By Form with Summary First Mult Qty","Calibri").
FOR EACH probe EXCLUSIVE-LOCK
    WHERE probe.company EQ gcCompany
    AND probe.est-no EQ gcEstimate:
    DISPLAY probe.est-qty probe.eqty.
    DELETE probe.
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
