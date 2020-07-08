/* subjectID130Defs.i -  */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.

DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllJobNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartJobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndJobNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE iStartJobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndJobNo2 AS INTEGER NO-UNDO.
DEFINE VARIABLE lAllEstimate AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartEstimate AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndEstimate AS CHARACTER NO-UNDO.
DEFINE VARIABLE iJobStatus AS INTEGER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")        
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")       
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
	    lAllJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","allJobNo") EQ "YES"
        cStartJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","startJobNo")       
        cEndJobNo = DYNAMIC-FUNCTION("fGetDynParamValue","endJobNo")
	    iStartJobNo2 = DYNAMIC-FUNCTION("fGetDynParamValue","startJobNo2")       
        iEndJobNo2 = DYNAMIC-FUNCTION("fGetDynParamValue","endJobNo2")
        lAllEstimate = DYNAMIC-FUNCTION("fGetDynParamValue","allEstimateNo") EQ "YES"
        cStartEstimate = DYNAMIC-FUNCTION("fGetDynParamValue","startEstimateNo")       
        cEndEstimate = DYNAMIC-FUNCTION("fGetDynParamValue","endEstimateNo")  
        iJobStatus = DYNAMIC-FUNCTION("fGetDynParamValue","jobStatus")      
        .
END PROCEDURE.
