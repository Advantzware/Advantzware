/* subjectID125Defs.i - auto generated 06.04.2020 @  9:19:42 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE lAllFGItem AS Logical NO-UNDO.
DEFINE VARIABLE cStartFGItem AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItem AS Character NO-UNDO.
DEFINE VARIABLE cShipToStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFGItemStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllPrepNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartPrepNo AS Character NO-UNDO.
DEFINE VARIABLE cEndPrepNo AS Character NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllCustNo= DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo= DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        lAllFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cShipToStatus = (DYNAMIC-FUNCTION("fGetDynParamValue","ShipToStatus"))
        cFGItemStatus = (DYNAMIC-FUNCTION("fGetDynParamValue","fgItemStatus"))
	    lAllPrepNo= DYNAMIC-FUNCTION("fGetDynParamValue","allPrepNo") EQ "YES"
        cStartPrepNo= DYNAMIC-FUNCTION("fGetDynParamValue","startPrepNo")
        cEndPrepNo = DYNAMIC-FUNCTION("fGetDynParamValue","endPrepNo")
        .
END PROCEDURE.
