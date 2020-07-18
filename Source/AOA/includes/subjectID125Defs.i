/* subjectID125Defs.i - auto generated 07.13.2020 @  7:25:38 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE cCustomerStatus AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE cShipToStatus AS Character NO-UNDO.
DEFINE VARIABLE cFGItemStatus AS Character NO-UNDO.
DEFINE VARIABLE lAllFGItems AS Logical NO-UNDO.
DEFINE VARIABLE cStartFGItem AS Character NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItem AS Character NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS Character NO-UNDO.
DEFINE VARIABLE cPrepStatus AS Character NO-UNDO.
DEFINE VARIABLE lAllPrepNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartPrepNo AS Character NO-UNDO.
DEFINE VARIABLE cStartPrepName AS Character NO-UNDO.
DEFINE VARIABLE cEndPrepNo AS Character NO-UNDO.
DEFINE VARIABLE cEndPrepName AS Character NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cCustomerStatus = DYNAMIC-FUNCTION("fGetDynParamValue","CustomerStatus")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        cShipToStatus = DYNAMIC-FUNCTION("fGetDynParamValue","ShipToStatus")
        cFGItemStatus = DYNAMIC-FUNCTION("fGetDynParamValue","FGItemStatus")
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        cPrepStatus = DYNAMIC-FUNCTION("fGetDynParamValue","PrepStatus")
        lAllPrepNo = DYNAMIC-FUNCTION("fGetDynParamValue","allPrepNo") EQ "YES"
        cStartPrepNo = DYNAMIC-FUNCTION("fGetDynParamValue","startPrepNo")
        cStartPrepName = DYNAMIC-FUNCTION("fGetDynParamValue","startPrepName")
        cEndPrepNo = DYNAMIC-FUNCTION("fGetDynParamValue","endPrepNo")
        cEndPrepName = DYNAMIC-FUNCTION("fGetDynParamValue","endPrepName")
        .
END PROCEDURE.
