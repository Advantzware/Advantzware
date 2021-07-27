/* subjectID125Defs.i - auto generated 07.06.2021 @  7:17:36 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustomerStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShipToStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFGItemStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrepStatus AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllPrepNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartPrepNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartPrepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndPrepNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndPrepName AS CHARACTER NO-UNDO.

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
