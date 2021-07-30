/* subjectID10Defs.i - auto generated 07.06.2021 @  7:12:40 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInventoryClasses AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludeInactiveCust AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeZeroQty AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        cInventoryClasses = DYNAMIC-FUNCTION("fGetDynParamValue","inventoryClasses")
        lIncludeInactiveCust = DYNAMIC-FUNCTION("fGetDynParamValue","includeInactiveCust") EQ "YES"
        lIncludeZeroQty = DYNAMIC-FUNCTION("fGetDynParamValue","includeZeroQty") EQ "YES"
        .
END PROCEDURE.
