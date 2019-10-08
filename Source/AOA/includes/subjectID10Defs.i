/* subjectID10Defs.i - auto generated 06.14.2019 @  3:14:16 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cInventoryClasses AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludeInactiveCust AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeZeroQty AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSvRecipients AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSvSetAlignment AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSvShowAll AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSvShowReportHeader AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSvShowReportFooter AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSvShowPageHeader AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSvShowPageFooter AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSvShowGroupHeader AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSvShowGroupFooter AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSvShowParameters AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cInventoryClasses = DYNAMIC-FUNCTION("fGetDynParamValue","inventoryClasses")
        lIncludeInactiveCust = DYNAMIC-FUNCTION("fGetDynParamValue","includeInactiveCust") EQ "YES"
        lIncludeZeroQty = DYNAMIC-FUNCTION("fGetDynParamValue","includeZeroQty") EQ "YES"
        cSvRecipients = DYNAMIC-FUNCTION("fGetDynParamValue","svRecipients")
        cSvSetAlignment = DYNAMIC-FUNCTION("fGetDynParamValue","svSetAlignment")
        lSvShowAll = DYNAMIC-FUNCTION("fGetDynParamValue","svShowAll") EQ "YES"
        lSvShowReportHeader = DYNAMIC-FUNCTION("fGetDynParamValue","svShowReportHeader") EQ "YES"
        lSvShowReportFooter = DYNAMIC-FUNCTION("fGetDynParamValue","svShowReportFooter") EQ "YES"
        lSvShowPageHeader = DYNAMIC-FUNCTION("fGetDynParamValue","svShowPageHeader") EQ "YES"
        lSvShowPageFooter = DYNAMIC-FUNCTION("fGetDynParamValue","svShowPageFooter") EQ "YES"
        lSvShowGroupHeader = DYNAMIC-FUNCTION("fGetDynParamValue","svShowGroupHeader") EQ "YES"
        lSvShowGroupFooter = DYNAMIC-FUNCTION("fGetDynParamValue","svShowGroupFooter") EQ "YES"
        lSvShowParameters = DYNAMIC-FUNCTION("fGetDynParamValue","svShowParameters") EQ "YES"
        .
END PROCEDURE.
