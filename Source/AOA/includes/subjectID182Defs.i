/* subjectID154Defs.i - auto generated 09.21.2021 @ 10:47:03 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lAllPO AS LOGICAL NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartPONumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndPONumber AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartPoDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndPoDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSvRunSync AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSvRecipients AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSvAutoClose AS LOGICAL NO-UNDO.
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
        lAllPO = DYNAMIC-FUNCTION("fGetDynParamValue","allPO") EQ "YES"
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cStartPONumber = DYNAMIC-FUNCTION("fGetDynParamValue","StartPONumber")
        cEndPONumber = DYNAMIC-FUNCTION("fGetDynParamValue","endPONumber")
        dtStartPoDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startPoDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartPoDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartPoDate)
        dtEndPoDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endPoDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndPoDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndPoDate)
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReceiptDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtStartReceiptDate)
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReceiptDate"))
        cDatePickList-4 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-4")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-4,dtEndReceiptDate)
        lSvRunSync = DYNAMIC-FUNCTION("fGetDynParamValue","svRunSync") EQ "YES"
        cSvRecipients = DYNAMIC-FUNCTION("fGetDynParamValue","svRecipients")
        lSvAutoClose = DYNAMIC-FUNCTION("fGetDynParamValue","svAutoClose") EQ "YES"
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
