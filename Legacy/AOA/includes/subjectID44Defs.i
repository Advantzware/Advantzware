/* subjectID44Defs.i - auto generated 09.10.2019 @  9:04:58 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartTransDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndTransDate AS DATE NO-UNDO.
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
        cShift = DYNAMIC-FUNCTION("fGetDynParamValue","shift")
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startTransDate"))
        dtStartTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartTransDate)
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndTransDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endTransDate"))
        dtEndTransDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndTransDate)
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
