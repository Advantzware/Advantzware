/* subjectID17Defs.i - auto generated 06.20.2019 @  2:25:31 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtPostDate AS DATE NO-UNDO.
DEFINE VARIABLE lAllInvNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartInvNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndInvNo AS INTEGER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
DEFINE VARIABLE lInvoiceReportDetail AS LOGICAL NO-UNDO.
DEFINE VARIABLE lGLReportDetail AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPrintTon AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        cLocation = DYNAMIC-FUNCTION("fGetDynParamValue","location")
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","postDate"))
        dtPostDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtPostDate)
        lAllInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","allInvNo") EQ "YES"
        iStartInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","startInvNo")
        iEndInvNo = DYNAMIC-FUNCTION("fGetDynParamValue","endInvNo")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startInvoiceDate"))
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtStartInvoiceDate)
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endInvoiceDate"))
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtEndInvoiceDate)
        lInvoiceReportDetail = DYNAMIC-FUNCTION("fGetDynParamValue","invoiceReportDetail") EQ "YES"
        lGLReportDetail = DYNAMIC-FUNCTION("fGetDynParamValue","GLReportDetail") EQ "YES"
        lPrintTon = DYNAMIC-FUNCTION("fGetDynParamValue","printTon") EQ "YES"
        lPost = DYNAMIC-FUNCTION("fGetDynParamValue","post") EQ "YES"
        .
END PROCEDURE.
