/* pInvoicePostUpdateGL.i - auto generated 11.03.2016 @ 12:47:25 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtPostDate AS DATE NO-UNDO.
    DEFINE VARIABLE cPostDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllInvNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartInvNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndInvNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lInvoiceReportDetail AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lGLReportDetail AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPrintTon AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-inve&p.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svPostDate"))
        cPostDateOption = DYNAMIC-FUNCTION("fGetParamValue","svPostDateOption")
        dtPostDate = DYNAMIC-FUNCTION("fDateOptionDate",cPostDateOption,dtPostDate)
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lAllInvNo = DYNAMIC-FUNCTION("fGetParamValue","svAllInvNo") EQ "yes"
        iStartInvNo = DYNAMIC-FUNCTION("fGetParamValue","svStartInvNo")
        iEndInvNo = DYNAMIC-FUNCTION("fGetParamValue","svEndInvNo")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDate"))
        cStartInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDateOption")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartInvoiceDateOption,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDate"))
        cEndInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDateOption")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndInvoiceDateOption,dtEndInvoiceDate)
        lInvoiceReportDetail = DYNAMIC-FUNCTION("fGetParamValue","svInvoiceReportDetail") EQ "yes"
        lGLReportDetail = DYNAMIC-FUNCTION("fGetParamValue","svGLReportDetail") EQ "yes"
        lPrintTon = DYNAMIC-FUNCTION("fGetParamValue","svPrintTon") EQ "yes"
        lPost = DYNAMIC-FUNCTION("fGetParamValue","svPost") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttInvoicePostUpdateGL:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllInvNo THEN
    ASSIGN
        iStartInvNo = 0
        iEndInvNo   = 99999999
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "OB4", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
