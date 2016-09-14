/* pInvoicePostUpdateGL.i - auto generated 08.24.2016 @ 11:38:37 pm from aoa/aoaParam.w */

    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBatch   AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipcUserID  AS CHARACTER NO-UNDO.

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.
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
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-inve&p.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lPost = DYNAMIC-FUNCTION("fGetParamValue","svPost") EQ "yes"
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
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttInvoicePostUpdateGL:HANDLE,
                     cAvailableColumns,
                     cSelectedColumns
                     ).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(255)
        .

    IF lAllInvNo THEN
    ASSIGN
        iStartInvNo = 0
        iEndInvNo   = 99999999
        .

    RUN pBuildCustList (ipcCompany, lCustList, cStartCustNo, cEndCustNo, "OB4").
