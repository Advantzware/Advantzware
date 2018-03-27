/* pAPInvoicePosting.i - auto generated 09.27.2017 @  4:18:55 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtPostDate AS DATE NO-UNDO.
    DEFINE VARIABLE cPostDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPeriod AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllVendNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartVendNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndVendNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndInvoiceDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndInvoiceDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPostOutOfPeriod AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPostIntoClosedPeriod AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPost AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-apve&p.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        dtPostDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svPostDate"))
        cPostDateOption = DYNAMIC-FUNCTION("fGetParamValue","svPostDateOption")
        dtPostDate = DYNAMIC-FUNCTION("fDateOptionDate",cPostDateOption,dtPostDate)
        iPeriod = DYNAMIC-FUNCTION("fGetParamValue","svPeriod")
        lAllVendNo = DYNAMIC-FUNCTION("fGetParamValue","svAllVendNo") EQ "yes"
        cStartVendNo = DYNAMIC-FUNCTION("fGetParamValue","svStartVendNo")
        cEndVendNo = DYNAMIC-FUNCTION("fGetParamValue","svEndVendNo")
        dtStartInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDate"))
        cStartInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartInvoiceDateOption")
        dtStartInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartInvoiceDateOption,dtStartInvoiceDate)
        dtEndInvoiceDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDate"))
        cEndInvoiceDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndInvoiceDateOption")
        dtEndInvoiceDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndInvoiceDateOption,dtEndInvoiceDate)
        lAllUserID = DYNAMIC-FUNCTION("fGetParamValue","svAllUserID") EQ "yes"
        cStartUserID = DYNAMIC-FUNCTION("fGetParamValue","svStartUserID")
        cEndUserID = DYNAMIC-FUNCTION("fGetParamValue","svEndUserID")
        lPostOutOfPeriod = DYNAMIC-FUNCTION("fGetParamValue","svPostOutOfPeriod") EQ "yes"
        lPostIntoClosedPeriod = DYNAMIC-FUNCTION("fGetParamValue","svPostIntoClosedPeriod") EQ "yes"
        lPost = DYNAMIC-FUNCTION("fGetParamValue","svPost") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttAPInvoicePosting:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllVendNo THEN
    ASSIGN
        cStartVendNo = CHR(32)
        cEndVendNo   = CHR(254)
        .

    IF lAllUserID THEN
    ASSIGN
        cStartUserID = CHR(32)
        cEndUserID   = CHR(254)
        .

