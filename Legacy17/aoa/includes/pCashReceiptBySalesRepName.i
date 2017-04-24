/* pCashReceiptBySalesRepName.i - auto generated 11.03.2016 @ 12:46:40 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIncludeTermsDiscount AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludePrepCharges AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iDayOld AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-cashs2.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDate"))
        cStartReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDateOption")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartReceiptDateOption,dtStartReceiptDate)
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDate"))
        cEndReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDateOption")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndReceiptDateOption,dtEndReceiptDate)
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        lIncludeTermsDiscount = DYNAMIC-FUNCTION("fGetParamValue","svIncludeTermsDiscount") EQ "yes"
        lIncludePrepCharges = DYNAMIC-FUNCTION("fGetParamValue","svIncludePrepCharges") EQ "yes"
        iDayOld = DYNAMIC-FUNCTION("fGetParamValue","svDayOld")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttCashReceiptBySalesRepName:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(254)
        .

