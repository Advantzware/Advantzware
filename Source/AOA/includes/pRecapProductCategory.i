/* pRecapProductCategory.i - auto generated 08.29.2018 @ 10:16:17 am from AOA/aoaParam.w */

    {AOA/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllProCat AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lIncludePrepMiscChg AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lExcludeSetComponents AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lExcludeTransferReleasesOrders AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPrintOrderUnderPct AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iUnderValue AS INTEGER NO-UNDO.
    DEFINE VARIABLE lPrintOrderOverPct AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iOverValue AS INTEGER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "recappc.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDate"))
        cStartOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDateOption")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartOrderDateOption,dtStartOrderDate)
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDate"))
        cEndOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDateOption")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndOrderDateOption,dtEndOrderDate)
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDueDate"))
        cStartDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDueDateOption")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartDueDateOption,dtStartDueDate)
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDueDate"))
        cEndDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndDueDateOption")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndDueDateOption,dtEndDueDate)
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        lAllProCat = DYNAMIC-FUNCTION("fGetParamValue","svAllProCat") EQ "yes"
        cStartProCat = DYNAMIC-FUNCTION("fGetParamValue","svStartProCat")
        cEndProCat = DYNAMIC-FUNCTION("fGetParamValue","svEndProCat")
        lIncludePrepMiscChg = DYNAMIC-FUNCTION("fGetParamValue","svIncludePrepMiscChg") EQ "yes"
        lExcludeSetComponents = DYNAMIC-FUNCTION("fGetParamValue","svExcludeSetComponents") EQ "yes"
        lExcludeTransferReleasesOrders = DYNAMIC-FUNCTION("fGetParamValue","svExcludeTransferReleasesOrders") EQ "yes"
        lPrintOrderUnderPct = DYNAMIC-FUNCTION("fGetParamValue","svPrintOrderUnderPct") EQ "yes"
        iUnderValue = DYNAMIC-FUNCTION("fGetParamValue","svUnderValue")
        lPrintOrderOverPct = DYNAMIC-FUNCTION("fGetParamValue","svPrintOrderOverPct") EQ "yes"
        iOverValue = DYNAMIC-FUNCTION("fGetParamValue","svOverValue")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttRecapProductCategory:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(254)
        .

    IF lAllProCat THEN
    ASSIGN
        cStartProCat = CHR(32)
        cEndProCat   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "OR17", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).