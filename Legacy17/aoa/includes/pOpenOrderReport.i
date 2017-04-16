/* pOpenOrderReport.i - auto generated 11.03.2016 @  9:56:03 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllPONumber AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartPONumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndPONumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllJobNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iStartJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE cEndJobNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iEndJobNo2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllCAD AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCAD AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCAD AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndDueDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrimarySort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSecondarySort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cJobStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cOrderStatus AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cWIPQty AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubRpt_PrintJobQtyDetails AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lDropOrderUnderrun AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeZeroOrderBalanceItems AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeZeroQtyActReleaseQty AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeJobsQOH AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeZeroQtyWIPItems AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeInactiveItems AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-ordopn.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDate"))
        cStartOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDateOption")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartOrderDateOption,dtStartOrderDate)
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDate"))
        cEndOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDateOption")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndOrderDateOption,dtEndOrderDate)
        lAllPONumber = DYNAMIC-FUNCTION("fGetParamValue","svAllPONumber") EQ "yes"
        cStartPONumber = DYNAMIC-FUNCTION("fGetParamValue","svStartPONumber")
        cEndPONumber = DYNAMIC-FUNCTION("fGetParamValue","svEndPONumber")
        lAllJobNo = DYNAMIC-FUNCTION("fGetParamValue","svAllJobNo") EQ "yes"
        cStartJobNo = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo")
        iStartJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svStartJobNo2")
        cEndJobNo = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo")
        iEndJobNo2 = DYNAMIC-FUNCTION("fGetParamValue","svEndJobNo2")
        lAllItemNo = DYNAMIC-FUNCTION("fGetParamValue","svAllItemNo") EQ "yes"
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        lAllCAD = DYNAMIC-FUNCTION("fGetParamValue","svAllCAD") EQ "yes"
        cStartCAD = DYNAMIC-FUNCTION("fGetParamValue","svStartCAD")
        cEndCAD = DYNAMIC-FUNCTION("fGetParamValue","svEndCAD")
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartDueDate"))
        cStartDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartDueDateOption")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartDueDateOption,dtStartDueDate)
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndDueDate"))
        cEndDueDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndDueDateOption")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndDueDateOption,dtEndDueDate)
        lAllUserID = DYNAMIC-FUNCTION("fGetParamValue","svAllUserID") EQ "yes"
        cStartUserID = DYNAMIC-FUNCTION("fGetParamValue","svStartUserID")
        cEndUserID = DYNAMIC-FUNCTION("fGetParamValue","svEndUserID")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        cPrimarySort = DYNAMIC-FUNCTION("fGetParamValue","svPrimarySort")
        cSecondarySort = DYNAMIC-FUNCTION("fGetParamValue","svSecondarySort")
        cJobStatus = DYNAMIC-FUNCTION("fGetParamValue","svJobStatus")
        cOrderStatus = DYNAMIC-FUNCTION("fGetParamValue","svOrderStatus")
        cWIPQty = DYNAMIC-FUNCTION("fGetParamValue","svWIPQty")
        lSubRpt_PrintJobQtyDetails = DYNAMIC-FUNCTION("fGetParamValue","svSubRpt_PrintJobQtyDetails") EQ "yes"
        lDropOrderUnderrun = DYNAMIC-FUNCTION("fGetParamValue","svDropOrderUnderrun") EQ "yes"
        lIncludeZeroOrderBalanceItems = DYNAMIC-FUNCTION("fGetParamValue","svIncludeZeroOrderBalanceItems") EQ "yes"
        lIncludeZeroQtyActReleaseQty = DYNAMIC-FUNCTION("fGetParamValue","svIncludeZeroQtyActReleaseQty") EQ "yes"
        lIncludeJobsQOH = DYNAMIC-FUNCTION("fGetParamValue","svIncludeJobsQOH") EQ "yes"
        lIncludeZeroQtyWIPItems = DYNAMIC-FUNCTION("fGetParamValue","svIncludeZeroQtyWIPItems") EQ "yes"
        lIncludeInactiveItems = DYNAMIC-FUNCTION("fGetParamValue","svIncludeInactiveItems") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttOpenOrderReport:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllPONumber THEN
    ASSIGN
        cStartPONumber = CHR(32)
        cEndPONumber   = CHR(254)
        .

    IF lAllJobNo THEN
    ASSIGN
        cStartJobNo = CHR(32)
        cEndJobNo   = CHR(254)
        .

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(254)
        .

    IF lAllCAD THEN
    ASSIGN
        cStartCAD = CHR(32)
        cEndCAD   = CHR(254)
        .

    IF lAllUserID THEN
    ASSIGN
        cStartUserID = CHR(32)
        cEndUserID   = CHR(254)
        .

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "OR16", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
