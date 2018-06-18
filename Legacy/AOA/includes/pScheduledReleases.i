/* pScheduledReleases.i - auto generated 09.28.2017 @ 11:49:02 pm from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE cLocation AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllLoc AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartReleaseDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartReleaseDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndReleaseDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndReleaseDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllCarrier AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCarrier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCarrier AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllProdCategory AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllShipFrom AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartShipFrom AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndShipFrom AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubRpt_PrintSpecNotes AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartSpecNote AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSpecNote AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrintOHQty AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSubTotalByCustomerNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOnlyNegativeAvailable AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOnlyNegOHRelQty AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSubRpt_PrintScheduleStats AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lScheduled AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lLate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPastLastShipDate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lActual AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lBackorder AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lBillOfLading AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lInvoiceUnposted AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lCompleted AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lAllCSR AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCSR AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCSR AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-sched.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        cLocation = DYNAMIC-FUNCTION("fGetParamValue","svLocation")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lAllOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svAllOrderNo") EQ "yes"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderNo")
        lAllItemNo = DYNAMIC-FUNCTION("fGetParamValue","svAllItemNo") EQ "yes"
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        lAllLoc = DYNAMIC-FUNCTION("fGetParamValue","svAllLoc") EQ "yes"
        cStartLoc = DYNAMIC-FUNCTION("fGetParamValue","svStartLoc")
        cEndLoc = DYNAMIC-FUNCTION("fGetParamValue","svEndLoc")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        dtStartReleaseDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartReleaseDate"))
        cStartReleaseDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartReleaseDateOption")
        dtStartReleaseDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartReleaseDateOption,dtStartReleaseDate)
        dtEndReleaseDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndReleaseDate"))
        cEndReleaseDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndReleaseDateOption")
        dtEndReleaseDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndReleaseDateOption,dtEndReleaseDate)
        lAllCarrier = DYNAMIC-FUNCTION("fGetParamValue","svAllCarrier") EQ "yes"
        cStartCarrier = DYNAMIC-FUNCTION("fGetParamValue","svStartCarrier")
        cEndCarrier = DYNAMIC-FUNCTION("fGetParamValue","svEndCarrier")
        lAllProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svAllProdCategory") EQ "yes"
        cStartProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svStartProdCategory")
        cEndProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svEndProdCategory")
        lAllShipFrom = DYNAMIC-FUNCTION("fGetParamValue","svAllShipFrom") EQ "yes"
        cStartShipFrom = DYNAMIC-FUNCTION("fGetParamValue","svStartShipFrom")
        cEndShipFrom = DYNAMIC-FUNCTION("fGetParamValue","svEndShipFrom")
        lSubRpt_PrintSpecNotes = DYNAMIC-FUNCTION("fGetParamValue","svSubRpt_PrintSpecNotes") EQ "yes"
        cStartSpecNote = DYNAMIC-FUNCTION("fGetParamValue","svStartSpecNote")
        cEndSpecNote = DYNAMIC-FUNCTION("fGetParamValue","svEndSpecNote")
        cPrintOHQty = DYNAMIC-FUNCTION("fGetParamValue","svPrintOHQty")
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        lSubTotalByCustomerNo = DYNAMIC-FUNCTION("fGetParamValue","svSubTotalByCustomerNo") EQ "yes"
        lOnlyNegativeAvailable = DYNAMIC-FUNCTION("fGetParamValue","svOnlyNegativeAvailable") EQ "yes"
        lOnlyNegOHRelQty = DYNAMIC-FUNCTION("fGetParamValue","svOnlyNegOHRelQty") EQ "yes"
        lSubRpt_PrintScheduleStats = DYNAMIC-FUNCTION("fGetParamValue","svSubRpt_PrintScheduleStats") EQ "yes"
        lScheduled = DYNAMIC-FUNCTION("fGetParamValue","svScheduled") EQ "yes"
        lLate = DYNAMIC-FUNCTION("fGetParamValue","svLate") EQ "yes"
        lPastLastShipDate = DYNAMIC-FUNCTION("fGetParamValue","svPastLastShipDate") EQ "yes"
        lActual = DYNAMIC-FUNCTION("fGetParamValue","svActual") EQ "yes"
        lBackorder = DYNAMIC-FUNCTION("fGetParamValue","svBackorder") EQ "yes"
        lBillOfLading = DYNAMIC-FUNCTION("fGetParamValue","svBillOfLading") EQ "yes"
        lInvoiceUnposted = DYNAMIC-FUNCTION("fGetParamValue","svInvoiceUnposted") EQ "yes"
        lCompleted = DYNAMIC-FUNCTION("fGetParamValue","svCompleted") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")

        lAllCSR = DYNAMIC-FUNCTION("fGetParamValue","svAllCSR") EQ "yes"
        cStartCSR = DYNAMIC-FUNCTION("fGetParamValue","svStartCSR")
        cEndCSR = DYNAMIC-FUNCTION("fGetParamValue","svEndCSR")

        .

    RUN pGetColumns (TEMP-TABLE ttScheduledReleases:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllOrderNo THEN
    ASSIGN
        iStartOrderNo = 0
        iEndOrderNo   = 99999999
        .

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(254)
        .

    IF lAllLoc THEN
    ASSIGN
        cStartLoc = CHR(32)
        cEndLoc   = CHR(254)
        .

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(254)
        .

    IF lAllCarrier THEN
    ASSIGN
        cStartCarrier = CHR(32)
        cEndCarrier   = CHR(254)
        .

    IF lAllProdCategory THEN
    ASSIGN
        cStartProdCategory = CHR(32)
        cEndProdCategory   = CHR(254)
        .

    IF lAllShipFrom THEN
    ASSIGN
        cStartShipFrom = CHR(32)
        cEndShipFrom   = CHR(254)
        .

    IF lAllCSR THEN
    ASSIGN
        cStartCSR = CHR(32)
        cEndCSR   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "OR2", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
