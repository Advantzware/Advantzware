/* pInventoryValue.i - auto generated 11.03.2016 @ 12:47:17 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE iShowQOHOlderThanDays AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
    DEFINE VARIABLE cAsOfDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllLoc AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllLocBin AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartLocBin AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndLocBin AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllProdCategory AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndProdCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cItemCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPrintSetAndComponentsOnly AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeZeroBalance AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeCustomerOwnerdWarehouse AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPrintSummaryByBinQty AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lOnlyCustomerOwnedWarehouse AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lIncludeInactiveItems AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPrintCost AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lDLMATOnly AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-fgohbb.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        iShowQOHOlderThanDays = DYNAMIC-FUNCTION("fGetParamValue","svShowQOHOlderThanDays")
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svAsOfDate"))
        cAsOfDateOption = DYNAMIC-FUNCTION("fGetParamValue","svAsOfDateOption")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cAsOfDateOption,dtAsOfDate)
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lAllLoc = DYNAMIC-FUNCTION("fGetParamValue","svAllLoc") EQ "yes"
        cStartLoc = DYNAMIC-FUNCTION("fGetParamValue","svStartLoc")
        cEndLoc = DYNAMIC-FUNCTION("fGetParamValue","svEndLoc")
        lAllLocBin = DYNAMIC-FUNCTION("fGetParamValue","svAllLocBin") EQ "yes"
        cStartLocBin = DYNAMIC-FUNCTION("fGetParamValue","svStartLocBin")
        cEndLocBin = DYNAMIC-FUNCTION("fGetParamValue","svEndLocBin")
        lAllItemNo = DYNAMIC-FUNCTION("fGetParamValue","svAllItemNo") EQ "yes"
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        lAllProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svAllProdCategory") EQ "yes"
        cStartProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svStartProdCategory")
        cEndProdCategory = DYNAMIC-FUNCTION("fGetParamValue","svEndProdCategory")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svAllSalesRep") EQ "yes"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svStartSalesRep")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetParamValue","svEndSalesRep")
        cSort = DYNAMIC-FUNCTION("fGetParamValue","svSort")
        cItemCode = DYNAMIC-FUNCTION("fGetParamValue","svItemCode")
        lPrintSetAndComponentsOnly = DYNAMIC-FUNCTION("fGetParamValue","svPrintSetAndComponentsOnly") EQ "yes"
        lIncludeZeroBalance = DYNAMIC-FUNCTION("fGetParamValue","svIncludeZeroBalance") EQ "yes"
        lIncludeCustomerOwnerdWarehouse = DYNAMIC-FUNCTION("fGetParamValue","svIncludeCustomerOwnerdWarehouse") EQ "yes"
        lPrintSummaryByBinQty = DYNAMIC-FUNCTION("fGetParamValue","svPrintSummaryByBinQty") EQ "yes"
        lOnlyCustomerOwnedWarehouse = DYNAMIC-FUNCTION("fGetParamValue","svOnlyCustomerOwnedWarehouse") EQ "yes"
        lIncludeInactiveItems = DYNAMIC-FUNCTION("fGetParamValue","svIncludeInactiveItems") EQ "yes"
        lPrintCost = DYNAMIC-FUNCTION("fGetParamValue","svPrintCost") EQ "yes"
        lDLMATOnly = DYNAMIC-FUNCTION("fGetParamValue","svDLMATOnly") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttInventoryValue:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllLoc THEN
    ASSIGN
        cStartLoc = CHR(32)
        cEndLoc   = CHR(254)
        .

    IF lAllLocBin THEN
    ASSIGN
        cStartLocBin = CHR(32)
        cEndLocBin   = CHR(254)
        .

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(254)
        .

    IF lAllProdCategory THEN
    ASSIGN
        cStartProdCategory = CHR(32)
        cEndProdCategory   = CHR(254)
        .

    IF lAllSalesRep THEN
    ASSIGN
        cStartSalesRep = CHR(32)
        cEndSalesRep   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "IR2", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
