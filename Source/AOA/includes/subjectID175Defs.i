/* subjectID175Defs.i - auto generated 07.07.2021 @  6:19:44 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllLocs AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllLocBin AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartLocBin AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLocBin AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrintSetandComponentsOnly AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeZeroBalance AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeCustomerOwnedWarehouse AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPrintSummaryByBinQty AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOnlyCustomerOwnedWarehouse AS LOGICAL NO-UNDO.
DEFINE VARIABLE lIncludeInactiveItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE dtAsOfDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOnlyShowQOHOlderThanDays AS INTEGER NO-UNDO.
DEFINE VARIABLE cItemCode2 AS CHARACTER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lSecure = DYNAMIC-FUNCTION("fGetDynParamValue","secure") EQ "YES"
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        lAllLocs = DYNAMIC-FUNCTION("fGetDynParamValue","allLocs") EQ "YES"
        cStartLoc = DYNAMIC-FUNCTION("fGetDynParamValue","startLoc")
        cStartLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startLocDescription")
        cEndLoc = DYNAMIC-FUNCTION("fGetDynParamValue","endLoc")
        cEndLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endLocDescription")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        lAllLocBin = DYNAMIC-FUNCTION("fGetDynParamValue","allLocBin") EQ "YES"
        cStartLocBin = DYNAMIC-FUNCTION("fGetDynParamValue","startLocBin")
        cEndLocBin = DYNAMIC-FUNCTION("fGetDynParamValue","endLocBin")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        lPrintSetandComponentsOnly = DYNAMIC-FUNCTION("fGetDynParamValue","PrintSetandComponentsOnly") EQ "YES"
        lIncludeZeroBalance = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeZeroBalance") EQ "YES"
        lIncludeCustomerOwnedWarehouse = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeCustomerOwnedWarehouse") EQ "YES"
        lPrintSummaryByBinQty = DYNAMIC-FUNCTION("fGetDynParamValue","PrintSummaryByBinQty") EQ "YES"
        lOnlyCustomerOwnedWarehouse = DYNAMIC-FUNCTION("fGetDynParamValue","OnlyCustomerOwnedWarehouse") EQ "YES"
        lIncludeInactiveItems = DYNAMIC-FUNCTION("fGetDynParamValue","IncludeInactiveItems") EQ "YES"
        dtAsOfDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","asOfDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtAsOfDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtAsOfDate)
        iOnlyShowQOHOlderThanDays = DYNAMIC-FUNCTION("fGetDynParamValue","OnlyShowQOHOlderThanDays")
        cItemCode2 = DYNAMIC-FUNCTION("fGetDynParamValue","itemCode2")
        .
END PROCEDURE.
