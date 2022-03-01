/* subjectID162Defs.i - auto generated 02.23.2022 @  4:40:47 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE lAllCompany AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShipFromNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShipFromNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShipFromName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShipFromNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShipFromName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartDueDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndDueDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustPart AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustPart AS CHARACTER NO-UNDO.
DEFINE VARIABLE lIncludePrepMiscChg AS LOGICAL NO-UNDO.
DEFINE VARIABLE lExcludeSetComponents AS LOGICAL NO-UNDO.
DEFINE VARIABLE lExcludeTransferReleasesOrders AS LOGICAL NO-UNDO.
DEFINE VARIABLE lLOrdWithNoRel AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPrintOrderUnderPct AS LOGICAL NO-UNDO.
DEFINE VARIABLE iUnderValue AS INTEGER NO-UNDO.
DEFINE VARIABLE lPrintOrderOverPct AS LOGICAL NO-UNDO.
DEFINE VARIABLE iOverValue AS INTEGER NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        lAllCompany = DYNAMIC-FUNCTION("fGetDynParamValue","allCompany") EQ "YES"
        cStartCompany = DYNAMIC-FUNCTION("fGetDynParamValue","startCompany")
        cStartDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startDescription")
        cEndCompany = DYNAMIC-FUNCTION("fGetDynParamValue","endCompany")
        cEndDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endDescription")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        lAllShipFromNo = DYNAMIC-FUNCTION("fGetDynParamValue","allShipFromNo") EQ "YES"
        cStartShipFromNo = DYNAMIC-FUNCTION("fGetDynParamValue","startShipFromNo")
        cStartShipFromName = DYNAMIC-FUNCTION("fGetDynParamValue","startShipFromName")
        cEndShipFromNo = DYNAMIC-FUNCTION("fGetDynParamValue","endShipFromNo")
        cEndShipFromName = DYNAMIC-FUNCTION("fGetDynParamValue","endShipFromName")
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startOrderDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartOrderDate)
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endOrderDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndOrderDate)
        dtStartDueDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startDueDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtStartDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtStartDueDate)
        dtEndDueDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endDueDate"))
        cDatePickList-4 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-4")
        dtEndDueDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-4,dtEndDueDate)
        lAllCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","allCustPart") EQ "YES"
        cStartCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","startCustPart")
        cEndCustPart = DYNAMIC-FUNCTION("fGetDynParamValue","endCustPart")
        lIncludePrepMiscChg = DYNAMIC-FUNCTION("fGetDynParamValue","IncludePrepMiscChg") EQ "YES"
        lExcludeSetComponents = DYNAMIC-FUNCTION("fGetDynParamValue","ExcludeSetComponents") EQ "YES"
        lExcludeTransferReleasesOrders = DYNAMIC-FUNCTION("fGetDynParamValue","ExcludeTransferReleasesOrders") EQ "YES"
        lLOrdWithNoRel = DYNAMIC-FUNCTION("fGetDynParamValue","lOrdWithNoRel") EQ "YES"
        lPrintOrderUnderPct = DYNAMIC-FUNCTION("fGetDynParamValue","PrintOrderUnderPct") EQ "YES"
        iUnderValue = DYNAMIC-FUNCTION("fGetDynParamValue","UnderValue")
        lPrintOrderOverPct = DYNAMIC-FUNCTION("fGetDynParamValue","PrintOrderOverPct") EQ "YES"
        iOverValue = DYNAMIC-FUNCTION("fGetDynParamValue","OverValue")
        .
END PROCEDURE.
