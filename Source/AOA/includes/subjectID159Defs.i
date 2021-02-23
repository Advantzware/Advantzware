/* subjectID159Defs.i - auto generated 02.16.2021 @  8:36:40 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartReleaseDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndReleaseDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrintOHQty AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE lSubRpt_PrintSpecNotes AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSpecNote AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSpecNote AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCarrNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCarrNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCarrName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCarrNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCarrName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSubTotalByCustomerNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOnlyNegativeAvailable AS LOGICAL NO-UNDO.
DEFINE VARIABLE lOnlyNegOHRelQty AS LOGICAL NO-UNDO.
DEFINE VARIABLE lSubRpt_PrintScheduleStats AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllProCats AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCat AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndProCatDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lScheduled AS LOGICAL NO-UNDO.
DEFINE VARIABLE lLate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPastLastShipDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lActual AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBackOrder AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBillOfLading AS LOGICAL NO-UNDO.
DEFINE VARIABLE lInvoiceUnposted AS LOGICAL NO-UNDO.
DEFINE VARIABLE lCompleted AS LOGICAL NO-UNDO.
DEFINE VARIABLE cSort AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllLocs AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndLocDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShipFromNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShipFromNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShipFromName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShipFromNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShipFromName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllSalesRep AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRep AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndSalesRepName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPrinted AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        dtStartReleaseDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReleaseDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartReleaseDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartReleaseDate)
        dtEndReleaseDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReleaseDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndReleaseDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndReleaseDate)
        cPrintOHQty = DYNAMIC-FUNCTION("fGetDynParamValue","PrintOHQty")
        lAllOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllOrderNo") EQ "YES"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","startOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","endOrderNo")
        lSubRpt_PrintSpecNotes = DYNAMIC-FUNCTION("fGetDynParamValue","SubRpt_PrintSpecNotes") EQ "YES"
        cStartSpecNote = DYNAMIC-FUNCTION("fGetDynParamValue","StartSpecNote")
        cEndSpecNote = DYNAMIC-FUNCTION("fGetDynParamValue","EndSpecNote")
        lAllCarrNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCarrNo") EQ "YES"
        cStartCarrNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCarrNo")
        cStartCarrName = DYNAMIC-FUNCTION("fGetDynParamValue","startCarrName")
        cEndCarrNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCarrNo")
        cEndCarrName = DYNAMIC-FUNCTION("fGetDynParamValue","endCarrName")
        lSubTotalByCustomerNo = DYNAMIC-FUNCTION("fGetDynParamValue","SubTotalByCustomerNo") EQ "YES"
        lOnlyNegativeAvailable = DYNAMIC-FUNCTION("fGetDynParamValue","OnlyNegativeAvailable") EQ "YES"
        lOnlyNegOHRelQty = DYNAMIC-FUNCTION("fGetDynParamValue","OnlyNegOHRelQty") EQ "YES"
        lSubRpt_PrintScheduleStats = DYNAMIC-FUNCTION("fGetDynParamValue","SubRpt_PrintScheduleStats") EQ "YES"
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        lAllProCats = DYNAMIC-FUNCTION("fGetDynParamValue","allProCats") EQ "YES"
        cStartProCat = DYNAMIC-FUNCTION("fGetDynParamValue","startProCat")
        cStartProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startProCatDescriptn")
        cEndProCat = DYNAMIC-FUNCTION("fGetDynParamValue","endProCat")
        cEndProCatDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endProCatDescriptn")
        lScheduled = DYNAMIC-FUNCTION("fGetDynParamValue","Scheduled") EQ "YES"
        lLate = DYNAMIC-FUNCTION("fGetDynParamValue","Late") EQ "YES"
        lPastLastShipDate = DYNAMIC-FUNCTION("fGetDynParamValue","PastLastShipDate") EQ "YES"
        lActual = DYNAMIC-FUNCTION("fGetDynParamValue","Actual") EQ "YES"
        lBackOrder = DYNAMIC-FUNCTION("fGetDynParamValue","BackOrder") EQ "YES"
        lBillOfLading = DYNAMIC-FUNCTION("fGetDynParamValue","BillOfLading") EQ "YES"
        lInvoiceUnposted = DYNAMIC-FUNCTION("fGetDynParamValue","InvoiceUnposted") EQ "YES"
        lCompleted = DYNAMIC-FUNCTION("fGetDynParamValue","Completed") EQ "YES"
        cSort = DYNAMIC-FUNCTION("fGetDynParamValue","Sort")
        lAllLocs = DYNAMIC-FUNCTION("fGetDynParamValue","allLocs") EQ "YES"
        cStartLoc = DYNAMIC-FUNCTION("fGetDynParamValue","startLoc")
        cStartLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startLocDescription")
        cEndLoc = DYNAMIC-FUNCTION("fGetDynParamValue","endLoc")
        cEndLocDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endLocDescription")
        lAllShipFromNo = DYNAMIC-FUNCTION("fGetDynParamValue","allShipFromNo") EQ "YES"
        cStartShipFromNo = DYNAMIC-FUNCTION("fGetDynParamValue","startShipFromNo")
        cStartShipFromName = DYNAMIC-FUNCTION("fGetDynParamValue","startShipFromName")
        cEndShipFromNo = DYNAMIC-FUNCTION("fGetDynParamValue","endShipFromNo")
        cEndShipFromName = DYNAMIC-FUNCTION("fGetDynParamValue","endShipFromName")
        lAllSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","allSalesRep") EQ "YES"
        cStartSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRep")
        cStartSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","startSalesRepName")
        cEndSalesRep = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRep")
        cEndSalesRepName = DYNAMIC-FUNCTION("fGetDynParamValue","endSalesRepName")
        lAllUserID = DYNAMIC-FUNCTION("fGetDynParamValue","allUserID") EQ "YES"
        cStartUserID = DYNAMIC-FUNCTION("fGetDynParamValue","startUserID")
        cStartUserName = DYNAMIC-FUNCTION("fGetDynParamValue","startUserName")
        cEndUserID = DYNAMIC-FUNCTION("fGetDynParamValue","endUserID")
        cEndUserName = DYNAMIC-FUNCTION("fGetDynParamValue","endUserName")
        cPrinted = DYNAMIC-FUNCTION("fGetDynParamValue","Printed")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        .
END PROCEDURE.
