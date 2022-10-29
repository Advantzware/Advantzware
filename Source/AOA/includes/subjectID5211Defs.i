/* subjectID5211Defs.i - auto generated 07.19.2022 @ 11:09:36 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllDMIID AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartDMIID AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndDMIID AS INTEGER NO-UNDO.
DEFINE VARIABLE dtStartDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllShifts AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShift AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndShftDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseTimes AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE lReceipts AS LOGICAL NO-UNDO.
DEFINE VARIABLE lShipments AS LOGICAL NO-UNDO.
DEFINE VARIABLE lTransfers AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAdjustments AS LOGICAL NO-UNDO.
DEFINE VARIABLE lCreditRefunds AS LOGICAL NO-UNDO.
DEFINE VARIABLE lCycleCount AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllFGItems AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndFGItemDescriptn AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllUserID AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserID AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndUserName AS CHARACTER NO-UNDO.
DEFINE VARIABLE lShowDetail AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lAllDMIID = DYNAMIC-FUNCTION("fGetDynParamValue","AllDMIID") EQ "YES"
        iStartDMIID = DYNAMIC-FUNCTION("fGetDynParamValue","StartDMIID")
        iEndDMIID = DYNAMIC-FUNCTION("fGetDynParamValue","EndDMIID")
        dtStartDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","StartDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartDate)
        dtEndDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","EndDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndDate)
        lAllShifts = DYNAMIC-FUNCTION("fGetDynParamValue","allShifts") EQ "YES"
        cStartShift = DYNAMIC-FUNCTION("fGetDynParamValue","startShift")
        cStartShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startShftDescription")
        cEndShift = DYNAMIC-FUNCTION("fGetDynParamValue","endShift")
        cEndShftDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endShftDescription")
        lUseTimes = DYNAMIC-FUNCTION("fGetDynParamValue","useTimes") EQ "YES"
        cStartTime = DYNAMIC-FUNCTION("fGetDynParamValue","startTime")
        cEndTime = DYNAMIC-FUNCTION("fGetDynParamValue","endTime")
        lReceipts = DYNAMIC-FUNCTION("fGetDynParamValue","Receipts") EQ "YES"
        lShipments = DYNAMIC-FUNCTION("fGetDynParamValue","Shipments") EQ "YES"
        lTransfers = DYNAMIC-FUNCTION("fGetDynParamValue","Transfers") EQ "YES"
        lAdjustments = DYNAMIC-FUNCTION("fGetDynParamValue","Adjustments") EQ "YES"
        lCreditRefunds = DYNAMIC-FUNCTION("fGetDynParamValue","CreditRefunds") EQ "YES"
        lCycleCount = DYNAMIC-FUNCTION("fGetDynParamValue","CycleCount") EQ "YES"
        lAllFGItems = DYNAMIC-FUNCTION("fGetDynParamValue","allFGItems") EQ "YES"
        cStartFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItem")
        cStartFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","startFGItemDescriptn")
        cEndFGItem = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItem")
        cEndFGItemDescriptn = DYNAMIC-FUNCTION("fGetDynParamValue","endFGItemDescriptn")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllUserID = DYNAMIC-FUNCTION("fGetDynParamValue","allUserID") EQ "YES"
        cStartUserID = DYNAMIC-FUNCTION("fGetDynParamValue","startUserID")
        cStartUserName = DYNAMIC-FUNCTION("fGetDynParamValue","startUserName")
        cEndUserID = DYNAMIC-FUNCTION("fGetDynParamValue","endUserID")
        cEndUserName = DYNAMIC-FUNCTION("fGetDynParamValue","endUserName")
        lShowDetail = DYNAMIC-FUNCTION("fGetDynParamValue","ShowDetail") EQ "YES"
        .
END PROCEDURE.
