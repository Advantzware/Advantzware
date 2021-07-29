/* subjectID18Defs.i - auto generated 07.06.2021 @  7:13:36 pm */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndCustName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStartItemDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEndItemDescription AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseReceiptDate AS LOGICAL NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
DEFINE VARIABLE cPrintOrderRemaining AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrintMiscCharges AS LOGICAL NO-UNDO.
DEFINE VARIABLE lPrintContribution AS LOGICAL NO-UNDO.
DEFINE VARIABLE dtStartShipDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-5 AS CHARACTER NO-UNDO.
DEFINE VARIABLE dtEndShipDate AS DATE NO-UNDO.
DEFINE VARIABLE cDatePickList-6 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lUseShipDate AS LOGICAL NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startOrderDate"))
        cDatePickList-1 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-1")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-1,dtStartOrderDate)
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endOrderDate"))
        cDatePickList-2 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-2")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-2,dtEndOrderDate)
        lAllItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllItemNo") EQ "YES"
        cStartItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","startItemNo")
        cStartItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startItemDescription")
        cEndItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","endItemNo")
        cEndItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endItemDescription")
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReceiptDate"))
        cDatePickList-3 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-3")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-3,dtStartReceiptDate)
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReceiptDate"))
        cDatePickList-4 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-4")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-4,dtEndReceiptDate)
        lUseReceiptDate = DYNAMIC-FUNCTION("fGetDynParamValue","UseReceiptDate") EQ "YES"
        lAllOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllOrderNo") EQ "YES"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","startOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","endOrderNo")
        cPrintOrderRemaining = DYNAMIC-FUNCTION("fGetDynParamValue","printOrderRemaining")
        lPrintMiscCharges = DYNAMIC-FUNCTION("fGetDynParamValue","printMiscCharges") EQ "YES"
        lPrintContribution = DYNAMIC-FUNCTION("fGetDynParamValue","printContribution") EQ "YES"
        dtStartShipDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startShipDate"))
        cDatePickList-5 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-5")
        dtStartShipDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-5,dtStartShipDate)
        dtEndShipDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endShipDate"))
        cDatePickList-6 = DYNAMIC-FUNCTION("fGetDynParamValue","DatePickList-6")
        dtEndShipDate = DYNAMIC-FUNCTION("fDateOptionDate",cDatePickList-6,dtEndShipDate)
        lUseShipDate = DYNAMIC-FUNCTION("fGetDynParamValue","UseShipDate") EQ "YES"
        .
END PROCEDURE.
