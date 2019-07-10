/* subjectID18Defs.i - auto generated 06.21.2019 @ 11:07:29 am */

{AOA/includes/dynRunBusinessLogicDefs.i}

/* parameter values loaded into these variables */
DEFINE VARIABLE cCompany AS Character NO-UNDO.
DEFINE VARIABLE dtStartReceiptDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndReceiptDate AS Date NO-UNDO.
DEFINE VARIABLE cStartCustName AS Character NO-UNDO.
DEFINE VARIABLE cEndCustName AS Character NO-UNDO.
DEFINE VARIABLE lAllCustNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartCustNo AS Character NO-UNDO.
DEFINE VARIABLE cEndCustNo AS Character NO-UNDO.
DEFINE VARIABLE lCustList AS Logical NO-UNDO.
DEFINE VARIABLE cStartItemDescription AS Character NO-UNDO.
DEFINE VARIABLE cEndItemDescription AS Character NO-UNDO.
DEFINE VARIABLE lAllItemNo AS Logical NO-UNDO.
DEFINE VARIABLE cStartItemNo AS Character NO-UNDO.
DEFINE VARIABLE cEndItemNo AS Character NO-UNDO.
DEFINE VARIABLE lAllOrderNo AS Logical NO-UNDO.
DEFINE VARIABLE iStartOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE iEndOrderNo AS Integer NO-UNDO.
DEFINE VARIABLE dtStartOrderDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndOrderDate AS Date NO-UNDO.
DEFINE VARIABLE dtStartShipDate AS Date NO-UNDO.
DEFINE VARIABLE dtEndShipDate AS Date NO-UNDO.
DEFINE VARIABLE cPrintOrderRemaining AS Character NO-UNDO.
DEFINE VARIABLE lPrintMiscCharges AS Logical NO-UNDO.
DEFINE VARIABLE lPrintContribution AS Logical NO-UNDO.
DEFINE VARIABLE lUseReceiptDate AS Logical NO-UNDO.
DEFINE VARIABLE lUseShipDate AS Logical NO-UNDO.

PROCEDURE pAssignParamVariables:
    /* load dynamic parameter values into variables */
    ASSIGN
        cCompany = DYNAMIC-FUNCTION("fGetDynParamValue","company")
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startReceiptDate"))
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endReceiptDate"))
        cStartCustName = DYNAMIC-FUNCTION("fGetDynParamValue","startCustName")
        cEndCustName = DYNAMIC-FUNCTION("fGetDynParamValue","endCustName")
        lAllCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","allCustNo") EQ "YES"
        cStartCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","startCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetDynParamValue","endCustNo")
        lCustList = DYNAMIC-FUNCTION("fGetDynParamValue","custList") EQ "YES"
        cStartItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","startItemDescription")
        cEndItemDescription = DYNAMIC-FUNCTION("fGetDynParamValue","endItemDescription")
        lAllItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllItemNo") EQ "YES"
        cStartItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","startItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetDynParamValue","endItemNo")
        lAllOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","AllOrderNo") EQ "YES"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","startOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetDynParamValue","endOrderNo")
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startOrderDate"))
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endOrderDate"))
        dtStartShipDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","startShipDate"))
        dtEndShipDate = DATE(DYNAMIC-FUNCTION("fGetDynParamValue","endShipDate"))
        cPrintOrderRemaining = DYNAMIC-FUNCTION("fGetDynParamValue","printOrderRemaining")
        lPrintMiscCharges = DYNAMIC-FUNCTION("fGetDynParamValue","printMiscCharges") EQ "YES"
        lPrintContribution = DYNAMIC-FUNCTION("fGetDynParamValue","printContribution") EQ "YES"
        lUseReceiptDate = DYNAMIC-FUNCTION("fGetDynParamValue","UseReceiptDate") EQ "YES"
        lUseShipDate = DYNAMIC-FUNCTION("fGetDynParamValue","UseShipDate") EQ "YES"
        .
END PROCEDURE.
