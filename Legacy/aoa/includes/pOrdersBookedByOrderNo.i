/* pOrdersBookedByOrderNo.i - auto generated 11.03.2016 @ 12:47:58 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllItemNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndItemNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndOrderDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndOrderDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtStartReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndReceiptDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndReceiptDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUseReceiptDate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE dtStartShipDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartShipDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndShipDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndShipDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lUseShipDate AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cPrintOrderedRemaining AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lPrintMiscCharges AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lPrintContribution AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "r-booko#.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lAllOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svAllOrderNo") EQ "yes"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderNo")
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lAllItemNo = DYNAMIC-FUNCTION("fGetParamValue","svAllItemNo") EQ "yes"
        cStartItemNo = DYNAMIC-FUNCTION("fGetParamValue","svStartItemNo")
        cEndItemNo = DYNAMIC-FUNCTION("fGetParamValue","svEndItemNo")
        dtStartOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDate"))
        cStartOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderDateOption")
        dtStartOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartOrderDateOption,dtStartOrderDate)
        dtEndOrderDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDate"))
        cEndOrderDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderDateOption")
        dtEndOrderDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndOrderDateOption,dtEndOrderDate)
        dtStartReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDate"))
        cStartReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartReceiptDateOption")
        dtStartReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartReceiptDateOption,dtStartReceiptDate)
        dtEndReceiptDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDate"))
        cEndReceiptDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndReceiptDateOption")
        dtEndReceiptDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndReceiptDateOption,dtEndReceiptDate)
        lUseReceiptDate = DYNAMIC-FUNCTION("fGetParamValue","svUseReceiptDate") EQ "yes"
        dtStartShipDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartShipDate"))
        cStartShipDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartShipDateOption")
        dtStartShipDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartShipDateOption,dtStartShipDate)
        dtEndShipDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndShipDate"))
        cEndShipDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndShipDateOption")
        dtEndShipDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndShipDateOption,dtEndShipDate)
        lUseShipDate = DYNAMIC-FUNCTION("fGetParamValue","svUseShipDate") EQ "yes"
        cPrintOrderedRemaining = DYNAMIC-FUNCTION("fGetParamValue","svPrintOrderedRemaining")
        lPrintMiscCharges = DYNAMIC-FUNCTION("fGetParamValue","svPrintMiscCharges") EQ "yes"
        lPrintContribution = DYNAMIC-FUNCTION("fGetParamValue","svPrintContribution") EQ "yes"
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttOrdersBookedByOrderNo:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllOrderNo THEN
    ASSIGN
        iStartOrderNo = 0
        iEndOrderNo   = 99999999
        .

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllItemNo THEN
    ASSIGN
        cStartItemNo = CHR(32)
        cEndItemNo   = CHR(254)
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "OR11", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
