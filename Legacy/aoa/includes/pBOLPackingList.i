/* pBOLPackingList.i - auto generated 11.03.2016 @ 12:46:32 am from aoa/aoaParam.w */

    {aoa/includes/aoaInputDefParams.i}

    /* parameter values loaded into these variables */
    DEFINE VARIABLE lCustList AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lAllCustNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cStartCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cEndCustNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllBOL AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndBOL AS INTEGER NO-UNDO.
    DEFINE VARIABLE lAllOrderNo AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iStartOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndOrderNo AS INTEGER NO-UNDO.
    DEFINE VARIABLE dtStartBOLDate AS DATE NO-UNDO.
    DEFINE VARIABLE cStartBOLDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dtEndBOLDate AS DATE NO-UNDO.
    DEFINE VARIABLE cEndBOLDateOption AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPrinter AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSecure AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cAvailableColumns AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cSelectedColumns AS CHARACTER NO-UNDO.

    /* locate parameter values record */
    RUN pGetParamValues (ipcCompany, "bolpcklst.", ipcUserID, ipiBatch).

    /* load parameter values from above record into variables */
    ASSIGN
        lCustList = DYNAMIC-FUNCTION("fGetParamValue","svCustList") EQ "yes"
        lAllCustNo = DYNAMIC-FUNCTION("fGetParamValue","svAllCustNo") EQ "yes"
        cStartCustNo = DYNAMIC-FUNCTION("fGetParamValue","svStartCustNo")
        cEndCustNo = DYNAMIC-FUNCTION("fGetParamValue","svEndCustNo")
        lAllBOL = DYNAMIC-FUNCTION("fGetParamValue","svAllBOL") EQ "yes"
        iStartBOL = DYNAMIC-FUNCTION("fGetParamValue","svStartBOL")
        iEndBOL = DYNAMIC-FUNCTION("fGetParamValue","svEndBOL")
        lAllOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svAllOrderNo") EQ "yes"
        iStartOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svStartOrderNo")
        iEndOrderNo = DYNAMIC-FUNCTION("fGetParamValue","svEndOrderNo")
        dtStartBOLDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svStartBOLDate"))
        cStartBOLDateOption = DYNAMIC-FUNCTION("fGetParamValue","svStartBOLDateOption")
        dtStartBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cStartBOLDateOption,dtStartBOLDate)
        dtEndBOLDate = DATE(DYNAMIC-FUNCTION("fGetParamValue","svEndBOLDate"))
        cEndBOLDateOption = DYNAMIC-FUNCTION("fGetParamValue","svEndBOLDateOption")
        dtEndBOLDate = DYNAMIC-FUNCTION("fDateOptionDate",cEndBOLDateOption,dtEndBOLDate)
        cPrinter = DYNAMIC-FUNCTION("fGetParamValue","svPrinter")
        lSecure = DYNAMIC-FUNCTION("fGetParamValue","svSecure") EQ "yes"
        cAvailableColumns = DYNAMIC-FUNCTION("fGetParamValue","svAvailableColumns")
        cSelectedColumns = DYNAMIC-FUNCTION("fGetParamValue","svSelectedColumns")
        .

    RUN pGetColumns (TEMP-TABLE ttBOLPackingList:HANDLE, cAvailableColumns, cSelectedColumns).

    IF lAllCustNo THEN
    ASSIGN
        cStartCustNo = CHR(32)
        cEndCustNo   = CHR(254)
        .

    IF lAllBOL THEN
    ASSIGN
        iStartBOL = 0
        iEndBOL   = 99999999
        .

    IF lAllOrderNo THEN
    ASSIGN
        iStartOrderNo = 0
        iEndOrderNo   = 99999999
        .

    IF lCustList THEN
    RUN pBuildCustList (ipcCompany, "OS9", OUTPUT cStartCustNo, OUTPUT cEndCustNo, OUTPUT lCustList).
